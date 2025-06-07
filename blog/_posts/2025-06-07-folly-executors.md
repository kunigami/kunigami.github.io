---
layout: post
title: "Folly Executors"
tags: [c++]
excerpt_separator: <!--more-->
vanity: "2025-06-07-folly-executors"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/folly-logo.svg" alt="Folly Logo" />
</figure>

In a previous post we discussed [Folly futures]({{blog}}/2025/05/02/folly-futures.html) where we introduced semi-futures and executors. Then in [Asynchronous I/O Overview]({{blog}}/2025/05/16/async-io.html) among other things we covered the libevent library.

In this post we want to delve into the executors. One of them being the IO executor which leverages libevent. So while reading those two posts is not strictly required, it's highly recommended for the bigger picture.

<!--more-->

At a high-level an executor is like a scheduler that receives lambda functions and decides when to run them. An executor is a class that implements this simple interface:

{% highlight cpp %}
class Executor {
  void add(folly::Function<void()>) = 0
};
{% endhighlight %}

As we can see it has a lot of leeway to do whatever it wants. Let's cover a few types of executors.

## InlineExecutor

The inline executor is the simplest type of executor, which doesn't do any scheduling: it just runs the function right away, on the same thread:

{% highlight cpp %}
class InlineExecutor {
  void add(Func f) override { f(); }
};
{% endhighlight %}

So it's basically a no-op executor. This is also the executor associated with the `Future`when we get it from a `Promise` via `Promise::getFuture()` [2]. Here's a simple example:

{% highlight cpp %}
#include "folly/executors/InlineExecutor.h"

auto& exec = folly::InlineExecutor::instance();
std::cout << "before\n";
exec.add([]() { std::cout << "during\n"; });
std::cout << "after\n";
{% endhighlight %}

This prints *before*, *during* and *after*. It is possible for the callback to also call `.add()` and schedule others callbacks. In this case they're executed as soon as they're scheduled.

It is very easy to understand but not very useful in general. The most useful ones are `CPUThreadPoolExecutor` and `IOThreadPoolExecutor`, which we cover next.

## CPUThreadPoolExecutor

The `CPUThreadPoolExecutor` has a thread pool of size `N`. Whenever we schedule a callback via `.add()`, this executor adds to a priority queue with a default priority. It's possible to specify the priority by calling `.addWithPriority()`.

A worker thread will then try to get a task from that queue and execute it to completion. As we discussed in the previous section, the execution of a function might cause new entries to be added to the end of the queue.

<figure class="center_children">
  <img src="{{resources_path}}/cpu-executor.svg" alt="See caption." width="500" />
  <figcaption>Figure 1: The CPUThreadPoolExecutor has a thread-safe priority queue from which multiple threads (workers) can pull events from and execute.</figcaption>
</figure>


## IOThreadPoolExecutor

Like the CPU counterpart, the `IOThreadPoolExecutor` also has a thread pool of size `N`. Typically `N` is the number of CPU cores available in the system. Each of these threads is running an async event loop via the `libevent` library, as we covered in [Asynchronous I/O Overview
]({{blog}}/2025/05/16/async-io.html).

When we call `.add()`, it picks a thread from the pool in a round-robin fashion, but the picking is sticky: if will pick the same thread for the same calling thread (it basically memoizes which thread was picked for this current thread).

Then it adds the callback to a queue on that thread, the event loop will then get tasks from that queue to execute.

<figure class="center_children">
  <img src="{{resources_path}}/io-executor.svg" alt="See caption." width="500" />
  <figcaption>Figure 2: The IOThreadPoolExecutor chooses a thread to schedule the callback on. Each thread has its own queue and an async event loop.</figcaption>
</figure>

### Example

I find an example makes it a lot easier to grok the `IOThreadPoolExecutor`. We'll basically have a function that sleeps for a specified amount of time and when it wakes up we want to execute a callback.

In Linux, we can use `timerfd.h`, which essentially puts a thread to sleep for a period of time and exposes a file descriptor (hence `-fd`) that can be used for polling. It's one of the simplest ways to simulate I/O.

Folly has a class called `folly::TimerFD` that wraps `timerfd.h`, and registers a callback, `folly::TimerFD::onTimeout()`, to libevent's async loop, itself wrapped by the class `folly::EventBase`.

In our example, we'll use `folly::Future` since it's one of the most common uses for Folly executors (see [2] for details). The first thing we have to do is define a wrapper class that returns a `folly::SemiFuture`:

{% highlight cpp %}
#include <folly/io/async/TimerFD.h>

struct PromiseTimer : public folly::TimerFD {
  PromiseTimer(folly::EventBase* evb) : TimerFD(evb) {
    promise_ = folly::Promise<folly::Unit>();
  }

  folly::SemiFuture<folly::Unit> schedule(
    std::chrono::microseconds timeout
  ) {
    folly::TimerFD::schedule(timeout);
    return promise_.getSemiFuture();
  }

  void onTimeout() noexcept override {
    promise_.setValue();
  }

  folly::Promise<folly::Unit> promise_;
};
{% endhighlight %}

When we call `schedule(timeout)` on `folly::TimerFD`, it will create an instance of `timerfd`, get a file descriptor back, associate it with the `onTimeout()` callback and then add it to the monitoring list of libevent (it does this via `folly::EventBase`). When a change happens to the file descriptor (in this case when `timeout` amount of time is elapsed), `onTimeout()` will be invoked.

`PromiseTimer` wraps `schedule()` to return a `SemiFuture` linked to an internal promise (again, see [2] for details) and when `onTimeout()` is invoked, we fulfill the promise by calling `.setValue()`.

A caller could look like:

{% highlight cpp %}
#include <folly/executors/GlobalExecutor.h>

PromiseTimer timer(folly::getGlobalIOExecutor()->getEventBase());

timer.schedule(std::chrono::seconds(10))
    .via(folly::getGlobalIOExecutor().get())
    .thenValue([](auto&&) { std::cout << "done" << std::endl; })
    .get();
{% endhighlight %}

Note that we'll use the same `folly::EventBase` instance for both scheduling `folly::TimerFD` and running the callback of `.thenValue()`.

When we fulfill the promise, the callback of `.thenValue()` will be added to `IOThreadPoolExecutor`, which will pick a thread, and add to its queue. Eventually the event loop (`folly::EventBase`) will process it.

### Why Is It Useful?

It's not immediately obvious why `IOThreadPoolExecutor` is useful say, compared to the simpler `CPUThreadPoolExecutor`, so let's discuss in more details.

If we look at the previous example `.thenValue()` has nothing to do with file descriptors or monitoring for them via libevent. Sure, we could come up with another example where we schedule another timer inside the callback and return another promise, but the registering of the file descriptor + callback on `folly::EventBase` would be done by `folly::TimerFD` itself, not by `IOThreadPoolExecutor`.

To me, the biggest insight is is that `folly::EventBase` is also able to handle non-IO functions. The major benefit of running non-IO functions in the same `folly::EventBase` is that it can better coordinate between IO and non-IO operations since it has a "view" of all of them.

More specifically, `folly::EventBase` has discretion when to process events from its queue. It might decide to check for any file descriptors being ready first (via libevent). If there are any, it will process their callbacks (e.g. `onTimeout()`). If there are none, it can process a function from its queue before checking again.

## Conclusion

I spent a long time trying to get a good understanding of `IOThreadPoolExecutor`. It prompted me to study [Asynchronous I/O Overview]({{blog}}/2025/05/16/async-io.html), but even then it wasn't clear how futures and libevent were connected.

There were a few times I thought I had understood it until something didn't quite make sense. But I think I'm finally at a point that things "clicked" and it's a very satisfying feeling.

## Appendix

### Installation

In order to follow along the examples, you might want to install Folly in your operating system. I had a lot of difficulty building it from source on MacOS and gave up, but it should be available via [homebrew]({{docs}}/macos/homebrew.html) and it will be downloaded as a shared library (either for [static or dynamic linking]({{blog}}/2025/04/25/shared-libraries.html)).

I was able to install it from source on a Ubuntu system (see *Folly* in [C++ External Libraries Cheatsheet
]({{docs}}/cpp/external_libs.html)) and all examples in this post have only been tested here.

## References

* [[1]({{blog}}/2025/05/16/async-io.html)] NP-Incompleteness: Asynchronous I/O Overview
* [[2]({{blog}}/2025/05/02/folly-futures.html)] NP-Incompleteness: Folly Futures
* [[3]](https://github.com/facebook/folly) Github - facebook/folly
"
