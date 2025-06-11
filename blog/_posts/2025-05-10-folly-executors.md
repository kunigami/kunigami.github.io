---
layout: post
title: "Folly Executors"
tags: [c++]
excerpt_separator: <!--more-->
vanity: "2025-05-02-folly-futures"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/folly-logo.svg" alt="Folly Logo" />
</figure>

In our last post we discussed Folly futures and in the process also introduced semi-futures and executors. In this post we want to delve into the executors.

Reading that post is recommended but not required, we'll mostly discuss executors in isolation but understanding Folly futures helps with some context.

<!--more-->

## Executors

At a high-level an executor is like a scheduler that receives lambda functions and decides when to run them. An executor is a class that implements this simple interface:

{% highlight cpp %}
class Executor {
  void add(folly::Function<void()>) = 0
};
{% endhighlight %}

As we can see it has a lot of leeway to do whatever it wants. Let's cover a few types of executors.

### InlineExecutor

The inline executor is the simplest type of executor, which doesn't do any scheduling: it just runs the function right away, on the same thread:

{% highlight cpp %}
class InlineExecutor {
  void add(Func f) override { f(); }
};
{% endhighlight %}

This is the executor associated with the `Future`when we get it from a `Promise`.

We can test this with a simple example:

{% highlight cpp %}
#include "folly/executors/InlineExecutor.h"

auto& exec = folly::InlineExecutor::instance();
std::cout << "before\n";
exec.add([]() { std::cout << "during\n"; });
std::cout << "after\n";
{% endhighlight %}

This prints *before*, *during* and *after*. It is possible for the callback to also call `.add()` and schedule others callbacks. In this case they're executed as soon as they're scheduled.

Another approach is to use a queue so

### IO Thread Pool

* threads = cpu count

* eventfd - communication between threads
* epoll
    * epoll_wait()


TODO: what happens when a thread blocks

## Appendix

### Installation

In order to follow along the examples, you might want to install Folly in your operating system. I had a lot of difficulty building it from source on MacOS and gave up, but it should be available via [homebrew]({{docs}}/macos/homebrew.html) and it will be downloaded as a shared library (either for [static or dynamic linking]({{blog}}/2025/04/25/shared-libraries.html)).

I was able to install it from source on a Ubuntu system (see *Folly* in [C++ External Libraries Cheatsheet
]({{docs}}/cpp/external_libs.html)) and all examples in this post have only been tested here.
