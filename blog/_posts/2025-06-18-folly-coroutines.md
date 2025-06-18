---
layout: post
title: "Folly Coroutines"
tags: [cpp]
excerpt_separator: <!--more-->
vanity: "2025-06-18-folly-coroutines"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/folly-logo.svg" alt="Folly Logo" />
</figure>

In this post we'd like to discuss Folly coroutines. At a high-level, coroutines are a syntax sugar to Future and SemiFutures, which in turn are mechanisms for implementing asynchronous execution.

<!--more-->

If you're not familiar with [Folly Futures]({{blog}}/2025/05/02/folly-futures.html) or [Folly executors]({{blog}}/2025/06/07/folly-executors.html), it's highly recommended to read about them first.

Some familiarity with coroutines in other languages such as Python will help with the syntax but it's not at all required.

## Callback Hell

One problem with futures is the so called "callback hell", in which we chain several futures or semi-futures. Here's a contrived example:

{% highlight cpp %}
#include <folly/futures/Future.h>

folly::SemiFuture<int> value(int x) {
  return folly::makeSemiFuture().deferValue([x](auto) { return x; });
}

folly::SemiFuture<int> slow() {
  return folly::makeSemiFuture().deferValue([](auto) {
    return folly::futures::sleep(std::chrono::seconds{1})
        .deferValue([](auto) { return value(10); })
        .deferValue([](auto value) { return value + 1; });
  });
}

// Wait for the Future to finish
int result = slow().via(folly::getGlobalCPUExecutor()).get();
std::cout << result << std::endl;
{% endhighlight %}

The corresponding version using coroutines is:


{% highlight cpp %}
#include <folly/coro/BlockingWait.h>
#include <folly/coro/Task.h>
#include <folly/futures/Future.h>

folly::coro::Task<int> co_value(int x) {
  co_return x;
}

folly::coro::Task<int> co_slow() {
  co_await folly::futures::sleep(std::chrono::seconds{1});
  co_return co_await co_value(10) + 1;
}

// Wait for the Future to finish
int co_result = folly::coro::blockingWait(
    co_withExecutor(folly::getGlobalCPUExecutor(), co_slow()));
std::cout << result << std::endl;
{% endhighlight %}

As we can see, the version using coroutines is significantly easier to read and understand. Let's dive into more details on coroutines.

## Coroutine

A function is a **coroutine** if it has one of the keywords `co_await`, `co_return` or `co_yield`. In such cases the return type is `folly::coro::Task<T>`.

{% highlight cpp %}
folly::coro::Task<int> co_value(int x) {
  co_return x;
}
{% endhighlight %}

If the coroutine does not return anything we can do:

{% highlight cpp %}
folly::coro::Task<folly::Unit> empty() {
    co_return;
}
{% endhighlight %}

Note that in a normal function, a `return;` at the end is optional, but in a coroutine it must be specified, otherwise there's no way for the compiler to tell it's a coroutine.

Speaking of returning, we cannot use `return` in a coroutine, that is, we cannot mix it with the `co_*` keywords.

## SemiFutures

Coroutines and `SemiFuture`s are interoperable. A coroutine can seamlessly `await` functions that return `SemiFuture`, as we can see in our initial example: the function `folly::futures::sleep(std::chrono::seconds{1})` actually returns a `SemiFuture` but we can still "await" it:

{% highlight cpp %}
co_await folly::futures::sleep(std::chrono::seconds{1});
{% endhighlight %}

Conversely, we can convert a `folly::Task` into a `SemiFuture`:

{% highlight cpp %}
folly::SemiFuture<int> to_semi() {
  return std::move(co_slow()).semi();
}
{% endhighlight %}

## Executor

A `Task` is the analogous to a `SemiFuture` in the sense that they do not have an executor [2] attached and cannot be run as is. When we attach an executor to a `SemiFuture` we obtain a `Future`. When we do the same for `Task` we get a `TaskWithExecutor`.

We can `co_await` a `TaskWithExecutor` the same way we do with a `Task`. So in theory we can work with the former everywhere and not have to attach an executor at the end, like:

{% highlight cpp %}
folly::coro::TaskWithExecutor<int> co_slow_with_executor();

int result = folly::coro::blockingWait(
    co_slow_with_executor()
);
{% endhighlight %}

But in general it's preferrable to work with `SemiFuture` and `Task` to provide more flexibility in how they end up being executed.

Recall that an executor is a simple interface that has a `.add(fn)` method taking a callback `fn`, and decides when and where to run it. From the perspective of a coroutine all the matters is the `.add()` method.

To get an intuition about how coroutine code gets scheduled in an executor, we can write our custom executor that looks like `InlineExecutor`, except that it prints when something is added to it:

{% highlight cpp %}
#include <folly/Executor.h>

namespace folly {
class MyExecutor : public folly::Executor {
 public:
  virtual ~MyExecutor() override {}

  void add(Func f) override {
    std::cout << "adding" << std::endl;
    f();
  }
};
} // namespace folly
{% endhighlight %}

We can then use this executor as:

{% highlight cpp %}
folly::MyExecutor myExec;
auto exec = folly::getKeepAliveToken(myExec);
{% endhighlight %}

with either `SemiFuture` or coroutines. For the semi-future case, we'll see it prints `"adding"` as many times as we call `.deferValue()`, 4 times in our example, but for the coroutine case it only prints twice. The reason is that the low-level coroutine machinery is able to combine "callbacks" for efficiency. Noting this is independent from which executor is used.

## Lifetimes

One cumbersome aspect of `SemiFuture::deferValue()` is that we need to manually pass scope around. For example, if we want to pass an object `Context` to the callbacks in the `SemiFuture` example we can do:

{% highlight cpp %}
struct Context {
  int x;
  int y;
};

folly::SemiFuture<int> slow_with_context() {
  Context ctx {10, 20};
  return folly::makeSemiFuture().deferValue([](auto) {
    return folly::futures::sleep(std::chrono::seconds{1})
        .deferValue([ctx](auto) { return value(ctx.x); })
        .deferValue([ctx](auto value) { return value + ctx.y; });
  });
}
{% endhighlight %}

The problem here is that we're making a lot of copies of `Context` (when capturing them by value in the callback). Due to the `SemiFuture` machinery it actually produces more than just the 2 apparent copies. In my test it called the copy constructor dozens of times.

We could try getting around that by capturing the context by reference, but by the time the callback is run, `ctx` is long out of scope. We can't use a `std::unique_ptr` easily because we need it in two callbacks (we could have the first one return it to the next). The ultimate solution is to use a `std::shared_ptr`.

In coroutines this process is much simpler. We can simply do:

{% highlight cpp %}
folly::coro::Task<int> co_slow() {
  auto ctx = Context(10, 20);
  co_await folly::futures::sleep(std::chrono::seconds{1});
  co_return co_await co_value(ctx.x) + ctx.y;
}
{% endhighlight %}

The coroutine machinery is able to create `ctx` in the coroutine stack so even after we resume from the first `co_await`, it is still in scope. And this process does not involve copying `Context`, making it easier to reason about asynchronous code.

The one scenario where the illusion might break is if we take in a reference as parameter.

{% highlight cpp %}
folly::coro::Task<int> co_reference(const Context& ctx) {
  co_await folly::futures::sleep(std::chrono::seconds{1});
  co_return co_await co_value(ctx.x) + ctx.y;
}
{% endhighlight %}

It's possible that `ctx` was valid by the time we called this callback and perhaps even when we did `co_await co_value(ctx.x)`, but it could have been deleted by the time we resumed and called `ctx.y`. This wouldn't happen in synchronous code.

As a general practice, one should not depend on references or raw pointers inside coroutines.


## Parallelism

Using coroutine as synchronous code doesn't bring much benefits, since no matter which executors we'll be using, we'll wait until a callback finishes executing before scheduling the next one.

As an example, suppose we have:

{% highlight cpp %}
folly::coro::Task<int> co_add() {
  int x = co_await co_value(10);
  int y = co_await co_value(20);
  co_return x + y;
}
{% endhighlight %}

Here, we'll not schedule the second `co_await` until `x` is assigned to.

Like in Python coroutines [3], the benefits appear when we can schedule multiple callbacks at the same time (in Python it can be done via `asyncio.gather()`). The Folly equivalent is `folly::coro::collectAll()`, which we can use in the example above:

{% highlight cpp %}
folly::coro::Task<int> co_add() {
  auto [x, y] = folly::coro::collectAll(
    co_value(10),
    co_value(20)
  );
  co_return x + y;
}
{% endhighlight %}

If we use `InlineExecutor`, we'll still run one coroutine after the other, but if we use `IOThreadPoolExecutor`, it would more likely run them in parallel.

Worth noting that in Python we can't run multiple threads at the same time, so running coroutines at the same time means we run them concurrently, but in C++ we're actually able to run them in parallel.


## Mixing Async vs. Non-Async Code

In continuing the analogy with Python, one of the annoying parts of working with async code is mixing it with non-async code. In particular when non-async code is sandwiched between async code, for example:

{% highlight python %}
async def h():
  return 42

def g():
  x = asyncio.run(h())
  return x

async def f():
  x = g()
  print(x)
{% endhighlight %}

This doesn't work if `f()` is already running inside an event loop because Python only allows one event loop per thread. This issue is generally known as [colored vs. uncolored functions](https://langdev.stackexchange.com/questions/3430/colored-vs-uncolored-functions).

There are several ways to address this, depending largely on how easy it is to change the sandwiched non-async code, but we'll not delve into it here. In Folly coroutines we don't have the constraint of having to run all the coroutines in the same executor, so an analogous implementation would be:

{% highlight python %}
folly::coro::Task<int> h() {
  co_return 42;
}

void g() {
  int x = folly::coro::blockingWait(h());
  return x;
}

folly::coro::Task<int> f() {
  int x = g();
  std::cout << x << std::endl;
}
{% endhighlight %}


Without arguments, `folly::coro::blockingWait()` uses a custom executor, `BlockingWaitExecutor`, which runs the callback in the same thread.

In this example, this works even if we schedule `f()` on say, `CPUThreadPoolExecutor`, but I've seen cases where mixing executors made the callback from `h()` hang, though I'm not sure why.

If we want to use the same executor, we can obtain the executor inside a coroutine via `co_await folly::coro::co_current_executor`. So we could get it in `f()` and pass it explicitly to `g()`:


{% highlight python %}
void g(folly::Executor::KeepAlive<> executor) {
  int x = folly::coro::blockingWait(
    folly::coro::co_withExecutor(std::move(executor), h()));
  return x;
}

folly::coro::Task<int> f() {
  folly::Executor::KeepAlive<> executor =
    co_await folly::coro::co_current_executor;
  int x = g(executor);
  std::cout << x << std::endl;
}
{% endhighlight %}

Unfortunately, this requires a signature change in `g()` and, in a more realistic scenario, all functions between the calling coroutine and the one eventually issuing `blockingWait()`, but with default parameters, it might still be more feasible than converting them all to coroutines.

## Exceptions

When discussing Folly futures [1], we mentioned:

> A pitfall with this API is that it doesn’t handle nor propagate exceptions, it ignores it.

Meaning that we need to explicitly register a handle for exceptions via `thenError()`. In coroutines the exception is propagated as we'd expect.

{% highlight cpp %}
folly::coro::Task<int> co_slow() {
  co_await folly::futures::sleep(std::chrono::seconds{1});
  co_return co_await co_value(10) + 1;
}

folly::coro::Task<int> co_slow2() {
  try {
    co_return co_await co_slow();
  } catch (const std::exception& e) {
    std::cout << "caught exception: " << e.what() << std::endl;
    throw;
  }
}
{% endhighlight %}

We can do the usual try/catch, rethrow, etc.

## Conclusion

I feel like a have a much better understanding of Folly coroutines after having studied [Folly Futures]({{blog}}/2025/05/02/folly-futures.html), [Folly executors]({{blog}}/2025/06/07/folly-executors.html) and now the Folly coroutines themselves.

This is in contrast with my unsuccessful attempt at starting from first principles and studying [C++ coroutines]({{blog}}/2024/06/03/coroutines-in-cpp.html). I still don't have a good grasp on them, but have abandoned the plan for now.

I still haven't studied some aspects of coroutines such as cancellations, limiting concurrency, etc. but if the opportunity arises I can write about them.

## References

* [[1]({{blog}}/2025/05/02/folly-futures.html)] NP-Incompleteness: Folly Futures
* [[2]({{blog}}/2025/06/07/folly-executors.html)] NP-Incompleteness: Folly Executors
* [[3]({{blog}}/2020/02/02/python-coroutines.html)] NP-Incompleteness: Python Coroutines
