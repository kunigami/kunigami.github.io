---
layout: post
title: "Folly Futures"
tags: [c++]
excerpt_separator: <!--more-->
vanity: "2025-05-02-folly-futures"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/folly-logo.svg" alt="Folly Logo" />
</figure>

In this post I wish to cover futures in the [Folly library](https://github.com/facebook/folly) [2]. I've discussed C++ futures in a [previous post]({{blog}}/2023/06/08/futures-in-cpp.html), but Folly futures are slightly different, so this post aims to cover the differences. It's high recommended to familiarize yourself with C++ futures first.

The reason for studying Folly futures is because I want to study Folly coroutines and they make use of Folly futures and related mechanims such as executors, so I thought it would be beneficial to cover them separately.

<!--more-->

Throughout the post, an unqualified *future* refers to the Folly one and the C++ will always be qualified. If you wish to run the code along the post, check *Installation* in the *Appendix*.

## Folly Futures vs. C++ Futures

Much like with the [C++ futures]({{blog}}/2023/06/08/futures-in-cpp.html), Folly offers the pair promise + future that can be used as a one-way communication channel. Our example with C++ future was:

{% highlight cpp %}
#include <iostream>
#include <future>

int main() {
  std::promise<int> prom;

  // get a handle to the (future) result of the promise
  std::future<int> fut = prom.get_future();

  // run some computation in another thread
  std::thread t([&]() {
      // set the result on the future explicitly
      prom.set_value(42);
  });

  // blocks until there's data in fut
  auto result = fut.get();
  std::cout << result << std::endl;
  t.join();
}
{% endhighlight %}

The corresponding example in Folly would look like:

{% highlight cpp %}
#include <iostream>
#include <folly/futures/Promise.h>

int main() {
  folly::Promise<int> prom;

  // get a handle to the (future) result of the promise
  folly::Future<int> fut = prom.getFuture();

  // run some computation in another thread
  std::thread t([&]() {
      // set the result on the future explicitly
      prom.setValue(42);
  });

  // blocks until there's data in fut
  auto result = std::move(fut).get();
  std::cout << result << std::endl;
  t.join();
}
{% endhighlight %}

Almost everything looks the same with naming differences (`std::promise` -> `folly::Promise`, `set_value()` -> `setValue()`).

The major difference is that `get()` can only be called on a [rvalue reference]({{blog}}/2022/03/01/moving-semantics-cpp.html) which in turn requires us to `std::move()` the future. This is useful because we're only supposed to retrieve the value from a future once, and by forcing it to be moved, we don't have access to `fut` anymore and thus wouldn't call `get()` again by mistake.

Folly futures also have a non-blocking and non-moving from way to access the value, `value()`, however this requires the promise to be fulfilled and throws an exception otherwise.

### Check if future has been consumed

In our post about C++ futures [1] we mentioned the method `valid()`, which determines whether the future has been consumed. The exact same method exists in Folly futures, but it has a slightly different meaning: it returns `true` as long as the future hasn't been moved.

This happens to coincide with a call to `get()` because as we saw above it requires the future to be moved:

{% highlight cpp %}
folly::Promise<int> p;
folly::Future<int> f = p.getFuture();

assert(f.valid());
p.setValue(1);
assert(f.valid());
assert(std::move(f).get() == 1);
assert(!f.valid()); // not valid anymore
{% endhighlight %}

However, as we saw in the previous section, there's the non-blocking `value()` API which doesn't require the future to be moved, so it doesn't invalidate the future:

{% highlight cpp %}
folly::Promise<int> p;
folly::Future<int> f = p.getFuture();

p.setValue(1);
assert(f.value() == 1);
assert(f.valid()); // continues to be valid
{% endhighlight %}


### Non-blocking check of future readiness

We mentioned in [1] that there's no simple API to determine whether a C++ future has its value set. The workaround we mentioned was to use `wait_until()` with a `0` wait:

{% highlight cpp %}
std::promise<int> p;
std::future<int> f = p.get_future();
p.set_value(42);
assert(f.wait_until(seconds(0)) == std::future_status::ready);
{% endhighlight %}

Folly futures has such an API, `hasValue()`:

{% highlight cpp %}
folly::Promise<int> p;
folly::Future<int> f = prom.getFuture();
p.setValue(42);
assert(f.hasValue());
{% endhighlight %}

Note that `hasValue()` only returns `true` if it "succeeded", i.e. if an exception was set, it would return false. To account for either value or exception, the method `isReady()` should be used.

### Handling exceptions

In C++ promises we can set an exception in a promise [1], which is then rethrown when calling `get()`:

{% highlight cpp %}
std::promise<int> p;
auto fut = p.get_future();

try {
    throw std::runtime_error("Error");
} catch(...) {
    p.set_exception(std::current_exception());
}

assert(fut.valid());

try {
    std::cout << fut.get() << std::endl;
} catch (const std::exception& e) {
    // "Error"
    std::cout << e.what() << std::endl;
}
{% endhighlight %}

The corresponding version for Folly futures is almost the same:

{% highlight cpp %}
folly::Promise<int> p;
folly::Future<int> f = p.getFuture();

try {
  throw std::runtime_error("Error");
} catch(...) {
  p.setException(folly::exception_wrapper(std::current_exception()));
}

assert(f.valid());
assert(f.isReady());
assert(!f.hasValue());

try {
  std::cout << std::move(f).get() << std::endl;
} catch (const std::exception& e) {
  // "Error"
  std::cout << e.what() << std::endl;
}
{% endhighlight %}

The major difference is that we need to wrap an exception into a `folly::exception_wrapper()`.

### Continuations

In [1], when comparing C++ promises with JavaScript promises, we noticed the lack of the `.then()` API, technically called a **continuation**, which allows us to register a callback that is invoked when the promise sets a value. We provided our own version using threads, but in Folly future such mechanism already exists via `.thenValue()`:

{% highlight cpp %}
folly::Promise<int> p;
auto t = std::thread([&] {
  promise.setValue(42);
});

folly::Future<int> f = p.getFuture();

std::move(f).thenValue(
  [](int x) { std::cout << x << std::endl; }
);

t.join();
{% endhighlight %}

So here the callback provided to `.thenValue()` is invoked when `f` has a value. Note that like with `.get()`, we need to move from the future to avoid reusing it by accident. Also not that if the future already has a value by the time we call `.thenValue()` it is executed right away.

A pitfall with this API is that it doesn't handle nor propagate exceptions, it ignores it. To handle exceptions we need to explicitly register a `.thenError()`:

{% highlight cpp %}
std::move(f).thenValue(
  [](int x) { std::cout << x << std::endl; }
).thenError(
  [](folly::exception_wrapper&& e) { std::cout << e.what() << std::endl; }
);
{% endhighlight %}

The return of the callback passed to `.thenValue()` is wrapped into a promise, so you can retrieve the result from it:

{% highlight cpp %}
auto f2 = std::move(f).thenValue(
  [](int x) { return x + 1; }
);
std::move(f2).get(); // 43
{% endhighlight %}

And because the return is a future, we can call `.thenValue()` on it, effectively obtaining the "promise chain" we see in JavaScript:

{% highlight cpp %}
auto f2 = std::move(f).thenValue(
  [](int x) { return x + 1; }
).thenValue(
  [](int y) { return y * 2; }
);
std::move(f2).get(); // 86
{% endhighlight %}

This example is not very useful given we could combine code into a single callback:

{% highlight cpp %}
auto f2 = std::move(f).thenValue(
  [](int x) {
    int y = x + 1;
    return y * 2;
  }
);
std::move(f2).get(); // 86
{% endhighlight %}

We'll see eventuall learn how to make use of this when we talk about executors, but for now, know the syntax exists.

## Executors and Semi-futures

In Folly, futures have an associated **executor**, which dictates when and how the future is executed. In C++ futures, an analogous mechanism is provided by the `async` function [1]. When we do:

{% highlight cpp %}
#include <iostream>
#include <future>
#include <unistd.h>

int fn() {
  std::cout << "calling fn()" << std::endl;
  return 42;
}

std::future<int> f = std::async(fn);
std::cout << "called async" << std::endl;

sleep(3);
int r = f.get();
std::cout << "result = " << r << std::endl;

{% endhighlight %}

By default the `std::async` function can determine whether to run `fn()` in a separate thread (`std::launch::async`) or defer it until we try to get a value on the future (`std::launch::deferred`). We can be explicit by passing either of the enums:

{% highlight cpp %}
std::future<int> f = std::async(std::launch::async, fn);
{% endhighlight %}

In this example the function is executed right away in a separate thread. We'll see `"called async"`, `"calling fn()"` and after a pause `"result = 42"`. If we use the deferred mode:

{% highlight cpp %}
std::future<int> f = std::async(std::launch::deferred, fn);
{% endhighlight %}

Function `fn()` won't get executed until we call `f.get()`, so we'll see `"called async"`, a pause and then `"calling fn()"` and `"result = 42"`. Folly futures offer a similar mechanism but instead of the scheduling being done by an external function like `std::async()` it's done by the executor which is associated with the future.

A future without an executor is called a **semi-future** (class `SemiFuture`), or conversely a future is a semi-future + an executor. A future can be obtained from a semi-future via `.via()`:

{% highlight cpp %}
folly::SemiFuture<int> semiFut = folly::makeSemiFuture(1);
folly::Future<int> fut =
  std::move(semiFut).via(folly::getIOExecutor().get());
{% endhighlight %}

We'll cover `folly::getIOExecutor()` and other executors in a separate post, but the gist is that `.via()` takes an instance of an executor, which is any class implementing:

{% highlight cpp %}
class Executor {
  void add(folly::Function<void()>) = 0
};
{% endhighlight %}

Here `folly::Function` is analogous to `std::function` with some minor differences. An executor is typically a singleton so futures using the same executor share the same executor instance. Whenever we pass a callback to `.thenValue()` on a future, it calls the `add()` method of the corresponding executor. The executor has discretion on when to run the callbacks.

The one that is attached to `Promise::getFuture()` is the `InlineExecutor`:

{% highlight cpp %}
template <class T>
Future<T> Promise<T>::getFuture() {
  return getSemiFuture().via(&InlineExecutor::instance());
}
{% endhighlight %}

This is the simplest possible executor and it runs the function as soon as it is added to it:

{% highlight cpp %}
class InlineExecutor {
  void add(Func f) override { f(); }
};
{% endhighlight %}

## Conclusion

In this post we learned about Folly futures. I've opted to cover it in reference to C++ futures. I really like to learn things this way, it both reduces the cogntive effort because I only need to learn the differences between two concepts but also helps with remembering because it [creates associations]({{blog}}/2022/12/27/on-memory.html).

It was particularly interesting to map the concept of executors to C++ futures and `std::async()` seemed like a reasonable analogous.

I planned to cover executors in this post as well, the post was getting too long. Further, I realized they're independent from futures because they're also used by Folly coroutines which are not dependent on futures. In the end I really wanted to learn about executors not futures, but studying executors in isolation wouldn't be intuitive, so setting the context with futures was still useful.

## Appendix

### Installation

In order to follow along the examples, you might want to install Folly in your operating system. I had a lot of difficulty building it from source on MacOS and gave up, but it should be available via [homebrew]({{docs}}/macos/homebrew.html) and it will be downloaded as a shared library (either for [static or dynamic linking]({{blog}}/2025/04/25/shared-libraries.html)).

I was able to install it from source on a Ubuntu system (see *Folly* in [C++ External Libraries Cheatsheet
]({{docs}}/cpp/external_libs.html)) and all examples in this post have only been tested here.

## Reference

* [[1]]({{blog}}/2023/06/08/futures-in-cpp.html) NP-Incompleteness: Futures in C++
* [[2](https://github.com/facebook/folly/blob/main/folly/docs/Futures.md)] Folly Futures - README
