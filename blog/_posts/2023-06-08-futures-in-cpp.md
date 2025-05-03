---
layout: post
title: "Futures in C++"
tags: [c++]
vanity: "2023-06-08-futures-in-cpp"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/cpp-logo.png" alt="C++ Logo" />
</figure>

In this post we'll learn about the basic components of asynchronous execution in C++, namely `std::futures`, `std::async`, `std::packaged_task` and `std::promise`.

As an addendum, we'll also compare C++ and JavaScript promises.

<!--more-->

## Asynchronous Computation

### Threads

Threads are the basic primitives for performing asynchronous computation. If we want to compute a function `f()` without blocking the current thread we can do:

{% highlight c++ %}
#include <thread>

std::thread t([](){
    f();
});
t.join();
{% endhighlight %}

The problem with running functions in a different thread is retrieving the results. In C++11 we can share memory by capturing variables by reference in the lambda function passed to the thread. In our example above we could do:

{% highlight c++ %}
#include <thread>
int result;
std::thread t([&](){
    result = f();
});
cout << result << endl;
t.join();
{% endhighlight %}

Now the problem is that by the time we print `result`, it might not have been computed yet. Worse yet, it might be that thread `t` writes to `result` at the same time the current thread reads from it (race condition), resulting in undefined behavior.

One way to avoid this race condition is to use atomic variables or locks and then keep checking if the result was set in a loop. A simpler way to achieve this is by using `std::async`.

### Async

The code is somewhat similar to the thread version but this time we get a handle to the (eventual) result returned by the lambda function. By calling `.get()` on that handle, we block until the result is set.

{% highlight c++ %}
#include <future>
std::future<int> fut = std::async([](){
    return f();
});
// block until fut is set
auto result = fut.get();
cout << result << endl;
{% endhighlight %}

Here's our first encounter with `std::future` which is the return type of `std::async`. The C++ runtime might opt to run `std::async` in the same thread, depending on conditions. To force it running on a new thread, we must use the flag `std::launch::async`.

### Packaged Task

In both threads and `async()`, the computation of the lambda function starts right away. A *packaged task* on the other hand is like a function object (e.g. lambda) which can be invoked at a later time.

Differently from a normal function object however, we can get a handle to the future result of a packaged task, which will be set when the packaged task is eventually invoked and finishes running. For example:

{% highlight c++ %}
std::packaged_task<int()> pkg([]() {
    return f();
});

// get a handle to the (future) result of the packaged task
std::future<int> fut = pkg.get_future();
// run the packaged task (blocking)
pkg();
// fut should be set already
auto result = fut.get();
cout << result << endl;
{% endhighlight %}

This example is not very illuminating, since we could just do

{% highlight c++ %}
auto result = f();
{% endhighlight %}

The key point is that a packaged task can then be executed elsewhere, including other threads, while we hold a handle to its future results. For example:

{% highlight c++ %}
std::packaged_task<int()> pkg([]() {
    return f();
});

// get a handle to the (future) result of the packaged task
std::future<int> fut = pkg.get_future();
// run the packaged task in another thread
std::thread t(std::move(pkg));
t.join();
// access the result even though we don't have
// access to pkg itself (it has been moved)
auto result = fut.get();
cout << result << endl;
{% endhighlight %}

One observation is that like with `std::function`, `std::packaged_task` can take in any function object, not just lambdas. Also like `std::function`, we always need to specify the "function signature" of `std::packaged_task`, in our example `<int()>`. We discussed this in more details in [Function Objects in C++
]({{blog}}/2023/06/01/functions-in-cpp.html).

The reason `std::async` does not need the explicit "function signature" is that we never directly invoke the function we passed as argument, as opposed to a `packaged_task`, where we can pass arguments to it:

{% highlight c++ %}
std::packaged_task<int(const std::string& s)>
pkg([](const std::string& s) {
    return s.length();
});
pkg("hello world");
{% endhighlight %}

### Promise

As we just saw, a packaged task lets us defer computing to some time after and access that result independently. However, once we do start the computation the result will invariably be set on the future.

Promises allow us to defer setting the results on the future, by requiring us to perform a call, `promise::set_value()`, explicitly. For example:

{% highlight c++ %}
std::promise<int> prom;

// get a handle to the (future) result of the promise
std::future<int> fut = prom.get_future();

// run the function another thread
std::thread t([&]() {
    auto r = f();
    // set the result on the future explicitly
    prom.set_value(r);
});

// blocks until there's data in fut
auto result = fut.get();
cout << result << endl;
t.join();
{% endhighlight %}

Because of this property of being able to send data from `promise.set_value()` and receive it via `future.get()` even from different threads, the promise-future pair can be seen as a one-way, single-use communication channel. *Item 39 - Consider void futures for one-shot event communication* in Scott Meyers's [Effective Modern C++]({{blog}}/2022/10/25/review-effective-modern-cpp.html) explores this mechanism.

We have now seen several mechanisms for asynchronous computation with different capabilities for communicating the results. Now let's think about how they're related to each other.

## Relationship Between std::async, std::packaged_task and std::promise

First-off, they all leverage `std::future` as a handle for acessing the computation asynchronously. It's not surprising then, that they're all defined in the `<future>` header.

We claim that `std::async()` is a special case of `std::packaged_task` and this a special case of `std::promise`. To show this we can provide implementations of the special versions using their general counterparts.

### std::async via std::packaged_task

One possible implementation of `std::async(f, std::launch::async)` via `std::packaged_task` is:

{% highlight c++ %}
template <typename T>
auto my_async(T f) {
    std::packaged_task<T> pack(std::move(f));
    auto fut = pack.get_future();
    std::thread t(std::move(pack));
    t.detach();
    return fut;
}
auto fut = my_async<int()>([](){
    return f();
});

cout << fut.get() << endl;
{% endhighlight %}

The major difference is that we need to pass an explicit type to `my_async`'s template due to the fact that `std::packaged_task` requires it too, but these are syntactic details. The point is that it's possible to implement `std::async` with `std::packaged_task` fairly easily.

### std::packaged_task via std::promise

One possible implementation of `std::packaged_task` via `std::promise` is:

{% highlight c++ %}
template <typename T>
struct my_packaged_task;

template <typename R, typename... Args>
struct my_packaged_task<R(Args...)> {

    template<typename T>
    my_packaged_task(T f) : f_(std::move(f)) { }

    std::future<R> get_future() {
        return std::move(prom_.get_future());
    }

    void operator() (Args... args) {
        auto result = f_(args...);
        prom_.set_value(result);
    }

    std::function<R(Args...)> f_;
    std::promise<R> prom_;
};

my_packaged_task<int()> pack = []() {
    return f();
};

auto fut = pack.get_future();
pack();
std::cout << fut.get() << std::endl;
{% endhighlight %}

It requires some typing gymnastics, which are similar to those we studied in [Function Objects in C++
]({{blog}}/2023/06/01/functions-in-cpp.html).


## Futures

Now that we've seen the major ways `std::future` can be created, let's explore a few use cases.

### Check if future has been consumed

Futures can only be consumed once, i.e. we can only call `get()` once. Calling it a second time will result in undefined behavior, so if we can't guarantee it hasn't been called before, we need to check if a future has been read, we can use `valid()`:

{% highlight c++ %}
std::promise<int> p;
std::future<int> f = p.get_future();

std::cout << f.valid() << std::endl; // true
p.set_value(1);
std::cout << f.valid() << std::endl; // true
f.get();
std::cout << f.valid() << std::endl; // false
{% endhighlight %}

Note that even before the future is ready (i.e. it was set a value), it's on a valid state. Only once it has been read from it becomes invalid.

Since futures are meant to be used once, they cannot be copied, so if we wish to pass a future to a different function we should likely `std::move()` it. There's a version of futures that can be read multiple times, `std::shared_future`, which is useful for consuming it by multiple threads. It can be obtained via `future.share()`:

{% highlight c++ %}
std::promise<int> p;
std::future<int> f = p.get_future();
std::shared_future<int> sf = p.share();
p.set_value(1);
sf.get();
sf.get(); // ok to call shared future twice
{% endhighlight %}

Note that calling `p.share()`, "moves" the future into `sf`, so `f.valid()` would be now false.

### Non-blocking check of future readiness

In all examples we provided above, we used `get()` which blocks until the future is ready. We can instead define a timeout and check if the future is ready by then via the method `wait_for(duration)`:

{% highlight c++ %}
f.wait_until(seconds(2));
{% endhighlight %}

If the future is ready, it returns `future_status::ready` otherwise it returns either `future_status::deferred` or `future_status::timeout`.

The `future_status::deferred` seems to only be used when we call `std::async` with `std::launch::deferred`, in which case the function is executed in the current thread, so unless we call `get()` it won't be ready. On the other hand, `future_status::timeout` can be executed asynchronously, so we could in theory keep calling `wait_for()` in a loop until the status changes.

At the time of this writing, there isn't an official way to check for the status of a future right now, but in a StackOverflow [question](https://stackoverflow.com/questions/10890242/get-the-status-of-a-stdfuture) [3], it's suggested to use:

{% highlight c++ %}
f.wait_until(seconds(0));
{% endhighlight %}

### Handling exceptions

If the callback passed to `std::async` or `std::packaged_task` throws exceptions, it gets stored inside the `std::future`. Similarly, we can set an exception to a future using a promise via `promise.set_exception()`. The exception in the future is re-thrown when we call `future.get()`:

{% highlight c++ %}
std::promise<int> p;
auto fut = p.get_future();

try {
    throw std::runtime_error("Error");
} catch(...) {
    p.set_exception(std::current_exception());
}

// 1
std::cout << fut.valid() << std::endl;

try {
    std::cout << fut.get() << std::endl;
} catch (const std::exception& e) {
    // "Error"
    std::cout << e.what() << std::endl;
}
{% endhighlight %}



## Conclusion

The Chapter 4 of *C++ Concurrency in Action* by Anthony Williams, describes all these concepts pretty well. The one thing that was over my head was the relationship between `std::async`, `std::packaged_task` and `std::promise`.

After writing this post I feel like my grasp on them has improved a lot. While trying to implement `my_packaged_task`, I realized I didn't know much how `std::function` works and it led me to write another post: [Function Objects in C++
]({{blog}}/2023/06/01/functions-in-cpp.html).


## Related Posts

[JavaScript Promises]({{blog}}/2015/07/19/javascript-promises.html). I've first heard of the concept of promises in JavaScript and remember finding them a bit hard to understand.

I found that my mental model for JavaScript promises didn't translate well into C++, first because JavaScript promises don't have the concept of future, and conversely C++ promises do not have the concept of chaining promises via `then()`.

JavaScript is single threaded but does have the concept of the event loop which can defer the execution of a function (e.g. when doing an AJAX call) to be executed later.

We can bridge these gaps between them by trying to "translate" a typical JavaScript promise example into C++.

### JavaScript promise in C++

Consider an example where we have a function `request()` which returns a promise whose result we can consume via a callback passed to `.then()`:

{% highlight javascript %}
request().then(result => {
    console.log(result);
});
{% endhighlight %}

Promises are not native to JavaScript, so at some point somewhere in this API a promise will have to be created. Let's suppose it's within `request()` itself:

{% highlight javascript %}
function request() {
    const promise = new Promise((resolve, reject) => {
        setTimeout(() {
            resolve("hello");
        }, 1000);
    });
    return promise;
}
{% endhighlight %}

So here, calling `resolve("hello")` is equivalent to fulfilling a promise, which corresponds to `set_value("hello")` in `std::promise`. So a possible implementation of `request()` in C++ could be:

{% highlight c++ %}
std::future<std::string> request() {
    std::promise<std::string> p;
    auto fut = p.get_future();
    std::thread t([p_ = std::move(p)]() mutable {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        p_.set_value("hello");
    });
    t.detach();
    return fut;
}
{% endhighlight %}

One way we could simulate the `then()` API in C++ is by waiting for the future to be set in another thread and then invoke a callback once it's ready:

{% highlight c++ %}
template<typename T>
void then(std::future<T> fut, std::function<void(T)> cb) {
    std::thread t([cb, fut_ = std::move(fut)]() mutable {
        auto result = fut_.get();
        cb(result);
    });
    t.detach();
}
{% endhighlight %}

The code using these APIs would look like:

{% highlight c++ %}
auto fut = request();
then<std::string>(std::move(fut), [](std::string s) {
    std::cout << s << std::endl;
});
{% endhighlight %}

In summary, the major difference between C++ and JavaScript promises is that C++ splits promises into `std::promise` (write side) and `std::future` (read side). In JavaScript promise is the read side, while the write side happens via the callback `resolve()` provided during the construction of the promise.

A hypothetical API to make them more similar could be:

{% highlight c++ %}
std::future<std::string> fut = std::make_promise(
    [](std::function<void(std::string)> resolve) {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        resolve("hello");
    }
);
{% endhighlight %}

We could handle the `reject` in an analogous way we handle exceptions via `promise.set_exception()`.

## References

* [1] *C++ Concurrency in Action*: Williams, A. - Chapter 4
* [[2](https://en.cppreference.com/w/cpp/thread/future)] cppreference.com - std::future
* [[3](https://stackoverflow.com/questions/10890242/get-the-status-of-a-stdfuture)] StackOverflow - Get the status of a std::future
