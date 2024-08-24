---
layout: post
title: "Coroutines in C++: A Minimal Library"
tags: [cpp]
vanity: "2024-08-23-coroutines-in-cpp-2"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/logo.svg" alt="C++ logo" style="width: 100px;" />
</figure>

In the [previous post]({{blog}}/2024/06/03/coroutines-in-cpp.html) we started studying coroutines in C++. One of the main takeaways is that the coroutine API offered by STL is pretty low level and not very useful for end developers.

In this post I'd like to cover the implementation of a minimal library that supports `co_await` and `co_return`. It's a dummy library in the sense that it doesn't do anything with `co_await` and treats `co_return` as `return`, so it doesn't add any value compared to non-coroutine code.

The goal however is to understand the necessary machinery that has to happen for this simplest example to work.

<!--more-->

## Multiple `co_await`s

At the end of [1] we mentioned that while we had an example using `co_await`, it didn't handle multiple co-awaits in the same function. Suppose we have the following code:

{% highlight c++ %}
Task h() {
    co_return "hello";
}

Task g() {
    std::string v1 = co_await h(); // (1)
    std::string v2 = co_await h();
    co_return v1 + v2;
}

Task f() {
    std::string r = co_await g();
    co_return r; // (2)
}

int main() {
    auto coro = f();
    coro();
}
{% endhighlight %}

The problem is that when we await the first `h()` inside `g()` at `(1)`, it will return control back to `f()`,  which will resume, call `co_return` at `(2)` and return to `main()`, and we'll never call the second `h()`.

On the other hand, if we are to ignore `co_await` and treat `co_return` as regular `return`, the expected sequence of events is:

{% highlight text %}
- f runs
- f awaits g
- g runs
- g awaits h
- h runs
- h returns
- g runs (resumes)
- g await h
- h runs
- h returns
- g runs (resumes)
- g returns
- f runs (resumes)
- f returns
{% endhighlight %}

Translating it into the coroutine API calls we learned in [1], this would look like (ignoring some APIs like `initial_suspend()`):

{% highlight text %}
- f.resume()
- g.await_suspend(f) // (1)
- g.resume()
- h.await_suspend(g)
- h.resume()
- h.final_suspend()
- g.resume()
- h.await_suspend(g)
- h.resume()
- h.final_suspend()
- g.resume()
- g.final_suspend()
- f.resume()
- f.final_suspend()
{% endhighlight %}

The first thing to observe is `(1)`, that `await_suspend` is called on the right hand side object of `co_await`, i.e. the awaitable, not on the function calling it. A handler to the caller is passed as parameter. In other words, if we do `co_await h` inside `g()`, the API called is `h.await_suspend(g)`.

Further, `await_suspend` is the API that gives us the opportunity to connect the caller `g` and the "callee", `h`. This is important because we need `h` to call `g.resume()` after it is finished, so when we call `await_suspend`, we must store a reference to `g` in `h`. In order to do so, we add an optional `Task` (defined in [1]) to the `Promise` class (also defined in [1]):

{% highlight c++ %}
struct Promise {
    ...
    std::optional<Task> caller;
    ...
};
{% endhighlight %}

We also want to call `await_suspend()` **before** we start executing the awaitable, because if such awaitable only has a `co_return` (such as `h()`), it will finish executing and return before we have the opportunity to store who the caller was, so we have to change the `Promise` to always start suspended:

{% highlight c++ %}
struct Promise {
    ...
    std::suspend_always initial_suspend() {
        return {};
    }
    ...
};
{% endhighlight %}

Then we modify our implementation of `Task::await_suspend()` to save a reference to the caller and only then start the execution of the awaitable via `.resume()`:

{% highlight c++ %}
void Task::await_suspend(Task handler) {
    _handle.promise().caller = handler;
    // start executing callee
    _handle.resume();
}
{% endhighlight %}

To summarize, let's go over what's happening in `f()`:

{% highlight c++ %}
Task f() {
    std::string r = co_await g();
    co_return r;
}
{% endhighlight %}

Recall that a `co_await` gets transpiled roughly to:

{% highlight c++ %}
T co_await(P& promise, Awaitable& awaitable) {
    using handle_t = std::experimental::coroutine_handle<P>;

    if (!awaitable.await_ready()) {
        // (1) <suspend-coroutine>

        awaitable.await_suspend(
            handle_t::from_promise(promise)
        );
        // (2) <return-to-caller>

        // (3) <resume-point>
    }
    return awaitable.await_resume();
}
{% endhighlight %}

So when we run `std::string r = co_await g();`, first `g()` returns an instance of `Task` (an `Awaitable`). We then suspend `f()` at `(1)`. We then call `Task::await_suspend()` which as we saw stores a reference of `f` in `g()`'s promise. Then we start executing `g()`.

As we'll show next, `g()` will run to completion before we hit `(2)`. In fact we'll completely bypass `(2)` because we'll have `g()` resume `f()` "manually", which will cause the execution to jump to `(3)`.

Let's now show how the callee can resume the caller once it is done. Recall  that a co-routine is roughly transpiled to [1]:

{% highlight c++ %}
{
    auto promise = new Promise();
    co_await promise.initial_suspend();
    try {
        <body-statements>
    } catch (...) {
        promise.unhandled_exception();
    }
    co_await promise.final_suspend();
}
{% endhighlight %}

As we see above, `final_suspend` returns an awaitable which will be `co_await`'ed. We'll actually have that awaitable resume the caller. For that end, we can create a new awaitable type to support this custom behavior, which we'll call it `FinalAwaiter`:

{% highlight c++ %}
// forward declaration
struct Promise;

struct FinalAwaiter {
    using Handle = std::coroutine_handle<Promise>;

    bool await_ready() noexcept { return false; }

    void await_suspend(Handle h) noexcept;

    void await_resume() noexcept {}
};
{% endhighlight %}

Which is what we return in `final_suspend()`:

{% highlight c++ %}
struct Promise {
    ...

    FinalAwaiter final_suspend() noexcept {
        return FinalAwaiter{};
    }

    ...
}
{% endhighlight %}

Note that we don't need to pass any info about the `Promise` when creating a `FinalAwaiter`. The promise will be available via the `Handle` parameter to `FinalAwaiter::await_suspend()`. The implementation of `await_suspend()` then resumes the caller:

{% highlight c++ %}
void FinalAwaiter::await_suspend(Handle h) noexcept {
    if (h.promise().caller) {
        h.promise().caller->resume();
    }
}
{% endhighlight %}

Let's go over it via another example to see the whole picture. When a function `g()` does:

{% highlight c++ %}
Task g() {
    std::string v1 = co_await h();
    std::string v2 = co_await h();
    co_return v1 + v2;
}
{% endhighlight %}

`g()` suspends on the first `co_await`, `h()` runs to completion (i.e. without yielding control back to `g()`) and then resumes `g()` explicitly. It proceeds to the second `co_await` and it suspends again, the second `h()` runs to completion and then resumes `g()` once again. Finally `g()` itself hits a `co_return v1 + v2`, which gets translated into:

{% highlight c++ %}
...
promise.return_value(v1 + v2); // (1)
FinalAwaiter fa = promise.final_suspend(); // (2)
co_await fa;
{% endhighlight %}

In `(1)` we store `value` inside the `Promise`. In `(2)`, `promise.final_suspend()` will return a `FinalAwaiter` which has a pointer to the caller, so when we `co_await` it in `(3)` it will call `await_suspend` and resume the caller, in this case `f()`.

So when the caller gets resumed, it will call

{% highlight c++ %}
return awaitable.await_resume();
{% endhighlight %}

Which we can implement as:

{% highlight c++ %}
std::string Task::await_resume() {
    return promise().get_value();
}
{% endhighlight %}

to retrieve the value set via `promise.return_value()` by the callee. The code for this part is available on [Github]({{github}}/coro_lib.cpp).

Needless to say, this flow is super confusing because we seem to be hijacking the coroutine API by resuming coroutines via the callee. If we look at API calls indented to highlight the call stack, we get a chain of function calls:

{% highlight c++ %}
- f.resume()
    // await g()
    - g.await_suspend(f)
        - g.resume()
            // await h()
            - h.await_suspend(g)
                - h.resume()
                    // co_return
                    - h.final_suspend()
                        - g.resume()
                            // await h()
                            - h.await_suspend(g)
                                - h.resume()
                                    // co_return
                                    - h.final_suspend()
                                        - g.resume()
                                            // co_return
                                            - g.final_suspend()
                                                - f.resume()
                                                     // co_return
                                                    - f.final_suspend()
{% endhighlight %}

Which has the additional downside that it can cause stack overflows.

## Symmetric Transfer

There's an alternative overload for `await_suspend()` which allows us to return a coroutine handler instead of `void` [3]. In this case `co_await()` gets transpiled to something like:

{% highlight c++ %}
T co_await(P& promise, Awaitable& awaitable) {
    if (!awaitable.await_ready()) {
        // (1) <suspend-coroutine>

        auto h = awaitable.await_suspend(
            handle_t::from_promise(promise)
        );
        h.resume();

        // (2) <return-to-caller>

        // (3) <resume-point>
    }
    return awaitable.await_resume();
}
{% endhighlight %}

In this overload the `h.resume()` is called outside the `await_suspend`, which avoids some of the stack calls. We have to modify `Task::await_suspend()` to:

{% highlight c++ %}
std::coroutine_handle<> Task::await_suspend(
    std::coroutine_handle<> handler
) {
    handle_.promise().caller = handler;
    return handle_;
}
{% endhighlight %}

and `FinalAwaiter::await_suspend()` to:

{% highlight c++ %}
std::coroutine_handle<> FinalAwaiter::await_suspend(
    Handle h
) noexcept {
    return h.promise().caller;
}
{% endhighlight %}

With this API our example callstack looks like:

{% highlight c++ %}
- f.resume()
    // await g()
    - g.await_suspend(f)
    - g.resume()
        // await h()
        - h.await_suspend(g)
        - h.resume()
            // co_return
            - h.final_suspend()
            - g.resume()
                // await h()
                - h.await_suspend(g)
                - h.resume()
                    // co_return
                    - h.final_suspend()
                    - g.resume()
                        // co_return
                        - g.final_suspend()
                        - f.resume()
                            // co_return
                            - f.final_suspend()
{% endhighlight %}

So it cut the maximum depth in half. The problem with this API is that upon a `co_return`, the snippet:

{% highlight c++ %}
auto h = awaitable.await_suspend(
    handle_t::from_promise(promise)
);
{% endhighlight %}

calls `FinalAwaiter::await_suspend()` which expects a "caller" to be set. In our current setup, the first coroutine function called (in our case `f()`) doesn't have it.

To handle this we can return a different awaitable (instead of `FinalAwaiter`) for the root task. One option is to define an "adapter" coroutine, that extracts the value of a `Task` but returns a different type of awaitable, `RootTask`.

{% highlight c++ %}
RootTask adapter(Task&& t) {
    std::string value = co_await t;
    co_return value;
}
{% endhighlight %}

Suppose `RootTask` is associated with the promise `RootTaskPromise` (which we'll define later). Recall that `co_return value` is transpiled to something like:

{% highlight c++ %}
auto promise = RootTaskPromise();
...
promise.return_value(value);
co_await promise.final_suspend();
{% endhighlight %}

Here we call `final_suspend()` on `RootTaskPromise`, not on `TaskPromise`, so we don't have to return the `FinalAwaiter`. Let's comple implementation:

{% highlight c++ %}
struct RootTask {
    using coroutine_handle_t = std::coroutine_handle<RootTaskPromise>;
    using promise_type = RootTaskPromise;

	RootTask(coroutine_handle_t coroutine)
		: m_coroutine(coroutine) {}

    void resume() {
        m_coroutine.resume();
    }

    std::string& result() {
        return m_coroutine.promise().m_result;
    }

    coroutine_handle_t m_coroutine;
};
{% endhighlight %}

and a corresponding promise `RootTaskPromise`:

{% highlight c++ %}
struct RootTaskPromise {

    std::suspend_never initial_suspend() {
		return{};
    }

    std::coroutine_handle<RootTaskPromise> get_return_object() {
        return {
            std::coroutine_handle<RootTaskPromise>::from_promise(*this)
        };
    }

    void unhandled_exception() noexcept {}

    void return_value(std::string result) {
        m_result = result;
    }

    std::suspend_always final_suspend() noexcept {
        return {};
	}

    std::string m_result;
};
{% endhighlight %}

We can then define a utility function that calls the adapter function and then extract the value:

{% highlight c++ %}
std::string sync_wait(Task&& t) {
    RootTask rt = adapter(std::move(t));
    return rt.result();
}
{% endhighlight %}

The code for this part is available on [Github]({{github}}/coro_lib_symmetric_xfer.cpp).

## Conclusion

In this post we aimed to build a minimal (and useless) coroutine library that doesn't add value on top of regular function. The goal was however to build something higher level, that has semantics closer to that in other programming languages since we've seen that coroutines in C++ are pretty low-level.

This exercise was heavily based on Lewiss Baker's [coroutine library](https://github.com/lewissbaker/cppcoro). Reading library code is a frustrating but rewarding exercise. Even with a reference code that I was able to run, I had a hard time groking the logic and for sure I'm not getting the overall picture.

Coroutines in C++ is one of the most difficult code I've seen in a while. Even after spending hours playing with it and writing this post I don't feel like a have a satisfactory understanding.

## References

* [[1](http://localhost:4000/blog/2024/06/03/coroutines-in-cpp.html)] NP-Incompleteness: Coroutines in C++ - The API
* [[2](https://github.com/lewissbaker/cppcoro)] GitHub: CppCoro - A coroutine library for C++
* [[3](https://lewissbaker.github.io/2020/05/11/understanding_symmetric_transfer)]  Asymmetric Transfer - C++ Coroutines: Understanding Symmetric Transfer
