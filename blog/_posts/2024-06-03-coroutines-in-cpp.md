---
layout: post
title: "Coroutines in C++ - The API"
tags: [c++]
vanity: "2024-06-03-coroutines-in-cpp"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}


<figure class="image_float_left">
  <img src="{{resources_path}}/logo.svg" alt="C++ logo" />
</figure>

In this post we'll discuss coroutines in C++, which is a feature introduced in C++20. We'll first start by understanding what coroutines mean in C++, comparing briefly with coroutines in other languages.

Then we'll provide a minimal example using coroutines and progressively add capabilities to it while introducing concepts and features from the coroutine toolkit.

<!--more-->

## Coroutines

Coroutines are common in languages like [Python]({{blog}}/2020/02/02/python-coroutines.html) and [JavaScript]({{blog}}/2019/07/01/async-functions-in-javascript.html) and they've more recently been added to C++ (starting from C++20).

If you're familiar with coroutines in Python, coroutines in C++ might feel familiar with the `async` functions and the `await` operator but at the same time a lot harder to grok. Why?

### No batteries included

In [2] Lewis says:

> C++ Coroutines TS provides in the language can be thought of as a low-level assembly-language for coroutines.

and:

> Coroutines TS does not actually define the semantics of a coroutine

In summary, coroutines in C++ are a lot more generic and low-level than their counterparts in other languages, which has potential for more use cases, but makes them hard to use directly by end users. The idea is thus to have libraries build on top of these primitives. Interestingly, the STL itself doesn't provide any such high-level implementations.

So unless you're a library developer, understanding C++ coroutines might not be necessary if the libraries you'll end up using are well abstracted. In any case, it might be interesting to peek under the hood.

### Coroutine operators

The only constructs the C++ language exposes to developers are the operators `co_await`, `co_yield` and `co_return`.

However, the compiler generates code behind the scenes that translate these operators into actual C++ code. I'll be using **coroutine machinery** as a vague term to refer to the combination of the compiler and this implicit code that is generated.

## Suspension

The crucial feature from coroutines is what we call **suspension**, the capability of a function to return to its caller midflight and then later be resumed from where it stopped, with all the local variables preserved.

Here's a minimal example where we can see suspension in action.

{% highlight c++ %}
Task f() {
    std::cout << "initializing" << std::endl;

    co_await std::suspend_always{};
    std::cout << "hello" << std::endl;

    co_await std::suspend_always{};
    std::cout << "world" << std::endl;
}

int main() {
    Task h = f(); // initializing
    h(); // hello
    h(); // world
}
{% endhighlight %}

In this example, `f` is a coroutine. In `main()` we invoke `f()` which executes it until the `co_await` and then it returns a handle, `Task`, back to `main()`. We can then resume the coroutine by invoking the handle `h()` again.

## Coroutine handle

In the example above, `Task` is not a structure defined in the STL. We have to define it ourselves. Here's an example:

{% highlight c++ %}
// forward declaration
struct Promise;

struct Task : std::coroutine_handle<Promise> {
    using promise_type = Promise;
};
{% endhighlight %}

So `Task` is a specialization of `std::coroutine_handle` for some `Promise` class, which we leave unspecified for now, plus the field `promise_type` which associates this class with a specific `Promise`.

We don't actually have to use inheritance to create our own coroutine handle. We can also use composition, making sure to implement the necessary methods, for example:

{% highlight c++ %}
struct Task {
    using promise_type = Promise;
    using Handle = std::coroutine_handle<Promise>;

    Task(Handle handle) : _handle(handle) {}

    static Task from_promise(Promise& promise) {
        return Task(Handle::from_promise(promise));
    }

    void operator ()() {
        // resumes coroutine
        _handle();
    }

    Handle _handle;
};
{% endhighlight %}

The one needed by the coroutine framework is `from_promise()`. The `operator()` is just a syntax sugar. We could also define a method called `resume()` and update `main()` accordingly, which is actually a bit more clear:

{% highlight c++ %}
int main() {
    Task h = f(); // initializing
    h.resume(); // hello
    h.resume(); // world
}
{% endhighlight %}

A coroutine handle is a very thin layer on top of a promise, which contains the bulk of the logic. It's almost like a pointer. Let's then cover the promise class.

## Promise

We shall not confuse the concept of promises in coroutines with the STL's `std::promise`. According to Lewis Baker [4]:

> I want you to try and rid yourself of any preconceived notions of what a "promise" is. While, in some use-cases, the coroutine promise object does indeed act in a similar role to the `std::promise` part of a `std::future` pair, for other use-cases the analogy is somewhat stretched. It may be easier to think about the coroutine's promise object as being a "coroutine state controller" object that controls the behaviour of the coroutine and can be used to track its state.

In the context of coroutines, a promise is any class `T` that implements the methods:

* `T get_return_object()`
* `Awaitable initial_suspend()`
* `Awaitable final_suspend()`
* `void unhandled_exception()`

Here's a possible implementation of `Promise`, which we associated with a `Task` above:

{% highlight c++ %}
struct Promise {
    Task get_return_object() {
        // construct Task from std::coroutine_handle<Promise>
        return {Task::from_promise(*this)};
    }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    void unhandled_exception() {}
};
{% endhighlight %}

The method `get_return_object()` constructs a coroutine handler, in our example a `Task`, from a promise. As we'll see soon, promise is an implementation detail of the coroutine machinery that is not exposed directly to code, so we need to wrap a promise in the coroutine handler before we return control to the caller.

The coroutine machinery knows the class of the promise to create based on the return type of the function. In our example, `f()` has return type `Task`, and it expects this type to have the field `promise_type`. So in this case it knows to use `Promise`.

The other methods customize the behavior of a coroutine. In our example, we declared:

{% highlight c++ %}
std::suspend_never initial_suspend() { return {}; }
{% endhighlight %}

Note this is equivalent to

{% highlight c++ %}
std::suspend_never initial_suspend() { return std::suspend_never(); }
{% endhighlight %}

The class `std::suspend_never` is roughly:

{% highlight c++ %}
struct suspend_never {
    // never suspends
    bool await_ready() { return true; }
    void await_suspend( std::coroutine_handle<> ) {}
    void await_resume() {}
};
{% endhighlight %}

The interesting method here is `await_ready()`. The coroutine machinery will call `promise.initial_suspend().await_ready()` to determine whether to start executing the function right away or wait until `resume()` is first called.

Since our `Promise` returns `std::suspend_never`, the function `f()` does not suspend until it hits a `co_await`. If we replace it with:

{% highlight c++ %}
std::suspend_always initial_suspend() { return {}; }
{% endhighlight %}

And run `main()`, we observe a change in behavior:

{% highlight c++ %}
int main() {
    Task h = f(); // f is not run
    h.resume(); // initializing
    h.resume(); // hello
}
{% endhighlight %}

The `final_suspend()` is an analogous method but for when the function ends or throws an exception. Summarizing, the coroutine machinery is essentially wrapping the body of the function as:

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

Notice that the coroutine machinery uses `co_await` to call `promise.initial_suspend()` and `promise.final_suspend()` above.

You'll notice that the class `std::suspend_always` is also used in the expression

{% highlight c++ %}
co_await std::suspend_always{};
{% endhighlight %}

inside `f()`. Further, the coroutine machinery uses `co_await` to call `.initial_suspend()` and `.final_suspend()`. Let's understand these better.

## Awaitable

An awaitable is any class that implements the methods:

* `bool await_ready()`
* `void await_suspend(std::coroutine_handle<>)`
* `T await_resume()`

An awaitable can be used on the right hand side of the operator `co_await`. When we do:

{% highlight c++ %}
// Recall that there's a Promise object in scope
auto awaitable = <expr>;
auto result = co_await awaitable;
{% endhighlight %}

This is implemented roughly as (I'm omitting a lot of the different branches, for the sake of simplicity. See [3] for a complete picture):

{% highlight c++ %}
template<typename T, typename P>
T co_await(P& promise, Awaitable& awaitable) {
    using handle_t = std::experimental::coroutine_handle<P>;

    if (!awaitable.await_ready()) {
        <suspend-coroutine>

        awaitable.await_suspend(
            handle_t::from_promise(promise)
        );
        <return-to-caller>

        <resume-point>
    }
    return awaitable.await_resume();
}

auto awaitable = <expr>;
auto result = co_await(promise, awaitable);
{% endhighlight %}

The main method is `await_suspend()`. It gets a reference to the coroutine handle of the *current* function. If it returns true, it returns control to the caller, otherwise it resumes right away.

Note that we never return the awaitable object directly to the caller. If we want to send information back, we can piggyback on the promise object, which can be accessed through the coroutine handle, itself passed to the awaitable's `await_suspend()`.

First we implement a new class that satisfies the awaitable constraints, `Awaitable`, and can hold a string:

{% highlight c++ %}
struct Awaitable {
    Awaitable(std::string value) :
        _value(std::move(value)) {}

    bool await_ready() { return false; }

    void await_suspend(Task handler) {
        handler.promise().value = _value;
    }

    void await_resume() {}

    std::string _value;
};
{% endhighlight %}

The key method is `await_suspend()`: when we get a handler we access the promise and set the string there. Of course we need to make sure the `Promise` object can store a string too:

{% highlight c++ %}
struct Promise {
    ... // same as before ^
    std::string value;
};
{% endhighlight %}

And we add a syntax sugar for accessing the promise `value` via the handler:

{% highlight c++ %}
struct Task {
    // ...
    std::string operator ()();

    Promise& promise() const {
        return _handle.promise();
    }
    // ...
};

std::string Task::operator ()() {
    _handle();
    return promise().value;
}
{% endhighlight %}

Function `f()` can now be written as:

{% highlight c++ %}
Task f() {
    std::cout << "initializing" << std::endl;

    co_await Awaitable {"hello"};
    co_await Awaitable{"world"};
}
{% endhighlight %}

We can then change the `main()` function:

{% highlight c++ %}
int main() {
    Task h = f(); // initializing
    std::cout << h() << std::endl; // hello
    std::cout << h() << std::endl; // world
}
{% endhighlight %}

## Syntax sugar: `co_yield` and `co_return`

Instead of defining the `Awaitable` class just to forward a value to the promise, we can add the methods `return_value()` and `get_value()` to the promise object:

{% highlight c++ %}
struct Promise {
    ...
    void return_value(std::string value_) {
        value = value_;
    }
    std::suspend_always yield_value(std::string value_) {
        value = value_;
        return {};
    }
    ...
};
{% endhighlight %}

And then use the special operators `co_yield` and `co_return`:

{% highlight c++ %}
Task f() {
    std::cout << "initializing" << std::endl;
    // co_await promise.yield_value("hello")
    co_yield "hello";
    // co_await promise.return_value("world)
    co_return "world";
}
{% endhighlight %}

One interpretation is that `co_yield` and `co_return` are ways to "wrap" a regular value into a coroutine handle. We've also seen how to "unwrap" the value from the handle, either by the overloaded `operator()` or `.resume()`, but we only did this outside of a coroutine (the `main()` function in our case).

We can unwrap the value of a handle inside a coroutine too as we'll see next.

## Awaiting Coroutines

In Python, if we have an async function, we can await its results and use it afterwards, as long as we're in an async function too. For example:

{% highlight python %}
async def f():
    return "hello"

async def g():
    x = await f()
    print(x) # hello
{% endhighlight %}

In C++, we can also "extract" the value from a coroutine handle using the `co_await` operator. The trick is to make the coroutine handle an awaitable too, by implementing the methods discussed in the *Awaitable* section.

{% highlight c++ %}
struct Task {
    ...
    bool await_ready() { return false; }
    void await_suspend(Task handler) { }
    std::string await_resume();
    ...
}

std::string Task::await_resume() {
    return promise().get_value();
}
{% endhighlight %}

The key difference is that `await_resume()` now returns a type. Recall that in the section *Awaitable*, the pseudo-implementation of `co_await` as a function, the return value is `awaitable.await_resume()`, so that's what gets assigned to the right hand side of `co_await`

{% highlight c++ %}
std::string v = co_await <coroutine handle>
{% endhighlight %}

To complete the example, we define a function `g()` that returns a coroutine handle with a value:

{% highlight c++ %}
Task g() {
    co_return "hello";
}
{% endhighlight %}

and then in `f()` we extract that value and combine with another:

{% highlight c++ %}
Task f() {
    std::string v = co_await g();
    std::cout << v << std::endl;
}
{% endhighlight %}

For this to work like the Python example, we'll need to change `Promise::initial_suspend` to not suspend, otherwise it would look like:

{% highlight c++ %}
Task f() {
    auto h = g();
    std::string v = co_await h();
    std::cout << v << std::endl;
}
{% endhighlight %}

It's worth clarifying that this code is only to demonstrate how to "extract" a value from a coroutine. It's not in the least a functional implementation that handles `co_await` correctly (for example, it doesn't handle calling multiple `co_await`).

We'll cover an implementation that handles arbitrary `co_await`s in a future post.

The full code for this example is available on [Github]({{github}}/coroutine.cpp).


## Conclusion

In this post we covered the basic of C++ coroutines. I found it pretty hard to understand them and that I couldn't build on top of my understanding of coroutines in JavaScript or Python. The observation from Marières [1], on it being a more lower-level with no batteries included, made me understand why.

I read Lewis's blog posts [2, 3, 4] which are very detailed and technical, so I found it isn't a very good source for ramping up on coroutines. Marières [1] provides a more digestible since the author provides a lot of insights from a first-learner perspective.

In my post, I tried to do a more example-based exposition, and am happy with the result of starting with a digestable example and progressively build on top of it and introducing other concepts little by little.

Both Marières and Lewis spend time discussing implementation details such as the fact that coroutine frames are stored on the heap instead of the stack, which I found not necessary for an introduction to coroutines.

In this post, I tried to focus more on the syntax and semantics of coroutines, and less how they're implemented in practice. The examples from this post did nothing useful, but for a future post I'd like to implement async operations using coroutines.

## Related Posts

[Python Coroutines]({{blog}}/2020/02/02/python-coroutines.html) and [Async Functions in JavaScript]({{blog}}/2019/07/01/async-functions-in-javascript.html) cover the same concept of suspendable functions, but in Python and Javascript this is implemented with event loops and is single threaded.

## References

* [[1](https://www.scs.stanford.edu/~dm/blog/c++-coroutines.pdf)] My tutorial and take on C++20 coroutines, David Mazières
* [[2](https://lewissbaker.github.io/2017/09/25/coroutine-theory)] Asymmetric Transfer - Coroutine Theory
* [[3](https://lewissbaker.github.io/2017/11/17/understanding-operator-co-await)] Asymmetric Transfer - C++  Coroutines: Understanding operator co_await
* [[4](https://lewissbaker.github.io/2018/09/05/understanding-the-promise-type)] Asymmetric Transfer - C++ Coroutines: Understanding the promise type
