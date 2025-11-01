---
layout: post
title: "Folly Coroutines: Lifetimes"
tags: [c++]
excerpt_separator: <!--more-->
vanity: "2025-11-01-folly-coroutines-lifetimes"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/folly-logo.svg" alt="Folly Logo" />
</figure>

In the post [Folly coroutines]({{blog}}/2025/06/18/folly-coroutines.html) [1] we briefly mentioned that coroutine lifetime is one of the more counter-intuitive aspects of it. It's confusing because it the imperative-looking syntax hides some of the asynchronous semantics behind the scenes. However, the lifetime of variables are still subject to those asynchronous semantics.

Thus, in this post I'd like to delve a bit into Folly coroutines lifetimes to get a better understanding and way to work around it.

<!--more-->

## Examples

We start with some example situations where coroutine lifetimes might be an issue and then follow with a utility that handles this problem.

### Deferred Execution

There are a few ways to defer execution of a coroutine. One common way is to batch one or more coroutines and executed them concurrently via `folly::coro::collectAllRange()` for example:

{% highlight cpp %}
struct C {
  void print() {
    std::cout << "hi" << std::endl;
  }
};

folly::coro::Task<void> co_print(const C& c) {
  c.print();
  co_return;
}

folly::coro::Task<void> co_all() {
  std::vector<folly::coro::Task<void>> tasks;

  C c1;
  auto task1 = co_print(c1);
  tasks.emplace_back(std::move(task1));

  C c2;
  tasks.emplace_back(co_print(c2));

  co_await folly::coro::collectAllRange(std::move(tasks));
  co_return;
}
{% endhighlight %}

Here we don't `co_await` when calling `co_print()` right away, but defer to run concurrently later. In this particular example, there are no issues because `c1` and `c2` are still in scope, but it's possible that when calling `collectAllRange()` they were not:

{% highlight cpp %}
folly::coro::Task<void> co_all() {
  std::vector<folly::coro::Task<void>> tasks;

  {
    C c1;
    auto task1 = co_print(c1);
    tasks.emplace_back(std::move(task1));
  } // c out of scope
  {
    C c2;
    tasks.emplace_back(co_print(c2));
  } // c out of scope

  co_await folly::coro::collectAllRange(std::move(tasks));
  co_return;
}
{% endhighlight %}

Here `c1` and `c2` were in scope when `co_print()` was invoked but not when the coroutine was actually run.

### Semi-Futures

Another way to defer execution is to convert a coroutine into a `SemiFuture` and pass it around, e.g.

{% highlight cpp %}
folly::SemiFuture<folly::Unit> semi() {
  std::vector<folly::coro::Task<void>> tasks;

  C c;
  auto task = co_print(c);
  // Convert coroutine into a semiFuture
  return std::move(task).semi();
}
{% endhighlight %}

In this case, by the time we block on the semifuture, `c` is long out of scope.

One solution in both cases is to pass `C` by value instead of a reference, so a copy is made, the new copy is added to the coroutine stack and kept in scope.

For the remainder of the post we'll use the `SemiFuture` example instead of the `collectAllRange()` since it's a bit simpler.

### Class Methods

A slightly more subtle problem is for class methods. If we were to defer the execution of a class method coroutine, we need to make sure `this*` is in scope.

{% highlight cpp %}
folly::coro::Task<void> co_print(int x) {
  std::cout << x << std::endl;
  co_return;
}

struct MyClass {
  MyClass(int x): x_(x) {}

  folly::coro::Task<void> co() {
    co_await co_print(x_);
    co_return;
  }
};

folly::SemiFuture<folly::Unit> semi() {
  MyClass c(1);
  auto task = c.co();
  return std::move(task).semi();
} // c has been destroyed, so *this inside co() is out of scope
{% endhighlight %}

In this example, accessing `co_print(x_)` will lead to undefined behavior because by the time this is called, `c` has been destroyed.

Noting that variables defined inside the coroutine method, and the function arguments, are properly captured in the coroutine stack, so if we had:

{% highlight cpp %}

struct MyClass {
  MyClass(int x): x_(x) {}

  folly::coro::Task<void> co(int x) {
    co_await co_print(x);
    co_return;
  }
};

folly::SemiFuture<folly::Unit> semi() {
  MyClass c(1);
  auto task = c.co(c.x_);
  return std::move(task).semi();
}
{% endhighlight %}

This would work because `co()` does not depend on `this*` and `x` is added to the coroutine stack.

### Lambdas

Lambdas can seem counterintuitive because even when we make a copy it won't behave as we expected. Consider the example:

{% highlight cpp %}
folly::SemiFuture<folly::Unit> semi() {
  Class c;
  auto task = [c]() -> folly::coro::Task<void> {
    co_await co_print(c);
    co_return;
  }();
  return std::move(task).semi();
}
{% endhighlight %}

This seems like it would be safe because we're capture `c` by value in the lambda, so it would survive past the original `c`'s lifetime. However, we need to recall how lambdas are implemented in C++, via functors.

A semantically similar implementation could be:

{% highlight cpp %}
struct Functor {
  Function(Class c): c_(c) {}
  folly::coro::Task<void> operator() () {
    co_await co_print(c_);
    co_return;
  }

  Class c_;
};

folly::SemiFuture<folly::Unit> semi() {
  Class c;
  Functor f(c);
  auto task = f();
  return std::move(task).semi();
}
{% endhighlight %}

Here the issue becomes more obvious: while we are making a copy of `c` into `Functor::c_`, it is `f` (i.e. the lambda itself) that is destroyed by the time we run the coroutine, so acessing `f.c_` is undefined behavior.

## Solution

Folly has the function `co_invoke()` which we can use to address the lifetime issue of the previous section, by passing the lambda to it:

{% highlight cpp %}
folly::SemiFuture<folly::Unit> semi() {
  Class c;
  auto task = folly::coro::co_invoke([c]() -> folly::coro::Task<void> {
    co_await co_print(c);
    co_return;
  });
  return std::move(task).semi();
}
{% endhighlight %}

I was curious how this was done, so decided to look at the code, which as of [this writing](https://github.com/facebook/folly/blob/616d8b0c3867713ef12d55d80e80589b17ef3a97/folly/coro/Invoke.h) is defined as:

{% highlight cpp %}
struct co_invoke_fn {
  template <typename F, typename... A>
  FOLLY_ERASE constexpr auto
  operator()(F&& f, A&&... a) const noexcept(noexcept(tag_invoke(
      tag<co_invoke_fn>,
      tag<invoke_result_t<F, A...>, F, A...>,
      static_cast<F&&>(f),
      static_cast<A&&>(a)...)))
      -> decltype(tag_invoke(
          tag<co_invoke_fn>,
          tag<invoke_result_t<F, A...>, F, A...>,
          static_cast<F&&>(f),
          static_cast<A&&>(a)...)) {
    return tag_invoke(
        tag<co_invoke_fn>,
        tag<invoke_result_t<F, A...>, F, A...>,
        static_cast<F&&>(f),
        static_cast<A&&>(a)...);
  }
};
FOLLY_DEFINE_CPO(co_invoke_fn, co_invoke)
{% endhighlight %}

This is pretty hard to read! It uses a pattern called [customization point objects](https://www.kuniga.me/blog/2025/09/10/cpo-in-cpp.html) or CPO, which we covered in a previous post [2]. We can strip out the boilerplate required by the compiler and get something like:

{% highlight cpp %}
struct co_invoke_fn {
  template <typename F, typename... A>
  constexpr auto operator()(F&& f, A&&... a) const noexcept {
    return tag_invoke(
        tag<co_invoke_fn>,
        tag<invoke_result_t<F, A...>, F, A...>,
        static_cast<F&&>(f),
        static_cast<A&&>(a)...);
  }
};
FOLLY_DEFINE_CPO(co_invoke_fn, co_invoke)
{% endhighlight %}

This pattern is used to have finer control over how this function can be overloaded by library users. We won't go into details, but it suffices to say that in the end it calls a function called `tag_invoke()`:

{% highlight cpp %}
template <typename F, typename... A, typename F_, typename... A_>
friend Task tag_invoke(
    tag_t<co_invoke_fn> /*unused*/,
    tag_t<Task, F, A...> /*unused*/,
    F_ f,
    A_... a
) {
  co_yield co_result(co_await co_awaitTry(
      invoke(static_cast<F&&>(f), static_cast<A&&>(a)...)));
}
{% endhighlight %}

This is a friend function of `class Task`, meaning it is like a normal function but has access to private members of `Task`.

If we remove the CPO machinery and all the bells and whistles to handle exceptions and corner cases, we can simplify co-invoke to:

{% highlight cpp %}
template <typename F, typename... A>
std::invoke_result_t<F, A...> co_invoke(F f, A... a) {
  co_return co_await f(a...);
}
{% endhighlight %}

Here we can see more clearly what it does: we pass the lambda as a variable to a the coroutine `co_invoke()`. Since a coroutine copies its local variables to a stack, a side effect is that it keeps the function object in scope.

From my experiments I see this ends up calling the move constructor of `f`'s arguments multiples times, which is good to keep in mind.

## Conclusion

It took me a long time to get a sense of Folly's `co_invoke()` implementation and while I now understand the "what" better, I don't have much insights on the "why".

* Why is `tag_invoke()` a friend function of `Task` as opposed to a free function?
* Why does `tag_invoke()` need two tags? and why the second tag requires the return type to be an explicit template argument `tag<invoke_result_t<F, A...>, F, A...>` (isn't the return type of a function captured in the `F` template?)
* Why does `tag_invoke()` require the template types `F_` and `A_`? Aren't they expected to match `F` and `A`?
* Why does `tag_invoke()` use `co_yield` since it's the only `co_` operation in it? Could it use `co_return`? I suspect it has to do with error handling, so I plan to study this dimension in more detail later.

## References

* [[1]({{blog}}/2025/06/18/folly-coroutines.html)] NP-Incompleteness - Folly Coroutines
* [[2]({{blog}}/2025/09/10/cpo-in-cpp.html)] NP-Incompleteness - Customization Point Objects in C++
