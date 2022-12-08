---
layout: post
title: "Type Traits in C++"
tags: [topology]
vanity: "2022-11-20-type-traits-in-cpp"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

In this post we'll explore the concept of type traits in C++. In my understanding they're basically constructs that can be used to work with types as if they were values and can be implemented using basic template mechanisms.

We'll start by first going over the basic C++ features that type traits build upon and then cover different applications of type traits, using examples from STL, including:

* `std::is_same`
* `std::conditional`
* `std::enable_if`
* `std::remove_reference`
* `std::add_pointer`
* `std::decay`


<!--more-->

## Type vs. Value Domains

A mental model we can use in reading this post is that of *type domain* and *value domain*. In type domain the primitives are templates and types (`int`, `std::string`, custom classes, etc.), while in type domain they are variables and values (`0`, `"foo"`, etc.).

Type traits work largely at the type domain, but there are mechanisms to convert between them as we'll see.

## Ingredients

We'll first go over the basic mechanisms of templates that allows us to implement type traits.

### Template For Values

We often see templates that expect types, but they can also take values, for example:

{% highlight c++ %}
template<int V>
int inc() {
  return V + 1;
}
std::cout << f<1>() << std::endl; // 2
{% endhighlight %}

We can see this being used for example in `std::get<n>` to access the `i`-th element in a tuple.

The fact that a template can take a value also explain why we need to include `typename` otherwise:

{% highlight c++ %}
template<typename T>
T id(T& x) {
  return x;
}
{% endhighlight %}

### Template Specialization

We can provide a different behavior for specific types when defining templates which is called **template specialization**. For example:

{% highlight c++ %}
template<typename T>
T g(T t) {
  return t;
}

template<>
int g(int i) {
  return i + 1;
}
{% endhighlight %}

The second version of `g()` is the template specialization for the `int` type. The syntax is to replace `T` with the specific type and leave the `template<>` empty.

When we call `g()`, it uses the most specific implementation available:

{% highlight c++ %}
g("a");  // Calls g<T>()
g(1);    // Calls g<int>()
g(1.01); // Calls g<T>()
{% endhighlight %}

Note it calls `g<T>` for the `double` value `1.01`. It doesn't do implicit conversions for types when trying to find a match.

Template specialization works for classes as well:

{% highlight c++ %}
template<typename T>
struct C {
  void display() {
    std::cout << "hello world" << std::endl;
  }
};

template<>
struct C<int> {
  int inc(int x) { return x + 1; }
}
{% endhighlight %}

It's important to clarify that template specialization is not inheritance. They're completely different classes. For example, if we make an instance of `C(1)`, it creates an instance of class `C<int>` and we can't invoke the  `diplay()` method.

Since they're completely different classes they can also inherit from different base classes, which is a key for type traits.

We can also do partial template specialization, for example:

{% highlight c++ %}
template <typename T, typename U>
struct Pair {};

template <typename U>
struct Pair<int, U> {};
{% endhighlight %}

Again, note that `Pair<int, U>` and `Pair<T, U>` are completely different classes.

### Template Aliasing

For readability purposes it's possible to alias templated types via `using`:

{% highlight c++ %}
template<typename T>
struct Wrapper { };

using str_wrapper = Wrapper<std::string>;
{% endhighlight %}

Suppose we want to access the inner type of `Wrapper`. We can do this if we use a templated function:

{% highlight c++ %}
template<typename T>
void f(Wrapper<T> w) {
  T w;
}
{% endhighlight %}

However, if we don't have access to the explicit type, we can "store" it as a type alias inside the `Wrapper` class:

{% highlight c++ %}
template<typename T>
struct Wrapper {
  using type = T;
};
using str_wrapper = Wrapper<std::string>;
{% endhighlight %}

Now if we have an instance of `Wrapper<T>` declared via some type alias, say `str_wrapper`, we don't have the explicit `T` to bind to. We can still use `str_wrapper::type`:

{% highlight c++ %}
void f(str_wrapper w) {
  str_wrapper::type s;
}
{% endhighlight %}

This example is a bit contrived since we know `std_wrapper::type` is a string, but we could have taken a template type as well:

{% highlight c++ %}
template <typename T>
void f(T w) {
  typename T::type s;
}
{% endhighlight %}

The qualifier `typename` is needed here since `T::type` could have been a value. This technique is used in the STL for example in `std::unique_ptr<T>`, where we have the alias `::element_type` for `T`.

Template alias can also be templates, so we can perform partial template aliasing, for example:

{% highlight c++ %}
template <typename T, typename U>
struct Pair {};

template <typename T>
using Single = Pair<int, T>;
{% endhighlight %}

Class `Pair` is a template on two types. We can define `Single` from `Pair` by "fixing" the first type, resulting in a template on one type.

With this basic syntax we're ready to explore type traits.

## Values as Type

Since templates can take values, we can have types that represent specific values (i.e. constants). One general way is to first define the set of integers:

{% highlight c++ %}
template<typename Type, Type V>
struct integral_constant {
    static constexpr Type value = V;
};
{% endhighlight %}

One thing we can note is that there's nothing specific to integers here, so why not just call it `constant`? I couldn't find a source for this naming. There's a reference as early as 2003 [1] but it doesn't include a rationale. Matt Calabrese in 2016 makes the point is could indeed be generalized [2]:

> The author (...) feels `std::constant` should be the primary template, with `std::integral_constant` reformulated to be an alias of `std::constant`. This option should be considered, although it does risk breaking code, such as code that uses `std::integral_constant` to create references.

Back to the present, another observation is that the second template in `integral_constant` is a value with a type defined by the first template.

We can template-specialize `integral_constant` for narrower integer subtypes like `bool` (containing 0 or 1):

{% highlight c++ %}
template <bool B>
using bool_constant = integral_constant<bool, B>;
{% endhighlight %}

And finally template-specialize for `true` and `false`:

{% highlight c++ %}
using true_type = bool_constant<true>;
using false_type = bool_constant<false>;
{% endhighlight %}

Having boolean values as types allows us to move from the value to the type domain. The alias `value` let's us "extract" the underlying `true`/`false` values from the type, converting back from the type to the value domain.

## Predicates as Types

We can define template classes that represent predicates. If the class inherits from `true_type` it represents a true predicate, otherwise false if it inherits from `false_type`. Let's consider some examples.

### Check if two types are equal

Suppose we'd like to tell whether two types `T1` and `T2` are the same. We can define a class `is_same` which takes two templates `T1` and `T2`. The specialization `is_same<T,U>` inherits from `false_type`, while `is_same<T, T>` inherits from `true_type`:

{% highlight c++ %}
template<typename T, typename U>
struct is_same : false_type {};

template<typename T>
struct is_same<T, T> : true_type {};
{% endhighlight %}

So `is_same<std::string, int>` will resolve to the first specialization while `is_same<int, int>` to the second. Note that this relies on the compiler resolving behavior: in theory `is_same<int, int>` could resolve to `is_same<T, U>: false_type`.

We can go from the world of types to the world of values by using the alias `::value` defined in `integral_constant`. Then we can use `static_assert` which works with `constexpr`:

{% highlight c++ %}
static_assert(is_same<int, int>::value);

// compilation error: static_assert failed due to
// requirement 'std::is_same_v<int, float>'
static_assert(is_same<int, float>::value);
{% endhighlight %}

The STL provides convenient aliases for extracting the value from `is_same`:

{% highlight c++ %}
template<typename T, typename U>
inline constexpr bool is_same_v = is_same<T, U>::value;
{% endhighlight %}

So we can do:

{% highlight c++ %}
static_assert(is_same_v<int, int>);
{% endhighlight %}

For this post in particular, let's define a helper function to compare types more succinctly since we'll do it a lot:

{% highlight c++ %}
template<typename T, typename U>
void assert_same_type() {
  static_assert(std::is_same_v<T, U>);
}

assert_same_type<int, int>();
{% endhighlight %}

### Custom type checker

We can define our own syntax sugar for checking if a type is of a specific type:

{% highlight c++ %}
struct MyClass {};

template<typename T>
using is_custom_type = is_same<MyClass, T>;
{% endhighlight %}

This wouldn't work if we'd like multiple types to be `is_custom_type`. In this case we can use template specialization:

{% highlight c++ %}
struct MyClass1 {};
struct MyClass2 {};

template<typename T>
struct is_custom_type : std::false_type {};

template<>
struct is_custom_type<MyClass1> : std::true_type {};

template<>
struct is_custom_type<MyClass2> : std::true_type {};
{% endhighlight %}

There are a lot of these custom-type predicates in the STL, including `std::is_array`, `std::is_function`, etc.

## Flow Control

With predicates as types, we can perform some high-level flow control too.

### In Code

Since predicates extend `bool_constant`, we can "extract" its value and use as a regular boolean expression inside a function body, i.e., in the value domain. For example:

{% highlight c++ %}
template <typename T, typename U>
void f(T a, U b) {
  if (std::is_same_v<T, V>) {
    std::cout << "same type" << std::endl;
  } else {
    std::cout << "different types" << std::endl;
  }
}

f(1, 2); // same type
f(1, 1.0); // different types
{% endhighlight %}

It's possible to perform flow control in the type domain as well, as we'll see next.

### Choosing Type

The struct `std::conditional<Pred, T, F>` works as follows: if `Pred` is true, it assumes the type `T`, otherwise it assumes the type `F`. It can be implemented as follows:

{% highlight c++ %}
template<bool Pred, class T, class F>
struct conditional {
  using type = T;
};

template<class T, class F>
struct conditional<false, T, F> {
  using type = F;
};
{% endhighlight %}

We'll see an application when we look at `std::decay`, but for now, here's a minimal example:

{% highlight c++ %}
template<bool Pred, class T, class F>
using conditional_t = typename conditional<Pred, T, T>::type;

assert_same_type<
  conditional_t<true, int, std::string>,
  int
>();

assert_same_type<
  conditional_t<false, int, std::string>,
  std::string
>();
{% endhighlight %}

Noting that instead of `true/false` we could have some arbitrary boolean (`const`) expression.

### Choosing Overload

It's very common to leverage function overloading to choose implementation based on the input types, for example:

{% highlight c++ %}
void f(int x) {
  std::cout << "int" << std::endl;
}

void f(std::string v) {
  std::cout << "string" << std::endl;
}

f(1);   // int
f("a"); // string
{% endhighlight %}

If we want a more complex condition we can use `std::enable_if_t`. Suppose for example we want a function `f(T t, U u)` with two overloads. One in which `T` and `U` are the same and another where they're different. We can do:

{% highlight c++ %}
template <typename T, typename U>
std::enable_if_t<std::is_same_v<T, U>>
f(T t, U u) {
  std::cout << "same" << std::endl;
}

template <typename T, typename U>
std::enable_if_t<!std::is_same_v<T, U>>
f(T t, U u) {
  std::cout << "different" << std::endl;
}

f(1, 10);  // same
f(1, "a"); // different
{% endhighlight %}

The key is that the overload which will be called is the one in which the return type resolves to `std::enable_if_t<true>`. We'll see shortly how this can be implemented.

For the first call `f(1, 10)`, both arguments are `int`, so `std::is_same_v<int, int>` is `true` and we thus call the overload which prints `"same"`. Conversely, `f(1, "a")` are `int` and `char*` and since `std::is_same_v<int, char*>` is `false`, the overload which prints `"different"` is called.

The `enable_if` is basically the following:

{% highlight c++ %}
template <bool, typename T = void>
struct enable_if {};

template <typename T>
struct enable_if<true, T> {
  typedef T type;
};
{% endhighlight %}

The `enable_if<true, T>` is a partial template specialization. The key is that it has a field, `type`, which the generic `enable_if` does not have, so when we use it in a function like:

{% highlight c++ %}
template <typename T, typename U>
typename enable_if<!std::is_same_v<T, U>>::type
f(T t, U u) {
  std::cout << "different" << std::endl;
}
{% endhighlight %}

By virtue of including `::type`, we'll make sure this overload will only "match" if the template `B` in `enable_if<B>` evaluates to `true`. We can also create the alias `enable_if_t` so we can use it as our original example:

{% highlight c++ %}
template<bool B, class T = void >
using enable_if_t = typename enable_if<B, T>::type;
{% endhighlight %}

While neat, this feels like a pretty involved construct. One might wonder why does `enable_if` have to take the `typename T` param? The second type argument is the final type of `enable_if<>::type`, so it becomes the actual return type of the function. For example:

{% highlight c++ %}
template <typename T, typename U>
enable_if_t<std::is_same_v<T, U>, int> // added int
f(T t, U u) {
  std::cout << "same" << std::endl;
  return 0;
}

template <typename T, typename U>
enable_if_t<!std::is_same_v<T, U>, std::string> // added string
f(T t, U u) {
  std::cout << "different" << std::endl;
  return "a";
}
{% endhighlight %}

Note that the different versions return different types: `int` and `std::string`. Also note that the default type of `T` is `void`, so that's what we get when omitting it like in our original examples.

It's possible to have `enable_if_t` in the template itself as opposed to in the return value [3]:

{% highlight c++ %}
template <
  typename T,
  typename U,
  enable_if_t<std::is_same_v<T, U>>* = nullptr
>
void f(T t, U u) {
  std::cout << "same" << std::endl;
}
{% endhighlight %}

In this case we need some gymnastics because we're not expected to provide the third template, so we need a default value for it. Since the resulting type is `void`, we can't provide a value, so need to use a pointer, `void*`, and provide `nullptr`.

Used in this way, `enable_if` would not need the second template type and could be alternatively defined as:

{% highlight c++ %}
template <bool>
struct enable_if_2 {};

template <>
struct enable_if_2<true> {
  typedef void type;
};
{% endhighlight %}

In my opinion, using `enable_if` in the in the template is even harder to read than the return-based one, mainly because of the need to provide an arbitrary default value.

## Transforming Types

### Removing

One common type transformation is to "remove" lvalue or rvalue references. This can be done via template matching:

{% highlight c++ %}
template<class T>
struct remove_reference {
  using type = T;
};

template<class T>
struct remove_reference<T&>  {
    using type = T;
};

template<class T>
struct remove_reference<T&&> {
    using type = T;
};
{% endhighlight %}

As usual, we can create an alias to avoid the `typename` and `::type`:

{% highlight c++ %}
template<class T>
using remove_reference_t = typename remove_reference<T>::type;
{% endhighlight %}

An example of it in action follows:

{% highlight c++ %}
assert_same_type<int, remove_reference_t<int&>::value>();
{% endhighlight %}

`remove_reference_t` is used in the implementation of `std::forward()`, which is essentially:

{% highlight c++ %}
template <typename T>
T&& forward(remove_reference_t<T>& t) {
  return static_cast<T&&>(t);
}

template <typename T>
T&& forward(remove_reference_t<T>&& t) {
  return static_cast<T&&>(t);
}
{% endhighlight %}

**A detour on std::forward()**. We discussed `std::forward()` in a [previous post]({{blog}}/2022/10/25/review-effective-modern-cpp.html#item-24---distinguish-universal-references-from-rvalue-references) and learned it can be used to preserve whether the lvalue-ness or rvalue-ness of the original type, for example:

{% highlight c++ %}
void g(std::string &s) {
  std::cout << "lvalue" << std::endl;
}

void g(std::string &&s) {
  std::cout << "rvalue" << std::endl;
}

template<typename T>
void f(T&& p) {
  g(forward<T>(p));
}

std::string s = "a";
f(s);            // lvalue
f(std::move(s)); // rvalue
{% endhighlight %}

Let's analyze it again but this time delving into the `forward()` implementation too.

Consider `f(s)` first. Since it's a lvalue reference, `T` in `f()` resolves to `std::string&`. We then call `forward<std::string&>(p)`. It matches the first overload of `forward`, since `p` is a lvalue reference and `remove_reference_t<std::string&>&` resolves to `std::string&`. Finally `static_cast<T&&>` resolves to `static_cast<std::string&>` and to this `f` is cast (see [4] for reference collapsing).

For `f(std::move(s))`, it's a rvalue reference, so `T` in `f()` resolves to `std::string&&`. It again matches the first overload of `forward`, because `p` is, again, a *lvalue* reference and `remove_reference_t<std::string&&>&` resolves to `std::string&`. Finally `static_cast<T&&>` resolves to `static_cast<std::string&&>` and to this `f` is cast.

The second overload only gets matched if we do something like `forward<T>(std::move(p))`.

Why is `remove_reference_t` needed in `forward()` [7]? Suppose we had:

{% highlight c++ %}
template <typename T>
T&& forward2(T& t) {
  return static_cast<T&&>(t);
}

template <typename T>
T&& forward2(T&& t) {
  return static_cast<T&&>(t);
}
{% endhighlight %}

If we call `forward2<std::string&>(p)`, we're explicitly setting `T` to `std::string&`, so `T&` is collapsed to `std::string&`, and the first overload is selected. Then `static_cast<T&&>` resolves to `static_cast<std::string& &&>` and collapses to `static_cast<std::string&>`.

If we call `forward2<std::string&&>(p)`, we're explicitly setting `T` to `std::string&&`,  so `T&` is also collapsed to `std::string&` and the first overload is selected. Then `static_cast<T&&>` resolves to `static_cast<std::string&& &&>` and collapses to `static_cast<std::string&&>`.

So it behaves exactly like `forward()`. Now suppose we call `forward2(p)`, i.e., without the explicit template. In `f()`, `p` is always a lvalue reference, `std::string&`, so in the first overload `T` resolves do `std::string` and `static_cast<T&&>` to `static_cast<std::string&&>`, so it casts to rvalue reference inconditionally and thus behaves like `std::move()`.

Now suppose we call `forward(p)`. This fails with a compile error:

> candidate template ignored: couldn't infer template argument 'T'

In theory if `T` were `std::string&`, the first overload would work, but the compiler is not smart about doing this sort of search when doing template matching. So at the end of the day, `remove_reference_t` is a trick to force callers of `forward` to provide an explicit type, based on the behavior of template matching.

End of detour.

There are other type traits that remove parts of the type (or no-op if not applicable) such as:

* `std::remove_cv`: removes `const` and `volatile`
* `std::remove_pointer`: turns `T*` into `T`
* `std::remove_extent`: turns `T[]` or `T[N]` into `T`

The `std::remove_extent` is worth looking into, especially the `T[N]` overload [5]. Since `N` is part of the type, it can be matched against:

{% highlight c++ %}
template<class T, std::size_t N>
struct remove_extent<T[N]> { using type = T; };
{% endhighlight %}

### Adding

Instead of removing, we can also add bits to the type, like `std::add_pointer` does. It basically turns `T` into `T*`, but treats references as a special case, turning `T&` into `T*`. One possible way to implement it is [8]:

{% highlight c++ %}
template<class T>
struct add_pointer {
  using type = T*;
};

template<class T>
struct add_pointer<T&>  {
    using type = T*;
};

template<class T>
struct add_pointer<T&&> {
    using type = T*;
};

template<class T>
using add_pointer_t = typename add_pointer<T>::type;

assert_same_type<add_pointer_t<int>, int*>();

assert_same_type<add_pointer_t<int&>, int*>();

assert_same_type<add_pointer_t<int*>, int**>();

assert_same_type<add_pointer_t<const int>, const int*>();

assert_same_type<add_pointer_t<int*&>, int**>();
{% endhighlight %}

### Decay

Let's look at `std::decay` as a special case since it's more complex. One possible implementation is [6]:

{% highlight c++ %}
template< class T >
struct decay {
  private:
      typedef std::remove_reference_t<T> U;
  public:
      typedef std::conditional_t<
          std::is_array_v<U>,
          std::add_pointer_t<remove_extent_t<U>>,
          std::conditional_t<
              std::is_function_v<U>,
              std::add_pointer_t<U>,
              std::remove_cv_t<U>
          >
      > type;
};

using decay_t = typename decay<T>::type;
{% endhighlight %}

With the alias versions using `_t` and `_v`, this isn't too bad to follow. It basically has a nested condition which first check for arrays, otherwise it checks for functions and then it handles the remaining cases.

I think this could been implemented using template matching but the advantage of using `std::conditional_t` is that we can reuse the building blocks such as `is_array_v` and `is_function_v`.

We can test a few conversions, basically by exercising each branch in `std::decay`:

{% highlight c++ %}
assert_same_type<decay_t<int>, int>();

assert_same_type<decay_t<const int>, int>();

assert_same_type<decay_t<int[]>, int*>();

assert_same_type<decay_t<int[10]>, int*>();

assert_same_type<decay_t<int(std::string)>, int (*)(std::string)>();

assert_same_type<
  decay_t<int (*)(std::string)>,
  int (*)(std::string)
>();
{% endhighlight %}

The last example if worth taking a closer look: it's not that it `std::add_pointer_t` is indempotent. It's that `int (*)(std::string)` is not a function (but rather a pointer), so it's handled by the last branch, `std::remove_cv_t`, not the second one.

## Conclusion

In this post we learned about how to leverage templates to work with types. We used the mental model of type vs. value domains to have a clearer separation between regular code and type traits.

I found it super clever how the STL uses seemingly unrelated syntax as the building block to add semantics for a more intuitive API, like `std::is_same`. This is similar how `std::move` and `std::forward` adds semantic to `std::static_cast`.

## Related Posts

[Peano Axioms in Lean]({{blog}}/2022/01/26/peano_axioms_lean.html). There seems to be some connection between these high-level types in C++ and the high-order types from languages like Lean. I lack the knowledge to understand this better. Templates seem to allow implementing inductive types so it's possible an equivalence exists [9].

## References

* [[1](https://open-std.org/jtc1/sc22/wg21/docs/papers/2003/n1424.htm)] A Proposal to add Type Traits to the Standard Library
* [[2](https://www9.open-std.org/JTC1/SC22/WG21/docs/papers/2016/p0377r0.html)] std::integral_constant with a Deduced Value Type, Matt Calabrese
* [[3](https://eli.thegreenplace.net/2014/sfinae-and-enable_if/)] SFINAE and enable_if, Eli Bendersky.
* https://github.com/Quuxplusone/from-scratch
* [[4](https://www.kuniga.me/blog/2022/10/25/review-effective-modern-cpp.html#item-28---understand-reference-collapsing)] NP-Incompleteness - Review: Effecive Modern C++ (Item 28)
* [[5](https://en.cppreference.com/w/cpp/types/remove_extent)]  C++ reference - `std::remove_extent`
* [[6](https://en.cppreference.com/w/cpp/types/decay)] C++ reference - `std::decay`
* [[7](https://stackoverflow.com/questions/27501400/the-implementation-of-stdforward)] StackOverflow - The implementation of std::forward
* [[8](https://en.cppreference.com/w/cpp/types/add_pointer)] C++ reference - `std::add_pointer`
* [[9](https://stackoverflow.com/questions/36504245/)] StackOverflow - Does C++ support inductive type definitions?
