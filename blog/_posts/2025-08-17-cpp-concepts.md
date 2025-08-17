---
layout: post
title: "C++ Concepts"
tags: [c++]
excerpt_separator: <!--more-->
vanity: "2025-08-17-cpp-concepts"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/cpp-logo.svg" alt="C++ Logo" />
</figure>

In this post I'd like to share my notes of concepts in C++. This feature was added to the standard in C++ 20 and is related to the metaprogramming (templates) system.

The content is mostly my notes on the tech [talk by Nicolai Josuttis](https://www.youtube.com/watch?v=jzwqTi7n-rg) at CppCon 2024. I used simpler examples and a different structure for the subtopics.

<!--more-->

## Motivation

Suppose we want to define a function that abstracts computing the length of a container. Using a templated function would work with the majority of STL containers, because they implement `size()`:

{% highlight c++ %}
template <typename T>
size_t my_size(T x) {
   return x.size();
}

...
std::vector<int> v{1, 2, 3};
std::cout << my_size(v) << std::endl; // ok

std::unordered_set<std::string> s{"a", "bc"};
std::cout << my_size(s) << std::endl; // ok
{% endhighlight %}

Now suppose we also want to support other types that do not implement `.size()`. Perhaps a custom class that implements `.len()` instead. If we add an "overload" for that,

{% highlight c++ %}
template <typename T>
size_t my_size(T x) {
   return x.len();
}
{% endhighlight %}

We get a compilation error because the compiler is not able to find the right function to call. This is one case where concepts can help.

## Concepts

One way to think about `concept` is of a type constraint. For example, suppose we want to define a constraint for types that have the function `.size()`. The syntax is almost like that of a function:

{% highlight c++ %}
template<typename T>
concept HasSize = requires (T c) {
  c.size();
};
{% endhighlight %}

The right hand size above is known as a **requirement**. `HasSize` is a **concept**, which we can think of as a named *requirement*. Note that the code inside requirement doesn't need to exist. It just to be syntactically correct. For example, we could have:

{% highlight c++ %}
template<typename T>
concept HasSizeWithTypo = requires (T c) {
  c.size123456();
};
{% endhighlight %}

and the compiler would accept it. It's just that no type would satisfy it. We can then use the concept `HasSize` to restrict the type a template can take. The first overload of function `my_size()` can be:

{% highlight c++ %}
template <typename T>
requires HasSize<T>
size_t my_size(T x) {
   return x.size();
}
{% endhighlight %}

Note the new `requires HasSize<T>` line. This is defined as a **constraint** (usage of a concept / requirement).

This should be enough to disambiguate between the two overloads of `my_size()` because types  match the most specific overload it can find (it still errors out if there are multiple overloads with same specificity, which we'll cover later).

So types with `.size()` will match with the overload with `requires HasSize<T>` while types with `.len()` will match with the one without it. We can define a similar concept for `.len()` and be explicit about it:

{% highlight c++ %}
template<typename T>
concept HasLen = requires (T c) {
  c.len(v);
};

template <typename T>
requires HasLen<T>
size_t my_size(T x) {
   return x.len();
}
{% endhighlight %}

As we'll see next, it's possible to avoid concepts entirely and work with requirements directly. However, concepts help with reuse and also error messages. The compiler has visibility on the concept name and can print it if compilation fails.

## Constraint Syntax

There are other ways to define constraints in a function signature. We can replace `typename` in a template definition by a concept name:

{% highlight c++ %}
template <HasSize T>
size_t my_size(T x) {
   return x.size();
}
{% endhighlight %}

This is a much cleaner syntax but it can only be used if the concept only depends on a single type. An even short form is using it with `auto`:

{% highlight c++ %}
size_t my_size(HasSize auto x) {
   return x.size();
}
{% endhighlight %}

An alternative is the trailing `requires`-clause:

{% highlight text %}
template <typename T>
size_t my_size(T x) requires HasSize<T> {
   return x.size();
}
{% endhighlight %}

One of the advantages of this form is that it doesn't have to be associated with the template type on the function, so it could be based off the one in a templated class. For example:

{% highlight c++ %}
template <typename T>
struct C {
    T x_;

    C(T x): x_(x) {}

    size_t my_size() requires HasSize<T> {
        return x_.size();
    }

    size_t my_size() {
        return x_.len();
    }
};
{% endhighlight %}

Here the method `my_size()` is not even templated, but we can choose the overload based on the class template.

It's also possible to not use concepts by defining a requirement inline:

{% highlight cpp %}
template <typename T>
requires requires (T c) {
  c.size();
}
size_t my_size(T x) {
   return x.size();
}
{% endhighlight %}

The double `requires` highlights the fact that the `requires` in the concept (definition) and the `requires` in the constraint (use) have different meaning. I think it would be clearer if they were different keywords.

We can also use constraints in static checks, in the function body. For example, we could have re-implemented `my_size()` as:

{% highlight c++ %}
template <typename T>
size_t my_size(T x) {
  if constexpr (HasSize<T>) {
    return x.size();
  } else {
    return x.length();
  }
}
{% endhighlight %}


## Multiple Parameters

Concepts support multiple types and the requirement can be a relationship between them. In [1] Josuttis provides an example with `.push_back()`:

{% highlight c++ %}
template<typename C, typename V>
concept HasPushBack = requires (C c, V v) {
  c.push_back(v);
};

template<typename C, typename V>
requires HasPushBack<C, V>
void add(C c, V v) {
  c.push_back(v);
}
{% endhighlight %}

Note that in this case the shorthand forms (see *Constraint Syntax*) cannot be used since it involves multiple types.

It's possible to have multiple parameters in a requirement even if we only provide one type in the constraint. If we wanted to just check if `C` has the function `push_back()` we can do:

{% highlight c++ %}
template<typename C, typename V>
concept HasPushBack = requires (C c, C::value_type v) {
  c.push_back(v);
};
{% endhighlight %}

## Requirement Composition

We can list multiple expressions inside of a requirement. For example, if we want the type to have both `.size()` and `.pop_back()`, we can do:

{% highlight c++ %}
template<typename T>
concept IsVectorish = requires (T c) {
  c.size();
  c.pop_back();
};
{% endhighlight %}

We can also specify the return type of functions:

{% highlight c++ %}
template<typename T>
concept HasSize = requires (T c) {
  { c.size() } -> std::convertible_to<std::size_t>;
};
{% endhighlight %}

If `T` has a method `.size()` that returns strings for example, it would not match.

The easiest way to compose multiple concepts / requirements is via the `&&` or `||` operators. Example:

{% highlight c++ %}
template <typename C, typename V>
  requires HasPushBack<C, V> && HasSize<C>
void add(C& c, V v) {
  c.push_back(v);
}
{% endhighlight %}

## Specificity

We mentioned that when deciding which overload to call for a given type, the compiler will choose the overload that is more specific. But how do we define specificity?

The easies case is the overload without constraints: it's the least specific.

When comparing the specificity between two constraints, the compiler can leverage composition information. In the example above, `HasPushBack<C, V> && HasSize<C>` is more specific than either `HasPushBack<C, V>` or `HasSize<C>`. On the other hand, `HasPushBack<C, V> && HasSize<C>` is less specific than either.

Note that the compiler has no visibility on what goes inside each requirement. It's operating at high-level rules such `<concept1> && <concept2>` and `<concept1> || <concept2>`. To clarify that, consider these two concepts:

{% highlight c++ %}
template <typename C, typename V>
concept Both1 = HasPushBack<C, V> && HasSize<C>;

template <typename C, typename V>
concept Both2 =
  requires(C x) { x.size(); } && requires { c.push_back(v); };
{% endhighlight %}

`Both1` is indeed more specific than `HasPushBack<C, V>` or `HasSize<C>` and while `Both2` is semantically equivalent to `Both1`, the compiler does not know that: `Both2` bears no relationship to `HasPushBack<C, V>` or `HasSize<C>`.

One way to think about it is via a graph of partial orders. Each concept is a node in an oriented graph and a concept composition corresponds to a directed edge. For example `C3 = C1 && C2` adds an edges `C3 -> C1` and `C3 -> C2`. Conversely `C3 = C1 || C2` adds the reverse edges `C1 -> C3` and `C2 -> C3`. Then a concept `S` is more specific than `D` if there's a directed path from `S` to `D` in this graph.

## Value Templates

For cases where we pass a literal to a template, e.g. `std::get<1>`, any compile-time predicate becomes a concept. For example:

{% highlight c++ %}
constexpr bool is_even(int x) {
  return x % 2 == 0;
}

template <auto T>
  requires(is_even(T))
struct EvenLiteral {};

EvenLiteral<4> i;
EvenLiteral<5> j; // Compile error
{% endhighlight %}

## Conclusion

I've been wanting to learn about concepts for a while, since after I reviewed some C++ code using them.

I still don't have a good idea on when they can provide most value. I suspect they can improve readability, especially if replacing the use of type traits such as `enable_if` and compilation error messages.

## References

* [[1](https://www.youtube.com/watch?v=jzwqTi7n-rg)] Back to Basics: Concepts in C++ - Nicolai Josuttis - CppCon 2024
