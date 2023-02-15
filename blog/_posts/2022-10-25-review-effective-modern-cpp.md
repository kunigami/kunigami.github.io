---
layout: post
title: "Review: Effective Modern C++"
tags: [review, c++]
excerpt_separator: <!--more-->
vanity: "2022-10-25-review-effective-modern-cpp"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/book_cover.jpg" alt="Streaming Systems book cover" />
</figure>


In this post I'll share my notes on the book *Effective Modern C++* by Scott Meyers.

As with *Effective C++*, Meyers' book is organized around items. Each item title describes specific recommendations (e.g. "Prefer nullptr to 0 and NULL") and then it delves into the rationale, while also explaning details about the C++ language.

The post lists each item with a summary and my thoughts when applicable. The goal is that it can be an index to find more details on the book itself.

The *Modern* in the title refers to C++11 and C++14 features. This book is a complement to *Effective C++*, not an updated edition.

<!--more-->

## Organization of the book
{:.no_toc}

The book is divided into 8 chapters and 42 items. Each chapter serves as a theme into which the items are organized.

To make look up easier, I've included a table of contents:

1. TOC
{:toc}

## Chapter 1 - Deducing Types

### Item 1: Understand template type deduction

This item explains how a given template `T` is resolved. Let's analyze some cases. First assume the template is declared as `T&`:

{% highlight c++ %}
template<typename T>
void f(T& x) { }

const int a = 1;
f(a); // T is const int

const int& b = a;
f(b); // T is const it
{% endhighlight %}

As `const T&`:

{% highlight c++ %}
template<typename T>
void f(const T& x) { }

const int a = 1;
f(a); // T is int

const int& b = a;
f(b); // T is it
{% endhighlight %}

As `T&&` (universal reference, see *Item 24*):

{% highlight c++ %}
template<typename T>
void f(const T&& x) { }

const int a = 1;
f(a); // T is int&

f(1); // 1 is rvalue, T is int&&
{% endhighlight %}

There are corner cases for arrays or function pointers which we won't cover.

### Item 2: Understand auto type deduction

The gist is that `auto` is resolved the same way templates are for the most part. The only difference is when an `auto` variable is initialized using curly braces. `auto` resolves to `std::initializer_list<>`, template doesn't compile:

{% highlight c++ %}
// std::initializer_list<int>
auto x = { 10 };

// couldn't infer template argument 'T'
f({10});
{% endhighlight %}

### Item 3: Understand decltype

`decltype` is used to get the type of a variable. It can be useful to bridge the gap between `auto` and templates. `auto` doesn't have an explicit type and templates might require one. *Item 18* provides one such example:

{% highlight c++ %}
auto deleter = [](C* ptr) {
  delete ptr;
};
std::unique_ptr<C, decltype(deleter)> uPtr(nullptr, deleter);
{% endhighlight %}

We won't go over the details, but type-wise, this version of `std::unique_ptr<T, D>` has two template parameters, the type of the underlying object `T` and that of the deleter function `D`. We don't know the type of `deleter` so we can use `decltype(deleter)`. *Item 33* has a simular use case.

Another use of `decltype` is combining with `auto` as `decltype(auto)`. One problem with `auto` is that it drops the reference modifier when resolving. For example:

{% highlight c++ %}
struct C {
  int& getRef() { return x_; }
  int x_;
}
auto f(C c) { // int f(C c)
  return c.getRef();
}
{% endhighlight %}

Here `auto` resolves to `int`. If we wish to preserve the `&` from `getRef()` we need `decltype`:

{% highlight c++ %}
decltype(auto) f(C c) { // int& f(C c)
  return c.getRef();
}
{% endhighlight %}

A third use of `decltype` is a technique to display the type of a given variable at compile time, as shown in *Item 4*.

### Item 4: Know how to view deduced types

There are several ways to inspect the deduced type of variables declared with auto:

{% highlight c++ %}
auto x = /* expr */
{% endhighlight %}

One interesting technique is to have a compilation error tell us that. We can use the following code:

{% highlight c++ %}
template<typename T>
class TD;

TD<decltype(x)> xType;
{% endhighlight %}

This will fail to compile and display the type in the error message. In `clang` (v14) I get:

> error: implicit instantiation of undefined template 'TD<int *>'

In runtime, we can instead use `typeid().name()`:

{% highlight c++ %}
std::cout << typeid(x).name() << std::endl;
{% endhighlight %}

It mangles the name but compilers have tools for prettifying it. For `clang`, we can use the `llvm-cxxfilt` CLI:

{% highlight text %}
llvm-cxxfilt --types Pi
{% endhighlight %}

## Chapter 2 - auto

### Item 5: Prefer auto to explicit parameters

The reasons provided include: easier to type and refactor. It also avoids subtle type mismatches which are hard to catch because the compiler tries to convert/cast types when possible. Examples are provided in the book.

*Item 6* discusses cases in which `auto` doesn't work well.

### Item 6: Use the explicitly typed initializer idiom when auto deduces undesired types

One example where `auto` doesn't infer the "expected" type is when accessing an element of a vector of booleans. This is because `vector<bool>` is optimized to use bitpack so each element only uses 1 bit instead of a whole byte.

However, this means when acessing a specific element, it needs to return a special structure, `std::__bit_reference<std::vector<bool>`, which can be implicitly converted to `bool`:

{% highlight c++ %}
std::vector<bool> f() {
  std::vector<bool> bv {true};
  return bv;
}
// Implicit conversion
bool b = f()[0];
std::cout << b << std::endl; // 1
{% endhighlight %}

However if we use `auto`:

{% highlight c++ %}
std::vector<bool> f() {
  std::vector<bool> bv {true};
  return bv;
}
// std::__bit_reference<std::vector<bool>
auto b = f()[0];
std::cout << b << std::endl; // ??
{% endhighlight %}

`b` holds a reference to an object that doesn't exist anymore (i.e. the temporary object created to hold `f()`'s return value), so its value is undefined.

More generally, in any case we use proxy classes, i.e. types that are not actually the type one would expect but can be implicitly converted to it, we might have such risk. The author suggests using `static_cast<T>` to solve this issue:

{% highlight c++ %}
auto b = static_cast<bool>(f()[0]);
{% endhighlight %}

but then I'm not sure about the advantage of using `auto`.

## Chapter 3 - Moving to Modern C++

### Item 7 - Distinguish between () and {} when creating objects

Variables can be initialized via assignment, parenthesis or curly braces:

{% highlight c++ %}
int a = 1;
int b (2);
int c {3};
{% endhighlight %}

The advantage of the curly braces is that it prevents *narrowing conversion*, which is when a broader type (e.g. `double`) gets converted to a narrower one (e.g. `int`), possibly causing information loss:

{% highlight c++ %}
double x = 1.1;
int b (x); // truncates to 1
int c {x}; // compile error
{% endhighlight %}

Curly braces won't compile. Another issue curly braces avoid is the *vexing parse* in which the initialization syntax is the same as a function declaration. For example:

{% highlight c++ %}
struct C {
  C(int x) {}
  C() {}
};

C c1(1); // creates instance of C
C c2();  // declares a function
{% endhighlight %}

The last expression might seem like it's creating an instance of `C` by calling the default constructor but it's actually declaring a function. Using curly braces does the intuitive thing:

{% highlight c++ %}
C c1{1}; // creates instance of C
C c2{};  // creates instance of C
{% endhighlight %}

Another scenario in which parenthesis and curly braces behave differently is passing two arguments to a `int` vector:

{% highlight c++ %}
// vector with 10 elements, all set to 20
std::vector<int> v1 (10, 20);
// vector with 2 elements, 10 and 20
std::vector<int> v2 {10, 20};
{% endhighlight %}

### Item 8 - Prefer nullptr to 0 and NULL

The item suggests `nullptr` is more readable when representing null pointers than either `0` and `NULL`.

There are also some cases when using templates where passing either `0` or `NULL` won't compile as pointers. For example:

{% highlight c++ %}
struct C {};

void f(std::shared_ptr<C> p) {}

template<typename F, typename P>
void apply(F fun, P p) {
  fun(p);
}

// ok
apply(f, nullptr);
// error because P is deduced to be int
apply(f, 0);
{% endhighlight %}

### Item 9 - Prefer alias declarations over typedefs

It boils down to templates. `typedef` cannot be templatized. An example using alias declaration:

{% highlight c++ %}
template<typename T>
using MyVec = std::vector<T>;
{% endhighlight %}

If we want to achieve the same using `typedefs` we need to use a `struct`:

{% highlight c++ %}
template<typename T>
struct MyVec {
  typedef std::vector<T> type;
};
{% endhighlight %}

Whenever we use this new type we need to do `typename MyVec<T>::type` as opposed to `MyVec<T>` for alias declaration.

### Item 10 - Prefer scoped enums to unscoped enums

The C++98 enums are known as unscoped enums:

{% highlight c++ %}
enum RGB { red, green, blue };
{% endhighlight %}

The C++11 enums are called scoped enums (note the `class` modifier):

{% highlight c++ %}
enum class RGB { red, green, blue };
{% endhighlight %}

To refer to a scoped enum value we do `RGB::red` as opposed to `red` previously. The need for qualifying the enum value is the origin of *scoped*. This prevents scope pollution (e.g. another enum including `red` would fail to compile).

One case where unscoped enums work better is to implement named tuple access for readability:

{% highlight c++ %}
enum Field { name, address };
std::tuple<std::string, std::string> info;
// same as std::get<0>(info)
std::get<name>(info);
{% endhighlight %}

The alternative using scoped enums would require explicit downcast to `std::size_t` because its default type is `int`.

### Item 11 - Prefer deleted functions to private undefined ones

Suppose we're inheriting from a class and we want to "hide" some of the methods from the parent class to all callers. One way to achieve this is by making the methods private. However, member methods or friend classes would still be able to call them by accident, so we can also not define them.

{% highlight c++ %}
class Child : public Parent {

private:
  // not defined
  void hiddenMethod();
}
{% endhighlight %}

The problem is that if being invoked by a different compilation unit, this would only fail at linking time which is harder to understand. We can instead delete the method:

{% highlight c++ %}
class Child : public Parent {
public:
  void hiddenMethod() = delete;
}
{% endhighlight %}

### Item 12 - Declare overriding functions override

To recap, suppose class `Child` inherits from `Parent`. Overriding functions allows us to call the method from the instance's `Child` type even when the type on the signature is of `Parent` type. Example:

{% highlight c++ %}
struct Parent {
  virtual void f() const {
    std::cout << "parent" << std::endl;
  }
};

struct Child : public Parent {
  void f() const {
    std::cout << "child" << std::endl;
  }
};

void g(Parent &x) {
  x.f();
}

Child c;
g(c); // prints "child"
{% endhighlight %}

It's easy to get this wrong. If we forget to add `virtual` to the parent method or make a mistake when defining the signature of the child the override won't take place. Example:

{% highlight c++ %}
struct Child : public Parent {
  void f() {
    std::cout << "child" << std::endl;
  }
};
{% endhighlight %}

Here we forgot to add `const` to the `Child::f()` so it's not overriding. The `override` keyword will cause a compilation error if that happens.

{% highlight c++ %}
struct Child : public Parent {
  void f() override {
    std::cout << "child" << std::endl;
  }
};
{% endhighlight %}

`clang` reports this error:

> hidden overloaded virtual function 'C::f' declared here: different qualifiers ('const' vs unqualified)

### Item 13 - Prefer const iterators

Use `cbegin()` and `cend()` from `stl` collections whenever possible as opposed to `begin()` and `end()`.

### Item 14 - Declare functions noexcept when possible

We can annotate a function with `noexcept` to indicate it doesn't throw exceptions:

{% highlight c++ %}
void f() noexcept {
  ...
}
{% endhighlight %}

`noexcept` functions can be better optimized by the compilers. However, there's no compile time constraint to enforce a `noexcept` doesn't really throw exceptions or call functions that do.

My take on this item is that it's not very broadly applicable.

### Item 15 - Use constexpr wherever possible

The `constexpr` can be used when declaring variables or functions. For variables, its value is resolved at compile time:

{% highlight c++ %}
constexpr auto SIZE = 2 + 3;
{% endhighlight %}

`constexpr` variables can be a function of other `constexpr` variables:

{% highlight c++ %}
constexpr auto ONE = 1;
constexpr auto TWO = 2;
constexpr auto SIZE = ONE + TWO;
{% endhighlight %}

For functions, its behavior depends on whether *all* the arguments are `constexpr`. If yes, then its result is also a `constexpr`, else it's a regular function. The body of a `constexpr` function can only depend on other `constexpr` functions.

{% highlight c++ %}
constexpr int add(int x, int y) {
  return x + y;
}

constexpr int inc(int x) {
  return add(x, 1);
}

// ok
const auto ONE = 1;
constexpr auto SIZE = inc(ONE);

// ok too
int x = 1;
int y = inc(x);
{% endhighlight %}

Downside: it’s hard to debug or profile `constexpr` functions because `printf()` is considered side-effect.

### Item 16 - Make const members thread safe

The gist of this item is that there's a backdoor to mutate variables in `const` methods: declaring them as `mutable`, for example:

{% highlight c++ %}
class C {
  void f() const {
    x++;
  }

  mutable int x = 10;
};
{% endhighlight %}

And hence not thread-safe.

### Item 17 - Understand special member function generation

This item discusses the conditions in which the default constructor, destructor and assignment operators are auto-generated.

Let's abbreviate:

* CC: Copy constructor
* CA: Copy assignment
* MC: Move constructor
* MA: Move assignment
* D: Destructor

We can build a table to encode the rules for when a member function is auto-generated. To read the table: a member function corresponding to a row is only auto-generated if *none* of the columns in which an ✓ exists is user defined.

|    | CC | CA | MC | MA | D |
| -- | -- | -- | -- | -- | - |
| CC |  ✓ |    |  ✓ |  ✓ |   |
| CA |    |  ✓ |  ✓ |  ✓ |   |
| MC |  ✓ |  ✓ |  ✓ |  ✓ | ✓ |
| MA |  ✓ |  ✓ |  ✓ |  ✓ | ✓ |

It also mentions the *Rule of three*:

> Rule of Three: if you declare any of copy constructor, copy assignment or destructor, you should declare all three.

Templated operations do not count towards special member functions. For example:

{% highlight c++ %}
class C {
  // Not a copy constructor even when T=C
  template<typename T>
  C(const T& c);

  // Not a move assignment even when T=C
  template<typename T>
  C& operator=(const T&& c);
};
{% endhighlight %}

## Chapter 4: Smart Pointers

### Item 18 - Use std::unique_ptr for exclusive-ownership resource management

We've discussed unique pointers in [Smart Pointers in C++]({{blog}}/2022/06/10/smart-pointers-cpp.html). This section describes other things I've learned from the book.

`std::unique_ptr` supports custom deleters which are made part of the type:

{% highlight c++ %}
auto deleter = [](C* ptr) {
  delete ptr;
};
std::unique_ptr<C, decltype(deleter)> uPtr(nullptr, deleter);
{% endhighlight %}

The size of `std::unique_ptr` is the same as raw pointers unless custom deleters are used.

### Item 19 - Use std::shared_ptr for shared-ownership resource management

We've discussed shared pointers in [Smart Pointers in C++]({{blog}}/2022/06/10/smart-pointers-cpp.html). This section describes other things I've learned from the book.

Moving shared pointers (as opposed to copying) avoids reference count changes (which can be expensive since it's atomic).

Shared pointers allocate memory for a control block which among other things stores the reference count, so the size of `std::shared_ptr` is at least twice as big than `std::unique_ptr`.

Differently from `std::unique_ptr`, the custom deleter is not part of the type of a `std::shared_ptr` because it can be stored in the control block.

### Item 20 - Use std::weak_ptr for std::shared_ptr like pointer that can dangle

A `std::weak_ptr` can be obtained from `std::shared_ptr` but does not increase reference count. This is useful to prevent cyclical dependencies in which case reference count doesn't work but this is very uncommon.

`std::weak_ptr` also have a control block like `std::shared_ptr` but it has a different reference count.

### Item 21 - Prefer std::make_unique and std::make_shared to direct use of new

We've discussed the merits of `std::make_unique` and `std::make_shared` in [Smart Pointers in C++]({{blog}}/2022/06/10/smart-pointers-cpp.html). This section describes other things I've learned from the book.

One interesting bit is that when using `std::make_shared`, it allocates the object being created and the control block in the same chunk of memory.

### Item 22 - When using the Pimpl idiom, define special member functions in the implementation file

The Pimpl idiom is a technique used to reduce build times. The idea is to move heavy dependencies from the `.h` file to the `.cpp` one. For example, suppose we have some dependency `a.h`:

{% highlight c++ %}
// a.h
struct A {
  int get() {
    return 1;
  }
};
{% endhighlight %}

Our main class `B` depends on `A`, so its header includes it:

{% highlight c++ %}
// b.h
#include "a.h"
struct B {
  A a;
  int get();
};
{% endhighlight %}

And here's the implemention:

{% highlight c++ %}
// b.cpp
#include "b.h"
int B::get() {
  return a.get();
}
{% endhighlight %}

If we want to not depend on header `a.h` in `b.h`, a technique is to define a `struct Impl` which depends on `A` but we only forward declare in `b.h` and create a single member variable as a unique pointer to it (hence the Pimpl name: pointer + implementation):

{% highlight c++ %}
// b.h
#include <memory>
struct B {
  struct Impl;
  std::unique_ptr<Impl> impl;

  B();
  int get();
};
{% endhighlight %}

Then in the `b.cpp` we actually define the struct `Impl` and have the dependency on `a.h` there:

{% highlight c++ %}
// b.cpp
struct B::Impl { A a; };

B::B() : impl(std::make_unique<Impl>()) {}

int B::get() {
  return impl->a.get();
}
{% endhighlight %}

Note that we don't need the destructor because `impl` calls `delete` when it falls out of scope. There's some issue with the auto-generated destructor that the book delves into but I didn't get errors for that.

## Chapter 5: RValue References, Move Semantics and Perfect Forwarding

### Item 23 - Understand std::move and std::forward

We've discussed `std::move` in [Move Semantics in C++]({{blog}}/2022/03/01/moving-semantics-cpp.html). This section describes other things I've learned from the book.

One important observation is that function arguments are always lvalue even if their type is a rvalue reference. For example:

{% highlight c++ %}
struct C {};

void log(std::string s) {
  std::cout << s << std::endl;
}

void g(C &c) { log("g: lvalue ref"); }

void g(C &&c) { log("g: rvalue ref"); }

void f(C &c) {
  log("g: lvalue ref");
  g(c);
}

void f(C &&c) {
  log("f: lvalue ref");
  g(c);
}

// f: lvalue ref, g: lvalue ref
f(c);
// f: rvalue ref, g: lvalue ref
f(std::move(c));
{% endhighlight %}

In [2], we've seen that `std::move()` is a static cast that converts any reference to a rvalue reference. `std::forward<T>()` converts to a rvalue reference *conditionally*, only if the type `T` is itself a rvalue reference.

This is clearer from an example:

{% highlight c++ %}
void g(std::string &s) {
  std::cout << "lvalue" << std::endl;
}

void g(std::string &&s) {
  std::cout << "rvalue" << std::endl;
}

template<typename T>
void f(T&& p) {
  g(std::forward<T>(p));
}

std::string s = "a";
f(s); // lvalue
f(std::move(s)); // rvalue
{% endhighlight %}

Since `T&&` is a universal reference, the way it is resolved depends on whether the passed value is a rvalue or lvalue reference. In `f(s)`, the type `T&&` in `f()` resolves to `std::string &`. In `f(std::move(s))`, `T&&` resolves to `std::string &&`.

At `f()`, `p` is a lvalue, so if passed to `g()` as is, we'd always call `g(std::string &s)` regardless of whether `f()` was initially called with a rvalue. We'd like to preserve the rvalue information as if `g()` was being called directly. This is what `std::forward<T>` does.

Simplistically `std::forward<T>` is basically a `static_cast<T&&>` but there are [nuances](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2009/n2951.html) we won't discuss here.

Worth noting that `std::move` and `std::forward` are both static casts.

### Item 24 - Distinguish universal references from rvalue references

Universeal references are rvalue references where type deduction happens, either via `auto` or templates. Examples:

{% highlight c++ %}
template<typename T>
void f(T&& p) {}

auto&& x = "hello";
{% endhighlight %}

It's called a universal reference because it properly handles both rvalue and lvalue references.

### Item 25 - Use std::move on rvalue references, std::forward on universal references

This item basically says that if we got `p` as a universal reference we should pass it along using `std::forward<T>`:

{% highlight c++ %}
template<typename T>
void f(T&& p) {
  g(std::forward<T>(p));
}
{% endhighlight %}

But if we got it as a rvalue reference we should use `std::move`:

{% highlight c++ %}
void f(C&& p) {
  g(std::move(p));
}
{% endhighlight %}

### Item 26 - Avoid overloading on universal references

This item basically says that if we have a single parameter function:

{% highlight c++ %}
void f(int x) { }
{% endhighlight %}

Do not add an overload using universal references like:

{% highlight c++ %}
template<typename T>
void f(T&& x) { }
{% endhighlight %}

This might make the overload resolution difficult to reason about. For example, if we pass `short` to `f()` it actually calls the universal reference overload.

This is even worse if we use universal references in the constructor, because it mixes up with copy and move constructors.

{% highlight c++ %}
class C {
  template<typename T>
  explicit C(T&& x) {}

  C(const C& x) {}
}

C c1;
C c2(c1); // which constructor does it call?
{% endhighlight %}

The book delves into the details on why the last line calls the universal reference constructor.

### Item 27 - Alternatives to overloading universal references

This item provides several ways to avoid the universal references overloading. One of the most interesting is *tag dispatch*. It basically leverages static checks to make sure the right overload is used. So if we have:

{% highlight c++ %}
void f(int x) { }

template<typename T>
void f(T&& x) { }
{% endhighlight %}

We can turn `f` into a dispatcher function and have the int and non-int logic as `fImpl`:

{% highlight c++ %}
template<typename T>
void f(T&& x) {
  fImpl(
    std::forward<T>(x),
    std::is_integral<std::remove_ref<T>>
  );
}

void fImpl(int x, std::true_type) { }

template<typename T>
void fImpl(T&& x, std::false_type) { }
{% endhighlight %}

When we call `f()` with an integer type, `std::is_integral<std::remove_ref<T>>` will resolve to `std::true_type` and call `fImpl(int x, std::true_type)`. Otherwise it resolves to  `std::false_type` and calls `fImpl(T&& x, std::false_type)`.

### Item 28 - Understand reference collapsing

You can't directly write a reference to a reference:

{% highlight c++ %}
int x = 10;
int& & a = x;
{% endhighlight %}

But compilers might as intermediate steps when deducing types, for example:

{% highlight c++ %}
int x = 10;
int &y = x;
auto& a = y;
{% endhighlight %}

Since `y` is `int &`, `auto&` resolves to `int& &`, but gets collapsed to `int&` in the end.

The rule for collpasing is simple: if both references are rvalue, the result is an rvalue reference, otherwise it's a lvalue reference. This explains the behavior of universal references:

{% highlight c++ %}
int x = 10;
int &y = x;
auto&& a = y;
auto&& b = 10;
{% endhighlight %}

For `a`, `auto &&` resolves to `int& &&` and thus `int &`. For `b`, `auto &&` resolves to `int&& &&` and thus `int &&`.

### Item 29 - Assume move operations are not present, not cheap, and not used

One case where move is not cheaper than copying is for when small string optimization (SSO) is used. In this case the content is stored along side the `std::string` and not dynamically allocated, so we can't simply do a pointer swap.

For cases where it's not used, some STL code only makes use of move operations if they're are `noexcept`, for some back-compatibility reasons.

### Item 30 - Familiarize yourself with perfect forwarding failures

This item discusses cases in which using:

{% highlight c++ %}
template<typename T>
void fwd(T&& x) {
  f(std::forward<T>(x));
}
{% endhighlight %}

doesn't work. The failure scenarios described are due to universal references not to `std::forward` in particular.

*Casr 1: Braced initializers.*

This is explained in *Item 2*, the reason being that `T` cannot deduce the type of `std::initializer_list`.

*Case 2: 0 or NULL as null pointers*

This is explained in *Item 8* and is also related to template type deduction.

*Case 3: Declaration-only integral static const and constexpr data members*

This is a super specific scenario when we have:

{% highlight c++ %}
struct C {
  static constexpr std::size_t k = 10;
};
ftw(C::k); // might fail
{% endhighlight %}

The explanation is that `C::k` doesn't have an address in memory and since references are often implemented as pointers, this might fail in some compilers.

One natural question to ask is why rvalues are allowed to have references then? That's because the compiler will create a temporary object for the rvalue which in turn has some address.

This temporary object creation doesn't happen for `static constexpr`, at least for some compilers. For `clang` it works.

*Case 4: Overloaded function names and template names*.

{% highlight c++ %}
void callback(int x);
void callback(std::string x);

fwd(callback); // doesn't know which overload to pick
{% endhighlight %}

This also happens if `callback` is a template function.

*Case 5: Bitfields.*

Bitfields allow splitting a single type into multiple variables, for example:

{% highlight c++ %}
struct C {
  std::uint32_t field1:10, field2:22;
};
{% endhighlight %}

Here `field1` uses 10 bits and `field2` uses 22 bits from the 32 bits of `std::uint32_t`.

This also doesn't work with universal references:

{% highlight c++ %}
D d;
// non-const reference cannot bind to bit-field 'field2'
f(d.field2);
{% endhighlight %}

## Chapter 6 - Lambda Expressions

Things I learned from the chapter introduction:

The compiler creates classes for lambdas behind the scenes, called **closure class**. The lambda logic goes in the  `()` operator, which is `const` by default. The `mutable` keyword in the lambda changes that.

Assigning a lambda to a variable incurs in the creation of an instance, called **closure**. Closures can be copied.

### Item 31 - Avoid default capture modes

The item advises against using default capture by value (`[=]`):

{% highlight c++ %}
auto f = [=](...) {
  ...
};
{% endhighlight %}

or default capture by reference:

{% highlight c++ %}
auto f = [&](...) {
  ...
};
{% endhighlight %}

Because they can cause dangling references.

### Item 32 - Use init capture to move objects into closure

For move-only objects like `std::unique_ptr` we can use this syntax to move the object into the closure:

{% highlight c++ %}
auto p = std::make_unique<C>();
...
auto f = [p = std::move(p)](...) {
  ...
};
{% endhighlight %}

### Item 33 - Use decltype on auto&& to std::forward them

Generic lambdas are those having `auto` in their argument list:

{% highlight c++ %}
auto f = [](auto x) {
  return x;
};
{% endhighlight %}

The underlying closure class is implemented using templates, possibly as:

{% highlight c++ %}
class Closure {
public:
  template<typename T>
  auto operator()(T x) {
    return x;
  }
};
{% endhighlight %}

If we want the closure to take a universal reference (`auto&&`) and forward that argument, we don't have the template `T` available, so we can use `decltype`:

{% highlight c++ %}
auto f = [](auto&& x) {
  return g(std::forward<decltype<x>>(x));
};
{% endhighlight %}

### Item 34 - Prefer lambdas over std::bind

According to this item, there's never a reason to use `std::bind` after C++14. It claims that lambdas are more readable, expressive and can be more efficient than `std::bind`.

## Chapter 7 - The Concurrency API

### Item 35 - Prefer task-based programming to thread-based.

In other words, prefer `std::async` to `std::thread`. Example using threads:

{% highlight c++ %}
#include <thread>

void f() {
  std::cout << "work" << std::endl;
}
std::thread t(f);
t.join(); // wait on thread to finish
{% endhighlight %}

And async:

{% highlight c++ %}
#include <future>

auto fut = std::async(f);
fut.get();
{% endhighlight %}

The item suggests thread is a lower level abstraction than async, so async handles a lot of the details for you.

Another advantage of async is that you can get the result from the async function more easily than in a thread:

{% highlight c++ %}
int g() { return 1; }

auto fut = std::async(g);
auto result = fut.get();
std::cout << result << std::endl; // 1
{% endhighlight %}


### Item 36 - Specify std::launch::async if asynchronicity is essential

In line with Item 35's claim that async handles a lot of the details for you, one thing you can't assume is that it will always run the callback in a separate thread. It might actually wait and run the function in the current thread.

To force it to run as a separate thread we must use `std::launch::async`:

{% highlight c++ %}
auto fut = std::async(std::launch::async, f);
{% endhighlight %}

### Item 37 - Make std::thread unjoinable on all paths

A unjoinable thread is one in which the `.join()` cannot be called on. One example is when a thread has already been joined:

{% highlight c++ %}
std::thread t(f);
t.join();
// Exception: thread::join failed: Invalid argument
t.join();
{% endhighlight %}

Or when the thread has been moved:

{% highlight c++ %}
std::thread t(f);
g(std::move(t));
// Exception: thread::join failed: Invalid argument
t.join();
{% endhighlight %}

Or detached:

{% highlight c++ %}
std::thread t(f);
t.detach();
// Exception: thread::join failed: Invalid argument
t.join();
{% endhighlight %}

If a thread is *joinable* by the time it's destructed, the program crashes, for example:

{% highlight c++ %}
{
  auto t = std::thread([]() {
    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
  });
} // t is destroyed. Abort trap: 6
{% endhighlight %}

The recommendation of this item is to make sure threads are made unjoinable before they get destroyed.

As one way to achieve this, the author proposes a RAII-wrapper around `std::thread` called `ThreadRAII`, that enables configuring whether to call `.join()` or `.detach()` on the underlying thread at the `ThreadRAII`'s destructor, effectively guaranteeing a thread is never left joinable on destruction.

### Item 38 - Be aware of varying thread handle destructor behavior

This item discusses the behavior of the destructor of `std::future` (the type returned by `std::async`). If it's executed asynchronously either explicitly via the flag `std::launch::async` or implicitly (*Item 36*), the destructor behavior changes, because it calls `.join()` on the underlying thread.

{% highlight c++ %}
{
  auto fut = std::async (std::launch::async, []() {
      std::this_thread::sleep_for(std::chrono::milliseconds(10000));
  });
} // blocks
{% endhighlight %}

Note this behavior is different from when a raw `std::thread` is destroyed (*Item 37*).

### Item 39 - Consider void futures for one-shot event communication

This item discusses a scenario where we have 2 threads, `t1` and `t2` and we'd like `t2` to wait for `t1` until it signals it. One way to do this is using `std::conditional_variable` + `std::unique_lock` + `std::mutex`:

{% highlight c++ %}
#include <condition_variable>
#include <mutex>

std::condition_variable cv;

auto t1 = std::thread([&cv]() mutable {
  std::this_thread::sleep_for(std::chrono::milliseconds(1000));
  std::cout << "setting value" << std::endl;
  cv.notify_one();
});

auto t2 = std::thread([&cv](){
  std::cout << "waiting" << std::endl;
  std::mutex m;
  std::unique_lock lk(m);
  cv.wait(lk);
  std::cout << "waited" << std::endl;
});

t1.join();
t2.join();
{% endhighlight %}

The item argues this is hacky and suffers from issues like `cv.notify_one()` running before `cv.wait(lk)`, which causes the latter to hang. It proposes an alternative using `std::promise` + `std::future`:

{% highlight c++ %}
#include <future>

std::promise<void> p;
auto fut = p.get_future();
auto t1 = std::thread([p = std::move(p)]() mutable {
  std::this_thread::sleep_for(std::chrono::milliseconds(1000));
  std::cout << "setting value" << std::endl;
  p.set_value();
});

auto t2 = std::thread([fut = std::move(fut)](){
  std::cout << "waiting" << std::endl;
  fut.wait();
  std::cout << "waited" << std::endl;
});

t1.join();
t2.join();
{% endhighlight %}

The major downside of this approach is that it can only be used once.

### Item 40 - Use std::atomic for concurrency, volatile for special memory

Independent assignment like:

{% highlight c++ %}
a = b;
x = y;
{% endhighlight %}

Can be re-ordered either by the compiler or by the underlying hardware to improve efficiency. This poses a problem for concurrent programming because we might use an independent variable to indicate some computation has taken place:

{% highlight c++ %}
bool isReady {false};
auto result = compute();
// indicates that computation has taken place
isReady = true;
{% endhighlight %}

If another thread relies on `isReady` to determine `compute()` has been run, we can't let the compiler re-order the last two statements.

`std::atomic` prevents that by telling the compiler: if an expression appears before a write to an `std::atomic` variable in the source code, then it has to be executed before such write in runtime.

{% highlight c++ %}
std::atomic<bool> isReady {false};
auto result = compute();
isReady = true;
{% endhighlight %}

There's another optimization compilers can do, regarding redundant reads and writes. In the code below, the initial assignment of `y` is never used and is later overwritten:

{% highlight c++ %}
auto y = x;
compute();
y = x;
{% endhighlight %}

The compiler might want to re-write this as:

{% highlight c++ %}
compute();
auto y = x;
{% endhighlight %}

However, it's possible that `y` writes to a special memory (e.g. an external device) instead of RAM and some other system might depend on that side-effect. `volatile` prevents this optimization from happening.

{% highlight c++ %}
volatile auto y = x;
compute();
y = x;
{% endhighlight %}

Note that this is still subject to re-ordering, so we could combine `volatile` and `std::atomic`:

{% highlight c++ %}
volatile std:atomic<int> y = x;
compute();
y = x;
{% endhighlight %}

## Chapter 8 - Tweaks

### Item 41 - Consider pass by value for copyable parameters that are cheap to move and always copied

Suppose we have a function that takes a reference and makes a copy of it internally:

{% highlight c++ %}
class B {
  void set(C& r) {
    c_ = r;
  }
  C c_;
};
{% endhighlight %}

We'd want to also support a rvalue reference overload to avoid making additional copies for rvalues:

{% highlight c++ %}
class B {
  void set(C& r) {
    c_ = r;
  }
  void set(C&& r) {
    c_ = std::move(r);
  }
  C c_;
};
{% endhighlight %}

If `C` is cheap to move, we can simplify things and just take `r` by value:

{% highlight c++ %}
class B {
  void set(C r) {
    c_ = std::move(r);
  }
  C c_;
};
{% endhighlight %}

Let's first compare this new form with the `set(C& r)` case. When we call `set(C r)` with an lvalue, we'll copy-construct it when calling `set()`, but avoid the copy when move-assigning to `c_`. Whereas for `set(C& r)`, we avoid a copy when calling `set()` but make a copy when assigning to `c_`. Assuming moving is cheap, they incur in roughly the same cost.

Now compare with the `set(C&& r)` case. When we call `set(C r)` with an rvalue we'll move-construct it when calling `set()` and we'll move-assign to `c_`. For `set(C& r)` we'll do two move-assigns. Again, assuming moving is cheap, they incur in roughly the same cost.

### Item 42 - Consider emplacement instead of insertion

Many STL containers support emplacement instead of insertion. For example, `std::vector` has `emplace_back()`. Emplace methods take the constructor arguments instead of object, so it avoids temporary object creation if the argument has different type but has a contructor that accepts it.

For example, suppose we have a class `C` that can be constructed from `int`. If we call `push_back(1)`, first we'll create a temporary object via `tmp = C(1)` then copy it when doing `push_back(tmp)`:

{% highlight c++ %}
struct C {
  C(const C &c) {
    std::cout << "copy-construct" << std::endl;
    x_ = c.x_;
  }
  C(int x) {
    std::cout << "construct from int" << std::endl;
    x_ = x;
  }
  int x_;
};

std::vector<C> v;
// construct from int
// copy-construct
v.push_back(1);
{% endhighlight %}

If we use `emplace_back(1)`, we'll only call `C(1)` done inside `std::vector`:

{% highlight c++ %}
std::vector<C> v;
// construct from int
v.emplace_back(1);
{% endhighlight %}


## Conclusion
{:.no_toc}

I really liked reading this book cover-to-cover and learned a lot! The book is rather verbose but it has a very fluid narrative and thus is smooth to read.

Despite the verbosity, the book has a lot of content and I had trouble summarizing, even leaving out the rationale for the recommendation. The markdown text for the post has over 1k lines (usually it's fewer than 200).

The book also manages to make the content accessible but also detailed and technically precise. One downside is that at times the author spends a lot of time discussing what it seems like an extreme corner case, for example, in *Item 27* (on avoiding overloaded universal references), the section called "Constraining templates that take universal references".

## Related Posts
{:.no_toc}

* [Namespace Jailing]({{blog}}/2021/07/02/namespace-jail.html) - In that post we used thread syncrhonization using pipes, which could is another alternative for *Item 39*.
* [Move Semantics in C++]({{blog}}/2022/03/01/moving-semantics-cpp.html) - As discussed above, that post overlaps in content with *Item 23*.
* [Smart Pointers in C++]({{blog}}/2022/06/10/smart-pointers-cpp.html) - As discussed above, that post overlaps in content with *Item 18*, *Item 19* and *Item 21*.

## References
{:.no_toc}

* [1] Effective Modern C++, Scott Meyers.
