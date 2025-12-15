---
layout: post
title: "[Book] Effective C++"
tags: [review, c++]
excerpt_separator: <!--more-->
vanity: "2023-02-15-review-effective-cpp"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/book_cover.jpg" alt="Effective C++ book cover" />
</figure>


In this post I'll share my notes on the book *Effective C++* by Scott Meyers.

Meyers’ book is organized around items, each of which describes specific recommendations and then it delves into the rationale, while also explaning details about the C++ language.

In my [review]({{blog}}/2022/10/25/review-effective-modern-cpp.html) for *Effective Modern C++* I included a summary for every single item, but in this post I'll only include items I found particularly useful. The reason is that I read this book after *Effective Modern C++* and I knew more "non-modern" C++ than I did modern C++ so I learned less thoroughout than before.


<!--more-->

## Organization of the book
{:.no_toc}

The book is divided into 9 chapters and 55 items. Each chapter serves as a theme into which the items are organized.

1. TOC
{:toc}


## Chapter 1: Accustoming Yourself with C++

### Item 3 - Use const whenever possible

Using more restricted types whenever possible seems like a good practice for reasoning about code, so no surprises here. This is the same rationale behind Item 13 ([Prefer const iterators]({{blog}}/2022/10/25/review-effective-modern-cpp.html#item-13---prefer-const-iterators)) and 15 ([Use constexpr wherever possible
](https://www.kuniga.me/blog/2022/10/25/review-effective-modern-cpp.html#item-15---use-constexpr-wherever-possible
)) on *Effective Modern C++* [2].

Some things I learned: `const MyType *p` and `MyType const *p` are the same thing, i.e. the object the pointer points to is `const` (object cannot be mutated). This is different from `MyType * const p`, in which the pointer itself is constant (pointer cannot be re-assigned).

Some examples:

{% highlight c++ %}
struct C {
  void change() {}
};

const C *p1 = new C();
p1->change(); // error: cannot mutate underlying object
p1 = nullptr; // reassigning is ok

C * const p2 = new C();
p2->change(); // mutating underlying object is ok
p2 = nullptr; // error: cannot reassign
{% endhighlight %}

**Syntactic vs Semantic constness.** The syntactic constness is the one enforced by the compiler. Semantic constness is one that is application-dependent.

Sometimes we want to bypass syntatic constness if we can guarantee semantic constness. For example, suppose we have a wrapper class with method `get()` that returns its value:

{% highlight c++ %}
struct C {
  C(int x): x_(x) {}
  int get() const {
    return x_;
  }
  int x_;
};
{% endhighlight %}

We can make the method `const` since we don't change the internal state. However, suppose we want to keep track of how many times `get()` is called:

{% highlight c++ %}
struct C {
  C(int x): x_(x) {}
  int get() const {
    c_++;
    return x_;
  }
  int x_;
  int c_ = 0;
};
{% endhighlight %}

This will not compile because we're mutating `c_`. However, `c_` is an implementation detail that is never exposed (maybe it logs to a file upon destruction). So we can still mutate it without violating *semantic* constness. We can thus use `mutable` to bypass *syntatic* constness:

{% highlight c++ %}
struct C {
  C(int x): x_(x) {}
  int get() const {
    c_++;
    return x_;
  }
  int x_;
  mutable int c_ = 0;
};
{% endhighlight %}

### Item 4 - Make sure that objects are initialized before they’re used

One interesting bit on this item is on the presence of `extern` and multiple translation units. For example, suppose we declare a class `C` and a global variable `c` in a file `singleton.h`. Since `singleton.h` might be included by multiple translation units, we can't initialize `c` there, but rather in the `.cpp` file":

{% highlight c++ %}
// singleton.h
#include <string>
struct C {
  void f(std::string s) {
    std::cout << "value: " << s << std::endl;
  }
};
extern C c; // this doesn't call any constructor!

// singleton.cpp
C c(1); // actual init
{% endhighlight %}

Suppose we then use `c` in `use.cpp`:

{% highlight c++ %}
// use.cpp

#include "singleton.h"

void f() {
    c.f("use1");
}
{% endhighlight %}

The item claims that there's no guarantee `singleton.cpp` will be executed and hence that `c` will be initialized before `use.cpp` uses it, so one option is to declare `c` as static behind a function:

{% highlight c++ %}
// singleton.h
#include <string>
struct C {
  void f(std::string s) {
    std::cout << "value: " << s << std::endl;
  }
};
C& get();

// singleton.cpp
C& get() {
  static C c(1);
  return c;
}
{% endhighlight %}

Then whenever `use.cpp` needs `c`, it gets via `get()`. This will guarantee `get()` will be executed and `c` initialized.

## Chapter 2: Constructors, Destructors, and Assignment Operators

### Item 7 - Declare destructors virtual in polymorphic base classes

Suppose we have class `Base` and `Derived`. It's possible to create an object of `Derived` on the heap and carry it around via a pointer of type `Base *`:

{% highlight c++ %}
struct Base {
};
struct Derived : Base {
};

Base* ptr = new Derived();
{% endhighlight %}

However, when we delete `ptr`, it doesn't know it has to call `Derive`'s destructor:

{% highlight c++ %}
struct Base {
  ~Base () {
    std::cout << "~Base" << std::endl;
  }
};
struct Derived : Base {
  ~Derived () {
    std::cout << "~Derived" << std::endl;
  }
};

Base* ptr = new Derived();
// Prints ~Base
delete ptr;
{% endhighlight %}

This could lead to memory leak since `Derived`'s variables haven't been cleaned up. The solution is to declare `Base`'s destructor virtual, so that `ptr`'s destructor will be kept in a vtable in runtime, so that when it comes time to delete it will call `~Derived()`:

{% highlight c++ %}
struct Base {
  ~Base () {
    std::cout << "~Base" << std::endl;
  }
};
struct Derived : Base {
  ~Derived () {
    std::cout << "~Derived" << std::endl;
  }
};

Base* ptr = new Derived();
// Prints ~Derived, ~Base
delete ptr;
{% endhighlight %}

Note that `~Derived` implicitly calls its parent destructor `~Base`. Also note the destructor call order (child, parent) is the reverse of of the constructor order (parent, child), which makes sense.

A rule of thumb is that if there is at least one virtual method in `Base`, we should declare the destructor `virtual`. The condition is an indicator `Base` is meant to be derived from and possible that instances of the derived classes will be assigned to `Base *`, so it might be deleted via such pointer as well.

We should avoid always declaring a destructor virtual since it has the usual overhead of virtual functions.

### Item 8 - Prevent exceptions from leaving destructors

This item suggests that using `std::vector` or any other STL container with an object whose destructor can throw exception can lead to undefined behavior. The solutions involve things like terminating the program or swallowing the exception.

### Item 9 - Never call virtual functions during construction and destruction

The key is that inside a class constructor the type of `this` has the type of the class itself and not the type of the class being instantiated, so virtual functions have no effect. Example:

{% highlight c++ %}
struct C {
  C() {
    // *this is C
    f();
  }
  virtual void f() {
    std::cout << "base" << std::endl;
  }
};

struct D : C {
  void f() {
    cout << "derived" << endl;
  }
};

// prints base
D d;
{% endhighlight %}

### Item 11 - Handle assignment to self in operator=

One thing I realized from this item and which is clear in hindsight is that when we assign something to a variable `x`, say `x = y`, we don't simply make `x` point to the value of `y`. We actually invoke `x`'s `operator=`, which either copie or moves the contents from `y` onto `x`. This is different from higher level languages such as Python. Thus if we take the address of `x` before and after the assignment, they're the same.

The self assignment `x = x` is not a no-op operation, so if we're implementing our own `operator=`, this case must be handled.

## Chapter 3: Resource Management

For this chapter, resource managing classes refer to classes implementing the RAII (*Resource Acquisition Is Initialization*) pattern.

### Item 15 - Provide access to raw resources in resource managing classes

The item claims that RAII is not meant to provide encapsulation, so providing access to the underlying resource is not an anti-pattern.

One new thing I learned from this item is the implicit conversion operator! Here's a minimal example:

{% highlight c++ %}
struct B {
};
struct A {
  operator B() const {
    B b;
    return b;
  }
};

A a;
// operator B() allows A to be
// implicitly converted to B
B b = a;
{% endhighlight %}

## Chapter 4: Design and Declarations

I feel like this chapter has many subjective recommendations than the others, and concerns not necessarily with C++ but rather object oriented programming and sofware engineering as a whole.

### Item 20 - Prefer pass-by-reference than pass-by-value

One issue that pass-by-reference avoids is the *parameter slicing*. Parameter slicing happens when we pass an instance of a derived class to a function expecting a base class as value. Since a copy happens, the derived class instance gets "downcast" to the base class, so virtual function have no effect. For example:

{% highlight c++ %}
struct C {
  virtual void f() {
    cout << "c" << endl;
  }
};

struct D : C {
  void f() {
    cout << "d" << endl;
  }
};

void g(C c) {
  c.f();
}

D d;
g(d); // prints "c"
{% endhighlight %}

If we switch `g()` to:

{% highlight c++ %}
void g(const C& c) {
  c.f();
}

D d;
g(d); // prints "d"
{% endhighlight %}

Then the virtual function behaves as expected.

### Item 25 - Consider support for a non-throwing swap

This item provides some guidance on customizing the `std::swap()` for a specific class. By default, `std::swap()` relies on the copy-constructor of the class to perform swaps generically.

However it might be the case the class can perform swaps more efficiently, so it recommends adding a template specializion for the`std::swap()`:

{% highlight c++ %}
struct C {
  void swap(C &other) {
    cout << "custom swap" << endl;
  }
};
namespace std {
  template<>
  void swap(C &a, C &b) {
    a.swap(b);
  }
}
{% endhighlight %}

Things get more complicated when `C` is a template class, in which we cannot template-specialize `std::swap()`. In this case the recommendation our own `swap()` but when doing any swaps, we do:

{% highlight c++ %}
using std::swap;
swap(a, b);
{% endhighlight %}

This will first look for any custom `swap()` functions and if one is not found, it fallsback to `std::swap()`.

## Chapter 5: Implementations

### Item 27 - Minimize casting

Cast is often a sign of bad design. The `dynamic_cast<C>()` function adds overhead since it requires using `strcmp` and needs to traverse the hierarchy of `C`.

Static casts are not simply a compile time thing that gets erased during execution like say JavaScript's flow or Python's mypy. For example in the code below:

{% highlight c++ %}
struct C {
  void f() {
    static_cast<C>(*this).inc();
    cout << _cnt << endl;
  }
  void inc() {
    _cnt++;
  }
  int _cnt = 0;
};

C c;
// prints 0
c.f();
{% endhighlight %}

`static_cast<C>(*this)` creates a new temporary object which is different from `this`, so `this->_cnt` is not incremented.

### Item 30 - Understand inlining

Inline is a request to the compiler, not a command, so they're free to ignore. They'll ignore inline requests for virtual functions, for example.

Inline functions should be in header files because inlining usually happens during compilation (i.e. not linking). It can be explicit via the `inline` qualifier or implicit by defining the method along the class declaration.

We should avoid inlining unless it’s proven they help with code size or performance.

### Item 32 - Make sure public inheritance models is-a

This one seems like basic OOP but it's not obvious. I haven't thought of the difficulties of modeling a square class as a derived class from a rectangle but this item provides an example where a rectangle violates invariants of square, for example:

{% highlight c++ %}
struct Rectangle {
    Rectangle(int w, int h) : w_(w), h_(h) {}
    int w_;
    int h_;
};

struct Square : Rectangle {
    Square(int l) : Rectangle(l, l) {}
};
{% endhighlight %}

This looks reasonable until we add a method `setHeight()` to `Rectangle`. This breaks the invariant that `Square`'s width and height must be the same. So in OOP terms, the item claims that `Square` is-a `Rectangle` is false and so we shouldn't have this hierarchy.

It's unclear how this heuristic applies in the presence of virtual methods. If we made `setHeight()` virtual we could simply overload it in `Square` by either throwing or changing the width as well.

### Item 33 - Avoid hiding inherited names

When we override a function from a base class in a derived one, none of the overloads of that function name are inherited. For example, suppose we have a method `f()` in a base class `C` with two overloads: `f()` and `f(int)`, and that a derived class `D` overrides `f()`:

{% highlight c++ %}
struct C {
  virtual void f() {
  }
  void f(int x) {
  }
};

struct D : public C {
  void f() override {
  }
};
{% endhighlight %}

Because `D` overrides `f()`, none of the other overloads are inherited, in this case `f(int)`, so when we try to use it we get an error:

{% highlight c++ %}
D d;
d.f(1); // error!
{% endhighlight %}

The fix is to explicitly inherit the overloads via `using`:

{% highlight c++ %}
struct D : public C {
  using C::f;
  void f() override {
  }
};
{% endhighlight %}


### Item 37 - Never redefine a function's inherited default parameter

Default params are statically bound, meaning a function will use the default value from the variable's static type, not the one from runtime. This is regardless of whether the function in question is virtual. For example,

{% highlight c++ %}
struct C {
  virtual void f(int x = 0) {
    std::cout << "C: " <<  x << std::endl;
  }
};

struct D : C {
  void f(int y = 1) override {
    std::cout << "D: " <<  y << std::endl;
  }
};

D* d = new D();
d->f(); // D: 1
C *c = new D();
c->f(); // D: 0
{% endhighlight %}

When we do `C *c = new D()`, we'll correctly call `D::f()` since it's overriding a virtual function but the default value will be that from `C::f()`, that is, `0`.

### Item 40 - Use multiple inheritance judiciously

When a class inherits the same function signature from different parents, for example:

{% highlight c++ %}
struct Left {
  void f() {
    std::cout << "Left" << std::endl;
  }
};

struct Right {
  void f() {
    std::cout << "Right" << std::endl;
  }
};

struct C : public Left, public Right {
};
{% endhighlight %}

We can disambiguate which `f()` we refer to via the following syntax:

{% highlight c++ %}
C c;
c.Left::f();
c.Right::f();
{% endhighlight %}

When we have a diamond pattern, like below:

{% highlight c++ %}
struct Base {
  int x;
};

struct Left : Base {};

struct Right : Base {};

struct C : public Left, public Right {};
{% endhighlight %}


Class `C` inherits variable `x` twice, one via `Base > Left` and one via `Base > Right`. So doing

{% highlight c++ %}
C c;
c.x++;
{% endhighlight %}

Will result in ambiguous call. It's possible to inherit `x` only once by using `virtual inheritance`:

{% highlight c++ %}
struct Base {
  int x;
};

struct Left : virtual Base {};

struct Right : virtual Base {};

struct C : public Left, public Right {};

C c;
c.x++; // okay
{% endhighlight %}

## Chapter 7: Templates and Generic Programming

### Item 41: Understand implicit interfaces and compile-time polymorphism

This item explains that templates are implicit interfaces as opposed to classes, which are explicit interfaces. Polymorphism in templates is done at compile time vs. in classes where they are done in runtime (by use of the virtual table).

### Item 42: Understand the two meanings of typename

In template declarations `class` and `typename` are equivalent:

{% highlight c++ %}
template<typename T>
struct C {};

template<class T>
struct C {};
{% endhighlight %}

When referring to inner fields of template types `T`, as known as **nested dependent names**, such as `T::f`, `f` could be either a type itself or value:

{% highlight c++ %}

struct A {
  using f = int;
};

struct B {
  inline static int f = 1;
};
{% endhighlight %}

In this example `A::f` is `int` but `B::f` is `1`. In the context of templates, if the compiler doesn't know the actual type bound to a type `T`, it assumes `T::f` is not a type, unless we specify it explicitly by qualitying it with `typename`:

{% highlight c++ %}
template<typename T>
struct C {
  void f() {
    // T::f is not a type
    int a = T::f;
  }
  void g() {
    // T::f is a type
    typename T::f b;
  }
};
{% endhighlight %}

### Item 43 - Know how to access names in templatized base classes

Template specialization does not inherit methods from its template class. For example:

{% highlight c++ %}
template<typename T>
struct C {
  void f() {}
};

template<>
struct C<int> {
};

template<typename T>
struct B : C<T> {
  void g() {
    f();
  }
};

B<int> b;
{% endhighlight %}

This will not compile because `B<int>` inherits from `C<int>` which doesn't not have the `f()` method. To work around that, using `this->f()` in `g()` will indicate to the compiler we want `C<int>` to inherit `f()`.

### Item 44 - Factor parameter-independent code out of templates

Because template classes generate copies for each type the template is invoked with, it's recommended to move as much code as possible outside of templated classes. To experiment with this problem, I wrote this setup:

{% highlight c++ %}
// main.cpp
#include "lib.h"

int main() {
  C<bool> b;
  f();
  return 0;
}

// lib.h
#pragma once

template<typename T>
struct C {
  int f() {
    return 1 + 1;
  }
};

int f(void);

// lib.cpp
#include "lib.h"
int f() {
  C<int> c;
  return c.f();
}
{% endhighlight %}

It's possible to see the generated code using `clang` via [2]:

{% highlight text %}
clang++ -Xclang -ast-print -fsyntax-only
{% endhighlight %}

In this case the body of the function `f()` is generated twice, one for `C<int>`, another for `C<bool>`.

## Chapter 8 - Customizing new and delete

I found the items on this chapter *very* specific. It was interesting to learn about `std::set_new_handler()` though.

### Item 49 - Understand the behavior of new handler

One interesting bit I learned is the `std::set_new_handler()` function which allows registering a callback for when the allocator fails to allocate enought memory. Meyers provides an RAII implementation such that each class has its own handler.

Unrelated to the new handler, I also learned about the *Curiously Recurring Template Pattern* (CRTP). This is useful when you want one static variable per class but have it declared in a base class. For example:

{% highlight c++ %}
template<typename T>
struct C {
  inline static int x = 0;
};

struct B1 : C<B1> {
  B1() { x++; }
};

struct B2 : C<B2> {
  B2() { x++; }
};

B1 b1;
B2 b2, b3;
// 1
std::cout << B1::x << std::endl;
// 2
std::cout << B2::x << std::endl;
{% endhighlight %}

Since `C<B1>` is completely independent of `C<B2>`, it also has its own copy of `C::x`, so `B1::x` and `B2::x` are independent.

## Chapter 9 - Misc

### Item 54 - Familiarize yourself with the standard library, including TR1

Many of TR1 features from this time made into C++11 onwards, so there wasn't much new. I don't recall seeing `std::mem_fn` before and it's handy for keeping pointer to class methods. The [documentation](https://cplusplus.com/reference/functional/mem_fn/) has a short and clear example:

{% highlight c++ %}
struct int_holder {
  int value;
  int triple() {return value*3;}
};
int_holder five {5};
auto triple = std::mem_fn (&int_holder::triple);
std::cout << triple(five) << '\n';
{% endhighlight %}

Another thing I learned is that you can alias namespaces:

{% highlight c++ %}
namespace b::c {
    int x;
}

namespace a {
    namespace d = b::c;
}

a::d::x; // ok
{% endhighlight %}

## Conclusion
{:.no_toc}

*Effective C++* is useful but overall I got a lot more value from *Effective Modern C++*. I found the latter less subjective and more generally applicable.

I thought I liked the typography better from *Effective Modern C++* but comparing them side by side I can't decide anymore. One thing I noticed only when comparing the fonts is that *Effective C++* code snippets are not in monospace! I'm very surprised I didn't noticed it while reading the book.

## Related Posts
{:.no_toc}

[[Book] Effective Modern C++]({{blog}}/2022/10/25/review-effective-modern-cpp.html). It was interesting to read the book series in reverse chronological order and seeing how things evolved. The smart pointers and type traits from TR made into the STL for example and some recommendations around `std::bind` were made obsolete.

[Type Traits in C++]({{blog}}/2022/12/08/type-traits-in-cpp.html). Type traits leverage the template mechanisms discussed in *Chapter 7*. In fact *Item 47* is about type traits but I omitted in my review because many things changed regarding type traits in C++11 and I had learned a lot about type traits from writing the post already.

## Recap
{:.no_toc}

* *Why declare the destructor virtual if there is at least one virtual method in the class?* Because if we delete a derived class instance from a base class pointer the derived class destructor won't be called.
* *What is parameter slicing?* It's when we "downcast" a derived object to a base one when passing it by value to a function.
* *Why inline functions should be in header files?* Because inline happens during compilation not linking.
* *Why do we need to include `typename` when using nested dependent names?* Because nested dependent names can be either types or values and we need to disambiguate.

## References
{:.no_toc}

* [1] Effective C++, Scott Meyers.
* [[2](https://stackoverflow.com/questions/4448094/can-we-see-the-template-instantiated-code-by-c-compiler)] StackOverflow - Can we see the template instantiated code by C++ compiler
