---
layout: doc
title: "C++ Cheatsheet"
---

Syntax for common tasks I run into often. Assumes C++17.

# Index
{:.no_toc}

1. TOC
{:toc}

# Basic Types

## Array

Literal initialization:

{% highlight c++ %}
int int_array[5] = { 1, 2, 3, 4, 5 };
{% endhighlight %}

Array of objects, initialize later:

{% highlight c++ %}
MyClass class_array[3];
MyClass c;
class_array[1] = c;
{% endhighlight %}

### Array vs std::array

`std::array()` has friendlier semantics ([discussion](https://stackoverflow.com/questions/30263303/stdarray-vs-array-performance)).

## Conversions

### int to string

{% highlight c++ %}
#include <string>
std::string = to_string(10);
{% endhighlight %}

## Enum

{% highlight c++ %}
enum class MyBoolean { kYes, kNo };
{% endhighlight %}

## Optional

Create:

{% highlight c++ %}
// None
std::optional<std::string> x = std::nullopt;
// Value
std::optional<std::string> y("hello");
{% endhighlight %}

Access:

{% highlight c++ %}
std::cout << *y << std::endl;
{% endhighlight %}

Check:

{% highlight c++ %}
std::optional<int> y = 0;
if (!y) {
  std::cout << "none" << std::endl;
} else {
  std::cout << *y << std::endl;
}
{% endhighlight %}

## Strings

### Literal

> The constexpr specifier declares that it is possible to evaluate the value of the function or variable at compile time.

{% highlight c++ %}
constexpr char s[] = "some_constant";
{% endhighlight %}


### View

[C++17] Optimization for read-only strings which does not make copies on assignment.

{% highlight c++ %}
std::string_view str1{"my_string"};
std::string_view str2{ str2 }; // No copy
{% endhighlight %}

## Sstream

### Osstream

Construct streams using the `<<` syntax.

{% highlight c++ %}
#include <sstream>
#include <iostream>

std::ostringstream ss;
ss << "hello";
ss << " world";
std::cout << ss.str() << std::endl;
{% endhighlight %}

## Struct

{% highlight c++ %}
struct MyStruct {
    int x;
    double z;
}
{% endhighlight %}

Initialization:

{% highlight c++ %}
MyStruct my_struct = {.x = 1, .z = 0.5};
{% endhighlight %}

# Collections

## Hash Map

In C++ `unordered_map` implements a hash map. Search, insertion, and removal of elements have average constant-time complexity.

### Import

{% highlight c++ %}
#include <unordered_map>
{% endhighlight %}

### Initialization

Empty map:

{% highlight c++ %}
std::unordered_map<std::string, int> h;

std::unordered_map<std::string, int> m = { {'a', 1}, {'b', 2} };
{% endhighlight %}

### Access

{% highlight c++ %}
std::cout << h["key"] << std::endl;
{% endhighlight %}

### Insert

{% highlight c++ %}
h["key"] = 100;
{% endhighlight %}

### Iterating

`x` is a pair `(key, value)`.

{% highlight c++ %}
for (auto x:h) {
    std::cout << x.first << ", " << x.second << std::endl;
}
{% endhighlight %}

### Search

{% highlight c++ %}
bool has_key = h.find("key") != h.end();
{% endhighlight %}

## Set


### Initialization

From literals:

{% highlight c++ %}
std::set<int> s = {4, 16, 9, 25};
{% endhighlight %}

### Remove

Remove entry (in-place):

{% highlight c++ %}
s.erase(value);
{% endhighlight %}


## Vector

### Initialization

{% highlight c++ %}
std::vector<int> vec{10, 20, 30};
{% endhighlight %}

### Iterate

{% highlight c++ %}
for(auto e:vec) {
    std::cout << e << std::endl;
}
{% endhighlight %}

### Empty

{% highlight c++ %}
vec.empty();
{% endhighlight %}


### Size

{% highlight c++ %}
v.size();
{% endhighlight %}

### Slice

Getting a subset of a vector for index `a` to index `b`:

{% highlight c++ %}
std::vector<int>(v.begin() + a, v.begin() + b + 1);
{% endhighlight %}

If `b = v.size() -1`:

{% highlight c++ %}
std::vector<int>(v.begin() + a, v.end());
{% endhighlight %}

# Memory

## Shared Pointers

Creating pointers:

{% highlight c++ %}
struct C {
  C(int i) : i(i) {}
  int i;
}
auto pt = std::make_shared<C>(1); // overload (1)
{% endhighlight %}

De-referencing is the same as a regular pointer:

{% highlight c++ %}
std::shared_ptr<int> pt (new int);
std::cout << *pt << std::endl;
{% endhighlight %}

# Flow Control

## Exceptions

{% highlight c++ %}
class CustomException: public std::exception
{
  virtual const char* what() const throw() {
    return "My exception";
  }
} custom_exception;

try {
    throw custom_exception;
} catch (const std::exception& e) {
    std::cout << e.what() << "\n";
}
{% endhighlight %}

# Object-Oriented

## Class

### Constructor

Defining constructor outside class definition:

{% highlight c++ %}
// Point.hpp
class Point {
    private:
        int x;
        int y;
    public:
        Point();
};

// Point.cpp
Point::Point(void) {
    x = 0;
    y = 0;
}

Point::Point(int _x, int _y) {
    x = _x;
    y = _y;
}
{% endhighlight %}

Explicit initialization:

{% highlight c++ %}
Point::Point(int x, int y): x(x), y(y)  {}
{% endhighlight %}

### Destructor

{% highlight c++ %}
// Point.hpp
class Point {
    ...
    public:
        ~Point();
};

// Point.cpp
Point::~Point(void) {
    // Clean up allocated memory
}
{% endhighlight %}

### Instantiate

When to use `new`:

{% highlight c++ %}
// Automatically destroyed when leaving scope
Point point(10, 20);

// Only destroyed when called w/ delete
Point point = new Point(10, 20);
{% endhighlight %}

### Methods

**Read-only method.** Add const after function signature.

{% highlight c++ %}
struct C {
  void my_method() const {
    // Cannot modify x ()
  }
  int x;
};
{% endhighlight %}

## Inheritance

{% highlight c++ %}
class Base {
};

class Child : public Base {

}

// Parent constructor needs to be called
// first thing. If processing is need before
// that, can call a function that returns an
// argument to Base()
Child::Child(void) : Base() {
}
{% endhighlight %}

### Visibility

One of `public|protected|private` (default is `private`). Makes all methods from `Base` the most restrictive between the inheritance visibility and the original method visibility.

{% highlight c++ %}
class Base {
protected:
    void f() {};
};

// ChildPrivate::f() is private
class ChildPrivate : Base {}
// ChildPublic::f() is protected
class ChildPublic : public Base {}
{% endhighlight %}

For `struct` the inheritance mode is `public` by default.

{% highlight c++ %}
class Base {
  public:
  void f() {};
};

// ChildStruct::f() is public
struct ChildStruct : Base {};
{% endhighlight %}

### Abstract methods

If you're coming from Java, C++ version of abstract methods are pure virtual methods.

{% highlight c++ %}
class Base {
    virtual void abstract() = 0;
};
{% endhighlight %}

### Interface

If you're coming from Java, C++ doesn't have syntax for interfaces but it can be modeled as class with all pure virtual methods.

{% highlight c++ %}
class Interface {
    virtual void foo() = 0;
    virtual int bar() = 0;
    virtual ~Interface() {};
};
{% endhighlight %}

Note that we need to define the virtual destructor in this case.

### Override methods

By default C++ binds the methods to the type, even if it was re-implemented in the derived class. Example:

{% highlight c++ %}
struct Base {
  void f() { printf("base\n"); };
};

struct Child : Base {
  void f() { printf("child\n"); };
};

void g(Base *x) { x->f(); }
auto x = new Child();
g(x); // prints "base"
{% endhighlight %}

In `g()`, `f()` is bound to `Base` (type) not to `Child` (instance). To change this behavior, `f()` must be `virtual` in `Base`:

{% highlight c++ %}
struct Base {
  virtual void fv() { printf("base\n"); };
};

struct Child : Base {
  void fv() { printf("child\n"); };
};

void g(Base *x) { x->f(); }
auto x = new Child();
g(x); // prints "child"
{% endhighlight %}

It's a good practice to add `override` so that the compiler catches this subtlety:

{% highlight c++ %}
struct Base {
  void f() {};
  virtual void fv() {};
};

struct Child : Base {
  // Error: Base::f() is not virtual
  void f() override {};
  void fv() override {}
};
{% endhighlight %}

You can also add override when implementing pure virtual methods.

# Template

## Function

{% highlight c++ %}
template <typename T>
T id(T x) {
   return x;
}
{% endhighlight %}

## Class

{% highlight c++ %}
template <typename T>
struct Wrapper {
    T v_;
public:
    Wrapper(T v) {
      v_ = v;
    }
    T get() {
      return v_;
    }
};
{% endhighlight %}

## Inner Type

It's possible to access the type of a template:

{% highlight c++ %}
template <typename T>
struct Wrapper {
    T v_;
    using TInner = T;
};

struct WrapperInt : Wrapper<int> {
  int get() {
    return 0;
  }
};
struct WrapperStr : Wrapper<std::string> {
  std::string get() {
    std::string s = "str";
    return s;
  }
};

template <typename T, typename TType = typename T::TInner>
TType getter(T val) {
  return val.get();
}
{% endhighlight %}

## Specialization

### Type alias

{% highlight c++ %}
template <typename T>
struct Generic {
  T v_;
};

using Int = Generic<int>;
{% endhighlight %}

### Implementation

In this setup the generic class acts almost like a interface.

{% highlight c++ %}
template <typename T>
struct Cast {
  static T cast(void *ptr);
};

template <>
struct Cast<bool> {
  static bool cast(void *ptr) {
    bool b = (bool*)ptr;
    return b;
  }
};

template <>
struct Cast<int> {
  static int cast(void *ptr) {
    int* b = (int*)ptr;
    return *b;
  }
};

template <typename T>
T factory(void *ptr) {
  return Cast<T>::cast(ptr);
}

bool b = true;
void *ptr = &b;

auto c = factory<bool>(ptr);
auto d = factory<int>(ptr);
// Linker error:
// Undefined symbols for architecture
auto e = factory<float>(ptr);
{% endhighlight %}

### Variadic

The generic class can use `typename ...T` to allow specialization by any number of types:

{% highlight c++ %}
template <typename ...T>
struct C {};

template <>
struct C<bool> {};

template <>
struct C<int, int> {};

C<int, int> pair_int;
C<bool> boolean;
{% endhighlight %}

# Compile Time

## Static Assert

Syntax:

{% highlight c++ %}
static_assert(<expr>, "message");
{% endhighlight %}

Where `<expr>` must be a constant expression.

## Constant Expressions

### Syntax

Function:

{% highlight c++ %}
constexpr int factorial(int n) {
    return n <= 1 ? 1 : (n * factorial(n - 1));
}
{% endhighlight %}

Class member:

{% highlight c++ %}
struct C {
  static constexpr int my_expr = 3 + 4;
};
{% endhighlight %}

## Type Traits

The working unit of a type trait is the type trait `std::integral_constant`, which wraps a constant and its type. Example:

{% highlight c++ %}
typedef std::integral_constant<int, 2> two_t;
{% endhighlight %}

Access the value:

{% highlight c++ %}
std::cout << two_t::value << std::endl;
{% endhighlight %}

A boolean type trait is so common it has its own alias:

{% highlight c++ %}
template <bool B>
using bool_constant = integral_constant<bool, B>;
{% endhighlight %}

### Type Equality (=)

Done via `std::is_same()`:

{% highlight c++ %}
template <typename T>
void is_int() {
   if (std::is_same<T, std::int32_t>::value) {
     std::cout << "is int" << std::endl;
   } else {
     std::cout << "isn't" << std::endl;
   }
}
factory<int>(); // is int
factory<double>(); // isn't
{% endhighlight %}

### Conjunction (&&)

Check multiple types: `std::conjunction()`. It expects one or more `std::bool_constant` types:

{% highlight c++ %}
template<typename T, typename... Ts>
void f() {
  if (std::conjunction<std::is_same<T, Ts>...>::value) {
    std::cout << "all types are the same\n" << std::endl;
  } else {
    std::cout << "not all types are the same" << std::endl;
  }
}

f<int, int>(); // same
f<int, bool>(); // not
{% endhighlight %}

# Files

## Check if file exists

Works for directories too.

{% highlight c++ %}
#include <filesystem>
namespace fs = std::filesystem;

bool file_exists = fs::exists("path");
{% endhighlight %}

## Create directory

{% highlight c++ %}
#include <filesystem>
namespace fs = std::filesystem;

fs::create_directory("path");
{% endhighlight %}

## Create temporary file name

{% highlight c++ %}
std::string filename = std::tmpnam(nullptr);
{% endhighlight %}


## Read from file

{% highlight c++ %}
#include <fstream>

std::ifstream my_file("filename");
if (my_file.is_open()) {
    while (getline(my_file, line)) {
        std::cout << line << std::end;
    }
}
{% endhighlight %}

## Write to file

{% highlight c++ %}
#include <fstream>

std::ofstream my_file("filename");
if (my_file.is_open()) {
    my_file << "a line" << std::end;
}
{% endhighlight %}

# Modules

## Header Files

Avoid importing the same file multiple times:

{% highlight c++ %}
#pragma once
{% endhighlight %}

Cleaner and less error-prone than using the triad `#ifndef/#define/#endif`.

# Functional

## Lambda

{% highlight c++ %}
auto is_less_than = [](auto a, auto b) {
    return a < b;
};
is_less_than(3, 3.4); // true
{% endhighlight %}

With bindings:

{% highlight c++ %}
int target = 5;
auto is_less_than_target = [target](auto a) {
    return a < target;
};
is_less_than_target(3); // true
{% endhighlight %}

With references bindings:

{% highlight c++ %}
int target = 5;
auto dec = [&target]() {
    target--;
};
dec();
target; // 4
{% endhighlight %}

By default the reference binding is `const`. To change it use `mutable`:

## Passing Function As Parameter

Lambda:

{% highlight c++ %}
std::vector<int> map(std::vector<int> v, std::function<int(int)> f) {
  std::vector<int> v2;
  for(auto e:v) {
    v2.push_back(f(e));
  }
  return v2;
}

std::vector<int> vec = {1, 2, 3};
auto f = [](int x) { return 2 * x; };
auto v2 = map(vec, f); // v2 = {2, 4, 6}
{% endhighlight %}

Class method:

{% highlight c++ %}
struct Caller {
  std::function<int(int)> cb_;
  Caller(std::function<int(int)> cb) : cb_(cb) {}

  int call(int x) {
    return cb_(x);
  }
};

struct K {
  Caller c_;
  K() : c_(std::bind(&K::f, this, std::placeholders::_1)) {}

  int f(int x) {
    return x + 1;
  }
};

auto obj = K();
cout << obj.c_.call(3) << endl;
{% endhighlight %}
