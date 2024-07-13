---
layout: doc
title: "C++ Cheatsheet"
---

Syntax for common tasks I run into often. Assumes C++17.

# Index
{:.no_toc}

- TOC
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

Strong enumeration, scoped enumeration

{% highlight c++ %}
enum class MyBoolean { kYes, kNo };
{% endhighlight %}

Unscoped enumeration (without `class` modifier):

{% highlight c++ %}
enum MyBoolean { kYes, kNo };
{% endhighlight %}

### Assigning

(scoped enum)

{% highlight c++ %}
MyBoolean b = MyBoolean::kYes;
{% endhighlight %}

### Getting value

(scoped enum)
{% highlight c++ %}
MyBoolean b = MyBoolean::kYes;
cout << static_cast<bool>(my_enum) << endl;
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

## Pair

Create:

{% highlight c++ %}
#include <utility>
auto p = std::make_pair(10, true);
{% endhighlight %}

Destructuring assigment:

{% highlight c++ %}
std::pair<int, std::string> getPair();

int first;
std::string second;
std::tie(first, second) = getPair();
{% endhighlight %}

NOTE: Works with tuples too.

Ignoring one of the members in the pair:

{% highlight c++ %}
std::tie(std::ignore, second) = getPair();
{% endhighlight %}

## Strings

### Literal

> The constexpr specifier declares that it is possible to evaluate the value of the function or variable at compile time.

{% highlight c++ %}
constexpr char s[] = "some_constant";
{% endhighlight %}


### View

Optimization for read-only strings which does not make copies on assignment.

{% highlight c++ %}
std::string_view str1{"my_string"};
std::string_view str2{ str2 }; // No copy
{% endhighlight %}

* `string_view::substr()` does not copy either.

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

## Type Alias

{% highlight c++ %}
using MyNewType = std::vector<std::string>;
{% endhighlight %}

# Collections

## Hash Map

In C++ `unordered_map` implements a hash map. Search, insertion, and removal of elements have average constant-time complexity. Keys don't have any particular order. `map` keeps the keys sorted.

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

### Emplace

{% highlight c++ %}
h.emplace("key", 100);
{% endhighlight %}

Can be used to avoid the creation of temporary objects.

Warning: only adds to map if the corresponding key does not exist.

### Insert

{% highlight c++ %}
h["key"] = 100;
{% endhighlight %}

### Iterating

`x` is a pair `(key, value)`.

{% highlight c++ %}
for (auto [key, value]:h) {
    std::cout << key << ", " << value << std::endl;
}
{% endhighlight %}

### Search

C++20 and after:

{% highlight c++ %}
bool has_key = h.contains("key");
{% endhighlight %}

Before:

{% highlight c++ %}
bool has_key = h.find("key") != h.end();
{% endhighlight %}

## Set

Prefer `std::unordered_set` over `std::set`.

### Initialization

From literals:

{% highlight c++ %}
std::unordered_set<int> s = {4, 16, 9, 25};
{% endhighlight %}

### Membership

Returns 1 if `element` exists, 0 otherwise. Faster than `.find()`.

{% highlight c++ %}
s.count(element);
{% endhighlight %}

### Remove

Remove entry (in-place):

{% highlight c++ %}
s.erase(value);
{% endhighlight %}

## Tuples

Include:

{% highlight c++ %}
#include <tuple>
{% endhighlight %}

### Initialization

{% highlight c++ %}
std::tuple<int, std::string> t = std::make_tuple(1, "hello");
{% endhighlight %}

or:

{% highlight c++ %}
auto t = std::make_tuple<int, std::string>(1, "hello");
{% endhighlight %}

### Access

`std::get<n>`. Example:

{% highlight c++ %}
std::cout << std::get<0>(t); // 1
std::cout << std::get<1>(t); // "hello"
{% endhighlight %}

### Concatenate

`std::tuple_cat`. Example:

{% highlight c++ %}
auto t = std::make_tuple<int, std::string>(1, "a");

auto u1 = std::make_tuple<int>(1);
auto u2 = std::make_tuple<std::string>("a");
auto u = std::tuple_cat(u1, u2);

assert (t == u);
{% endhighlight %}

### Variadic

Using `std::apply`, lambdas and variadic. Example: handle tuple in a generic fashion:

{% highlight c++ %}
// Let t be any generic tuple. t1 is a copy of t
auto t1 = std::apply(
  [](auto... args) {
    return std::make_tuple(desc...);
  },
  t
);
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

### Passing as parameter

{% highlight c++ %}
void f(std::vector<int> v) {}

// Type deduction
f({1, 2});

// Explicit type
f(std::vector<int> {1, 2});

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

## Placement New

Allows pre-allocating into a larger chunk of memory into a buffer and then `new`ing objects into that memory.

{% highlight c++ %}
char *buf = new char[10000];
C *p1 = new (buf) C(1);
C *p2 = new (buf + sizeof(*p1)) C(2);
delete [] buf;
{% endhighlight %}

Note that deletion is not performed for the placement new, only for the original buffer.

# Flow Control

## Conditionals

Initialize and evaluate inline:

{% highlight c++ %}
if (bool x = false; !c) {
  cout << "prints" << endl;
}
{% endhighlight %}

## Exceptions

Basic try/catch:

{% highlight c++ %}
try {
  throw std::runtime_error("Error");
} catch (const std::exception& e) {
    std::cout << e.what() << "\n";
}
{% endhighlight %}

Custom exception:

{% highlight c++ %}
class CustomException: public std::exception
{
  virtual const char* what() const throw() {
    return "My exception";
  }
} custom_exception;

throw custom_exception;
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

### Static Member Variables

Shared across instances.

{% highlight c++ %}
struct C {
  static int x;
};

// Definition and initialization must be done outside the class
int C::x = 0;
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

# Templates

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
    my_file << "a line" << std::endl;
}
my_file.close();
{% endhighlight %}

# Date and Time

## Time

### Duration

Structure: `std::chrono::duration<Rep, Period>`. `Rep` is the underlying type, e.g. `double` or `int`. `Period` is a rational number (`std::ratio`) indicating the relative scale factor to 1 second and is only relevant when converting between units.

*Examples*

{% highlight c++ %}
// 1 tick = 1 second
std::chrono::duration<int, std::ratio<1> sec(1);

// 1 tick = 60 seconds
std::chrono::duration<int, std::ratio<60>> min1(1);
// 60 ticks = 60 seconds
std::chrono::duration<int, std::ratio<1>> min2(60);
// comparison handles different periods
assert (min1 == min2);

// 30 ticks = 30 ms
std::chrono::duration<int, std::ratio<1, 1000>> ms(30);
{% endhighlight %}

*Helper duration types*

The actual underlying `Rep` for these types is open for compilers to determine but the [spec](https://en.cppreference.com/w/cpp/chrono/duration) dictates signed integers of a minimum size (shown in parenthesis below, in bits).

| Type | Bits |
| ---- | ---- |
| `std::chrono::nanoseconds` | 64 |
| `std::chrono::microseconds` | 55 |
| `std::chrono::milliseconds` | 45 |
| `std::chrono::seconds` | 35 |
| `std::chrono::minutes` | 29 |
| `std::chrono::hours` | 23 |

Some of the examples from above simplified:

{% highlight c++ %}
// 1 tick = 1 second
std::chrono::seconds sec(1);

// 1 tick = 60 seconds
std::chrono::minutes min1(1);
{% endhighlight %}

*Get value from duration*

It always returns the value in the scale defined by `Period`, so basically the value we passed when constructing it:

{% highlight c++ %}
std::chrono::duration<double, std::ratio<1, 30>> hz30(3.5)
cout << hz30.count() << endl; // 3.5
{% endhighlight %}

*Convert between units*

Syntax: `std::chrono::duration_cast<duration_type>(duration)`

Example: converts 1 tick of 60 seconds to 60 ticks of 1 second.

{% highlight c++ %}
std::chrono::minutes min1(1);
cout << min1.count() << endl; // 1
auto sec60 = std::chrono::duration_cast<std::chrono::seconds>(min1);
cout << sec60.count() << endl; // 60
{% endhighlight %}

*Negative durations*

Durations can be negative. There's a `abs()` overload, so a negative duration can be convert either as is or when getting `.count()`:

{% highlight c++ %}
std::chrono::minutes min1(-1);
cout << min1.count() << endl; // -1
cout << abs(min1).count() << endl; // 1
cout << abs(min1.count()) << endl; // 1
{% endhighlight %}


### Time Point

Structure: `std::chrono::time_point<Clock>`. `Clock` is the underlying mechanism for measuring time, most of times we use `std::chrono::system_clock`.

A duration can be obtaining by the different between two time points.

*Get current Unix timestamp*

{% highlight c++ %}
#include <chrono>
auto timestamp = std::chrono::system_clock::now();
auto unixtime = std::chrono::duration_cast<std::chrono::seconds>(
  timestamp.time_since_epoch()
).count()
{% endhighlight %}

# Modules

## Header Files

Avoid importing the same file multiple times:

{% highlight c++ %}
#pragma once
{% endhighlight %}

Cleaner and less error-prone than using the triad `#ifndef/#define/#endif`.

# Concurrency

See [Concurrency]({{ site.url }}/docs/cpp/concurrency.html).

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

When values are captured via copy they're `const`. To change it use `mutable`:

{% highlight c++ %}
int target = 5;
auto dec = [target]() mutable {
    target--;
    std::cout << target << std::endl;
};
dec();
{% endhighlight %}

Return types are inferred but they can be provided explicitly:

{% highlight c++ %}
int target = 5;
auto dec = [target]() -> int {
    return target - 1;
};
std::cout << dec() << std::endl;
{% endhighlight %}

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

# Namespaces

## Empty Namespace

Keywords: anonymous, unnamed

This makes `x` local to the file and doesn't risk being exposed if some other file includes it.

{% highlight c++ %}
namespace {
  int x;
}
{% endhighlight %}

## Nested namespace

{% highlight c++ %}
namespace n1::n2::n3 {
  int x;
}
{% endhighlight %}

Which is equivalent to

{% highlight c++ %}
namespace n1 { namespace n2 { namespace n3 {
  int x;
}}}
{% endhighlight %}

## Namespace aliasing

{% highlight c++ %}
namespace my_alias = some::other_namespace;
{% endhighlight %}

## Namespace resolution

Suppose we're in namespace `a::b`, and inside it we refer to a variable `x` via `c::x`. What namespace does the compiler search in?

* Search for the first namespace matching `a::b::c`.
* If not found, search for a namespace matching `a::c`.
* If not found, search for a namespace matching `c`.

Once if has a namespace, use the `x` defined there. Report error if not. The key insight is that it binds to a namespace **before** searching for symbols there, so this case:

{% highlight c++ %}
namespace a::b::c { }
namespace a::c { int x = 1; }
namespace a::b {
  int y = x;
}
{% endhighlight %}

Will lead to a compilation error since it binds to `a::b::c` first, even though `x` *is* defined in `a::c`. If we comment the first line it works.
