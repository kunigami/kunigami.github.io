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

## Constants

Constant string:

{% highlight c++ %}
constexpr char s[] = "some_constant";
{% endhighlight %}

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

## String View

[C++17] Optimization for read-only strings which does not make copies on assignment.

{% highlight c++ %}
std::string_view str1{"my_string"};
std::string_view str2{ str2 }; // No copy
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
std::vector<int> vec = {10, 20, 30};
{% endhighlight %}

### Iterate

{% highlight c++ %}
for(auto e:vec) {
    std::cout << e << std::endl;
}
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

## Inheritance

{% highlight c++ %}
class Base {
};

// public|protected|private here makes all
// methods from Base at most that visibility
class Child : public Base {

}

// Parent constructor needs to be called
// first thing. If processing is need before
// that, can call a function that returns an
// argument to Base()
Child::Child(void) : Base() {
}
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
