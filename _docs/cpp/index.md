---
layout: doc
title: "C++ Cheatsheet"
---

Syntax for common tasks I run into often. Assumes C++17, and `using namespace std` for brevity.

# Index
{:.no_toc}

1. TOC
{:toc}

# Basic Types

## Conversions

### int to string

{% highlight c++ %}
#include <string>
string = to_string(10);
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
unordered_map<string, int> h;
{% endhighlight %}

### Access

{% highlight c++ %}
cout << h["key"] << endl;
{% endhighlight %}

### Insert

{% highlight c++ %}
h["key"] = 100;
{% endhighlight %}

### Iterating

`x` is a pair `(key, value)`.

{% highlight c++ %}
for (auto x:h) {
    cout << x.first << ", " << x.second << endl;
}
{% endhighlight %}

### Search

{% highlight c++ %}
bool has_key = h.find("key") != h.end();
{% endhighlight %}

## Vector

### Initialization

{% highlight c++ %}
vector<int> vec = {10, 20, 30};
{% endhighlight %}

### Iterate

{% highlight c++ %}
for(auto e:vec) {
    cout << e << endl;
}
{% endhighlight %}

# Object-Oriented

## Class

### Constructor

Defining constructor outside class definition:

{% highlight c++ %}
class Point {
    private:
        int x;
        int y;
    public:
        Line();
};

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


### Instantiate

{% highlight c++ %}
// Automatically destroyed when leaving scope
Point point(10, 20);

// Only destroyed when called w/ delete
Point point = new Point(10, 20);
{% endhighlight %}

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
