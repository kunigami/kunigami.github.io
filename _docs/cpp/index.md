---
layout: doc
title: "C++ Cheatsheet"
---

Syntax for common tasks I run into often. Assumes C++17, and `using namespace std` for brevity.

# Index
{:.no_toc}

1. TOC
{:toc}


# Collections

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
