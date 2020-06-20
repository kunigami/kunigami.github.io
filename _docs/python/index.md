---
layout: doc
title: "Python Cheatsheet"
---

Syntax for common tasks I run into often. Assumes Python 3.

# Collections

## List

### Map over list

{% highlight python %}
xs = [1, 2, 3]
ys = [f(x) for x in xs]
{% endhighlight %}

### Filter list

{% highlight python %}
xs = [1, 2, 3]
ys = [x for x in xs if x % 2 == 0]
{% endhighlight %}

NOTE: filter and map can be combined into one.

## Dictionaries

### Map over dictionary

{% highlight python %}
ys = {k: f(v) for k, v in xs.items()}
{% endhighlight %}

### Sort dictionary

Dictionaries are unordered sets, so you likely want to work with a list after sorting.

{% highlight python %}
d = {'a': 3, 'b': 2}
xs = sorted(d.items(), key=lambda x: x[1])
{% endhighlight %}

## Sets

### Create

{% highlight python %}
s = set([1, 2, 3])
{% endhighlight %}

### Difference

{% highlight python %}
s1 = set([1, 2])
s2 = set([2, 3])
s1 = s1 - s2 # {1}
{% endhighlight %}

### Union

{% highlight python %}
s1 = set([1, 2])
s2 = set([2, 3])
s1 = s1.union(s2) # {1, 2, 3}
{% endhighlight %}

# Object Oriented

Basic Syntax:

{% highlight python %}
class C:
    def __init__(self, param):
        self.param

    def method(self):
        return self.param

    @staticmethod
    def static_method():
        return 1
{% endhighlight %}


# Files

## Read file

{% highlight python %}
with open(filename, "r") as file:
    print(file.readLines())
{% endhighlight %}
