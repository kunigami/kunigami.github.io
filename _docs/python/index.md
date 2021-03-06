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

### Sort list

Using default sorting:

{% highlight python %}
>>> sorted([5, 2, 3, 1, 4])
[1, 2, 3, 4, 5]
{% endhighlight %}

Sorting using key function. For example, sort list of pairs by the second element:

{% highlight python %}
sorted([[1, 3], [2, 2], [3, 1]], key=lambda x: x[1])
{% endhighlight %}

Sorting by custom comparator:

{% highlight python %}
import functools

# Function that returns
# < 0 if a < b
# > 0 if a > b
# = 0 if a = b
def cmp(a, b):
    return a - b

sorted([3, 2, 1], key=functools.cmp_to_key(cmp))
{% endhighlight %}

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
s = {1, 2, 3}
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

### Map over set

{% highlight python %}
xs = {1, 2, 3}
ys = {f(x) for x in xs}
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
    print(file.readlines())
{% endhighlight %}

# Other Data Structures

## Queue

The `Queue` class is an advanced implementation that can be used for example in multi-thread applications. We can still use it as a plain queue data structure.

{% highlight python %}
from queue import Queue

# Create
q = Queue()

# Insert back
q.put(item)

# Retrieve and remove front
first = q.get()

# Front element without removing
first = q[0]

# Size
len(q)

# Is empty?
q.empty()
{% endhighlight %}
