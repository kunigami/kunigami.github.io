---
layout: doc
title: "Python Cheatsheet"
---

{% include blog_vars.html %}

Syntax for common tasks I run into often. Assumes Python 3.

# Index
{:.no_toc}

1. TOC
{:toc}

# Data Structures

## Enum

{% highlight python %}
from enum import Enum

class MyEnum(Enum):
  A = 'a'
  B = 'b'
{% endhighlight %}

### From primitive

{% highlight python %}
print(MyEnum('a')) # MyEnum.A
{% endhighlight %}

It does validation:

{% highlight python %}
print(MyEnum('c'))
{% endhighlight %}

### To primitive

{% highlight python %}
print(MyEnum.A.value) # 'a'
{% endhighlight %}


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

### Any / All

Find if any (all) element satisfy a predicate:

{% highlight python %}
xs = [1, 2, 3]
ys = any(predicate(x) for x in xs)
{% endhighlight %}

NOTE: this is a generator, so it will short-circuit once if finds an element satisfying the predicate. The version below creates a list first, so no short-circuit happens.

{% highlight python %}
xs = [1, 2, 3]
ys = [any(predicate(x) for x in xs)]
{% endhighlight %}


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

### Remove entry

{% highlight python %}
del dict[key]
{% endhighlight %}

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

Empty:

{% highlight python %}
s = set()
{% endhighlight %}

Note that `s = {}` creates an empty dictionary.

### Insert

Add/insert

{% highlight python %}
s.add(4)
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

Check if object is instance of a class:

{% highlight python %}
class MyClass:
    pass
x = MyClass()
isinstance(x, MyClass)
{% endhighlight %}

## Class Methods

Methods where the bound variable is an instance to the class.

{% highlight python %}
class C:
    @classmethod
    def class_method(cls):
        print(cls) # __main__.C

    def method(self):
        print(self) # <__main__.C instance at 0x12345>;
{% endhighlight %}

See also: Class Methods in [Revisiting Python: Object Oriented Programming]({{blog}}/2015/03/08/revisiting-python-object-oriented-programming.html)

## Dataclasses

Lightweight syntax for creating classes / records.

{% highlight python %}
from dataclasses import dataclass

@dataclass
class Point:
     x: int
     y: int

p = Point(10, 20)
print(p.x) # 10

q = Point(y=10, x=20)
print(q.x) # 20
{% endhighlight %}

Complex types:

{% highlight python %}
# ... continued from above

@dataclass
class Polygon:
    pts: [Point]

pol = Polygon([p, q])
print(pol.pts) # [Point(x=10, y=20), Point(x=20, y=10)]
{% endhighlight %}

### Cloning

`dataclasses.replace`:

{% highlight python %}
p = Point(1, 2)
p_copy = dataclasses.replace(p)
{% endhighlight %}

### Immutable

Prevents fields from being changed:

{% highlight python %}
@dataclass(frozen=True)
class Wrapper:
    v: str = 'abc'

    def __init__(self):
        self.v = 'cde' # error

w = Wrapper()
w.v = 'cde' # error
{% endhighlight %}

It's possible to bypass though:


{% highlight python %}
@dataclass(frozen=True)
class Wrapper:
    v: str = 'abc'

    def __init__(self):
        # internal backdoor
        object.__setattr__(self, "v", "backdoor")

w = Wrapper()
# external backdoor
object.__setattr__(w, "foo", 'backdoor')
{% endhighlight %}


# Flow Control

## Exceptions

Basic syntax:

{% highlight python %}
try:
    raise Exception('hello')
except Exception as ex:
    print('caught', str(ex))
{% endhighlight %}

Custom exception:

{% highlight python %}
class MyException(Exception):
    pass

try:
    raise MyException('hello')
except MyException as ex:
    print('caught specific', str(ex))
{% endhighlight %}

Re-raise exception

{% highlight python %}
try:
    raise Exception('hello')
except Exception as ex:
    print('caught specific', str(ex))
    raise
{% endhighlight %}

Raise new exception but preserve original stack trace:

{% highlight python %}
try:
    raise Exception('hello')
except Exception as ex:
    raise Exception('new title') from ex
{% endhighlight %}

# Stdin/Stdout

## Capture stdout

{% highlight python %}
out = io.StringIO()
with redirect_stdout:
    # call code which prints to stdout
    execute()
stdout_str = out.getValue()
{% endhighlight %}

# Files

## Read file

{% highlight python %}
with open(filename, "r") as file:
    print(file.readlines())
{% endhighlight %}

## Write file

Temporary file:

{% highlight python %}
# Note: file gets deleted once it exits the scope
tempfile.NamedTemporaryFile() as file:
    file.write("Hello World\n")

    # make sure to flush if something will read from it
    file.flush()
    read_my_file(file.name)
{% endhighlight %}


# Functions

## Decorators

A decorator is basically a function transformer. Basic template:

{% highlight python %}
import functools
def my_decorator(f):
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        # pre-process
        ...
        r = f(*args, **kwargs)
        # post-process
        ...
        return r
    return wrapper

@my_decorator
def my_func():
    pass
{% endhighlight %}

# Date and Time

Current time:

{% highlight python %}
datetime.now()
{% endhighlight %}

Adding/subtracting time:

{% highlight python %}
datetime.now() + datetime.timedelta(days=1)
{% endhighlight %}

## Elapsed time

Wall-clock time in seconds:

{% highlight python %}
start = time.time()
call()
elapsed_secs = time.time() - start
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
