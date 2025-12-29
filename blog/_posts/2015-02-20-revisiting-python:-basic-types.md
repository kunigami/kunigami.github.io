---
layout: post
title: "Revisiting Python: Basic Types"
tags: [python]
---

I've been using Python for quite a while now, but always on pet projects, small scripts, programming contests and I feel that my knowledge didn't really improve much. I had a chance to work on a Python project and realized there are many basic things I don't know.

<figure class="center_children">
    <a href="https://kunigami.files.wordpress.com/2016/02/python-logo.png"><img src="{{site.url}}/resources/blog/2015-02-20-revisiting-python:-basic-types/2016_02_python-logo.png" alt="python-logo" /></a>
</figure>

Every time I dedicate some time to learn and write about some subject, I feel like my knowledge about it improves a lot. Thus, I decided to revisit some Python concepts and write a series of posts about things I didn't know in Python 2.7. The first of our posts will be the [Built-in types](https://docs.python.org/2/library/stdtypes.html).

### Booleans Types

Boolean types can take `True` and `False` as values. Falsy values include `False`, 0, empty sequences (string, list, dictionary) and any class that implements `__len__()` and returns 0.

**Logical operators.** many languages use `&&` and `||`. In Python it is `and` and `or`, and it works the same way (short-circuiting).

Negation is also more verbose: `not` instead of `!`. Other logical operators test whether an element belongs to a sequence/container or if a string is a substring of another, `in`.

{% highlight python %}

> a = [1, 2, 3]
> 3 in a
True
> b = {'a': 'apple', 'b': 'banana'}
> 'b' in b
True
> 'banana' in b
False
> 'banana' not in b
True
> 'ban' in 'banana'
True

{% endhighlight %}

Note that it can be composed with the `not` operator for better readability. Another operator is the `is` operator (similar to triple equal in PHP or Javascript). It's the same as the `==` operator for basic types, but for other types, it only returns True if they point to the same object:

{% highlight python %}

> a = [1, 2]
> b = [1, 2]
> c = a
> a is b
False
> a is c
> [1,2] is [1,2]
False
> "abc" is "abc"
True
> (1-1) is 0
True
> 1 is not 0
True

{% endhighlight %}

This operator can also be combined with the `not` operator.

**Comparison operators.** It's the same as in most languages. In Python, they work with custom classes if these classes implement the following methods: `__lt__()`, `__le__()`, `__gt__()`, `__ge__()`, the `__cmp__()`.

### Numeric Types

Numeric types can be one of the following: `int` (plain integer), `float` (floating point numbers), `long` (long integers) and `complex`. `int`'s are like C++'s long, 32 bits of precision. `float`'s are equivalent to C++'s double, usually 64 bits, 53 bits for mantisse, 10 bits for exponents and 1 bit for sign. `long`'s have unlimited precision. `complex` is a pair of floats (named `real` and `imag`).

**Math operations.** Most operators are the same as other languages. The different ones include `//`, which performs floored quotient, for comparison

{% highlight python %}

> 11./2
5.5
> 11.//2
5.0
> 11/2
5

{% endhighlight %}

Note that if both values are integers, the division is integer. `divmod()` is also interesting. It's basically `divmod(a, b) = (a // b, a % b)`.

Bitwise operators works as in C++.

Numeric types are classes too, and implement `numbers.Integral`. We can invoke methods on variables, but not on literals:

{% highlight python %}

> 6.bit_length()
Syntax error
> b = 6
> b.bit_length()
3

{% endhighlight %}

### Iterator Types

Python supports a concept of iteration over containers. Classes that implement the `__iter__()` and `next()` methods are of type iterator. The `next()` method should return the current value and proceed to the next value, using `raise StopIteration` when the end is reached.

We can implement our own `(x)range` function as an example:

{% highlight python %}

class Range:
    def __init__(self, start, end = None, step = 1):
        if (end is None):
            end = start
            start = 0
        self.start = start
        self.end = end
        self.step = step
    def __iter__(self):
        return Iterator(self.start, self.end, self.step)

class Iterator:
    def __init__(self, start, end, step):
        self.end = end
        self.step = step
        self.counter = start
    def __iter__(self):
        return self
    def next(self):
        self.counter += 1
        if self.counter > self.end:
            raise StopIteration
        return self.counter

for i in Range(1, 10, 2):
    print i

{% endhighlight %}

### Sequence Types

The types that fall in this family of type are `str`, `unicode`, `list`, `tuple`, `bytearray`, `buffer`, `xrange`.

**Strings.** is a list of characters, which themselves are 8-bits encoded ascii values (strings have some overhead besides the characters [[1](http://deeplearning.net/software/theano/tutorial/python-memory-management.html)]). Strings literals can be written in single or double quotes.

Formatting strings: It accepts a syntax similar to *sprintf* from C. One interesting form is passing an dictionary of values and naming the patterns by the key name:

{% highlight python %}

> print '%(language)s has %(number)03d quote types.' % \
...       {"language": "Python", "number": 2}
Python has 002 quote types.

{% endhighlight %}

There also an alternative formatting using the `.format()` method. A discussion can be read [here](http://stackoverflow.com/questions/5082452/python-string-formatting-vs-format).

**Unicode strings.** Python uses UTF-8 encoding for unicode. Literals of this type can be created by prefixing the value with an `u`, for example `u'abc'`.

**Tuples.** are shallowly "immutable" containers. Their contents can't be changed, but the objects their elements point to might be. It can be used without parenthesis and can be used in the LHS to unwrap values. For example:

{% highlight python %}

> a, b = 1, 2
> a
1
> b
2

{% endhighlight %}

Here, `(1, 2)` and `(a, b)` are tuples. It has bracket access, for example

{% highlight text %}

> x = 1, 2
> x[0]
1

{% endhighlight %}

Tuples are hashable if all its elements are hashable. This allows using tuples in sets or dictionary keys.

**Lists.** are mutable sequences. They're indexed from 0 to length-1 and access out of this range throws an exception. The + operator can be used to concatenate lists. Conveniently, the * operator where the first operator is a list and second operant is an integer N, creates N shallow copies of the list (this works for tuples too).

Access to lists can be made using ranges, in which case it returns another list, for example

{% highlight python %}

> x = range(10) # [0, 1, 2, ..., 10]
> x[0:5]
[0, 1, 2, 3, 4]

{% endhighlight %}

We must be careful in making copies of arrays where the elements are references to objects (for example other lists). In the example below, it's likely not doing what we would want:

{% highlight python %}

> lists = [[]] * 3
> lists
[[], [], []]
> lists[0].append(3)
> lists
[[3], [3], [3]]

{% endhighlight %}

**Xranges.** The existence of the xrange type is justified by the `xrange()` function. The [python docs](https://docs.python.org/2.7/library/functions.html#xrange) explain it well:

>  This function is very similar to range(), but returns an xrange object instead of a list. This is an opaque sequence type which yields the same values as the corresponding list, without actually storing them all simultaneously. The advantage of xrange() over range() is minimal (since xrange() still has to create the values when asked for them) except when a very large range is used on a memory-starved machine or when all of the range’s elements are never used (such as when the loop is usually terminated with break).
> 

**Bytearrays.** are essentially mutable strings.

**Buffer.** is intended for memory-efficient manipulation of a large arrays, which otherwise would cause a copy. Guido van Rossum describes an example [[2](https://mail.python.org/pipermail/python-dev/2000-October/009974.html)]:

> It was created with the desire to avoid an expensive memory-copy operation when reading or writing large arrays.  For example, if you have an array object containing several millions of double precision floating point numbers, and you want to dump it to a file, you might prefer to do the I/O directly from the array's memory buffer rather than first copying it to a string.

[This Stack Overflow](http://stackoverflow.com/questions/3422685/what-is-python-buffer-type-for) question also discusses the subject.

### Set types

`Set and Frozenset.` The main difference between these two is that set is mutable while frozenset is immutable. These structures are implemented using a hash table, so all elements in a set/frozenset must be hashable. A type is *hashable* if it implements `__hash()__` (which shouldn't change during its lifetime) and either `__eq()__` or `__cmp()__`. Since frozenset is immutable and all its elements are hashable, frozenset itself is hashable.

In Python 2.7, sets can be constructed by a shorthand `{'foo', 'bar'}`.

### Map types

Dictionaries are associative arrays. They're called `dict` in Python code. Dictionary keys must be hashable.

We can get a dictionary's keys and values by `.keys()` and `.values()` respectively. The .items() method returns an array of pairs, each containing key and value. These methods all return copies, so if we assign .keys() to a variable and make changes to the dictionary, the changes won't get reflected in the list assigned to the variable.

To get references instead of copies, we can use the .viewkeys(), .viewvalues() and .viewitems() methods, which are read-only references.

### File Objects

Returned by the `open()` function. It's used for file manipulation operations.

### Memory View

It was introduced in Python 2.7 and is a replacement for the buffer type.

### Context Manager Types

Any user defined class that implements `__enter()__` and `__exit()__` has a context manager type. These types are useful for abstracting a specific try/finally pattern. More specifically, imagine we have the following pseudo-code:

{% highlight python %}

set things up
try:
  do something
finally:
  tear things down

{% endhighlight %}

If we do "set things up" and "tear things down" in many places and only change "do something", we can abstract those in a class implementing a context manager type:

{% highlight python %}

class ControlledExecution:
  def __enter__(self):
    set things up
      return thing
  def __exit__(self, type, value, traceback):
      tear things down

with ControlledExecution() as thing:
  do something

{% endhighlight %}

This [post from Effbot](http://effbot.org/zone/python-with-statement.htm) as a very clear explanation.

### Conclusion

In this post we covered the basic types from the Python environment. We were able to learn about some interesting features even from basic types like booleans and numeric. We also covered some more exoteric types like buffer and xrange. We got some exposure to other features like context manager.

In the next post in the series we'll talk about functions.

### References

* [[1](http://deeplearning.net/software/theano/tutorial/python-memory-management.html)] Theano - Python Memory Management
* [[2](https://mail.python.org/pipermail/python-dev/2000-October/009974.html)] Python-Dev - The buffer interface
* [[3](https://docs.python.org/2.7/library/stdtypes.html)] Python Docs - Built-in Types
