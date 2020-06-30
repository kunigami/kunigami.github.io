---
layout: post
title: "Revisiting Python: Functions"
tags: [python]
---

This is the second post in the series on revisiting Python. In the [first post]({{site.url}}/blog/2015/02/20/revisiting-python:-basic-types.html) we discussed the motivation for these posts and started by revisiting Built-in Types.

<figure class="center_children">
    <a href="https://kunigami.files.wordpress.com/2016/02/python-logo.png"><img src="{{site.url}}/resources/blog/2015-02-23-revisiting-python:-functions/2016_02_python-logo.png" alt="python-logo" /></a>
</figure>

In this post, we'll talk about [functions](https://docs.python.org/2/tutorial/controlflow.html#defining-functions) in Python 2.7.

### Functions

Functions in python consist of the keyword `def`, a function name and a list of arguments:

{% highlight python %}

def hello(arg1, arg2):
  print "hello world"

{% endhighlight %}

The pythonic-way to document what a function does is through docstrings (block of strings delimited by triple-double quotes:

{% highlight python %}

def hello(arg1, arg2):
  """
    Prints hello world
  """
  print "hello world"

{% endhighlight %}

Some tools are able to parse these docstrings to automatically build documentation.

**Namespace.** is a map from names (variables names, function names) to objects (their values). Examples of namespaces include the built-in namespace, the global namespace within a module and local namespace within a function.

**Scope.** is a *textual* region of a Python program where a namespace is directly accessible.

For example, within a function we have a local scope. Variable from the arguments or created within the function itself belong to this local scope. Consider the following example:

{% highlight python %}

d = 4
def foo(a):
    b = 1
    return abs(a - b)
foo(10)

{% endhighlight %}

In the code above, `a`, `b` are in the local scope. `d` is in the module global scope and the function `abs` is in the built-in scope. When we refer to a variable or function name, Python search scopes in the following order:

1. Local scope
2. Local scope of the calling functions
3. Global scope within a module
4. Built-in scope

Item 2 requires some clarification. In Python we can define a function inside another function. For example,

{% highlight python %}

def outer():
    scope_name = 'outer'
    def inner():
        print(scope_name)
    return inner

scope_name = 'global'
# Get the inner function
inner = outer()
inner() # outer

{% endhighlight %}

In the code above, `scope_name` is defined both in the global scope and in the `outer()` function scope. When referenced within `inner()`, the outer scope takes precedence, so this will print `'outer'`. Note how the scope is resolved statically, when defining `inner()`, not in runtime, explaining why the docs emphasize "*textual region*". This is known as lexical scoping, as we discussed when [studying the R language]({{site.url}}/blog/2014/12/30/introduction-to-the-r-language-for-programmers.html).

Variables that are not in the local scope are read-only by default. If we try to assign a new value to an existing variable in an outer scope, we are just creating a new local variable with the same name. Note that if the global variable is a reference to an object, we can modify the object, but we still can't re-assign a value to that global variable.

{% highlight python %}

x = [1, 2, 3]

def modify_global_variable_content():
    x.append(4)

def reassign_global_variable():
    x = [10, 20, 30]

modify_global_variable_content()
reassign_global_variable()
print x

{% endhighlight %}

In the code above, `modify_global_variable_content()` modifies the content of `x`, but `reassign_global_variable()` just creates a local variable `x`. To actually modify the variable we can make it explicit using the `global` keyword.

{% highlight python %}

def reassign_global_variable():
    global x
    x = [10, 20, 30]

{% endhighlight %}

Functions can be assigned to variables and passed around, for example:

{% highlight python %}

def inc(x):
    return x + 1

def print_f(f, x):
    print f(x)

print_f(inc, 2)

{% endhighlight %}

Here we pass the function `inc()`, which then is assigned to the variable `f` in `print_f()`, and used to invoke the original function.

**Default arguments.** It's possible to define default arguments to functions, like in C++. The initialization only happens once and the value is shared between subsequent calls, so for objects, this is what happens:

{% highlight python %}

def append(x, xs = []):
    xs.append(x)
    return xs

print append(1) # [1]
print append(2) # [1, 2]
print append(3) # [1, 2, 3]

{% endhighlight %}

**Keyword arguments.** The default match between the arguments passed to a function and the arguments the function receives is positional, that is, the first value is assigned to the first parameter and so on. Besides that, we can make the argument names explicit.

{% highlight python %}

def listify(a, b, c):
    return [a, b, c]
print listify(b = 2, c = 3, a = 1)

{% endhighlight %}

There are some constraints though: 1. Positional arguments must come before keyword arguments. 2. If we use a keyword for a given argument, we have to name all required arguments that appear after it in the function definition.

{% highlight python %}

def listify(a, b, c):
    return [a, b, c]

# Error: Violates rule 1
print listify(c = 3, 1, 2)

# Error: Violates rule 2
print listify(2, 3, a = 1)

{% endhighlight %}

This feature is one of my favorite in Python, which I miss from many other languages. This is particular useful in the scenario where a function takes many default arguments, but we only want to provide the last one.

**Var arg.** Functions can define two different sets of variable arguments. An argument named * is assigned with a list of all position arguments passed beyond the required arguments; an argument named ** is assigned with a dictionary with all keyword arguments not in the required arguments. A simple example:

{% highlight python %}

def f(fixed, *arguments, **keywords):
    print fixed     # 1
    print arguments # (2, 3)
    print keywords  # {a: 4, b: 5}

f(1, 2, 3, a = 4, b = 5)

{% endhighlight %}

**Argument unpacking.** With an analogous idea, it's possible to call a function with a list of arguments using the * modifier. For example:

{% highlight python %}

def f(a, b, c):
    print a, b, c
args = [1, 2, 3]
f(*args)

{% endhighlight %}

Or a map of keyword arguments using the ** modifier:

{% highlight python %}

def f(a, b, c):
    print a, b, c
args = {'b': 2, 'c': 3, 'a': 1}
f(**args)

{% endhighlight %}

### Generators

In the previous post we saw the [iterator type]({{site.url}}/blog/2015/02/20/revisiting-python:-basic-types.html). Remember how to create a custom iterator, he had to define a class and implement the required methods (`__iter__()` and `next()`).

We can also instantiate a generator from a function, by using the `yield` keyword. For example,

{% highlight python %}

def sample_generator(n):
    for i in range(n):
        yield i

{% endhighlight %}

The result of the function is an iterator. Every time we run `next()`, we execute the function until it reaches a `yield` statement, in which case it return that value.

{% highlight python %}

it = sample_generator(10)
it.next() # 0
it.next() # 1
...

{% endhighlight %}

Generators are particularly useful if the iterator has to work in sequential steps that performs different actions based at each step. For example, consider the following iterator:

{% highlight python %}

class MyIterator:
    def __init__(self, x):
        self.step = 0
        self.x = x

    def __iter__(self):
        return self

    def next(self):
        if self.step == 0:
            ret = self.x
        elif self.step == 1:
            ret = 2*self.x
        elif self.step == 2:
            ret = self.x*self.x
        else:
            raise StopIteration

        self.step += 1
        return ret

{% endhighlight %}

This is a toy example that runs different functions of `x` based on which stage it is on. Using generators, this would simply become:

{% highlight python %}

def my_generator(x):
    yield x
    yield 2*x
    yield x*x

{% endhighlight %}

A generator can use the `return` statement (without value) to stop the iterator, which is equivalent to `raise StopIteration`.

It's possible to send values back to generators through the method `send()`. Consider the following example:

{% highlight python %}

def generator_with_sends(x):
    while True:
        y = (yield x)
        if y is not None:
            x = y

it = generator_with_sends(5)
print it.next()   # 5
print it.send(10) # 10
print it.next()   # 10

{% endhighlight %}

Note that `it.send(10)` returns 10, not 5. This is because `yield` halts when the right hand side of the statement is evaluated but before the assignment. Calling `send()` will resume from there, but this time the return value of `(yield x)` will be the value passed to send. The code will then execute until the next `yield`, in which `x` contains the value passed to `y`. It's not allowed to call `send()` before the generator had yielded the first time.

When a generator is garbage-collected it raises an `GeneratorExit` exception. It's possible to force that by calling the `stop()` method.

{% highlight python %}

def stubborn_generator():
    try:
        while True:
            yield 'no'
    except GeneratorExit:
	print 'generator stopped. clean up...'
        raise StopIteration
    finally:
        print 'can do some shared clean up here'

def scoped():
    s = stubborn_generator()
    print s.next()

scoped()

{% endhighlight %}

### Map, filter, reduce

Python has built-in function that work with sequences: `map()`, `filter()` and `reduce()` being the most common. These functions work with iterators too. For example:

{% highlight python %}

# map_generator.py
def generate_ints(n):
    for i in range(n):
        yield i

def double(x):
    return x*2

print map(double, generate_ints(5))
# [0, 2, 4, 6, 8]

{% endhighlight %}

The problem is that it has to convert a generator to a list, so it has to evaluate a generator until it stops, thus we can't do this with an "infinite" iterator, like the one from this generator:

{% highlight python %}

def generate_forever():
    i = 0
    while True:
        yield i
        i += 1

{% endhighlight %}

the alternative is using the `itertools` library, which contains the `imap()` and `ifilter()` methods, which generate new iterators with the function applied.

{% highlight python %}

h = itertools.imap(double, generate_forever())
for i in range(10):
    print h.next()

{% endhighlight %}

We can see generators as lazily evaluated functions, which is the default behavior in languages like Haskell.

### Partial function application

One very useful functional paradigm is partial application. It's available in the `functools` module. For example, we can re-write our `double()` (from *map_generator.py*) function as follows:

{% highlight python %}

# partial.py
import operator
import functools

double = functools.partial(operator.mul, 2)

{% endhighlight %}

Here, `operator.mul` is just the function version for the * operator, that is,

{% highlight python %}

def mul(a, b):
    return a*b

{% endhighlight %}

The code `partial.py` will return another function where the first argument to the `operator.mul` function is replaced by 2. The nice thing about the `partial()` function is that it accepts keyword arguments:

{% highlight python %}

def f(a, b, c, d):
    return [a, b, c, d]
g = functools.partial(f, b=2, c=3)
print g(a=1, d=4)

{% endhighlight %}

### Conclusion

In this second post we covered functions in Python. We learned about namespaces, scopes and variable number of arguments. We also learned about generators and increased our knowledge on iterators by introducing the `itertools` module. Finally, we saw that Python can work with higher-order functions by using libraries like `functools`.

### References

* [[1](https://docs.python.org/2/tutorial/controlflow.html)] The Python Tutorial: More Control Flow Tools
* [[2](https://docs.python.org/2/howto/functional.html)] The Python HOWTOs: Functional Programming HOWTO
