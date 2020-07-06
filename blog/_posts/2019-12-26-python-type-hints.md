---
layout: post
title: "Python Type Hints"
tags: [python]
---

Jukka Lehtosalo is a Finnish Software Engineer at Dropbox. He developed an optional type system for Python, mypy, during his PhD thesis in Cambridge. After meeting in a Python conference, Guido van Rossum (creator of Python) invited Lehtosalo to join him at Dropbox. They started adopting mypy in real use cases during a Hackathon, which led to mypy being one of the most popular Python type checkers [2].

In this post we'll cover mypy in general terms as well many examples demonstrating the syntax and capabilities of this type checker.
# Introduction
**mypy** is a static type checker. This means it does not run during the code execution, so it's mostly useful during development, much like tests. It relies on manual annotations in the code called type hints, which identify the types of arguments, return types, internal variables and member variables.

One simple example of such annotations is for a function to compute the length of a string:

{% highlight python %}
def str_len(s: str) -> int:
    return len(s)

{% endhighlight %}

We are indicating that the input argument s is a string and it return an integer corresponding to its length. The main job of a type checker is to find inconsistencies in the annotations in our code (or in the libraries we use). For example, here's an incorrect type annotation:

{% highlight python %}
def invalid_inc(n: int) -> str:
    return n + 1
{% endhighlight %}

When both operands are integers, the operator + returns another integer, so n + 1 is an integer, which is a clear contradiction given we provided the return type as string.
### Type annotations are free unit tests
This contrived example doesn't make the usefulness of a type checker obvious, but imagine a case where we implicitly make assumption on the argument type. For example, a small program to compute how much an exchange house would give a user for their cash.

{% highlight python %}
def convert_currency(a):
    return 2*a


user_input = '$10'
fee = 10
value = convert_currency(user_input) - fee
{% endhighlight %}

If we don't perform input validation correctly, the code above might work for the tested scenarios but would fail in production the moment the user provided a string. Of course good QA will cover scenarios like these, but this is to sort of guarantees one gets for free by making the types explicit:

{% highlight python %}
def convert_currency(a: float) -> float:
    return 2*a
{% endhighlight %}

The previous code would now fail the type checker validation.
### Type hints are optional and incremental
Most of the design of type annotations are well documented in [PEP 484](https://www.python.org/dev/peps/pep-0484/), the document claims:

> 
> Python will remain a dynamically typed language, and the authors have no desire to ever make type hints mandatory, even by convention.

This also seems to imply that Python type hints will always be partial / gradual, since if full typing is required, it will make transition from non-typed to fully typed codebases prohibitive. Also, there are concrete benefits even with partial typing.

A partial type system makes it optional to add type annotations to variables, instead of it being fully mandatory (like Java or C++). The type checker then performs validation with whatever information it has in hands.

Incomplete typing can be dangerous if developers build trust on the type checker while it's only performing partial checks due to incomplete information. Let's consider an example:

{% highlight python %}
def expects_string(a: str):
    return a

def expects_int(a: int) -> int:
    return a + 1

def main():
    untyped = expects_string('a')
    expects_int(untyped)
{% endhighlight %}

At first glance it seems a well typed program and if we run the type checker it will pass. But if we run `main()`, we'll have a runtime error. The problem is that `expect_string` is missing the return type, so in `main()`, the type checker cannot infer the type of `untyped`, so it doesn't perform any validation on `expects_int()`.
### Local inference
The previous example also highlights an important aspect of the mypy type checker: it only performs inferences at the function boundary. In theory it should infer that `expects_string` returns `str` because we're returning its argument and then infer that `untyped` is a string.

This is fine in this example but it could be very expensive to make these inferences, especially if we need to consider branching and recursive calls to other functions. In theory the type checker could go only a few levels deep in the function call but this would make the behavior of the type checker very hard to reason about.

For that reason, the type check will only consider the type of the functions being called. For example, it knows `expects_string()` expects a string and returns no type, so this is what it will assign to `untyped` no matter what happens inside `expects_string()`.

Now that we know the basics of the type checker, let's cover some of the syntax and more advanced typing that mypy supports.
# Examples
### Setup
Before we start, it's useful to be able to test the snippets. To do so, copy the code into a file, say `example.py` and run this command in the terminal:

`mypy example.py`

which will print any type errors that exist in `example.py`. mypy can be installed via Python packaging system, pip. Make sure to user Python 3:

`pip3 install mypy`
### Primitive types
`bool`, `int`, `str`, `float` are the types one will most likely use in functions. As seen above, we can use these to type arguments and return types:

{% highlight python %}
def str_len(s: str) -> int:
    return len(s)

{% endhighlight %}
### Composed types
We'll look into generics later, but it should be straightforward to understand the typing of composed types like lists, dictionaries and tuples:

{% highlight python %}
from typing import List, Dict, Tuple

def list() -> List[int]:
    return [1, 2, 3]
  
def dict() -> Dict[str, int]:
    return {'a': 1, 'b': 2}

def tuple() -> Tuple[int, str]:
    return (1, 'a')

{% endhighlight %}

It's worth noting that these types need to be explicitly imported from the typing module.
### None vs. NoReturn
`None` is used to indicate that no return value is to be expected from a function.

{% highlight python %}
def my_print(s: str) -> None:
    print('>' + s)

# error: "my_print" does not return a value    
r = my_print('a')

{% endhighlight %}

`NoReturn` is to indicate the function should not return via the normal flow:

{% highlight python %}
def ready(s: str) -> NoReturn:
    print('implemented')
    # error: implicit return
    
def not_ready(s: str) -> NoReturn:
    print('not implemented')
    raise # ok: always throws
{% endhighlight %}
### Local Variables
The example below demonstrates the syntax for typing local variables. In general typing local variables is not necessary since their type can often be inferred from the assignment / initialization.

{% highlight python %}
b: bool = False
i: int = 1
s: str = 'abc'
f: float = 1.0
{% endhighlight %}
### Optional and Union
`Optional[T]` is a generic type that indicates the type is either `T` or `None`. For example:

{% highlight python %}
def first(xs: List[int]) -> Optional[int]:
    if len(xs) == 0:
        return None
    return xs[0]
{% endhighlight %}

`Optional` is a special case of a more general concept of `Union` of types:

{% highlight javascript %}
def int_or_str() -> Union[str, int]:
    if(random.randint(0, 1) == 0):
        return 1
    return 'a'
{% endhighlight %}

My personal take is that `Union` should be avoided (except for special cases like `Optional`) because it makes the code harder to deal with (having to handle multiple types) and it's often better via inheritance (base type representing the union).
### Any vs. object
`Any` is equivalent to not providing the type annotation. On the other hand, `object` is the base of all types, so it would be more like a `Union` of all the types. A variable of type object can be assigned a value of any type, but the value of an object variable can only be assigned to other object variables. It's possible to refine the type of a variable to a more specific one. See *"Refining types"*.

{% highlight python %}
def dummy(x: object) -> object:
    return x

def inc(x: int) -> int:
    return x + 1

dummy(Square()) # ok
inc(dummy(1))   # error - dummy(1) returns object

{% endhighlight %}
### Classes
There are three main things we need to consider when annotating variables in a class context:
* We can add types to the member variable (`_n` in this case).
* We don't type `self`: it's assumed to be the type of the class where it's defined.
* The return type of `__init__` is `None`
{% highlight python %}
class C:

    _n: int

    def __init__(self, n: int) -> None:
        self._n = n

    def inc(self) -> None:
        self._n += 1
{% endhighlight %}
### Callables
`Callables` can be used to type higher order functions. Here's an example where we pass a function (lambda) as argument to a map function:

{% highlight python %}
def typed_map(xs: List[int], f: Callable[[int], int]) -> List[int]:
    return [f(x) for x in xs]

print(typed_map([1, 2, 3], lambda x: x*2))

{% endhighlight %}

The type of the function is `Callable`. The first element, `[int]` in the example above, is a list of the types of the arguments. The second argument is the return type.

As another example, if we want to define a reduce function on strings, our callback has now type `Callable[[str, str], str]` because it takes 2 arguments.

{% highlight python %}
def typed_reduce(xs: List[str], f: Callable[[str, str], str], x0: str) -> str:
    r = x0
    for x in xs:
        r = f(r, x)
    return r

print(typed_reduce(['a', 'b', 'c'], lambda x, y: x + y, ''))
{% endhighlight %}
### Generics: Type Variables
Type variables allow us to add constraints to the types of one or more argument and/or return types so that they share the same type. For example, the function below is typed such that if we pass `List[int]` as argument, it will return `Optional[int]`:

{% highlight python %}
T = TypeVar('T')


def first(xs: List[T]) -> Optional[T]:
    if (len(xs) == 0):
        return None
    return xs[0]
{% endhighlight %}

Note that the string passed to the `TypeVar()` function must match the of the variable it is assigned to. This is an inelegant syntax but I'm imagining it's the result of working around syntax limitations of Python (and the difficulties in changing the core Python syntax for annotations).

We can use multiple `TypeVar`s in a function:

{% highlight python %}
T1 = TypeVar('T1')
T2 = TypeVar('T2')

def tuplify(a: T1, b: T2) -> Tuple[T1, T2]:
    return (a, b)
{% endhighlight %}

**Constraints.** According to [3] it's also possible to limit the type var to be of a specific types:
> TypeVar supports constraining parametric types to a fixed set of possible types (note: those types cannot be parametrized by type variables).
It also notes:
> There should be at least two constraints, if any; specifying a single constraint is disallowed.
Which makes sense, if we were to restrict a `TypeVar` to a single type we might as well use that type directly.

In the example below we allow `Tmix` to be bound to either `int` or `str`. Note this is different from `Union[int, str]` because the latter is **both** `int` and `str` at the same time, while the former is either `int` or `str`, depending on how it's called. The third call to `fmix()` below would be valid for a `Union`.

{% highlight python %}
Tmix = TypeVar('Tmix', int, str)

def fmix(a: Tmix, b: Tmix) -> Tmix:
    if(random.randint(0, 1) == 0):
        return a
    return b

fmix('a', 'b') # ok
fmix(1, 2) # ok
fmix('a', 1) # error
{% endhighlight %}
### Parametrized Classes
We've just seen how to parametrize functions via `TypeVar`. We can also extend such functionality to classes via the `Generic` base class:

{% highlight python %}
class Parametrized(Generic[T]):
    value: T

    def __init__(self, value: T) -> None:
        self.value = value

    def getValue(self) -> T:
        return self.value
{% endhighlight %}
### Ignoring type hints
During the transition from untyped to typed code, it might be necessary to temporarily turn off type checking in specific parts of the code. For example, imagine a scenario where types were added to a widely used function but many existing callers are passing incorrect types.

The comment `# type: ignore` makes the type checker skip the current line (if an inline comment) or the next line (if a line comment). Here's an obvious type violation that is ignored by the type checker:

{% highlight python %}
pseudo_int: int = 'a'  # type: ignore

{% endhighlight %}

It's also possible to turn off type-checking completely for the file:
> A # type: ignore comment on a line by itself at the top of a file, before any docstrings, imports, or other executable code, silences all errors in the file
### Refining types: isinstance() and cast()
In some cases we might receive values from untyped code or ambiguous types like Unions or object. Two ways of informing the type checker about the specific type is by explicit check via `isinstance()` or `cast()`. The `isinstance()` will be usually in a if clause:

{% highlight python %}
def inc(x: int) -> int:
    return x + 1
  
def delegate(x: object) -> object:
    if (isinstance(x, int)):
        return inc(x)
{% endhighlight %}

This allows the type checker to infer the type of x within the `x` block, so it won't complain about the call of `inc(x)`.

Another, more drastic, approach is to use `cast([type], x)` which returns `x` if it has in runtime but otherwise throws an exception, but this allows the type checker to refine the type of x to statically. Here's an example:

{% highlight python %}
def inc(x: int) -> int:
    return x + 1

def enforce(x: object) -> int:
    return inc(cast(int, x))

{% endhighlight %}

It's a bummer that the order of arguments of `cast([type], var)` and `isisntance(var, [type])` are inconsistent.
### Arbitrary argument lists
It's possible to type arbitrary argument lists, both the positional and named ones. In the example below, `args` has type `Tuple[str, ...]` and `kwds` has type `Dict[str, int]`. Note that the `...` in `Tuple[str, ...]` indicates an arbitrary-length tuple of strings. It's unclear to me why it's a tuple and not a list, but I'd guess it has to do with how it represents non-arbitrary argument lists (e.g. `foo(x: int, y: str)` would be `Tuple[int, str]`).

{% highlight python %}
def foo(*args: str, **kwds: int): 
    pass
{% endhighlight %}
# Conclusion
I really like Python and it's my go-to language for small examples and automation scripts. However, having had to work with a large code base before, I was really displeased by its lack of types.

I'm also a big proponent of typed code, especially for large, multi-person code bases, but I understand the appeal of rapid development for prototypes and Python now offers both.

As we saw in some examples, the syntax is cumbersome at times but overall I find the mpyp type checker pretty expressive.
# References
* [[1](http://mypy-lang.org/about.html)] mypy: About
* [[2](https://blog.dropbox.com/topics/company/thank-you--guido)] Dropbox Blog: Thank you, Guido
* [[3](https://www.python.org/dev/peps/pep-0484)] PEP 484 -- Type Hints
