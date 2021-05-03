---
layout: doc
title: "Python Type Hints Cheatsheet"
---

# Types

Common types.

## Primitive Types

* `bool`
* `int`
* `str`
* `float`

## Composite Types

* `List[int]`
* `Dict[str, int]`
* `Tuple[int, str]`

## Callables

High-order functions. `Callable[Tin, Tout]`. `Tin` is a tuple with the types of input arguments. `Tout` is the return type. Example:

* `Callable[[str, str], str]`

## Optional

`Optional[T]` means `None` or `T`. Example:

* `Optional[int]`

## Any vs. object

`Any` is equivalent to not providing the type annotation. `object` is the base of all types.

## Union Types

When the variable can be one of many types:

{% highlight python %}
from typing import TypeVar, Union
ID = Union[str, int]
{% endhighlight %}


# Annotation Syntax

How to provide annotation in different scenarios.

## Local Variables

{% highlight python %}
s: str = 'abc'
{% endhighlight %}

## Turn off type checking

{% highlight python %}
pseudo_int: int = 'a'  # type: ignore
{% endhighlight %}

## Classes

{% highlight python %}
class C:

    _n: int

    def __init__(self, n: int) -> None:
        self._n = n

    def inc(self) -> None:
        self._n += 1
{% endhighlight %}
