---
layout: doc
title: "Python Type Hints Cheatsheet"
---

# Types

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

# Annotation

## Local Variables

{% highlight python %}
s: str = 'abc'
{% endhighlight %}

## Turn off type checking

{% highlight python %}
pseudo_int: int = 'a'  # type: ignore
{% endhighlight %}
