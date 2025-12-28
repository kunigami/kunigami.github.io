---
layout: post
title: "Haskell – Typeclasses"
tags: [haskell]
excerpt_separator: <!--more-->
vanity: "2011-12-04-haskell-typeclasses"

---
{% include blog_vars.html %}

In this post I will comment the typeclass structure in Haskell. My main reference is chapter 6 of Real World Haskell [1].

<!--more-->

## Typeclasses

Since Haskell supports parameterized types, we may want to define functions that accept parameters of such types. However, the function implementation will likely be different for each specific type "instance".

Typeclass is a framework to provide this functionality. The basic syntax is as follows:

{% highlight haskell %}
class <Name> <Parameterized types> where
    <function 1> :: <Type signature>
    <function 2> :: <Type signature>
{% endhighlight %}

Despite the `class` keyword, a typeclass is quite different of a class that we are used to in object-oriented languages.

One case where typeclass is useful is in implementing the equality operator. Suppose we have two types `Boolean` and `RGBColor`:

{% highlight haskell %}
data RGBColor = Red | Green | Blue
    deriving (Show)

data Boolean = False | True
    deriving (Show)
{% endhighlight %}

We can define a typeclass containing an equality function, which receives two parameters of the same type parameterized `a`, and returns `True` if they are equal or `False` otherwise.

{% highlight haskell %}
class Compare a where
    equals :: a -> a -> Bool
{% endhighlight %}

Then we can define an implementation for any type we're interested in, using the `instance` construct. The syntax is similar to a typeclass, but we replace the parameterized type by the actual type and define the function implementation. For the case of `Boolean` and `RGBColor`, we have:

{% highlight haskell %}
instance Compare Boolean where
         equals True True = True
         equals False False = True
         equals _ _ = False

instance Compare RGBColor where
         equals Red Red = True
         equals Green Green = True
         equals Blue Blue = True
         equals _ _ = False
{% endhighlight %}

We can also provide default implementations for a typeclass by defining the implementation in the class structure. During runtime, the implementation used is always the most specific one.

For example, in the typeclass `Compare`, we can define the function `different` and define default implementations for it and also for the function `equals`:

{% highlight haskell %}
class Compare to where
    equals :: a -> a -> Bool
    equals x y = not (different x y)

    different :: a -> a -> Bool
    different x y = not (equals x y)
{% endhighlight %}

## Deriving

In defining `Boolean` and `RGBColor`, we added `deriving(Show)`. Essentially `Show` is a typeclass from the standard library. The function `show` in this typeclass transforms a generic type into a `String`.

Doing some tests in the terminal (ghci):

{% highlight text %}
> show(Red)
"Red"
> :type show
show :: Show a => a -> String
{% endhighlight %}

Note that the command `:type` uses a different syntax to say that the function show belongs to the typeclass `Show`.

We can write our own implementation for `RGBColor` instead of using the default. To do this, we remove the `deriving(Show)` statement and add:

{% highlight haskell %}
instance Show RGBColor where
    show Red = "Red"
    show _ = "Not implemented"
{% endhighlight %}

According to [2], the `derive` functionality can only be used with a specific set of built-in typeclasses.


## Typeclass with list of parametric types

Imagine that we want to implement the `equals()` function for a list of `Boolean`s. The first attempt would be:

{% highlight haskell %}
instance Compare [Boolean] where
    equals = undefined
{% endhighlight %}

In the above code, `undefined` is a wildcard type that does not cause compilation errors due to type mismatch, but does cause a runtime error. This technique of using `undefined`'s is interesting for compiling intermediate versions of your code even without implementing all the functions.

Anyway, we will see that such a code snippet leads to the following compilation error:

{% highlight text %}
   Illegal instance declaration for `Compare [Boolean]'
      (All instance types must be of the form (T a1 ... an)
       where a1 ... an are *distinct type variables*,
       and each type variable appears at most once in the instance head.
       Use -XFlexibleInstances if you want to disable this.)
    In the instance declaration for `Compare [Boolean]'
Failed, modules loaded: none.
{% endhighlight %}

There are several ways to get around this error. One is to define a unique function for lists, which in the case of the function `equals()` can be called `equalList()`:

{% highlight haskell %}
class Compare to where
  ...
  equalList :: [a] -> [a] -> Bool
  equalList (xa:a) (xb:b) | different xa xb = False
                           | otherwise = equalList ab
  equalList [ ] [ ] = True
  equalList _ _ = False
  ...
{% endhighlight %}

The advantage in this case is that the default implementation should work for most cases, since in general the test for equality between two lists consists of checking if all elements in both are equal.

In case you want to define the implementation only for a list of a particular type (eg `[Boolean]`), you can encapsulate that list in a structure called **newtype**.

The newtype is similar to an algebraic data type (keyword `data`), with some differences such as the newtype requiring exactly one data constructor and the type of a newtype is resolved at compile time (unlike `data`) and therefore its use does not cause overhead in program execution [1]. So we could do:

{% highlight haskell %}
newtype Wrapper = Wrapper [Boolean]
{% endhighlight %}

Now we can implement the function, with the inconvenience of wrapping the data of the new type with `Wrapper`:

{% highlight haskell %}
instance Compare Wrapper where
  equals (Wrapper x) (Wrapper y ) = equalAux xy
    where equalAux (xa:a) (xb:b) | different xa xb = False
                                 | otherwise = equalAux ab
          equalAux [ ] [ ] = True
          equalsAux _ _ = False
{% endhighlight %}

A good reference for these and other alternatives is the haskell wiki [3].

## Conclusion

I found this subject very complicated! To make matters worse, it seems that the book *Real World Haskell* I'm basing on, wrote chapters 5 and 6 very badly, giving very complex examples (maybe it's the "real world" vision, but this is definitely not didactic), confusing and boring. There are several reviews that share this opinion.

Despite everything, I will continue my studies with this book, even if I only use the topics to guide me and look for explanations in references like [2] and [3].

## References

* [[1](http://book.realworldhaskell.org/)] Real World Haskell - [Chapter 6](http://book.realworldhaskell.org/read/using-typeclasses.html), by Bryan O'Sullivan, Don Stewart, and John Goerzen.
* [[2](https://en.wikibooks.org/wiki/Haskell/Classes_and_types)] Wikibooks - Haskell/Classes and types
* [[3](http://www.haskell.org/haskellwiki/List_instance)] Haskell Wiki - List instance
