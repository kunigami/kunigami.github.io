---
layout: post
title: "Monads in Haskell - Part I"
tags: [category theory, haskell]
---

### Introduction

In this post we write some notes about monads and describe the Maybe and List Monad types. My main reference was [Chapter 14](http://book.realworldhaskell.org/read/monads.html) from *None* and [A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads) from *None*

### The Monad typeclass

I've written about typeclasses in an [old post](http://kuniga.wordpress.com/2011/12/04/haskell-typeclass/)  (in Portuguese). Haskell defines a typeclass called `Monad`:

{% highlight haskell %}

class Monad m where

    -- inject
    return :: a -> m a

    -- chain or bind
    (>>=)  :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg

{% endhighlight %}

When a type `m` implements this typeclass it is considered a *None*. Note that `>>` and `fail` have default implementation, but it's possible to override them.

One simplistic way to get a grasp of Monads is to think that the type `m` is a kind of a box. Then the `return` function puts the type `a` inside the box `m`. Also, the chain operator `(>>=)` receives a box containing `a` and a function that takes `a` and return the type `b` inside a box.

It's easier to understand those functions with an example. Let's consider the simplest monadic type, the Maybe Monad.

### Maybe Monad

The `Maybe` type can be define as an [Algebraic Data Type](http://kuniga.wordpress.com/2011/09/25/haskell-tipos-de-dados-algebricos/) as follows:

{% highlight haskell %}

data Maybe a = Nothing | Just a
    deriving (Show)

{% endhighlight %}

The standard implementation for the `Monad` typeclass for the type `Maybe` is the following

{% highlight haskell %}

instance Monad Maybe where
    -- chain
    Just x >>= k  =  k x
    Nothing >>= _ =  Nothing

    -- inject
    return x      =  Just x

    fail _ = Nothing

{% endhighlight %}

In the first implementation for the chain operator, `k` is a function that receives the value x wrapped inside `Just` and returns another value wrapped inside `Maybe`.

Note that `Maybe` overrides the default implementation for `fail`.

### Chaining

The chain operator has this name because we can concatenate several functions in a chain. Consider the following example using the Maybe monad:

{% highlight haskell %}

f1 a | a >= 0 = Just (sqrt a)
     | otherwise = Nothing

f2 b | b /= 0 = Just (1 / b)
     | otherwise = Nothing

f3 c = Just (round c)

-- Chaining f1, f2 and f3
f x = f1 x >>= f2 >>= f3

{% endhighlight %}

This chaining of Maybe monads is useful when we need to execute several functions such that if an error occurs, we stop further processing.

If we look at the default definition of the `(>>)` operator, it basically doesn't pass the value from the previous function forward, so the function to the right of `(>>)` doesn't have an input. For example, we can define a new function `f4`:

{% highlight haskell %}

f4 = Just 100.1
-- Chaining f1, f2, f4 and f3
f x = f1 x >>= f2 >> f4 >>= f3

{% endhighlight %}

### The 'do' notation

The chaining of the `(>>=)` operator has an alternative syntax using the keyword do. In this case we need to explicitly deal with the returned values and function parameters and pass to the following function. For the example above we would have:

{% highlight haskell %}

g_alt x = do
      y <- f1 x
      f2 y
      z <- f4
      f3 z

{% endhighlight %}

It's more verbose, but on the other hand the variables might be both used in the same scope. In the next example, `x` and `y` are available to be used in the last function:

{% highlight haskell %}

foo = do
  x <- Just 3
  y <- Just 4
  Just (x * y)

{% endhighlight %}

If we go with the `(>>=)` operators, we would have a less elegant solution with nested functions to keep both variables in the same scope:

{% highlight haskell %}

foo = Just 3 >>= (\x ->
        Just 4 >>= (\y ->
          Just (x * y)))

{% endhighlight %}

### List Monad

Lists also implement the `Monad` typeclass. The standard implementation is the following:

{% highlight haskell %}

instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []

{% endhighlight %}

In the analogy of boxes, we may think that the type `[]` can hold more than one item (of the same type). The return function inserts a single element in the box.

The bind operator receives a list of elements and a function that applies to elements ans return another list (its elements can have a different type).

If we have a function that receives an element and returns a list of one element, we have just a kind of map. For example:

{% highlight haskell %}

[1, 2, 3] >>= \x -> [x^2] // [1, 4, 9]

{% endhighlight %}

If it returns a list with two or more element, we can identify it's performing a cartesian product:

{% highlight haskell %}

[1, 2, 3] >>= \x -> [2*x-1, 2*x]

{% endhighlight %}

Let's consider an example using with nested functions

{% highlight haskell %}

f = [1, 2, 3] >>= (\n -> ['a', 'b'] >>= \m -> return(n, m))

{% endhighlight %}

For which we get `[(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]`, or using the `do` syntax:

{% highlight haskell %}

f = do
  n <- [1, 2, 3]
  m <- ['a', 'b']
  return (n, m)

{% endhighlight %}

If we compare with the list comprehension syntax that gets the same output, we can see how similar they are:

{% highlight haskell %}

[(n, m) | n <- [1, 2, 3], m <- ['a', 'b']]

{% endhighlight %}

### Monad Laws

When we implement the Monad typeclass for a given type, Haskell doesn't have means to check the properties that actually makes the type a Monad. So we have to guarantee it ourselves when declaring our type monadic by verifying the following 3 properties:

 `return x >>= f` is equivalent to `f x`

In our analogy, it means that if we put our element in the box (`return`) and apply the operator (`>>=`), it must extract this element and apply `f`, which should be the as applying it directly.

 `m >>= return` is equivalent to `m`

It means that we are sending the element inside a box `m` and applying the operator `(>>=)`, which will extract the element and just put it again inside the box (`return`), so the same thing that entered must come out, in this case, `m`.

 states that `(m >>= f) >>= g` is equivalent to `m >>= (\x -> f x >>= g)`

In a expression, the associativity property means that we can execute the operations in a chain in any order (e.g. `(a + b) + c == a + (b + c)`).

This is partially true here, because if we try to have `(m >>= f) >>= g` equal to `m >>= (f >>= g)`, which is not correct because the operator is not symmetric (it requires a monadic type and not a function that returns one).

To solve this problem, we can curry the function applying the first parameter. Since f has type `(Monad m) => a -> m b`, then `(f x)` for `x` of type `a`, we have the type `m b`.

In [2], the authors define a new symmetric operator `(<=<)` that makes it easy to spot the associative law:

{% highlight haskell %}

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)

{% endhighlight %}

Now we can say that `f <=< (g <=< h)` should be the same as `(f <=< g) <=< h`.

We must get x from somewhere though and we can do this by wrapping it inside a function, thus `(f >>= g)` becomes `(\x -> f x >>= g)`.

Note however that we're not actually executing the functions in the chain in different order, because we lifted the operation to another function that will only the executed after it has the element from the left of the operator `(>>=)`

### References

* [[1]("http://book.realworldhaskell.org/read/monads.html")] 
 Real World Haskell – Chapter 14. Monads
* [[2]("http://learnyouahaskell.com/a-fistful-of-monads")] 
 Learn You a Haskell for Great Good! - A Fistful of Monads