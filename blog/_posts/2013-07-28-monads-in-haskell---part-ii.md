---
layout: post
title: "Monads in Haskell - Part II"
tags: [category theory, haskell]
---

In this second post about Monads in Haskell, we'll talk about the three new types of Monads introduced in the chapter [For a Few Monads More](http://learnyouahaskell.com/for-a-few-monads-more), from the [Learn You A Haskell for Good](http://learnyouahaskell.com/): the Writer, the Reader and the State Monads.

### Writer Monad

The writer monad is useful when we want to attach some kind of logging to our value. The monadic type we're going to work with takes a value of type `a` and the "logging" type `w`.

{% highlight haskell %}

newtype Writer w a = Writer {runWriter :: (a, w)}

{% endhighlight %}

The definition of the Writer Monad is:

{% highlight haskell %}

import Data.Monoid

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x, v)) >>= f = let (Writer (y, v')) = f x
                           in Writer (y, v `mappend` v')

{% endhighlight %}

`Monoid` is a typeclass with a set of common functions that a monoid type must implement, which are, `mempty`, `mappend` and `mconcat`.

{% highlight haskell %}

class Monoid m where
  mempty :: m

  mappend :: m -> m -> m

  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

{% endhighlight %}

The `mempty` function returns an empty instance of the type `m`, `mappend` is any binary function over 2 instances of type `m` and `mconcat` is a generalization of `mappend`, a function over a list of instance of type `m`. The `Monoid` typeclass provides a default implementation of this function consisting of a `foldr`.

For example, the list type `[]` is can be seen as a monoid with the following implementation:

{% highlight haskell %}

instance Monoid [a] where
  mempty = []
  mappend = (++)

{% endhighlight %}

So going back to the Writer monad definition, we have that the "logging" type must be a monoid type. And we define the monad over the type `(Writer w)` which can be seem as wapper on the type `a`.

The `return` function consists in wrapping an instance of the type a into the type `(Writer w)`, so the simplest one is to just have an empty instance of type `w`. Since it's a monoid, we can use the functiion `mempty`.

The chain operator `(>>=)` takes an instance of type a wrapped into the `(Wrapper w)` and pass it to a function that operates on the type a and wraps the resulting type into `(Wrapper w)` again. In our implementation of the monad for `(Writer w)`, we extract the element `x`, apply `f` on it and compose a new log message combining the incoming log message and the one returned by function `f`, using the `mappend` function.

This implementation is available in the `Control.Monad.Writer` module. One simple example that uses the Writer Monad is a function that operates over numbers and log them:

{% highlight haskell %}

import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a*b)

{% endhighlight %}

Which will return

{% highlight haskell %}

ghci> runWriter multWithLog
(15,["Got number: 3","Got number: 5"])

{% endhighlight %}

### State Monad

The state monad when we want to carry some internal state along our computation. One example is a parser code presented in [2], where the authors use the partial parsed code as an internal state.

In the same fashion as the writer monadic type, we have a type that depends on two type parameters, one of them representing the wrapped value and the other the type of the state.

The difference here is that instead of a pair of the two types, our new type is actually a function that receives a value of type s and wraps it into a tuple together with an instance of type a.

{% highlight haskell %}

newtype State s a = State { runState :: s -> (a,s) }

{% endhighlight %}

The definition of the State Monad is given by:

{% highlight haskell %}

instance Monad (State s) where
  return x = State $ \s -> (x,s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
                                      (State g) = f a
                                    in  g newState

{% endhighlight %}

The implementation of the `return` function is straightforward, it just returns a function that receives a state and returns a pair of `x` with this state. This function is then wrapped into the type `State`.

The chain operator is more involved. To extract the value of type a from a `State` type, we first need to extract the function `h` from it (by pattern matching `(State h)`), and then extracting the value a by providing an state to it. Note that we can assume this state `s` because this is done within a lambda that receives a state.

After extracting the value a from the first parameter of `(>>=)`, we can finally apply the function `f` to it. This will return an instance of `State`, from which we extract yet another function `g` that receives an state and returns a pair. We use the state that resulted after applying the state `s` to function h and return the pair resulting from applying the new state to this function.

The State Monad is also useful when we're dealing with random number generation. In Haskell you have to keep a reference to the random number generator otherwise it will always generate the same number. The following example uses the random genetor as a state, which makes it simple to generate multiple random numbers:

{% highlight haskell %}

import System.Random
import Control.Monad.State

randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)

{% endhighlight %}

### Reader Monad

The Reader Monad [3] is similar to the State monad but in this case we have a state that is immutable, which we can see as an environment. The Reader type is a wrapper of a function that receives the environment `e` and returns an instance of type `a`. Since the environment is immutable, there's no need to return it along with our value.

{% highlight haskell %}

newtype Reader e a = Reader { runReader :: e -> a }

{% endhighlight %}

The definition of the Reader Monad is then:

{% highlight haskell %}

instance Monad (Reader e) where
  return a = Reader $ \_ -> a
  (Reader g) >>= f = Reader $ \e -> let (Reader h) = f (g e)
                                     in h e

{% endhighlight %}

For the `return` function, we have an analogous function as for the State Monad, but since in this case the function doesn't need to return the environment, we return a function that doesn't care about the parameter.

The chain operator is also a special case of the State Monad chain operator. We first extract the function `g` via pattern matching, within the lambda function we provide the environment `e` to `g` in order to retrieve our value of type `a` and apply the function `f`.

The function `f` will return another function `h` wrapped inside a Reader instance which we can extract using pattern matching again. This function wrapped back into a Reader instance is essentially the result of the chain operator.


### References

* [[1]("http://learnyouahaskell.com/for-a-few-monads-more")] 
 Learn You a Haskell for Great Good! – For a Few Monads More
* [[2]("http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html")] 
 Real World Haskell - Chapter 10. Code case study: parsing a binary data format
* [[3]("http://book.realworldhaskell.org/read/programming-with-monads.html")] 
 Real World Haskell - Chapter 15. Programming with monads