---
layout: post
title: "Monad Transformers"
tags: [category theory, haskell]
---

In this post we'll talk briefly about Monad Transformers. Chapter 18 from the Real World Haskell inspired this, but as usual, it was a bit hard for me to digest. The best source so far for this subject was the Haskell wiki [1].

<figure>
    <a href="https://kunigami.files.wordpress.com/2015/05/onion.jpg"><img src="{{site.url}}/resources/blog/2014-07-19-monad-transformers/2015_05_onion.jpg" alt="onion" /></a>
</figure>


In a high-level, monad transformers are monads generated by combining monads into a new one (thus transforming monads in monads).

Intuitively, it does so by using the analogy of wrapping, as we have for Monads, but monad transformers wraps monads inside monads. Thus, Dan Piponi makes an analogy of onion layers [3]. The idea of transformers is to avoid boilerplate in common scenarios where two monads are used in conjunction.

We'll present some monad transformers, all follow the pattern where two monads are combined, the first one is fixed, and the other is generic. The fixed monads are `Maybe`, `List` and `State` and their correspondent transformers are called `MaybeT`, `ListT` and `StateT`, respectively. The `Writer` and `Read` monads also have corresponding transformers, but we're not talking about them here.

As in [1], we'll be more detailed in describing the `MaybeT` which is the simplest of our examples, and for the other two, we'll limit ourselves to the definition and a brief explanation.

### The MaybeT monad

Let's start by recapping the Maybe monad, as seen in a [previous post]({{site.url}}/blog/2013/05/26/monads-in-haskell---part-i.html).

**Review of the Maybe monad**



The Maybe data type can be defined as follows:

{% highlight haskell %}

data Maybe a = Nothing | Just a
 deriving (Show)

{% endhighlight %}

The implementation of the monad interface is the following:

{% highlight haskell %}

instance Monad Maybe where
  return  = Just
  Just x   >>= f = f x
  Nothing  >>= f = Nothing

{% endhighlight %}

Remembering, `return` wraps an element in the monad and `>>=` is the bind operator, which takes an element wrapped in a monad, extracts it, applies a function `f`, which returns another element wrapped in the monad.

**The MaybeT data type**



Monads can contain other monads and one particular useful combination is of a monad containing the Maybe monad. While we can accomplish that by regular use of monads, we can also avoid some boilerplate code by having a special type that encodes this combination, in this case the `MaybeT` data type.

We can think of `MaybeT` data type as a 3-layer wrapping. The inner layer being the `Maybe` monad, then a generic monad and then the actual `MaybeT` wrapper.

{% highlight haskell %}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

{% endhighlight %}

**MaybeT is a Monad**



MaybeT is also a monad. One possible implementation is:

{% highlight haskell %}

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                        bindOfMaybe f maybe_value

bindOfMaybe f maybe_value = case maybe_value of
  Nothing    -> return Nothing
  Just value -> runMaybeT $ f value

{% endhighlight %}

Let's break in parts:

(1) `return  = MaybeT . return . Just`

The first part is `Just`, which encapsulates the inner element in the `Maybe` monad. The second `return` encapsulates in the generic monad `m` and finally we encapsulate in the `MaybeT` monad.

(2)

{% highlight haskell %}

x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                      bindOfMaybe f maybe_value

{% endhighlight %}

The type signature is given by:

`(>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b`

The bind operation has to do the opposite operation first, that is,
de-encapsulate the three layers of monads before running `f` on it.

Then, we need to encapsulate into `Maybe`, `m` and `MaybeT` again

Alternatively, we can use the chained notation:

{% highlight haskell %}

x >>= f = MaybeT $
            runMaybeT x >>=
              \maybe_value -> bindOfMaybe f maybe_value

{% endhighlight %}

### The ListT monad

Let's review the list `[]` monad, as we saw in a previous [post]({{site.url}}/blog/2013/05/26/monads-in-haskell---part-i.html).

{% highlight haskell %}

instance Monad [] where
  return x = [x]
  xs >>= f =
      let yss = map f xs
       in concat yss

{% endhighlight %}

The idea is very similar to the `Maybe` monad, we wrap the list in two other layers, the intermediate one being a generic monad. The implementation of the  monad class type is essentially the same, except that, again, we have to do extra wraps and unwraps:

{% highlight haskell %}

newtype ListT m a = ListT { runListT :: m [a] }

instance (Monad m) => Monad (ListT m) where
  return x = ListT $ return [x]
  tm >>= f = ListT $ do xs  <- runListT tm
		        yss <- mapM (runListT . f) xs
                        return (concat yss)
  -- Alternatively
  -- tm >>= f = ListT $ runListT tm
  --                      >>= \xs -> mapM (runListT . f) xs
  --                        >>= \yss -> return (concat yss)


{% endhighlight %}

### The StateT monad

We've talked about the State monad [before]({{site.url}}/blog/2013/07/28/monads-in-haskell---part-ii.html). It can be defined in the following way:

{% highlight haskell %}

newtype State s a =
    State { runState :: (s -> (a,s)) }

{% endhighlight %}

And the monad implementation is given by:

{% highlight haskell %}

instance Monad (State s) where
    return a        = State $ \s -> (a,s)
    (State x) >>= f = State $ \s ->
          let (v,s') = x s
                  in runState (f v) s'

{% endhighlight %}

The idea of combining it with another generic monad and wrapping it, leads to a analogous idea to the `List`/`ListT` classes. Let's define the `StateT` class:

{% highlight haskell %}

newtype StateT s m a =
    StateT { runStateT :: (s -> m (a,s)) }

{% endhighlight %}

In this case, the generic monad wraps the result of the previous function `s -> (a, s)`. The monad implementation is similar to the `State` one, except that we have to take into account the extra layer:

{% highlight haskell %}

instance (Monad m) => Monad (StateT s m) where
  return a         = StateT $ \s -> return (a,s)
  (StateT x) >>= f = StateT $ \s -> do
    (v,s') <- x s
    runStateT (f v) s'

{% endhighlight %}

For the return definition, we wrap `(a, s)` in using the `m` monad, by the use of return function of `m`.

The bind operator, `x` applied to `s` will return `m (a, s)`. We extract if from m by using the `<-` operator and then run the function on `a`, but since `(f v)` returns the result wrapped in `StateT`, we need to extract it using `runStateT`, and finally wrap into `m` again and then into `StateT`.

### The MonadTrans interface

All the monad transformers can implement the `MonadTrans` interface, which basically defines the function `lift`.

{% highlight haskell %}

ghci> :m +Control.Monad.Trans
ghci> :info MonadTrans
class MonadTrans t where lift :: (Monad m) => m a -> t m a
  	-- Defined in Control.Monad.Trans

{% endhighlight %}

Lift is a generic version of `liftM`, in a sense it allows a function that only applies to the inner element to be applicable to the top-level monad. The implementation of the `MaybeT` monad is the following:

{% highlight haskell %}

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)

{% endhighlight %}

### Stack of monads

After understand better the concept of Monad transformers, Chapter 18 from Real World Haskell [2] becomes easier to digest and it's quite interesting.

At one point, it discusses the real power of combining multiple monad transformers (for example, the generic monad in `MaybeT` could be another monad transformer, say `WriterT`). This "equips" a given data type with traits corresponding to the underlying monads (in the example the optional nature of `Maybe` and the logging capabilities of the `Writer` monad).

### References

* [[1](http://en.wikibooks.org/wiki/Haskell/Monad_transformers)] Wikibooks: Haskell/Monad
* [[2](http://book.realworldhaskell.org/read/monad-transformers.html)] Real World Haskell: Chapter 18. Monad transformers
* [[3](http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html)] A Neighborhood of Infinity: Grok Haskell Monad Transformers
