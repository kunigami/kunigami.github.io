---
layout: post
title: "Haskell Concurrent Programming"
tags: [haskell]
---

### Introduction

<figure class="image_float_left">
    <a href="https://kunigami.files.wordpress.com/9999/04/bookcover.jpg"><img src="{{site.url}}/resources/blog/2015-05-14-haskell-concurrent-programming/9999_04_bookcover.jpg" alt="bookcover" /></a>
</figure>

In this post we'll talk about concurrency and parallelism in Haskell. This post is a set of notes from [Chapter 24](http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html) of *Real World Haskell* and [Chapter 2](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html) and [Chapter 3](http://chimera.labs.oreilly.com/books/1230000000929/ch03.html) from *Parallel and Concurrent Programming in Haskell*.

Concurrency and parallelism are similar concepts, and it's hard to draw a line between their definitions. In general, **concurrency** represents the ability of an application handling multiple (concurrent) tasks at the same time. It doesn't require the underlying hardware to have multiple cores, so even applications running on single core can implement concurrency via threads.

**Parallelism**, in other hand, usually means breaking a slow task in smaller pieces that can be computed in parallel in order to speed the elapsed time. To be worth it, it requires multiple-cores or multiple machines.

In an operating system context, a **process** represents an instance of a program. It has a self-contained environment (e.g. memory space) and contains one or more threads. **Threads** can be thought as lightweight processes that share the environment with other threads in the same process [[4](https://docs.oracle.com/javase/tutorial/essential/concurrency/procthread.html)].

### Haskell threads

**Forking.** The module `Control.Concurrent` implements a thread system. The program starts out with a single thread (referred as main thread) and we create new threads from the current thread using a fork.

We can use the `forkIO` function, which takes an IO action and runs that in a separate thread. The remaining code continues running on the main thread. Because of the concurrent nature of threads, the system doesn't guarantee order of execution between those. In the example below, we fork the main thread to print "hello" and the the main thread prints "world".

{% highlight haskell %}

import Control.Concurrent

forkExample = do
  forkIO $ print "hello"
  print "world"

{% endhighlight %}

If we run this code many times we'll get varying results from `"hello""world"` to `"wor"lhde"llo"` (printing the whole sentence is not atomic because Haskell uses lazy evaluation - we'll see next how to avoid this behavior).

**Mutable variable** or **MVar** is a structure provided by the `Control.Concurrent` module. It can either have a value or be empty. If we try to write (`putMVar`) to a `MVar` that already has a value, the writing thread blocks until some other thread extracts (`takeMVar`) the current value from that `MVar`. On the other hand, if a thread tries to read from an empty `MVar`, it blocks until another thread puts a value into it.

We can use MVars to synchronize threads. In the fork example, there were no guarantees in the order of execution, but in the following example, we can guarantee the child thread will only execute `putStrLn` after `putMVar` is executed.

{% highlight haskell %}

mvarExample = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn ("received " ++ show v)
  putStrLn "sending"
  putMVar m "wake up!"

{% endhighlight %}

The MVar provides a mutex (mutual exclusion) for a piece of data (the `"wake up"` string), since only one thread can read the value from the MVar. They also provide a communication mechanism between two threads.

**Channel** or **Chan**, can be viewed as a generalization of a MVar. In a channel, a thread doesn't block when inserting a value. It rather enqueues that value. When a thread reads from the channel, it pops an element from the front of the queue, or blocks if the queue is empty.

We can think of `Chan` as containing a queue of infinite size, whereas `MVar` has a queue of size 1.

In the following example, we have two children threads writing to a channel and the main thread reading from them. We guarantee the program will only finish after but "hello world" and "now i quit" are printed, but the order is not guaranteed.

{% highlight haskell %}

chanExample = do
  ch <- newChan
  forkIO $ do
    putStrLn "thread 1"
    writeChan ch "hello world"
  forkIO $ do
    putStrLn "thread 2"
    writeChan ch "now i quit"
  putStrLn "main thread"
  readChan ch >>= print
  readChan ch >>= print

{% endhighlight %}

**MVar as a lock.** If we run the code above, chances are the output will come out mingled, like

{% highlight haskell %}

thrtmehaariden a 1dt
 h2r
ead
"hello world"
"now i quit"

{% endhighlight %}

We can make the print function atomic by using an MVar and require the threads to "acquire" it in order to invoke the print function. The following example creates an atomic version of `putStrLn` using the MVar lock:

{% highlight haskell %}

atomicPutStrLn :: MVar a -> String -> IO ()
atomicPutStrLn lock msg = withMVar lock $ (\_ -> putStrLn msg)

{% endhighlight %}

The function `withMVar()` takes an MVar and a function. It extracts the value of the MVar (`lock` in the example above), executes the function with that value and puts the value back into the MVar. In this case, since we're using it just for locking, we don't care about the value inside the MVar.

We can replace `putStrLn` with `atomicPutStrLn` in our previous code:

{% highlight haskell %}

chanExampleStrict = do
  ch <- newChan
  lock <- newMVar ()
  forkIO $ do
    atomicPutStrLn lock "thread 1"
    writeChan ch "hello world"
  forkIO $ do
    atomicPutStrLn lock "thread 2"
    writeChan ch "now i quit"
  atomicPutStrLn lock "main thread"
  readChan ch >>= print
  readChan ch >>= print

{% endhighlight %}

Note how we create the MVar `lock` with a dummy value and use it for the `atomicPutStrLn` calls [[5](http://stackoverflow.com/questions/2981984/can-i-ensure-that-haskell-performs-atomic-io#_=_)].

### Parallel Programming

**Setup.** By default Haskell only uses a single core, even if more are available. To turn on multi-threaded support, we must use the `-threaded` flag during compilation.

Then, in runtime, we can provide the `-Nx` flag to the RTS (Run Time System) when running the program (where `x` is a natural number representing the number of cores).

One challenge in parallelizing Haskell code is due to lazy evaluation. We have less control on when part of the code will be actually evaluated so we need to impose some strictness to guarantee it will be executed in parallel.

**Weak head normal form vs. normal form**



[This answer](http://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form) on StackOverflow gives a very nice explanation between **Weak Head Normal Form** (WHNF) and **Normal Form** (NF). Copying parts of it here.

> An expression in **normal form** is fully evaluated, and no sub-expression could be evaluated any further (i.e. it contains no un-evaluated thunks).
> 
> These expressions are all in normal form:
> 
> `
> 42
> (2, "hello")
> \x -> (x + 1)
> `
> 
> These expressions are not in normal form:
> 
> `
> 1 + 2                 -- we could evaluate this to 3
> (\x -> x + 1) 2       -- we could apply the function
> "he" ++ "llo"         -- we could apply the (++)
> (1 + 1, 2 + 2)        -- we could evaluate 1 + 1 and 2 + 2
> `
> 
> An expression in **weak head normal form** has been evaluated to the outermost data constructor or lambda abstraction (the head). Sub-expressions may or may not have been evaluated.
> 
> `
> (1 + 1, 2 + 2)       -- the outermost part is the data constructor (,)
> \x -> 2 + 2          -- the outermost part is a lambda abstraction
> 'h' : ("e" ++ "llo") -- the outermost part is the data constructor (:)
> `
> 
> These expressions are not in weak head normal form:
> 
> `
> 1 + 2                -- the outermost part here is an application
>                      -- of (+)
> (\x -> x + 1) 2      -- the outermost part is an application of
>                      -- (\x -> x + 1)
> "he" ++ "llo"        -- the outermost part is an application of (++)
> `
> 
> 

We can analyze whether a given expression has been evaluated in ghci using the :sprint command. It prints the contents of a variable if it was already evaluated of "_" otherwise. For example:

{% highlight haskell %}

> let a = 1 + 2
> :sprint a
a = _
> print a  -- evaluates a
3
> :sprint a
a = 3

{% endhighlight %}

This is useful to understand how things get evaluated. Another interesting example is working with lists.

{% highlight haskell %}

> let a = [1, 2, 3, 4, 5]
> :sprint a
a = _
> length a
5
> :sprint a
-- We know the length but don't have to evaluate the contents
a = [_, _, _, _, _]
> sum a
25
> :sprint a
-- To perform a sum, we have to go over all elements
a = [1, 2, 3, 4, 5]

{% endhighlight %}

Your results may vary, since Haskell might decide to perform more than the bare minimum evaluation.

The **seq** function takes two arguments (seq x y) and before y is evaluated to the WHFN, x is also evaluated to WHFN. The **par** function (from the `Control.Parallel` module) is similar to seq but it also tries to evaluate the first argument in parallel.

The compiler might decide to evaluate the second argument of `seq` before the first if it thinks it would improve performance. **pseq** is a stricter version in which the first argument is always evaluated first.

WHFN might not be enough for parallelizing. To make sure we're splitting an expensive task among cores, we need to force full evaluation (normal form). To see why, consider the following example, in which we want to parallelize the map function:

{% highlight haskell %}

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x:xs) = let r = f x
		       in r `par` r : parallelMap f xs
parallelMap _ _      = []

{% endhighlight %}

If b has a nested structure, it's not guaranteed that calling r par will fully evaluate r, since it only guarantees to evaluate the outermost constructor. To overcome that, one option is to use evaluation strategies.

**Evaluation Strategies**



Before talking about strategies, let's introduce the `Eval` monad, provided by `Control.Parallel.Strategies`:

{% highlight haskell %}

data Eval a
instance Monad Eval

runEval :: Eval a -> a

rpar :: a -> Eval a
rseq :: a -> Eval a

{% endhighlight %}

The `rpar` and `rseq` are the counterpart of `par` and `pseq` respectively. `rpar` indicates its argument can be evaluated in parallel (non-blocking), while `rseq` forces the evaluation of its arguments before continuing (blocking).

Using this monad, our `parallelMap` function would become:

{% highlight haskell %}

parallelMap :: (a -> b) -> [a] -> Eval [b]
parallelMap f (x:xs) = do
  r  <- rpar  (f x)
  rs <- parMap f xs
  return (r:rs)
parallelMap _ _ = return []

{% endhighlight %}

Strategy is a design pattern in which we make the code independent of algorithms, so different algorithms can be used interchangeably. The `Control.Parallel` module uses it to separate the evaluation strategy from the application code.

In this context, a strategy is basically a function that takes in a type and defines a way to evaluate that type. More specifically,

{% highlight haskell %}

type Strategy a = a -> Eval a

{% endhighlight %}

A simple example is defining a strategy for a pair. We can evaluate each element in parallel:

{% highlight haskell %}

parPair :: Strategy (a,b)
parPair (a,b) = do
  a' <- rpar a
  b' <- rpar b
  return (a',b')

{% endhighlight %}

Note that a strategy "wraps" expression in the `Eval` monad. We can use `runEval` to "extract" that and evaluate the code. For the example with pairs, we could do:

{% highlight haskell %}

runEval (parPair (fib 35, fib 36))

{% endhighlight %}

The `using` function expects a value and a strategy as parameter, apply the strategy over the value and then evaluates it using `runEval`.

{% highlight haskell %}

using :: a -> Strategy a -> a
x `using` s = runEval (s x)

{% endhighlight %}

This syntax is easier to read and is more explicit about the separation of the evaluation strategy and the actual code. The pair example with using would be:

{% highlight haskell %}

(fib 35, fib 36) `using` parPair

{% endhighlight %}

The problem with `rpar` and `rseq` is that they only force evaluation to Weak Head Normal Form. Thus, if the elements within the pair are nested in another constructor, they might not be fully evaluated. To solve that, we can use different evaluation strategies other than `rpar`.

In order to do that, we can step up one level of abstraction and generalize `parPair` by defining a function that defines which strategies to evaluate to each pair:

{% highlight haskell %}

evalPair :: Strategy a -> Strategy b -> Strategy (a,b)
evalPair sa sb (a,b) = do
  a' <- sa a
  b' <- sb b
  return (a',b')

{% endhighlight %}

We can redefine now `parPair` in terms of `evalPair`

{% highlight haskell %}

parPair :: Strategy (a,b)
parPair = evalPair rpar rpar

{% endhighlight %}

We can then use the `rdeepseq` which expect types implementing the `NFData` (NF is for normal form) interface. This strategy evaluates a structure to normal form, by traversing it recursively, by calling `force`.

{% highlight haskell %}

import Control.DeepSeq

rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (force x)

parPairDeepSeq :: NFData a, NFData b) => Strategy (a,b)
parPairDeepSeq = evalPair rdeepseq rdeepseq

{% endhighlight %}

### Conclusion

In this post we covered concurrency and parallelism in Haskell. We learned how to work with multi-threads using the Control.Concurrent module, which also provides mechanism for mutual exclusion and communication between threads.

In the second part of the post, we saw how to make use of multiple cores to speed up expensive parts of the code by dividing the task in smaller pieces, that can be then executed in parallel. We learned that one of the main difficulties in doing parallel work in Haskell is due to lazy evaluation. We've covered ways to address that problem using evaluation strategies, which are designed in such a way that they are decoupled from the actual code being parallelized.

I've heard about Parallel and Concurrent Programming in Haskell, written by Simon Marlow before, but didn't have a chance to check it out. Having read the first 3 chapters so far, I think it's really well written and easy to follow. I'm excited to read more.

### References

* [[1](http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html)] Real World Haskell, Chapter 24
* [[2](http://chimera.labs.oreilly.com/books/1230000000929)] Parallel and Concurrent Programming in Haskell, Chapter 2, 3
* [[3](http://www.amazon.com/Design-Patterns-Elements-Reusable-Object-Oriented/dp/0201633612/ref=tmm_hrd_title_0)] Design Patterns: Elements of Reusable Object-Oriented Software - Strategy
* [[4](https://docs.oracle.com/javase/tutorial/essential/concurrency/procthread.html)] Oracle - Processes and Threads
* [[5](http://stackoverflow.com/questions/2981984/can-i-ensure-that-haskell-performs-atomic-io#_=_)] StackOverflow - Can I ensure that Haskell performs atomic IO?
