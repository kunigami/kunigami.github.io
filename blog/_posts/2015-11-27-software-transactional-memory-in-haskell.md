---
layout: post
title: "Software transactional memory in Haskell"
tags: [haskell]
---

<figure class="image_float_left">
    <a href="https://kunigami.files.wordpress.com/9999/04/bookcover.jpg"><img src="{{site.url}}/resources/blog/2015-11-27-software-transactional-memory-in-haskell/9999_04_bookcover.jpg" alt="bookcover" /></a>
</figure>

In this post we'll discuss transactional memory in Haskell. This post is a set of notes from [Chapter 28](http://book.realworldhaskell.org/read/software-transactional-memory.html) of *Real World Haskell* and [Chapter 20](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html) of *Parallel and Concurrent Programming in Haskell*.

We'll first introduce the concept of software transactional memories, then explain what problem it tries to solve and then go through some toy examples in Haskell to learn about the API provided by the `Control.Concurrent.STM` library.

## Introduction

What is Software Transactional Memory? According to [[1](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html)]:

> Software Transactional Memory (STM) is a concurrency control mechanism analogous to database transactions for controlling access to shared memory in concurrent computing. STM is strategy implemented in software, rather than as a hardware component.
>

It's one of many alternatives to deal with shared memory among several concurrent threads in concurrent environments. We've discussed parallel and concurrent programming in a [previous post]({{site.url}}/blog/2015/05/14/haskell-concurrent-programming.html). In that case, our choice for thread synchronization was using locks implemented with mutable variables.

## The Problem

One problem with using locks to grant exclusive access to memory is that we need to careful with pitfalls like deadlocks. For example, consider the following  function that implements transferring values between two variables by using locks:

{% highlight haskell %}

import Control.Concurrent

transfer :: MVar Int -> MVar Int -> Int -> IO ()
transfer a b val = do
  valA <- takeMVar a
  valB <- takeMVar b
  putMVar a (valA - val)
  putMVar b (valB + val)

{% endhighlight %}

First, lets recap how `MVars` behave: an `MVar` either has a value or is empty. If a thread calls `takeMVar()` on an empty `MVar`, it blocks until another thread puts a value there. On the other hand, a thread blocks if it tries to `putMVar()` on a `MVar` that is not empty.

In our example, we're basically acquiring a lock to the variables `a` and `b`, so no other thread can read/write to them while we're performing our operations. After we're done, we write the new values back to the variables, releasing the acquired locks.

The problem here is the potential of a deadlock. Imagine a scenario in which two threads, T1 and T2, are calling `transfer()` at the same time, but T1 wants to transfer funds from `a1` to `a2` (calling `transfer a1 a2`) while T2 do the opposite (calling `transfer a2 a1`). It could happen that T1 acquires the lock to `a1`, but before it can acquire the lock to `a2`, T2 gets it. Now, T1 is blocked waiting for the lock to `a2` and T2 waiting for `a1`, causing a deadlock. A proposed solution is to always acquire the locks in a consistent order. For example by assigning IDs to the variables and acquiring the locks ordered by IDs. In this case, both T1 and T2 would try to acquire a1 and then a2.

This issue might not be as obvious if we're dealing with a complex real-world application, so we want models to prevent such cases. One such solution is using Software Transactional Memory (STM). In this model, the reading and writing to shared variables can be done atomically by using transactions, that is, either all operations succeed or none of them are executed (we rollback to the initial state).

## The STM Library

We can draw some parallels between `MVars` and the STM library (I recommend reading [this post]({{site.url}}/blog/2015/05/14/haskell-concurrent-programming.html) for an introduction).

**Transaction variable.** or `TVar`, is a parametrized type from the `Control.Concurrent.STM` library, similar to `MVar`. To create a new `TVar`, we can use the function `newTVar`. Let's take a look at the type interface for this function:

{% highlight haskell %}

> :t newTVar
newTVar :: a -> STM (TVar a)

{% endhighlight %}

We see it return a `TVar` wrapped in a `STM` monad (remember the analogous function for MVars, `newMVar()`, returns it wrapped in the `IO` monad). Before talking about this monad, let's describe the ways to access the contents of a `TVar`:

{% highlight haskell %}

> :t readTVar
readTVar :: TVar a -> STM a
> :t writeTVar
writeTVar :: TVar a -> a -> STM ()
{% endhighlight %}

**The STM monad**., similar to the `IO` monad, allow us to perform side effect actions, but `STM` limits side-effects to only `TVars`. [[1](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html)] provides a great explanation about the difference between these two monads:

> Why is STM a different monad from IO? The STM implementation relies on being able to roll back the effects of a transaction in the event of a conflict with another transaction (...). A transaction can be rolled back only if we can track exactly what effects it has, and this would not be possible if arbitrary I/O were allowed inside a transaction—we might have performed some I/O that cannot be undone, like making a noise or launching some missiles. For this reason, the STM monad permits only side effects on TVars, and the STM implementation tracks these effects to ensure the correct transaction semantics.

**Composability.** One feature of STM is composability, since we can combine two or more `STM` action in another `STM`. This enables better reuse, something we can't do easily if we use mechanisms like locks.

For an example, imagine we have a function that uses `MVars` to modify a variable atomically. Now suppose we want to modify two variables atomically. Even though we have the function to do this independently to each variable, we can't combine those in order to get a new function that modifies both variables atomically.

Since STM are modelled as monads, we can simply combine them using the `do` notation. For example, suppose we have a function `bump()`, which increments the content of a `TVar` by a given amount:

{% highlight haskell %}

import Control.Concurrent.STM

bump :: TVar Int -> Int -> STM ()
bump var amount = do
  val <- readTVar var
  writeTVar var (val + amount)

{% endhighlight %}

We can rewrite the transfer function we had for `MVar` in terms of `bump()`:

{% highlight haskell %}

transfer :: TVar Int -> TVar Int -> Int -> STM ()
transfer a b val = do
  bump a val
  bump b (-val)

{% endhighlight %}

Because we'll execute all these steps in a single transaction, we don't have to worry about acquiring the locks for both variables before hand, so we were able to combine independent functions but still have consistency guarantees.

**Executing a STM.** Even though we're combining smaller STM actions into more complex ones, we're still restricted to the STM land, but eventually we'll have to interface with the real-world, which means we'll have to convert a `STM` action to an `IO` action. That's exactly what the combinator `atomically` does:

{% highlight haskell %}

>:t atomically
atomically :: STM a -> IO a

{% endhighlight %}

This will execute the *entire* `STM` *action in a transaction*.

We also might want to create `TVars` in the `IO` space instead of the `STM` space, for example if we're interfacing with `IO` code that will execute the `STM` action. For example, if we want to test our composite `transfer()` function, we can define:

{% highlight haskell %}

testTransfer :: IO ()
testTransfer = do
  a <- newTVarIO 1
  b <- newTVarIO 2
  atomically $ incrementBoth a b
  printTVar a
  printTVar b

printTVar :: TVar Int -> IO ()
printTVar var = do
  val <- readTVarIO var
  putStrLn $ show val

{% endhighlight %}

Here we create two sample `TVars`, with 1 and 2 as values. Since we're inside an `IO` do block, we need to return the `TVars` in the `IO` space, so that's why we use the `newTVarIO()` variant. We can then execute the `transfer()` function atomically and finally print the contents of the variables afterwards. Since `testTransfer()` returns an IO action we can try this out on GHCI:

{% highlight haskell %}

> testTransfer
2
3

{% endhighlight %}

**Rollback.** How does `STM` implements the transaction? What happens when two threads try to access the same `TVar` at the same time? Consider two threads T1 and T2 calling `bump()` on the same variable at the same time. It might happen that both will execute the first line of the function:

{% highlight haskell %}

val <- readTVar var

{% endhighlight %}

before writing the incremented value back which, if we didn't have a transaction in place, it would cause a data consistency problem (the thread to write last would overwrite the results of the other). Because we're executing using a transaction, if either of the threads realizes the contents of their variables changed since the transaction began, it will rollback and restart the transaction. In this case, suppose T2 manages to write back the the variable first. T1 will have to rollback because the state it had at beginning of the transaction changed. T1 will then keep retrying the transaction until it succeeds.

**Custom rollbacks.** We just saw that STM implements the automatic rollback to guarantee data consistency. But what if we want to force a rollback when some condition is not met? For example, in our transfer example, we could rollback if the balance of one of the variables would become negative. There is another function, `retry`, which if called will cause the entire transaction to rollback.

Let's change our `bump()` function to rollback if the resulting value is less than 0.

{% highlight haskell %}

bumpIfValid :: TVar Int -> Int -> STM ()
bumpIfValid var amount = do
  val <- readTVar var
  if val + amount < 0
    then retry
    else writeTVar var (val + amount)

{% endhighlight %}

To test the code above, we can have one thread trying to decrement a variable by an invalid amount, while another thread will increment the content. The decrementing thread will keep trying to run the transaction until the increment thread is done:

{% highlight haskell %}

bumpAndPrint :: TVar Int -> Int -> MVar () -> IO ()
bumpAndPrint var amount lock = do
  atomically $ bumpIfValid var amount
  val <- readTVarIO var
  withMVar lock $
    \_ -> putStrLn ("New value: " ++ (show val))

testValidBumps :: Int -> IO ()
testValidBumps valA = do
  a <- newTVarIO valA
  lock <- newMVar ()
  forkIO $ bumpAndPrint a (-1) lock
  threadDelay 1000000 -- sleep 1 second
  bumpAndPrint a 3 lock

{% endhighlight %}

In this implementation, we fork a new thread that will try to decrement the `TVar a` by 1, if it doesn't succeed, it will try until the condition is met. We've added a delay to the main thread to make sure the child thread has a chance to execute first. Also, we're using `MVars` as a lock to make sure the `putStrLn()` operation happens atomically, so the output doesn't come out all scrambled (we discussed this technique [before]({{site.url}}/blog/2015/05/14/haskell-concurrent-programming.html)).

Let's test calling `testValidBumps()` in GHCI with 0:

{% highlight haskell %}

> testValidBumps
New value: 3
New value: 2

{% endhighlight %}

The child transaction didn't succeed at first. After a second, the main thread will increment the `TVar` by 3, after which the child thread will be able to proceed. Now, if we start with an amount enough for the child thread to succeed, say 1, we get:

{% highlight haskell %}

> testValidBumps
New value: 0
New value: 3

{% endhighlight %}

**OR'ing STMs.** Note that when combining multiple STM actions in a `do` block, we're essentially chaining them using the `bind` Monad operator. If any STMs in that block can triggers a retry, the entire composed STM will rollback. In a sense, the success of a composed STM is a "AND" of the success of the sub-STMs.

Another way to combine multiple STMs is to "OR" such that an STM succeeds if any of its sub-STMs succeed. We can do that by using the `orElse` combinator,

{% highlight haskell %}

> :t orElse
orElse :: STM a -> STM a -> STM a

{% endhighlight %}

It tries to execute the first STM and if that is rolled back via `retry`, then the second action is then executed. If the second one also triggers a `retry`, then the composite STM retries. Let's extend our previous example, by trying to bump two variables. If we fail to bump the first, we try the second.

{% highlight haskell %}

bumpEitherAndPrint :: TVar Int -> TVar Int -> Int -> MVar () -> IO ()
bumpEitherAndPrint a b amount lock = do
  atomically $
    (bumpIfValid a amount) `orElse` (bumpIfValid b amount)
  valA <- readTVarIO a
  valB <- readTVarIO b
  withMVar lock $ do
    let valStr = (show valA) ++ ", " ++ (show valB)
    \_ -> putStrLn ("New value: " ++ valStr)

testOrElse :: Int -> Int -> IO ()
testOrElse valA valB = do
  a <- newTVarIO valA
  b <- newTVarIO valB
  lock <- newMVar ()
  forkIO $ bumpEitherAndPrint a b (-1) lock
  threadDelay 1000000
  bumpEitherAndPrint a b 3 lock

{% endhighlight %}

We have two variables now and the second thread will try first to decrement `a` and if it doesn't succeed it tries `b`. Let's try with `a` few cases:

{% highlight haskell %}

> testOrElse 1 1
New value: 0, 1
New value: 3, 1

{% endhighlight %}

In the code above, it succeed in decrementing the first variable.

{% highlight haskell %}

> testOrElse 0 1
New value: 0, 0
New value: 3, 0

{% endhighlight %}

This time, it failed to decrement the first variable, but succeeded on the second.

{% highlight haskell %}

> testOrElse 0 0
New value: 3, 0
New value: 2, 0

{% endhighlight %}

Finally, in this case, it failed decrementing both variables, so the whole transaction was rolled out, until the main thread eventually bumped the first variable, allowing it to be then decremented.

## Conclusion

In this post we learned a new technique to deal with shared memory in a concurrent environment. The STM monad provides a neat design that allows composability and simplifies the work of using transactions in Haskell.

The main references I used for this post offers more practical and complex examples. I personally understand concepts better with toy examples, so I tried to use those in this post. The books offer interesting examples, which provide the power of this design.

After reading/writing about STMs, I feel like I've improved my understanding of the IO monad by seeing the differences between it and STM.

**Further Reading.** [[4](http://computationalthoughts.blogspot.com/2008/03/some-examples-of-software-transactional.html)] offers a good introduction to the subject and describes the dinning philosophers problem and implements a solution using STM.

Jones' paper [[3](http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/beautiful.pdf)] is very digestible and instructive. It offers a solution to a concurrency exercise, the Santa Claus problem [[5](http://www.crsr.net/files/ANewExerciseInConcurrency.pdf)].

Last but not least, Marlow [[1](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html)] offers a really nice discussion about performance, based on the current implementation of transactions by STM. The takeaway is to always minimize the amount of work we do inside an atomic block, since it increases the chance of rollbacks and those are not free.

Worth noting, in the same chapter [[1](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html)], Marlow implements a deque data structure (a list-like structure which allows inserting/removing elements from either ends in O(1) - amortized in this case) based on Okasaki’s Purely Functional Data Structures, which I'm planning to start reading soon.

## References

* [[1](http://chimera.labs.oreilly.com/books/1230000000929/ch10.html)] Parallel and Concurrent Programming in Haskell - Chapter 10. Software Transactional Memory
* [[2](http://book.realworldhaskell.org/read/software-transactional-memory.html)] Real World Haskell - Chapter 28. Software transactional memory
* [[3](http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/beautiful.pdf)] Beautiful concurrency, Simon Peyton Jones
* [[4](http://computationalthoughts.blogspot.com/2008/03/some-examples-of-software-transactional.html)] Computational Thoughts - Some examples of Software Transactional Memory in Haskell
* [[5](http://www.crsr.net/files/ANewExerciseInConcurrency.pdf)] A New Exercise in Concurrency, John A. Trono
