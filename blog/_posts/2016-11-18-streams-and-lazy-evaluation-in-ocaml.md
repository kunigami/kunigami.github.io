---
layout: post
title: "Streams and Lazy Evaluation in OCaml"
tags: [data structures, ocaml]
---

In this short post we'll discuss lazy evaluation in OCaml and study a data structure called *Stream*. It's based mainly on chapter 4 of *Purely Functional Data Structures* and it's part of a series of [study notes](https://kunigami.wordpress.com/tag/purely-funcional-data-structures/) on that book.
### Lazy Evaluation
Lazy evaluation is a property in which an expression is not evaluated immediately (suspended) and when it's evaluated the first time, the subsequent calls are cached (memoized). Functional languages like Haskell are lazy evaluated, but not OCaml, which is eagerly evaluated. Because the results are memoized, expressions that are lazily evaluated must always return the same value given the same inputs. In Haskell it's easy to enforce because functions are pure, that is, they do not rely on side effects.

<figure class="center_children">
    <a href="https://www.flickr.com/photos/briangratwicke/2726138218"><img src="{{site.url}}/resources/blog/2016-11-18-streams-and-lazy-evaluation-in-ocaml/3333_10_sloth.jpg" alt="Lazy?" /></a>
    <figcaption> Lazy? *Source: Flickr - Brian Gratwicke*</figcaption>
</figure>

In the book the author defines a notation for lazy evaluation:

`datatype a susp = $ of a`

In OCaml, we can work with lazily evaluated expressions through the [Lazy](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lazy.html) module. The definition of a suspension is similar:

`type 'a t = 'a lazy_t`

and we can use the **lazy** construct. Let's define a simple expensive function, a naive Fibonacci, which runs at `O(2^n)` time:

{% highlight ocaml %}

let rec fibo n =
  if n <= 1 then 1
  else (fibo (n - 1)) + (fibo (n - 2))
;;

{% endhighlight %}

We can create a lazy evaluated version of it:

{% highlight ocaml %}

let lazy_fibo n = lazy (fibo n);;

{% endhighlight %}

We can see that by assigning it to a variable, it doesn't cause the function to be executed:

{% highlight ocaml %}

let r = lazy_fibo 42;;

{% endhighlight %}

The author defines a matching operator (`$`) that causes a lazy expression to be evaluated, but I couldn't find a corresponding operator in OCaml. Nevertheless, the Lazy module has the `force()` function, which does exactly that:

{% highlight ocaml %}

Lazy.force r;; // It might take a while!

{% endhighlight %}

Note that if we execute the same expression again, the second time it returns much faster, because of the memoization.

We are now ready to introduce the stream data structure.
### Streams
A stream is a lazy version of a linked list. Recall that a linked list is composed of nodes which point to the next node, or equivalently, to the remaining of the list. The usual definition of a linked list is:

{% highlight ocaml %}

type 'a node = Nil | Node of 'a * 'a node

{% endhighlight %}

If we want to be explicit that a node is actually pointing to a sublist, we could use an intermediate type, listType:

{% highlight ocaml %}

type 'a node = Nil | Node of 'a * 'a list
and 'a list = 'a node

{% endhighlight %}

Note that `node` and `list` are mutually recursive (they depend on each other), so we have to define them together by using the **and** construct.

In a stream the pointer to the remaining of the list is lazily evaluated, so the type is:

{% highlight ocaml %}

type 'a streamCell = Nil | StreamCell of 'a * 'a stream
and
'a stream = ('a streamCell) Lazy.t

{% endhighlight %}

With this basic structure we can implement many of the list functions for streams.

**Concatenation**


Let's start with the concat operator (++):

{% highlight ocaml %}

let rec (++) (streamA: 'a stream) (streamB: 'a stream): ('a stream) =
  let computedStreamA = Lazy.force streamA in
  match computedStreamA with
    | Nil -> streamB
    | StreamCell (elem, rest) -> lazy (StreamCell (elem, rest ++ streamB))
;;

{% endhighlight %}

Note that it never evaluates `streamB` and it only evaluates the first cell of `streamA`.

To help us testing, we can define function to convert from a list:

{% highlight ocaml %}

let rec fromList (l: 'a list): ('a stream) = match l with
  | [] -> lazy Nil
  | x :: xs -> lazy (StreamCell (x, fromList xs))
;;

{% endhighlight %}

and a function that forces the evaluation of the entire stream, essentially converting it back to a list:

{% highlight ocaml %}

let rec toList (stream: 'a stream): ('a list) =
  let computedStream = Lazy.force stream in
  match computedStream with
    | Nil -> []
    | StreamCell (elem, rest) -> elem :: (toList rest)
;;

{% endhighlight %}

**Take**


The `take(n)` function returns the first n elements from a stream. Like the concat function, only the first node of the stream is evaluated. The recursive call is suspended.

{% highlight ocaml %}

let rec take (n: int) (stream: 'a stream) : ('a stream) =
  if n == 0 then lazy Nil
  else
    let computedStream = Lazy.force stream in
    match computedStream with
      | Nil -> lazy Nil
      | StreamCell (elem, rest) -> lazy (StreamCell (elem, (take (n - 1) rest)))
;;

{% endhighlight %}

**Drop**


The `drop(n)` function removes the first n elements from a stream and returns the result. In this case, we need to evaluate all the n recursive calls:

{% highlight ocaml %}

let rec drop (n: int) (stream: 'a stream): ('a stream) =
  if n == 0 then stream
  else
    let computedStream = Lazy.force stream in
    match computedStream with
      | Nil -> lazy Nil
      | StreamCell (_, rest) -> drop (n - 1) rest
;;

{% endhighlight %}

`take` and `drop` look very similar but one is lazy while the other is not. That's because the head of the stream is not suspended, but the tail is. In the drop case we need to find the (n+1)-th element that will be the new head of the stream. In the take case, we're not changing the head, and since the tail is suspended, it can wait.

**Reverse**


The reverse function reverts the order the elements in a stream. In this case it's more obvious that since we're changing the location of the head, it must be eagerly evaluated.

{% highlight ocaml %}

let reverse (stream: 'a stream): ('a stream) =
  let rec reverse' = fun oldStream newStream ->
    let computedStream = Lazy.force oldStream in
    match computedStream with
      | Nil -> newStream
      | StreamCell (elem, rest) -> reverse' rest  (lazy (StreamCell (elem, newStream)))
  in reverse' stream (lazy Nil)
;;

{% endhighlight %}
### Conclusion
In this post we saw that OCaml is not lazy evaluated but we can rely on the Lazy module to accomplish that. We also learned a new data structure, stream, which is recursively lazily evaluated and operations like `concat` and `take` play well with laziness, while other like `drop` and `reverse` do not.

The full implementation with comments is available on [github](https://github.com/kunigami/blog-examples/blob/master/2016-11-01-ocaml-streams/stream.ml).
### References
* [[1](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lazy.html)] OCaml Module Lazy
* [[2](http://blogs.perl.org/users/cyocum/2011/06/mutually-recursive-types.html)] cyocum - Mutually Recursive Types
* [[3](http://typeocaml.com/2014/11/13/magic-of-thunk-lazy/)] Implementing lazy from scratch in OCaml
