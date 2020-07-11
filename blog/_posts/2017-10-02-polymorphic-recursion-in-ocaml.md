---
layout: post
title: "Polymorphic Recursion in OCaml"
tags: [data structures, ocaml, parametric polymorphism, structural decomposition, universally quantified type]
---

In Chapter 10 of *Purely Functional Data Structures*, Okasaki describes recursive types that are non-uniform. In this post we'll learn more about these types, how to implement them in OCaml and see an example by studying the implementation of *Random Access Binary Lists* using such a construct.


**Uniform recursive type**



As an example of **uniform** recursive data structure we have a simple list

`Cons(1, Cons(2, Cons(3, Nil)))`

Which has a recursive type, for example

`type 'a myList = Nil | Cons of 'a * 'a myList`

Each element of the list is either `Nil` (terminal) or it has a value of a polymorphic type `'a`, followed a recursive list also of type `'a`.


**Non-uniform recursive type**



Now, say that the type of the recursive list is not the same as the current list? Then we have a **non-uniform polymorphic recursive type**, for example:

`type 'a seq = Nil | Cons of 'a * ('a * 'a) seq`

We'll name this a **sequence**. A `int seq` would have the value in the first node would have type int, but the element from the second node would have type `(int, int)`, the third type `((int, int), (int, int))` and so on. This structure is equivalent to a complete binary tree, where the `i-th` element of `seq` represents the `i-th` level of the tree.

An example of value with this type is:

`Cons (1, Cons ((2, 3), Cons (((4, 5), (6, 7)), Nil)))`

We need a special syntax to type recursive functions that take recursive non-uniform types, because the type of the function called recursively might be a different polymorphic type than the caller. OCaml by default tries to infer the generic types of the function and bind them to specific instances [2]. For example, in

`let f: 'a list -> 'a list = fun x -> 13 :: x`

OCaml will assume `'a` is `int` and will compile fine. We can see this by pasting that code in the command line, `utop`.

`utop # let f: 'a list -> 'a list = fun x -> 13 :: x;;
val f : int list -> int list = `

The function will then not be polymorphic anymore. To prevent OCaml from auto-binding specific type instances, we can use a special syntax introduced in OCaml 3.12 [3]

`utop # let f3: 'a. 'a list -> 'a list = fun x -> 13 :: x;;
`

This time we'll get a compilation error:

*Error: This definition has type int list -&gt; int list which is less general than 'a. 'a list -&gt; 'a list*

The important thing is that this allow us binding the recursive calls with different types. According to the Jane Street Tech Blog [3]:

>
> Note that a polymorphic type annotation holds inside the body of a recursive definition as well as outside, allowing what is known as polymorphic recursion, where a recursive call is made at some non-trivial instantiation of the polymorphic type.

So for example, we can write this function to calculate the size of a sequence:

{% highlight ocaml %}
let rec size: 'a. 'a seq -> int = fun xs
  | Nil -> 0
  | Cons (_, xs) -> 1 + 2 * (size xs)
;;
{% endhighlight %}

The problem with this structure is that it can only represent lists of size in the form of `2^k - 1`. To work around that, we allow some items to not hold any elements at all, so that each item corresponds to a digit in the binary representation of the size of the list.

`type 'a seq = Nil | Zero of ('a * 'a) seq | One of 'a * ('a * 'a) seq`

For example, we can now represent a list of 10 elements, as

`Zero(One((1, 2), Zero(One(((3, 4), (5, 6)), ((7, 8),(9, 10))), Nil))))`

<figure class="center_children">
    <a href="https://upload.wikimedia.org/wikipedia/commons/f/fc/Mandel_zoom_08_satellite_antenna.jpg"><img src="{{site.url}}/resources/blog/2017-10-02-polymorphic-recursion-in-ocaml/2017_10_mandelbrot.jpg" alt="mandelbrot" />
</a>
</figure>

### Sequence binary random access list

We can use a sequence to implement a random access binary access list in a concise way.


**Insertion**



Inserting an element at the beginning is analogous to incrementing the binary number, that is, starting from the least significant digit, if it's a zero, we make it one, if it's one we make it a 0, and carry over a 1, by adding it to the next digit.

The carry over process is simple in this structure. Because the type of an item following an item of type `'a` is `('a, 'a)`, to merge the element to be added with the existing element, we simply make a tuple and pass it to the recursive call.

{% highlight ocaml %}
let rec push: 'a. 'a -> 'a seq -> 'a seq =
  fun element digits -> match digits with
    | Nil -> One (element, Nil)
    | Zero restDigits ->  One (element, restDigits)
    | One (currentElement, restDigits) ->
      Zero (push (element, currentElement) restDigits)
;;
{% endhighlight %}


**Head and Tail**



Removing or retrieving the first element is analogous to decrementing a binary number. If the digit is one, we make it zero and return the element and the resulting list. If it's a zero, we make a recursive call to get the next available element. However since the returned element is of type `('a, 'a)`, and our return type is `'a`, we only use the first value of the pair.

{% highlight ocaml %}
let rec popAux: 'a. 'a seq -> ('a * 'a seq) = function
  | Nil -> raise EmptyListException
  | One (elem, Nil) -> (elem, Nil)
  | One (elem, restDigits) -> (elem, Zero restDigits)
  | Zero (restDigits) ->
    let ((left, right), restResult) = popAux restDigits in
    (left, One (right, restResult))
{% endhighlight %}

Implementing `head` and `tail` using `popAux` is now trivial

{% highlight ocaml %}
let head digits = let (elem, _) = popAux digits in elem;;
let tail digits = let (_, newDigits) = popAux digits in newDigits;;
{% endhighlight %}


**Lookup**



Finding an element can be done by transforming the problem into smaller instances.

It helps to look at some simple examples. Let's consider 3 cases.

**Case 1.** If we had a single element, we either return it if the index is 0, or throw if it's larger than that.

`0: (0)`

**Case 2.** If we have 2 elements,

`0: ()
1: (0, 1)
`

Notice that when we go from the first level to the second, all items "doubled" in size, so we can "transform" this to the single element case by treating pairs as a single element, but since the index has individual elements granularity, we need to transform it by halving it. We reduced it to Case 1.

If our initial index was either 0 or 1, it's now 0, and we found our element in level 1.

`
1: (0)`

The problem is that we need to return a single element at level 0, not a pair, so we need to undo the transformation. We can use the parity of the original index will to decide which side of the pair to return. If it's even, we return the first element, otherwise the second one.

**Case 3.** If we have 3 elements,
`0: (0)
1: (1, 2)
`

and our index is larger than 0, we move to the next level but we need to account for the level we're skipping, so the transformation of index would look like:
`0: ()
1: (0)
`

which is reduced to Case 2.

These 3 cases can be used to find elements larger than 3. For example, say we have 10 elements and want to find the element at position 6:

`0: ()
1: (0, 1)
2: ()
3: (((2, 3), (4, 5)), ((6, 7), (8, 9)))`

**Step 1.** This is Case 2. We need to transform this by treating pairs as elements and halving the index:

`1': (0)
2': ()
3': ((1, 2), (3, 4))`

Note how this reduced the problem of finding the element at position 3 of a list with size 5. **Step 2.** We now are in case 3, where we need to skip the current element:

`1': ()
2': ()
3': (((0), (1)), ((2), (3)))`

Our index is now 2. **Step 3.** we go one level down
`
2: ()
3: (0, 1)`

With an index of 1. **Step 4.** Finally, we halve it once again and we finally found the right level that contains our index.

`3: (0)`

We now need to recurse back to find exactly which element on that level to pick. On Step 4, we can see our index 1 was on the right side of the pair in level 3, so we pick the right side, that is, `((6, 7), (8, 9))`.

On Step 3, our index 2 was on the left size of the innermost pair, that is  `(6, 7)`. On Step 2, we skipped the current element but didn't change levels, so there's no need to choose an element from the pair. Finally, on Step 1, the index 6 was on the left side of the innermost pair, which should return the element with a corresponding index 6.

In general, we can tell which side of the innermost pair to pick by observing that the indexes are ordered from left to right in a given level. And because every level has an even number of elements, we can assume that the first index in the level - or the first element in the first pair - is even. Hence the parity of the index is sufficient to determine which side of the pair to pick.

With this algorithm in mind, the lookup function is quite compact:

{% highlight ocaml %}
let rec lookup: 'a. int -> 'a seq -> 'a =
  fun index digits -> match (index, digits) with
    | (index, Nil) -> raise IndexOutOfBoundsException
    (* Case 1 *)
    | (0, One (elem, restDigits)) -> elem
    (* Case 2 *)
    | (index, One (_, restDigits)) -> lookup (index - 1) (Zero restDigits)
    (* Case 3 *)
    | (index, Zero restDigits) ->
      let (left, right) = lookup (index / 2) restDigits
      in if index mod 2 == 0 then left else right
{% endhighlight %}

The update follows a similar idea as the lookup, but the problem is that we need to return the updated level when returning from the recursion. That is, we need to update the level before returning.

To accomplish that, we can pass a callback, the updater, that encodes which pair we would pick at each level. We start with a function that simply returns the element to be updated

`(fun _ -> element)`

Then, at each level we create a new updater, which applies the previous updater on the left or right side of the pair, depending on the parity of the index:

{% highlight standard ml %}
let updater' = fun (x, y) -> if index mod 2 == 0
  then (updater x, y)
  else (x, updater y)
{% endhighlight %}

When we finally find the level that has our index, we can apply the function, which has the effect of "narrowing" down the elements from a given level to a single element, replacing the value at the target index and then returning the updated elements when returning from the recursion.

After applying the updater, we return the updated level recursively.

{% highlight ocaml %}
let rec fupdate: 'a. ('a -> 'a) -> int -> 'a seq -> 'a seq =
  fun updater index digits -> match (index, digits) with
    | (index, Nil) -> raise IndexOutOfBoundsException
    | (0, One (elem, restDigits)) -> One (updater elem, restDigits)
    | (index, One (elem, restDigits)) ->
      push elem (fupdate updater (index - 1) (Zero restDigits))
    | (index, Zero restDigits) ->
    let updater' = fun (x, y) -> if index mod 2 == 0
      then (updater x, y)
      else (x, updater y)
      in Zero (fupdate updater' (index / 2) restDigits)

let rec update index element digits =
  fupdate (fun x -> element) index digits

{% endhighlight %}

### Structural Decomposition

Okasaki introduces this implementation in the context of *Structural Decomposition*, which is a technique for creating data structures from incomplete ones. In this example, the raw implementation of the sequence can only represent lists of size `2^k`, but modeling each node in the sequence to be zero or one, zero not having any elements, we can work around the size restriction.

### Conclusion

The implementation of random access binary access list using sequences is very neat, but very hard to understand.

One of the major selling points of shorter code is that it tends to contain less bugs and also less corner cases. On the other hand, if the code is also harder to read and understand, it might be harder to spot bugs.

This post helped me understand a bit more about OCaml's type system. Digging a little also led me to interesting topics such as Parametric Polymorphism [4] and Existential vs. Universally quantified types [5].

### References

* [1] Purely Function Data Structures - Chris Okasaki
* [[2](https://blog.janestreet.com/ensuring-that-a-function-is-polymorphic/)] Jane Street - Ensuring that a function is polymorphic
* [[3](https://blog.janestreet.com/ensuring-that-a-function-is-polymorphic-in-ocaml-3-12/)] Ensuring that a function is polymorphic in Ocaml 3.12
* [[4](https://en.wikipedia.org/wiki/Parametric_polymorphism)] Wikipedia - Parametric polymorphism
* [[5](https://stackoverflow.com/questions/14299638/existential-vs-universally-quantified-types-in-haskell)] Existential vs. Universally quantified types in Haskell
