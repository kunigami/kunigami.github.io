---
layout: post
title: "Mutually Recursive Modules in OCaml"
tags: [data structures, ocaml]
---

In Chapter 10 of *Purely Functional Data Structures*, Okasaki describes a technique called data structure bootstrapping. It's a way to reuse existing implementation of data structures to construct (bootstrap) new ones.

In one of the examples he creates a new heap implementation with an efficient merge operation using another heap as basis, but it turns out that to implement this, we need to rely on mutually recursive modules, that is, two modules A and B, where A depends on B, and B depends on A.

In this post we’ll study the bootstrapped heap and learn how to implement mutually recursive modules in OCaml.

### Heap with efficient merging

Assume we have a heap implementation with `O(1)` `insert`, and `O(log n)` `merge`, `findMin` and `deleteMin` operations. We've seen such an implementation with [Skewed Binomial Heaps]({{site.url}}/blog/2017/09/01/numerical-representations-as-inspiration-for-data-structures.html)

We'll see how to construct a new heap implementation which will improve the merge complexity to `O(1)`.

Let's call the base heap `PrimaryHeap` and define our heap type as

{% highlight ocaml %}
type 'a heap = Empty | Heap of 'a * ('a heap) PrimaryHeap.heap
{% endhighlight %}

this type can be either empty or a node with an element (root) and a primary heap whose element is the bootstrapped heap itself, that is, `heap` and `PrimaryHeap.heap` form a mutually recursive types. Note that the above **is not a valid OCaml code**. We're using it to explain the theoretical concepts.

We can think of this as a k-ary tree where the element is the root and the children of that node are the subtrees, but these subtrees are stored in a heap instead of an array.

The root element at each node is the smallest among all of the subtrees. Hence, to obtain the minimum element for a heap, `findMin`, is trivial: we can simply return that element:

{% highlight ocaml %}
let findMin (heap: heap): tv =
  match heap with
    | Empty -> raise Empty_heap
    | Heap(elem, _) -> elem
{% endhighlight %}

Merging two bootstrapped heaps is analogous to linking two trees. The only invariant we need to maintain is that the smallest root continues being the root.

{% highlight ocaml %}
let merge (heap1: heap) (heap2: heap) = match (heap1, heap2) with
  | (Empty, heap2) -> heap2
  | (heap1, Empty) -> heap1
  | (Heap (element1, primaryHeap1), Heap (element2, primaryHeap2)) ->
    if ((Element.compare element1 element2) < 0)
      then Heap(element1, PrimaryHeap.insert heap2 primaryHeap1)
      else Heap(element2, PrimaryHeap.insert heap1 primaryHeap2)


{% endhighlight %}

Since the primary heap has `O(1)` `insert`, the bootstrapped heap has `O(1)` `merge`, which was our goal. Note that we can implement `insert` using `merge` by creating a *singleton* node and merging it with an existing heap.

{% highlight ocaml %}
let singleton (elem: tv): heap = Heap(elem, PrimaryHeap.empty)

let insert (elem: tv) (heap: heap): heap =
  merge heap (singleton elem)
{% endhighlight %}

We need to handle the deletion of the minimum element, which is the more involved operation. It consists in discarding the root of the present node, and finding a new root from the primary heap.

Since each element in the primary heap is a bootstrapped heap, we first obtain the bootstrapped heap containing the smallest element:

{% highlight standard ml %}
(Heap (newMinElem, minPrimaryHeap)) =
              PrimaryHeap.findMin primaryHeap
{% endhighlight %}

then we remove this node from the `primaryHeap`, and we merge the `minPrimaryHeap` back into `primaryHeap`.

{% highlight ocaml %}
let restPrimaryHeap = PrimaryHeap.deleteMin primaryHeap
in PrimaryHeap.merge minPrimaryHeap restPrimaryHeap
{% endhighlight %}

finally we make `newMinElem` the new root element of our top level bootstrapped heap. The complete code is

{% highlight ocaml %}
let deleteMin (heap: heap) =
  match heap with
    | Empty -> raise Empty_heap
    | Heap(_, primaryHeap) ->
      if PrimaryHeap.isEmpty primaryHeap then Empty
      else
        let (Heap (newMinElem, minPrimaryHeap)) =
          PrimaryHeap.findMin primaryHeap
        in let restPrimaryHeap = PrimaryHeap.deleteMin primaryHeap
        in Heap (newMinElem, PrimaryHeap.merge minPrimaryHeap restPrimaryHeap)

{% endhighlight %}

The only missing part is defining the correct type of the bootstrapped heap.

### Mutually Recursive Modules

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2017-11-16-mutually-recursive-modules-in-ocaml/2018_11_escher.jpg" alt="escher" />
    <figcaption> Drawing Hands by M. C. Escher</figcaption>
</figure>

Okasaki mentions that recursive structures are not supported in Standard ML (at least at the time my copy of the book was printed), but they are supported in OCaml.

To make modules mutually depend on another, we need to mark it as recursive via the `rec` keyword, and declaring both modules at the same time by using the `and` connector. Let's work with a toy example: two modules `Even` and `Odd`, where each depend on the other.

{% highlight ocaml %}
module rec Even = struct
  type t = Zero | Succ of Odd.t
end
and Odd = struct
  type t = Succ of Even.t
end
{% endhighlight %}

This will lead to a compilation error:

>
> Error: Recursive modules require an explicit module type.
>

We need to write the signatures explicitly:

{% highlight ocaml %}
module rec Even : sig
  type t = Zero | Succ of Odd.t
end = struct
  type t = Zero | Succ of Odd.t
end
and Odd : sig
  type t = Succ of Even.t
end = struct
  type t = Succ of Even.t
end
{% endhighlight %}

This [blog post from Jane Street](https://blog.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/) describes a way to define mutually recursive modules by only listing its signatures:

{% highlight ocaml %}
module rec Even: sig
  type t = Succ of Odd.t
end = Even
and Odd: sig
  type t = Succ of Even.t
end = Odd
{% endhighlight %}

The OCaml compiler can infer the implementation part from the type definitions, but unfortunately this won't work if the module has function definitions, which is the case for our heap implementation.

Things get more complicated in our case because the primary heap implementation uses a functor to generate a heap taking the element's module as parameter. In this case the element's module is our bootstrapped heap. A valid module definition is given below:

{% highlight ocaml %}
module rec BootstrappedElement: sig
  type t = Empty | Heap of Element.t * PrimaryHeap.heap
  val compare: t -> t -> int
end = struct
  type t = Empty | Heap of Element.t * PrimaryHeap.heap
  let compare heap1 heap2 = match (heap1, heap2) with
    | (Heap (x, _), Heap (y, _)) -> Element.compare x y
end
and PrimaryHeap: IHeapWithMerge
  with type tv := BootstrappedElement.t = SkewBinomialHeap(BootstrappedElement);;
{% endhighlight %}

Let's understand what is going on.

`type t = Empty | Heap of Element.t * PrimaryHeap.heap`

is the definition we presented above. We also implement the methods from the `Set.OrderedType` interface, namely `compare`, since this is the interface the heap maker expects. The comparison is based solely on the root element.

Then we declare the PrimaryHeap type at the same time, with type `IHeapWithMerge`, and because `tv` is unbound in that interface, we need to bind it to `BootstrappedElement.t`:

`PrimaryHeap: IHeapWithMerge with type tv := BootstrappedElement.t`

Finally we provide the implementation, using the result of the `SkewBinomialHeap()` functor having the `BootstrappedElement` module as element type:

`PrimaryHeap (...) = SkewBinomialHeap(BootstrappedElement)`

The syntax is pretty involved, but it accomplishes what we wanted. We can further refine this definition by adding

`include Set.OrderedType with type t := t`

to the `BootstrappedElement` signature. This includes all the interface of `Set.OrderedType`.

These newly defined modules are defined within a functor, the `BootstrappedHeap`, together with the methods we defined above. Like other heap generators, the functor takes a module representing the element type as parameter. In this case we can also allow the primary heap type to be passed as parameter so we don't have to use `SkewBinomialHeap` as implementation. Any heap with `merge` will do.

{% highlight ocaml %}
module BootstrappedHeap
  (Element: Set.OrderedType)
  (MakeHeap: functor (Element: Set.OrderedType) -> IHeapWithMerge with type tv = Element.t)
: IHeap with type tv = Element.t =
  struct

    module rec BootstrappedElement: sig
      type t = Empty | Heap of Element.t * PrimaryHeap.heap
      include Set.OrderedType with type t := t
    end = struct
      type t = Empty | Heap of Element.t * PrimaryHeap.heap
      let compare heap1 heap2 = match (heap1, heap2) with
        | (Heap (x, _), Heap (y, _)) -> Element.compare x y
    end
    and PrimaryHeap: IHeapWithMerge
      with type tv := BootstrappedElement.t = MakeHeap(BootstrappedElement);;
   (* Methods definition below... *)
end
{% endhighlight %}

The constructors define within `BootstrappedElement` are visible within `BootstrappedHeap` but they need qualification, such as `BootstrappedElement.Heap`. To avoid repeating this qualifier, we can use:

`include BootstrappedElement`

The complete implementation for `BootstrappedHeap` can be found on [github](https://github.com/kunigami/ocaml-data-structures/blob/master/heap/bootstrappedHeap.ml).

### Conclusion

The idea of using implementations of a given data structure to yield improve implementations is amazing! The mutual recursion nature of the bootstrap heap got me at first, but making analogies with a k-ary tree made it easier to understand.

I was struggling a lot to get the syntax right for the recursive modules required for this implementation until I stumbled upon this [github](https://github.com/yuga/readpfds/blob/2a62a6303141ea579eedf7366f82504d39149d0c/OCaml/bootStrappedHeap.ml) repository, from which I learned many new things about OCaml.

### References

* [1] Purely Function Data Structures, Chapter 10 – Chris Okasaki
* [[2](https://blog.janestreet.com/a-trick-recursive-modules-from-recursive-signatures/)] Jane Street Tech Blog - A trick: recursive modules from recursive signatures
* [[3](https://github.com/yuga/readpfds/blob/2a62a6303141ea579eedf7366f82504d39149d0c/OCaml/bootStrappedHeap.ml)] Github yuga/readpfds: bootStrappedHeap.ml
