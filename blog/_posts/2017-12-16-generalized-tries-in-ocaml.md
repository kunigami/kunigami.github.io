---
layout: post
title: "Generalized Tries in OCaml"
tags: [data structures, ocaml]
---

In Chapter 10 of *Purely Functional Data Structures*, Okasaki describes a generalized trie. In this post we'll see how tries can be seen as maps over a specific type of keys and how we can construct maps over any type of keys.

We discussed [tries](https://en.wikipedia.org/wiki/Trie) in OCaml in a [previous post]({{site.url}}/blog/2017/05/22/largest-sets-of-subsequences-in-ocaml.html). At first glance, it looks like a special data structure but we'll now see it's basically a map.

The basic form of a map is a pair of (key, value). In most programming languages the key has to be a scalar (e.g. int, string) or some complex that can be converted to one (think of `hashCode()` in Java).

For simplicity, let's assume our scalar is an `int` and our value is of generic type `T`. Also, note that the syntax we present below is not valid OCaml (I find OCaml's type syntax a bit hard to read). A map with int keys can be represented as:

{% highlight ocaml %}
map<int, T>

{% endhighlight %}

If we want a two dimensional key, for example `(int, int)`, we can have a map of maps:

{% highlight ocaml %}
map<int, map<int, T>>
{% endhighlight %}

and if the dimensions are not fixed, for example, a list of integers, then we can define a recursive structure for our map

{% highlight ocaml %}
map<list<int>, T> =
  Node of (option<T> * map<int, mapOverList>)

{% endhighlight %}

If we think of strings as lists of characters, a trie is then a map where the key is a list of characters. From the type above we can simply change the key of our map above to a list of characters and for a basic trie T is a boolean

{% highlight ocaml %}
map<string, bool> =
  Node of (bool * map<char, mapOverList>)

{% endhighlight %}

Note that `option<bool>` is redundant, so we can use `bool` only.


**Maps over trees**



<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2017-12-16-generalized-tries-in-ocaml/2017_12_tree2.jpeg" alt="tree2" />
</figure>

We can now generalize the key to more complex recursive types such as trees. Suppose we fave the following type for `tree`:

{% highlight ocaml %}
tree<int> = (int * tree<int> * tree<int>)
{% endhighlight %}

The outermost map is indexed by the root of the tree. The inner maps are indexed by the left and right subtrees respectively:

{% highlight ocaml %}
map<tree<int>, T> =
  map<int, map<tree<int>, map<tree<int>, T>>>
{% endhighlight %}

If we expand the key of the second map we get the following:

{% highlight ocaml %}
map<tree<int>, map<tree<int>, T>> =
  map<int, map<tree<int>, map<tree<int>, map<tree<int>, T>>>
{% endhighlight %}

It gets pretty involved very quickly but because we traverse these types recursively, the implementation is still simple. Let's see how to implement these in OCaml. The type of a map over an int tree can be defined as follows:

{% highlight ocaml %}
open Map
module IntMap = Map.Make(Int64)

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
type key = IntMap.key tree

type 'a trie = Trie of 'a option * 'a trie trie IntMap.t

{% endhighlight %}

Note that the outermost map is a regular `IntMap`, which uses the root element of the map, a scalar, as key.

The search function takes a tree representing the key, and the map. The base case is when the tree is empty, when we're just past a leaf. The recursive case consists in obtaining the maps in order, first using the root element, then using the left tree and finally using the right tree:

{% highlight ocaml %}
let rec find: 'a. key -> 'a trie -> 'a =
  fun tree trie -> match (tree, trie) with
    | (Empty, Trie (None, children)) -> raise Not_found
    | (Empty, Trie (Some value, children)) -> value
    | (Node (root, left, right), Trie (_, children)) ->
      let trieForRoot = M.find root children in
      let trieForLeft = find left trieForRoot in
      find right trieForLeft
;;

{% endhighlight %}

Note that the recursive call is non-uniform, so we need explicit annotations, as we [discussed previously]({{site.url}}/blog/2017/10/02/polymorphic-recursion-in-ocaml.html).

The insertion is similar, expect that when we fail to find a key in any of the maps, we need to first insert it with an empty element before recursing.

Because the `Map.find()` implementation throws exceptions when a key doesn't exist, we can wrap the call with a `try` and if an exception occurs, we can insert the missing key (alternatively we could use `Map.mem()`).

{% highlight ocaml %}
let rec insert: 'a. key -> 'a -> 'a trie -> 'a trie =
  fun tree value trie -> match (tree, trie) with
    | (Empty, Trie (_, children)) -> Trie (Some value, children)
    | (Node (root, left, right), Trie (trieValue, children)) ->
      let trieForRoot = try
        M.find root children
      with
        Not_found -> empty
      in
      let trieForLeft = try
        find left trieForRoot
      with
        Not_found -> empty
      in
      let newTrieForLeft = insert right value trieForLeft in
      let newTrieForRoot = insert left newTrieForLeft trieForRoot in
      let newChildren = M.add root newTrieForRoot children in
      Trie (trieValue, newChildren)

{% endhighlight %}


**Maps over products and sums**



There are two ways a structure can branch out recursively: through *combination* or through *alternative*. For a *combination*, refer to our definition of `Node`:

`Node of 'a * 'a tree * 'a tree`

In theory, any combination of values for `'a`, `'a tree`, `'a tree` are possible values for a Node. The `*` in between the components in fact represent the cartesian product operator. This is also known as **product**.

For *alternative*, the type of the tree itself can be either Empty or Node:

{% highlight ocaml %}
type 'a tree = Empty | Node of (...)
{% endhighlight %}

In this case, the valid values for a tree is the sum of values of each alternative. Hence, this is also known as **sum**.

We can generalize the map with keys of any types by looking at their definition. If it's a product, we end up with nested maps. For example, if

{% highlight ocaml %}
Tk = (Tk1 * Tk2)
{% endhighlight %}

then the map over `Tk` can be defined as

{% highlight ocaml %}
map<Tk, T> = map<Tk1, map<Tk2, T>>
{% endhighlight %}

In our example, this came the nested maps `'a trie trie IntMap.t`.

For sums, if the type is

{% highlight ocaml %}
Tk = Tk1 | Tk2
{% endhighlight %}

the map over `Tk` would end up as a product of maps:

{% highlight ocaml %}
map<Tk, T> = (map<Tk1, T> * map<Tk2, T>)
{% endhighlight %}

In our example, this came the product `('a option)` and `('a trie trie IntMap.t)`. option can be thought as a one-dimensional map.

### Conclusion

In this post we saw how a trie can be modeled as a map using the same principles as the ones we use to construct matrices, that is, two-dimensional maps (nested maps). We then generalized the string keys to trees, and implemented the insert/find functions in OCaml. I found it pretty hard to reason about these structures.

We then went a step further and saw how to construct maps based on the key structure. And we learned about product and sum of types when discussing recursive types.