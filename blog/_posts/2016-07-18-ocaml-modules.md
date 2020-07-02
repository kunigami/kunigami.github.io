---
layout: post
title: "OCaml Modules"
tags: [ocaml]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2016-07-18-ocaml-modules/8888_06_camel.gif" alt="camel" />
</figure>

One prevalent syntax in some OCaml code I've encountered is about modules, so I decided to study them a little bit to be able to better understand the code I've been reading.

This post is based on Chapters 4 and 9 from *Real World OCaml* [1, 2], which we started in the [last post]({{site.url}}/blog/2016/06/27/exploring-ocaml.html).

### Defining modules

By default, a file defines a module. Module names are derived from filenames and is always capitalized (even if the filename is not). Let's define a toy example:

`myModule.ml`:

{% highlight ocaml %}

let a = 1;;

{% endhighlight %}

`main.ml`:

{% highlight ocaml %}

Printf.printf "%d\n" MyModule.a;;

{% endhighlight %}

We can now compile using the built-in `ocamlc` (make sure to follow the setup [here]({{site.url}}/blog/2016/06/27/exploring-ocaml.html)):

{% highlight ocaml %}

ocamlc myModule.ml main.ml

{% endhighlight %}

Note that the order is important. Since `main.ml` depends on `myModule.ml`, it has to be included first. In case the files do not live in the same directory, we can use the `-I` option. For example, if `myModule` was in `another_dir`, we could compile using:

{% highlight ocaml %}

ocamlc -I another_dir/ another_dir/myModule.ml main.ml

{% endhighlight %}

A module has two parts: the definition and the signature. A module defined by a file `filename.ml` can be constrained by a signature in a file called `filename.mli`. The definition (`mli`) should be included in the compiling step and appear before the definition. For example, we could have

`myModule.mli`:

{% highlight ocaml %}

val a : int;;

{% endhighlight %}

and compile using:

{% highlight ocaml %}

ocamlc myModule.mli myModule.ml main.ml

{% endhighlight %}

**Submodules**



It's possible to define multiple modules inside a file through submodules. We can add this to `myModule.ml`:

{% highlight ocaml %}

module MySubmodule : sig
  val inc : int -> int
end = struct
  let inc x = x + 1
end

{% endhighlight %}

and in `main.ml`:

{% highlight ocaml %}

Printf.printf "%d\n" (MyModule.MySubmodule.inc 1);;

{% endhighlight %}

Note that we still need to provide the module name defined by the filename. The first part of the definition is the type signature of the module and the second the definition. The general syntax is:

{% highlight ocaml %}

module <module name> : sig
  type <abstract type name>
  val <name> : <type signature>
end = struct
  type <abstract type name> = <concrete type>
  let <name> <definition>

{% endhighlight %}

Alternatively, we can separate the type definition from the implementation. In that case, we create a separate file with extension `.mli` containing the interface

`myModule.mli`:

{% highlight ocaml %}

module MySubmodule : sig
  val inc : int -> int
end

{% endhighlight %}

and in `myModule.ml`:

{% highlight ocaml %}

module MySubmodule = struct
  let inc x = x + 1
end

{% endhighlight %}

In general, the syntax for the signature is:

{% highlight ocaml %}

module type <module type name> : sig
  type <abstract type name>
  val <name> : <type signature>
end

{% endhighlight %}

and for the module definition is:

{% highlight ocaml %}

module <module name> : <module signature> = struct
  type <abstract type name> = <concrete type>
  let <name> <definition>

{% endhighlight %}

### Including modules

Modules are made available when linking during compile time, but if we want to use a function from a module, we still need to qualify it. We can alternatively include them explicitly to avoid qualifying module names (similar to use namespace in C++).

{% highlight ocaml %}

open Module
foo

{% endhighlight %}

Instead of:

{% highlight ocaml %}

Module.foo

{% endhighlight %}

We can also invoke `open` inline:

{% highlight ocaml %}

# let average x y =
    let open Int64 in
    x + y / of_int 2;;
val average : int64 -> int64 -> int64 = <fun>

{% endhighlight %}

or alias the module to a shorter name with the `let module` construct:

{% highlight ocaml %}

let print_median m =
  let module C = Counter in
  match m with
  | C.Median string -> printf "True median:\n   %s\n" string
  | C.Before_and_after (before, after) ->
    printf "Before and after median:\n   %s\n   %s\n" before after

{% endhighlight %}

### Functors

Functors are functions that transform modules, so it's a function from a module to a module. A basic example is provided in [2]. First we define a toy signature signature:

{% highlight ocaml %}

module type X_int = sig
  val x : int
end;;

{% endhighlight %}

Then, we define a functor, which we call `Increment`:

{% highlight ocaml %}

module Increment (M : X_int) = struct
    let y = M.x + 1
end;;

{% endhighlight %}

What tells us this is a function is the extra parameter the module takes `(M: X_int)`. In here, `M` is the name we give to the module and `X_int` is its interface. Since the interface tells us `M` has the `x` value, we can access it within our function. Note that `Increment` acts like a function, taking `M` (module) as parameter and returning another module, defined by the `struct` block. In this case, the type signature is different because the returned module has `y` instead of `x`. We can force the returned type of a function by adding a constraint:

{% highlight ocaml %}

module Increment (M : X_int) : X_int = struct
    let x = M.x + 1
end;;

{% endhighlight %}

Now, if we try to use `y`, we can a compilation error. To fix it, we just change `y` to `x`.

Functors cannot be used by themselves. They're useful for creating modules out of existing modules. For an example, imagine we have a module implementing `X_int`:

{% highlight ocaml %}

module Three = struct
  let x = 3
end;;

{% endhighlight %}

We can create a new module `Four`, by transforming our `Three` module:

{% highlight ocaml %}

module Four = Increment(Three);;

// Testing the modules
Printf.printf "%d\n" Three.x;; // 3
Printf.printf "%d\n" Four.x;;  // 4

{% endhighlight %}

### "Abstract" functors

In [2], the authors provide an example of an `MakeInterval` module, in which there's a dependent type. First it creates a `Comparable` signature:

{% highlight ocaml %}

module type Comparable = sig
  type t
  val compare : t -> t -> int
end;;

{% endhighlight %}

to make it shorter (and less "real world"), I've created a simpler version here, `MakeElement`:

{% highlight ocaml %}

module MakeElement (InputModule : Comparable) = struct
  type t = Element of InputModule.t
  let create x = Element x
end;;

{% endhighlight %}

we can then create a module:

{% highlight ocaml %}

module IntElement = MakeElement(Int);;

{% endhighlight %}

The above works because `Int` satisfies the constraint defined by the `Comparable` module signature. The authors make a point here that sticking to standard conventions can improve reuse such as cases like this. We can finally use

{% highlight ocaml %}

# let e = IntElement.create 10;;
val e : IntElement.t = IntElement.Element 10

{% endhighlight %}

The authors argue that the `IntElement` exposes implementation details because `Element` is "exposed":

{% highlight ocaml %}

# let e2 = IntElement.Element 10;;
val e2 : IntElement.t = IntElement.Element 10

{% endhighlight %}

One solution is to constrain the return type of the functor and not expose `Element` in the signature. The signature would look like:

{% highlight ocaml %}

module type ElementInterface = sig
  type t
  type element
  val create : element -> t
end;;

{% endhighlight %}

and the functor would look like:

{% highlight ocaml %}

module MakeElement (InputModule : Comparable): ElementInterface = struct
  type t = Element of InputModule.t
  let create x = Element x
end;;

{% endhighlight %}

The problem is the type element is not bound to anything, so we have to explicitly do it when defining the functor. The construct is the following

{% highlight ocaml %}

module MakeElement (InputModule : Comparable):
  (ElementInterface with type element = InputModule.t) = struct
  type t = Element of InputModule.t
  type element = InputModule.t
  let create x = Element x
end;;

{% endhighlight %}

now `MakeElement` return a module with interface `ElementInterface` which doesn't expose `Element`.

### Destructive Substitution

One problem with the approach above is the redundant binding of type `element`. One slightly different syntax removes that requirement:

{% highlight ocaml %}

module MakeElement (InputModule : Comparable):
  (ElementInterface with type element := InputModule.t) =
struct
  type t = Element of InputModule.t
  let create x = Element x
end;;

{% endhighlight %}

We basically changed from `=` to `:=`, which is called *destructive substitution*.

### Multiple Interfaces

It's possible to "extend" more than one module signature when creating a new one. For example:

{% highlight ocaml %}

module type MyFunctor = sig
   include MyModuleInterface
   include MyModuleInterface2 with type t := t
end;;

{% endhighlight %}

### Conclusion

Modules seems a very powerful concept in Ocaml. Besides organizing files, modules can act as functions and can model concepts from Object Oriented Programming like classes and interfaces.

I've been following a different study strategy while learning Ocaml. I try to read some real world code and when I get stuck understand a syntax or a pattern, I can search for them in a book. This makes it more interesting than reading a book cover to cover.

### References

* [[1](https://realworldocaml.org/v1/en/html/files-modules-and-programs.html)] Real World OCaml – Chapter 4. Files, Modules, and Programs
* [[2](https://realworldocaml.org/v1/en/html/functors.html)] Real World OCaml – Chapter 9. Functors
* [[3](https://ocaml.org/learn/tutorials/compiling_ocaml_projects.html)] Compiling OCaml projects
