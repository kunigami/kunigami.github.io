---
layout: post
title: "Exploring OCaml"
tags: [ocaml]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2016-06-27-exploring-ocaml/8888_06_camel.gif" alt="camel" />
</figure>

I've been learning the basics of OCaml in order to be able to follow the examples from *Purely Functional Data Structures*, from Chris Okasaki.

My background is that I have zero knowledge about OCaml, but having studied a bit of Haskell, I'm familiar with the main concepts in functional programming, so I'll try to focus on syntax + difference between the two languages. I'll probably skim through concepts from functional programming I've already covered previously on past [Haskell posts](https://kunigami.wordpress.com/category/programming/haskell/).

### Setup

I used to skip the setup from posts, but it might be useful to someone, especially with a similar environment, so I decided to include it here. This setup assumes MacOS and emacs.

**Installing**

Easily available on Brew:

{% highlight ocaml %}

$ brew install ocaml
$ brew install opam
$ ocaml

{% endhighlight %}

For other OS'es: [https://ocaml.org/docs/install.html](https://ocaml.org/docs/install.html)

**Emacs syntax highlighting**

Looks like tuareg is a popular mode for developing OCaml in emacs:

{% highlight ocaml %}

opam init
opam install tuareg

{% endhighlight %}

At the configuration file (`~/.emacs.d/init.el`) we can add:

{% highlight ocaml %}

(load "~/.opam/system/share/emacs/site-lisp/tuareg-site-file")

{% endhighlight %}

**OCaml CLI**

Typing ocaml in the terminal will bring up the CLI, but it's not very interactive, not having a good way to go back previous command or edit strings in the middle. I've [learned](http://stackoverflow.com/questions/11757239/how-to-repeat-last-command-in-ocaml-interpreter-shell) about this command line called `rlwrap` that implements this functionality. We can easily install it on mac:

{% highlight ocaml %}

brew install rlwrap

{% endhighlight %}

And invoke `ocaml` like this:

{% highlight ocaml %}

rlwrap ocaml

{% endhighlight %}

We can also add a simple alias to have these features as default:

{% highlight ocaml %}

alias ocaml='rlwrap ocaml'

{% endhighlight %}

### Language Basics

**Statements**

Statements boundaries are defined by `;;`. Example:

{% highlight ocaml %}

# 1 + 1;;
- : int = 2

{% endhighlight %}

This let us define multi-line expressions,

{% highlight ocaml %}

# 1 +
# 1
# ;;
- : int = 2

{% endhighlight %}

**Comments**

{% highlight ocaml %}

(* This is a single-line comment. *)

(* This is a
 * multi-line
 * comment.
 *)

{% endhighlight %}

**Load code from file**

It can become tedious to edit functions in the CLI. It's possible to execute the contents of a file:

{% highlight ocaml %}

> ocaml
# #use "my-file.ml";;

{% endhighlight %}


**Basic Types**

* int - 31 or 63-bits, depending on the platform one of the bits is used for internal memory management

When writing literal number values, the underscore character is ignored (as long as it's not the leading character). For example:

{% highlight ocaml %}

# 10_1___02__  + 1;;
- : int = 10103

{% endhighlight %}

This can be useful to define large numbers in a more user friendly way:

{% highlight ocaml %}

# let x = 12_434_934
val x : int = 12434934

{% endhighlight %}

* float - IEEE double-precision floating point

OCaml doesn't do explicit casts, especially between ints and floats. We have to cast using functions like int_of_float or float_of_int. Examples:

{% highlight ocaml %}

# 1 + 1;;
- : int = 2
# 1.3 +. 1.2;;
- : float 2.5
# 1 + 1.0;;
Error: This expression has type float but an expression was expected of type int
# 1 +. 1.
Error: This expression has type int but an expression was expected of type float
# 1 + (int_of_float 1.0)
- : int = 2
# (float_of_int 1) +. 1.
- : float 2.

{% endhighlight %}

Note that the `sum` operator for floats has an extra `.` character (`+.`)

* bool - true/false
* char - 8-bit ascii character
* string - more than a list of char, efficient internal representation

**Variables**

We can define variables by the use of `let`

{% highlight ocaml %}

# let a = 3 in
  let b = 4 in
  a + b;;
- : int = 3

{% endhighlight %}

This looks like imperative code at the first glance, but it's slightly different. `let <expr1> in <expr2>`. The `expr1` is only made available inside the scope of `expr2`. For example:

{% highlight ocaml %}

# let a = 3 in
  let b = 4 in
  a + b;;
- : int = 3
# a;;
Error: Unbound value a

{% endhighlight %}

Here the variable a was defined only for the expression:

{% highlight ocaml %}

  let b = 4 in
  a + b;;

{% endhighlight %}

When we terminated it with `;;` , `a` was out of scope. We can also bind multiple variables in the same expression, for example:

{% highlight ocaml %}

# let a = 3 and let b = 4 in
  a + b;;
- : int = 3

{% endhighlight %}

### Functions

**Defining Functions**

Example: A simple `sum` function

{% highlight ocaml %}

# let sum a b = a + b;;
val sum : int -> int -> int = <fun>
# sum 1 2
- : int = 3

{% endhighlight %}

Note how the type signature syntax is very similar to Haskell.

**Explicit function type signature**

Sometimes to avoid ambiguity, we might want to provide the types for the inputs/outputs of the function. For example, we might want to define a sum function only intended to be used by `int`s.

{% highlight ocaml %}

let intSum (a: int) (b: int) : int = a + b;;

{% endhighlight %}

**Lambda functions**

Denoted by the use of the `fun` construct, it's useful for passing simple functions as parameter (for example to a map over lists).

{% highlight ocaml %}

# let doubleValues ls =
  List.map (fun x -> 2*x) ls
;;
val doubleValues : int list -> int list = <fun>
# doubleValues [1; 2; 3];;
- : int list = [2; 4; 6]

{% endhighlight %}

**Recursive functions**

A function must be explicitly marked as recursive by add a `rec`, according to [2], due to technical reasons - mainly related to type inference.

Example: Computing the factorial of a natural number `n`:

{% highlight ocaml %}

# let rec factorial n =
  if n == 0 then 1
  else n * (factorial (n -1))
;;
val factorial : int -> int = <fun>
# factorial 10;;
- : int = 3628800

{% endhighlight %}

**Matching patterns**

Similar to Haskell, Ocaml has pattern match which we can use to decide which body of function to apply. For example, to invert a list we can do:

{% highlight ocaml %}

# let rec reverseList xs =
  match xs with
  | [] -> []
  | x :: xs -> (invertList xs) @ [x]
;;
# reverseList [1; 2; 3];;
- : int list = [3; 2; 1]

{% endhighlight %}

The `_` operator indicates all the non-matched cases. For example, we could rewrite the reverseList function as

{% highlight ocaml %}

# let rec reverseList xs =
  match xs with
  | x :: xs -> (invertList xs) @ [x]
  | _ -> []
;;
# reverseList [1; 2; 3];;
- : int list = [3; 2; 1]

{% endhighlight %}

**Labeled arguments**

We can prefix a function parameter with `~` to indicate it's a labeled (named) argument. Example:

{% highlight ocaml %}

# let div ~a ~b = float a /. float b;;
val div : a:int -> b:int -> float = <fun>
# div 10 2
- : float = 5.
# div ~b:10 ~a:2
- : float : 0.2

{% endhighlight %}

Note how the variable name shows up in the function's type signature. It's important because we may pass a function with labeled arguments to another function and it may make use of this fact (it's also useful for documentation).

If the variable name matches the named parameter, we don't need to repeat ourselves:

{% highlight ocaml %}

# let a = 10
val a : int = 10
# leb b = 2
val b : int = 2
# div ~b ~a

{% endhighlight %}

We can also apply currying using named arguments. For example, if we want generate a new function with the value b "bound", we can do

{% highlight ocaml %}

# let b = 2
val b : int = 2
# let half = div ~b;;
val half : a:int -> float = <fun>
# half ~a
- : float = 0.5

{% endhighlight %}

When currying, positional parameters (i.e. non-labeled arguments) are always bound before the labeled ones. For example:

{% highlight ocaml %}

# let makeList3 ~a ~b c = [a; b; c];;
val makeList3 : a:'a -> b:'a -> 'a -> 'a list = <fun>
# let makeList2 = makeList3 1
val makeList3 : a:'a -> b:'a -> 'a list = <fun>
# makeList2 2 3
- : int list = [2; 3; 1]

{% endhighlight %}

**Optional parameters**

Optional parameters are prefixed with `?` like `sep` in the example below:

{% highlight ocaml %}

# let concat ?sep x y =
  let sep = match sep with None -> "" | Some x -> x in
    x ^ sep ^ y
;;
val concat : ?sep:string -> string -> string -> string = <fun>

# concat "ab" "cd";;
- : string = "abbc"
# concat "ab" "bc" ~sep:",";;
- : string = "ab,cd"

{% endhighlight %}

The value coming from an optional parameter is either `None` or `Some x`. An optional parameter is also a labeled parameter.

In the example above, we use pattern matching to provide a default value to sep. There's a shorter syntax for this:

{% highlight ocaml %}

let concat ?(sep=" ") x y =
  x ^ sep ^ y
;;
val concat : ?sep:string -> string -> string -> string = <fun>

{% endhighlight %}

By providing a default value, the value in the sep variable won't be either None/Some, but the actual type expected, in this case a string.

It can be tricky to apply curry in the presence of optional arguments. [[2](https://realworldocaml.org)] discusses in detail the heuristics applied by the compiler in different scenarios.

### Conclusion

In this post we covered the very minimum to get our hands dirty with some OCaml code and learn the basic syntax. I don't plan to study [2] for now. I'm mostly interested in learning enough to follow along Purely Functional Data Structures.

Some first impressions: the ocaml CLI is pretty limited, but thanks to rlwrap it becomes manageable. Haskell is more elegant, Ocaml is more practical.

For the next post in the series I plan to study the most basic and simple data structure in functional programming: lists.

### References

* [[1](https://ocaml.org/learn/tutorials/basics.html)] OCaml.org - The Basics
* [[2](https://realworldocaml.org)] Real World OCaml - Chapter 2. Variables and Functions

