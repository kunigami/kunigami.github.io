---
layout: post
title: "An Introduction to Agda"
tags: [agda, artificial intelligence, emacs, haskell, logic, software]
---

<figure class="image_float_left">
    <a href="http://kunigami.files.wordpress.com/2031/10/martin-lof.png"><img src="{{site.url}}/resources/blog/2013-10-30-an-introduction-to-agda/2031_10_martin-lof.png" alt="Martin-Lof" /></a>
</figure>

Per Martin-Löf is a Swedish logician, philosopher and mathematician statistician. Since 1970 he has been mainly publishing on logic.

Martin-Löf has been active in developing intuitionistic type theory as a constructive foundation of mathematics and his work on type theory has influenced computer science.

He completed his PhD program under supervision of Andrei Kolgomorov and is currently Professor at Stockholm University, being a member of the Royal Swedish Academy of Sciences.

In this post we'll talk about assisted theorem proving, more specifically using a tool named Agda, which has it's syntax based on Haskell and the engine is based on the Per Martin-Löf's intuitionistic type theory.

We'll first talk about the basic concepts such as assisted theorem proving and dependent type theory and then present a very brief introduction to Agda based on a tutorial from Ulf Norell and James Chapman [3].



---



### Assisted Theorem Proving
We can classify theorem proving in the following categories:

1) Manual
2) Manual with machine support
3) Interactive
4) Automated

(1) is the oldest, most known and widely used method by mathematicians. (2) Since the introduction of computers, we now can have machines to verify syntax and make manual proofs less error prone. (3) We can also write proofs in a way the machine understands so that it can perform some logical deductions steps for us. (4) In the last extreme case, we can let the machine do all the job and find proofs by itself.

### Dependent Types

Basically speaking, dependent type is a type that depends on a value. In typed programming languages we can have a type like `vector T`, representing a vector containing elements of type `T`, but in a dependent type language we could also have `vector T n`, representing vectors with elements of type `T` and length `n`. We'll see an example for this type in the next section using Agda.



---



### Agda

Agda is a dependent type language based on intuitionistic type theory, introduced by Martin-Löf. It was first implemented by Catarina Coquand (who is married to Thierry Coquand, which inspired the Coq tool, another famous assisted proof tool). Later it was rewritten as Agda 2 by Ulf Norell, one of the authors of the tutorial we're going to follow.

**Installing**



The first thing we want to do before starting is to get Agda running. This can be done using the cabal package manager from Haskell:

{% highlight haskell %}

> cabal install Agda

{% endhighlight %}

On Mac, this will install some binaries to `~/Library/Haskell`. We want to use some of those binaries, so let's include in our bin path:

{% highlight haskell %}

export PATH="$HOME/Library/Haskell/bin:$PATH"

{% endhighlight %}

Agda has a very good integration with Emacs. On Mac we can make use of that integration in Aquamacs. To set up the right configuration into the `.emacs` file, we can now run:

{% highlight haskell %}

> agda-mode setup

{% endhighlight %}

**Main Concepts**



**Latex and Unicode.** Agda uses makes use of unicode characters to make the code more readable and accepts LaTeX syntax to define them. For example, we can input `\rightarrow` which will be transformed into the `→` character. Also there are shortcuts to avoid typing a lot (for example `\r` is expanded to `\rightarrow`)

**Delimiters.** Agda is very flexible in naming, by only considering very few characters as delimiter (including space, parenthesis and curly brackets). This means we can have names like `:<a*`, but then we have to be careful in adding space whenever we need to separate two variables.

**Modules and name convention.** Agda files have the extension `.agda`. For our tutorial,  let's create one called `basic.agda`. The first thing we want to do is to define the module, which is used to manage namespaces. By convention, the module name has to match the file name, so we write:

{% highlight haskell %}

module basic where

{% endhighlight %}

**Data Types.** Our basic working blocks will be types and we can define them by data types, which have syntax similar to data types in Haskell. For example, we can model the boolean type, which we call `Bool`. In the following code we have the type named `Bool` which is a subset of `Set` (pre-defined) and we provide two constructors, both being of the type `Bool`.

{% highlight haskell %}

data Bool : Set where
  true : Bool
  false : Bool

{% endhighlight %}

In the same manner we can define the set of natural numbers. Since it's impossible to explicitly define all value like we did for booleans, we do so by defining a function `suc` recursively, which takes a `Nat` and returns its successor, like in the following example:

{% highlight haskell %}

-- Natural numbers
data Nat : Set where
  zero : Nat
  suc  : Nat -> Nat

{% endhighlight %}

Note that we are saying the constructor `suc` represents a function that receives a `Nat` and return a `Nat`, but we are not providing the implementation for it.

So let's write some functions to work with the Nat type. More specifically, addition and multiplication.

**Functions.** In Agda, when we write a function surrounded by underscores, like `_+_`, we're defining a function that takes two arguments using infix notation. Let's define the addition operation:

{% highlight haskell %}

-- Addition
_+_ : Nat -> Nat -> Nat
zero  + m = m
suc n + m = suc (n + m)

{% endhighlight %}

Note how we define everything in terms of the basic elements from Nat, that is, `zero` and `suc`, we have no notion of values. This is important because if we want to encode proofs as programs, we have to be generic and 'value agnostic'. In this function we pattern match on different values of the first argument: If it's `zero`, we are in the base case. Otherwise, the first argument must be a successor of some another value `n`, so we do recursion on `n` until it hit the base case.

Multiplication is analogous, but here we use the `_+_` operator to define the second matched pattern:

{% highlight haskell %}

-- Multiplication
_*_ : Nat -> Nat -> Nat
zero  * m = zero
suc n * m = m + n * m

{% endhighlight %}

The code above will cause an error because Agda requires strictly different priorities for different operators.

We can attribute some random values to these operators using `infixl`, which takes a priority value and a function. The higher the value, the higher the priority. Since we want multiplication to have higher priority than addition we can assign a higher number for it, for example:

{% highlight haskell %}

-- Defining priority
infixl 6 _*_
infixl 4 _+_

{% endhighlight %}

**Parametrized types.** We can define a type that depends on other types, like parametrized types in Haskell or templates in C++. In this tutorial, we'll create a list of elements of a given type `A`, as in the following data type:

{% highlight haskell %}

-- Definition of a list with parametrized type
data List (A : Set) : Set where
  -- empty operator
  [] : List A
  -- append operator
  _::_ : A -> List A -> List A

{% endhighlight %}

Let's write the map function over `List`. Differently from Haskell, Agda doesn't perform type inference by default, so we need to explicitly provide the types when defining/calling a function that depends on other types. In the following example, the types `A` and `B` must be provided as parameters:

{% highlight haskell %}

map1 : (A : Set) -> (B : Set) -> (A -> B) -> List A -> List B
map1 A B f [] = []
map1 A B f (x :: xs) = f x :: map1 A B f xs

{% endhighlight %}

We have the following syntax sugar to combine two or more types that are subset of the same type:

`(A : Set) -> (B : Set)` is equivalent to `(A B : Set)`

**Implicit Types.** We have the option to make types implicit and let Agda figure out the right one. The syntax is to use curly brackets instead of parenthesis:

{% highlight haskell %}

map2 : {A B : Set} -> (A -> B) -> List A -> List B
map2 f [] = []
map2 f (x :: xs) = f x :: map2 f xs

{% endhighlight %}

**Dependent Types.** So far the types and operations we discussed so far don't seem to show the real power of dependent types. So let's define a new version of `List`, enhanced with the length of the list, which we'll then call `Vector`:

{% highlight haskell %}

data Vec (A : Set) : Nat -> Set where
  []   : Vec A zero
  -- Make the length type implicit
  _::_ : {n : Nat} -> A -> Vec A n -> Vec A (suc n)

{% endhighlight %}

Here we have two parameters when defining the data type, `Nat -> Set`. Notice also how when defining the list recursively we update the value `n` properly.

One nice thing about this is that in defining a head function, we don't need to handle the empty case as long as we restrict the type of non-empty lists:

{% highlight haskell %}

-- Doesn't need to check for empty lists. Restriction is encoded in
-- the function type!
head : {A : Set}{n : Nat} -> Vec A (suc n) -> A
head (x :: xs) = x

{% endhighlight %}

**Proofs as types.** We are now going to write an example on how to use dependent types to encode proofs ( as types.

Let's start by defining data types to represent the two possible values of a boolean. Note that we don't specify constructors here.

{% highlight haskell %}

data False : Set where
data True  : Set where

{% endhighlight %}

Now we define a function to convert a boolean values into these types:

{% highlight haskell %}

isTrue : Bool -> Set
isTrue true = True
isTrue false = False

{% endhighlight %}

Let's, for instance, define a function to be use as predicate, in this case, the `<` operator:

{% highlight haskell %}

-- Definition of the < operator
_<_ : Nat -> Nat -> Bool
_ < zero = false
zero  < suc n = true
suc m < suc n = m < n

{% endhighlight %}

We will need the following function, that returns the length of a list, in our final example:

{% highlight haskell %}

-- Definition of the length function
length : {A : Set} -> List A -> Nat
length [] = zero
length (x :: xs) = suc (length xs)

{% endhighlight %}

Finally, we can now write a sample function that encodes a predicate as a type. In the following example, the function `lookup` accepts one member of the family of predicates in which `n` less than the size of the list is a true statement.

{% highlight haskell %}

-- Function that looks up the i-th element in a list
-- param 1: list
-- param 2: look up index
-- param 3: proof
lookup : {A : Set}(xs : List A)(n : Nat) -> isTrue (n < length xs) -> A
lookup [] n ()
lookup (x :: xs) zero p = x
lookup (x :: xs) (suc n) p = lookup xs n p

{% endhighlight %}

This concludes our brief introduction to Agda syntax and dependent types. In following posts we'll learn more about writing proofs and have Agda to perform logical derivations for us.

### Conclusion

I have close to zero knowledge in formal systems. I've been exposed briefly to it by reading parts of Gödel, Escher and Bach and the subject sounds very interesting. I based most of the content of this post in a few introductory tutorials [3-6], but I still didn't get a good grasp of the power of systems like Agda.

I was initially interested in learn Coq but due to some problems in setting it up, I ended up looking for alternatives and found Agda. It seems that Coq is based on Ocaml and Agda is based on Haskell and since I've been focused in learning Haskell, it was a pleasant surprise. Agda has also has a good integration with Emacs, which is my favorite editor, so this was a plus.

### References
* [[1](http://en.wikipedia.org/wiki/Per_Martin-L%C3%B6f)] 
 Per Martin-Löf - Wikipedia
* [[2](http://en.wikipedia.org/wiki/Proof_assistant#Comparison_of_systems)] 
 Proof Assistant - Comparison of Systems - Wikipedia
* [[3](http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf)] 
 Dependently Typed Programming in Agda - Ulf Norell and James Chapman
* [[4](http://www.cs.swan.ac.uk/~csetzer/lectures/intertheo/07/intertheodraft0.pdf)] 
 Interactive Theorem Proving, Lecture Notes 0 - Anton Setzer
* [[5](http://www.cs.swan.ac.uk/~csetzer/lectures/intertheo/07/intertheodraft1.pdf)] 
 Interactive Theorem Proving, Lecture Notes 1 - Anton Setzer
* [[6](http://www.cs.swan.ac.uk/~csetzer/lectures/intertheo/07/intertheodraft2.pdf)] 
 Interactive Theorem Proving, Lecture Notes 2 - Anton Setzer