---
layout: post
title: "Peano's Axioms in Lean"
tags: [lean]
excerpt_separator: <!--more-->
vanity: "2022-01-22-peano_axioms_lean"

---
{% include blog_vars.html %}


<figure class="image_float_left">
    <a href="https://commons.wikimedia.org/wiki/File:Giuseppe_Peano.jpg">
        <img src="{{resources_path}}/peano.jpeg" alt="Giuseppe Peano thumbnail" />
    </a>
</figure>

Giuseppe Peano was a Italian mathematician who also studied [historical linguistics](https://en.wikipedia.org/wiki/Historical_linguistics). Peano is known for a defining a set of axioms from which many properties about natural numbers can be derived. Peano's work was built on Hermann Grassmann, Charles Sanders Peirce and Richard Dedekind.

In this post we'll explore some of Giuseppe Peano's axioms and derive lemmas from them. The goal of this post is to provide a practical introduction to Lean without getting too specific on the details.

This post borrows heavily on Kevin Buzzard's Natural Number Game [2] which I highly recommend! It has been tested on Lean 3.38 (community fork). One of the easiest ways to play with Lean is using the [online editor](https://leanprover-community.github.io/lean-web-editor), to which you can paste the code from the post as we go along.

<!--more-->

## The Natural Numbers

The first thing we have to do is define the natural numbers.

{% highlight lean %}
namespace sandbox

inductive Nat
| zero : Nat
| succ (n : Nat) : Nat

open Nat

end sandbox
{% endhighlight %}

In Lean the syntax `inductive Nat` describe a **inductive type**, and according to [1]:

> Intuitively, an inductive type is built up from a specified list of constructors

If you're familiar with Haskell, this is similar to the concept of [recursive data types]({{blog}}/2011/09/25/haskell-algebraic-data-types.html).

To reference the constructors `zero` and `succ`, we need to qualify them with `Nat`: `Nat.zero` and `Nat.succ`. The `open Nat` line "imports" definitions from `Nat`, so we can refer to `zero` and `succ` directly.

In Lean you can create named scopes by wrapping the code within:

{% highlight lean %}
namespace my_name
-- code
end my_name
{% endhighlight %}


If you're familiar with C++, this is very similar to C++ namespaces. The reason to create a new scope here is that `Nat` is already defined and included (in the prelude library), so if we want to reuse that name it has to be inside a scope.

Alternatively we could name is something else, like `my_nat`.

## Peano Axioms


**Axiom 1.** *The constant 0 is a natural number.*

The first axiom states that the constant 0 is a natural number. This is implicit in the fact that `zero` is a constructor for `Nat`.

For convenience we can also associate `Nat.zero` with the symbol `0`:

{% highlight lean %}
instance : has_zero Nat := ⟨zero⟩
{% endhighlight %}

Note that `⟨zero⟩` is not parenthesis but angle brackets (unicodes: U+27E8, U+27E9)! The `has_zero` is a known as a typeclass, which in OOP roughly maps to an *interface* with the method `has_zero` that must be implemented by classes, `Nat` in this case.

If you're familiar with Haskell, you might have seen [typeclasses]({{blog}}/2011/12/04/haskell-typeclasses.html) before.

### Equality Relations

This set of axioms establish the properties of the $=$ operator. According to Wikipedia [2]:

> Since they are logically valid in first-order logic with equality, they are not considered to be part of "the Peano axioms" in modern treatments.

In fact, Lean incorporate these axioms in its $=$ operator but we'll discuss them for completeness.

**Axiom 2.** *For every natural number $x$, $x = x$.*

Lean already has the so-called tactic [4] `refl` which is short for reflexivity, so we don't need to define this explicitly, but this is how we could have done it:

{% highlight lean %}
axiom reflexivity (n : Nat) : n = n
{% endhighlight %}


**Axiom 3.** *For all natural numbers $x$ and $y$, if $x = y$, then $y = x$.*

Lean has a tactic called `symmetry` that encodes this axiom. This is one way we can express it otherwise:

{% highlight lean %}
axiom symmetry (n m: Nat) : n = m <-> m = n
{% endhighlight %}

Here the `<->` represents "if and only if".

**Axiom 4.** *For all natural numbers $x$, $y$ and $z$, if $x = y$ and $y = z$, then $x = z$.*

The corresponding tactic in Lean is `transitivity`, which we can define as:

{% highlight lean %}
axiom trans (a b c: Nat) : a = b ∧ b = c -> a = c
{% endhighlight %}

Here the `∧` represents *and*. The `->` means "implies".

**Axiom 5.** *For all $a$ and $b$, if $b$ is a natural number and $a = b$, then $a$ is also a natural number.*

I don't really know how to express this as a Lean axiom, but my hunch is that Lean's type system implicitly incorporates this since both sides of `=` must have the same type. For example, if we try to write a axiom starting from instances of different types being equal:

{% highlight lean %}
inductive Unnat

axiom nope (a: Nat) (b: Unnat) : a = b
{% endhighlight %}

Lean will throw an error:

{% highlight text %}
type mismatch at application
  a = b
term
  b
has type
  Unnat
but is expected to have type
  Nat
{% endhighlight %}

### Arithmetical Properties

**Axiom 6.** *For every natural number $n$, $S(n)$ is a natural number.*

Like *Axiom 1*, this is implicit in the fact that `succ (n : Nat) : Nat` is a constructor for `Nat`.

**Axiom 7.** *For all natural numbers $m$ and $n$, $m = n$ if and only if $S(m) = S(n)$.*

This says that a natural number cannot have multiple successors nor can it be the successor of multiple numbers.

{% highlight lean %}
axiom injection (m n: Nat) :  m = n <-> succ(m) = succ(n)
{% endhighlight %}

**Axiom 8.** *For every natural number $n$, $S(n) = 0$ is false.*

This means that 0 is not the successor of any other number or $S(n) \neq 0$.

{% highlight lean %}
axiom non_zero_succ (n : Nat) : succ(n) = 0 -> false
{% endhighlight %}

Recall that if we have an implication $P \implies Q$, for it to be valid either $P$ must be false or both $P$ and $Q$ must be true. Thus if we "hard-code" $Q$ as false like above, $P$ is also false and it must be that $succ(n) \neq 0$.

**Axiom 9.** *Let $K$ be a set such that: $0 \in K$ and for every natural number $n$, if $n \in K$ then $S(n) \in K$. Then $K$ contains every natural number.*

The axioms up to *Axiom 8* only described things that *are* naturals numbers but did not say anything about things that *are not*.

For example, if we take the set of numbers $\mathbb{N} = \curly{0, 1, \cdots}$ and union with the set $\curly{0.5, 1.5, 2.5, \cdots}$, we'll see that they satisfy all the axioms above, including *Axiom 6*, for which we can think of two lines of succession, one starting from 0 and one starting from 0.5 and both satisfy .

If we include *Axiom 9* however, we'll see that $\mathbb{N}$ is a possible candidate for $K$, and thus $\mathbb{N}$ contains all natural numbers, meaning that $\curly{0.5, 1.5, 2.5, \cdots}$ are not in it.

Maybe a more intuitive way to phrase this axiom is that the set of natural numbers is the **smallest** set such that its elements satisfy Axioms 1-8. But this relies on being able to compare sizes of infinite sets which doesn't seem like a good idea for an axiom.


### Addition Axioms

We can define the `add` as a recursive function:

{% highlight lean %}
def add : Nat → Nat → Nat
| m 0 := m
| m (succ n) := succ (m + n)
{% endhighlight %}

Note how we only use the available constructors from `Nat` and rely on pattern matching to "extract" the `n` from `(succ n)`. For convenience, we can implement the `has_add` typeclass so the operator `+` maps to `add`.

{% highlight lean %}
instance : has_add Nat := ⟨add⟩
{% endhighlight %}

We need to encode the definitions in `add` as axioms so that we can explicitly use them when writing proofs.

**Axiom 10.** *For every natural number $n$, $n + 0 = n$.*

In other words, it says $0$ is the identity element for sum. But it's worth noting this is only when $0$ is on the right-hand side of $+$. We haven't proved commutativity yet. This encodes the first constructor of `add`.

{% highlight lean %}
axiom add_zero (n : Nat) : n + 0 = n
{% endhighlight %}

**Axiom 11.** *For all natural numbers $m$ and $n$, $m + S(n) = S(m + n)$.*

This seems like a useless rewrite at first but because we're "extracting" $n$ from $S(n)$, we can rely on the inductive nature of `Nat` and assume that function `m + n` is defined. This encodes the second constructor of `add`.

{% highlight lean %}
axiom add_succ (m n: Nat) : m + succ n = succ (m + n)
{% endhighlight %}

We're not finally able to write out first Lemma!

**Lemma 1.** *For all natural numbers $a, b$ and $c$, $(a + b) + c = a + (b + c)$.*

In other words, that addition is *associative*. We can prove this by induction on $c$.

{% highlight lean %}
axiom zero_is_nat : zero = 0

lemma add_assoc (a b c : Nat) : (a + b) + c = a + (b + c) :=
begin
  induction c with d hd,

  -- base
  rw zero_is_nat,
  repeat {rw add_zero},

  -- inductive step
  repeat {rw add_succ},
  rw hd,
end
{% endhighlight %}

The lemma looks a lot like a `axiom`, but this time we need to prove it by providing the "implementation" to the right of the `:=` operator.

The general idea of proving a lemma consists in achieving a *goal*. We initially start with a single goal which is to prove the original lemma but we can use constructs known as tactics to help us which might end up splitting the goal into multiple ones as we'll see next.

Let's analyze some of the lines. Ignore `axiom zero_is_nat` for now, we'll get back to it soon.

First, consider `induction c with d hd`. This will split the lemma into two goals: one where `c` is replaced with `zero` (base) and another where `c` is replaced with `succ(d)` but we'll have the inductive hypothesis `hd` which we assume is true for `a, b, d`, that is `(a + b) + d = a + (b + d)`.

Let's consider the base case. We need to show that `(a + b) + zero = a + (b + zero)`. The unfortunate thing is that the `induction` tactic doesn't seem to understand that `Nat.zero = 0` even though we implemented `has_zero` [3], so we need to define the `zero_is_nat` axiom to make this explicit.

The `rw` is a short for *rewrite* and the way it works is as follows:

* You give an axiom/lemma that states an equality of the form `expr1 = expr2`.
* It will try to find the first expression that matches `expr1`
* It will replace that expression everywhere in the current state.

In our example, we start with `(a + b) + zero = a + (b + zero)`. By applying `rw zero_is_nat` we get `(a + b) + 0 = a + (b + 0)`. Note that it replaced **both** `zero`s with `0`, because they're the same expression.

Now we rewrite using `add_zero`. If we do `rw add_zero` we'll not have `(a + b) = a + (b + 0)`. Note that it's smart to match `(a + b) + 0` because it knows `(a + b)` is `Nat`. Note also that it didn't replace `(b + 0)` because it's a different expression than the first match `(a + b) + 0`.

This subtle behavior is confusing, but [this answer](https://leanprover.zulipchat.com/#narrow/stream/113489-new-members/topic/.E2.9C.94.20rw.20tactic/near/268984637) clarifies things.

We need to then apply `rw add_zero` again. There's a shorthand for "apply X while there's a match", which is `repeat {X}`, and it is what we use in: `repeat {rw add_zero}`. This will get us to `a + b = a + b` and since equality is reflexive, Lean will consider this proved.

Now to the induction step. We have `(a + b) + succ(d) = a + (b + succ(d))`. By applying `rw add_succ` we get `succ(a + b + d) = a + (b + succ(d))` and applying once more we get `succ(a + b + d) = a + (succ(b + d))` and then `succ((a + b) + d) = succ(a + (b + d))`.

The `(a + b) + d` is in the form of our inductive hypothesis so we can use it `rw hd` to obtain `succ(a + (b + d)) = succ(a + (b + d))`, which proves the second goal.


### Multiplication Axioms

We can define the `mul` as a recursive function:

{% highlight lean %}
def mul : Nat → Nat → Nat
| m 0 := 0
| m (succ n) := m + (mul m n)

{% endhighlight %}

For convenience, we can implement the `has_mul` typeclass so the operator `*` maps to `mul`.

{% highlight lean %}
instance : has_mul Nat := ⟨mul⟩
{% endhighlight %}

**Axiom 12.** *For every natural number $n$, $n * 0 = 0$.*

It's worth noting this is only when $0$ is on the right-hand side of $+$. We haven't proved commutativity yet. This encodes the first constructor of `mul`.

{% highlight lean %}
axiom mul_zero (m : Nat) : m * 0 = 0
{% endhighlight %}

**Axiom 13.** *For all natural numbers $m$ and $n$, $m S(n) = m + mn$.*

As it was the case with `add` we're "extracting" $n$ from $S(n)$, so we can rely on the inductive nature of `Nat` and assume that function `mn` is defined. This encodes the second constructor of `mul`.

{% highlight lean %}
axiom mul_succ (m n: Nat) : m * (succ n) = m + m * n
{% endhighlight %}

We won't overextend the post with other proofs besides that of *Lemma 1*, but with these axioms it's possible to show for example that addition is commutative and multiplication is associative and commutative.

The code is available on [Github](({{github}}/peano.lean)).

## Conclusion

In this post we covered basic aspects of the Lean language and revisited the Peano axioms.

My initial plan was to write this in Lean 4, which is in active development by Microsoft. I started reading the [tutorial](https://leanprover.github.io/theorem_proving_in_lean4/propositions_and_proofs.html) but got lost quickly and wasn't finding the abstract examples intuitive.

I decided to try *The Natural Number Game* [4] which is a much more friendly introduction and suitable for people who are not familiar with formal methods. There are a couple of quirks from the language that the game hides from us to make it easier, and I only ran into them while trying to write a standalone version.

*The Natural Number Game* uses Lean 3.38 which is incompatible with Lean 4 as I quickly learned. It seems like the community fork is much more active and there are a lot more resources so I'll stick with it for now.

Overall I was very happy to finally write some proofs using a theorem prover. I had tried Agda before but I ended up dropping the ball for some reason. I keep hearing about Lean and the Xena project and that encourage me to try theorem provers again. I'm really hoping to stick with learning more about it this time.

## References

* [[1](https://leanprover.github.io/theorem_proving_in_lean/inductive_types.html)] Theorem Proving in Lean - 7. Inductive Types
* [[2](https://en.wikipedia.org/wiki/Peano_axioms#Arithmetic)] Wikipedia: Peano axioms
* [[3](https://leanprover.zulipchat.com/#narrow/stream/113489-new-members/topic/has_zero/)] Zulip Lean Discussion
* [[4](https://leanprover.github.io/theorem_proving_in_lean/tactics.html)] Theorem Proving in Lean - 5. Tactics
