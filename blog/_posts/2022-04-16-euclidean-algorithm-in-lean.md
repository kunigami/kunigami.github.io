---
layout: post
title: "The Euclidean Algorithm in Lean"
tags: [lean, number theory]
excerpt_separator: <!--more-->
vanity: "2022-04-16-euclidean-algorithm-in-lean"

---
{% include blog_vars.html %}


<figure class="image_float_left">
    <a href="https://commons.wikimedia.org/wiki/File:Portrait_of_Euclid_Wellcome_L0019815.jpg">
        <img src="{{resources_path}}/euclid.jpeg" alt="Euclid thumbnail" />
    </a>
</figure>

Euclid of Alexandria was a Greek mathematician and is considered the father of Geometry. He is thought to have lived in Alexandria, Egypt, during the Ptolemaic reign [1].

His book *Elements* contains a collection of definitions, theorems and proofs and known for its mathematical rigour. Among them is an algorithm for computing the greatest common divisor (GCD) of two natural numbers, simply known as the *Euclidean algorithm*.

In this post we'll use Lean to prove a property of GCD and is the basis of the Euclidean algorithm. We'll provide a proof using our own version of the natural numbers for educational purposes, extending the work started in [Peano's Axioms]({{blog}}/2022/01/26/peano_axioms_lean.html).

<!--more-->

## Natural Numbers

Let's start by defining our own version of the natural numbers, `Nat`, as in [2]. The main addition here is the definition of the symbol `1`, which is useful for multiplication.

{% highlight lean %}
namespace sandbox

inductive Nat
| zero : Nat
| succ (n : Nat) : Nat

open Nat

instance : has_zero Nat := ⟨zero⟩
axiom zero_is_nat : zero = 0

instance : has_one Nat := ⟨succ zero⟩
axiom one_is_nat : succ zero = 1
{% endhighlight %}

## Basic Operations

We'll rely on addition, subtraction, multiplication and also the divisibility operations. Let's define them now.

### Addition

{% highlight lean %}
def add : Nat → Nat → Nat
| m 0 := m
| m (succ n) := succ (add m n)

instance : has_add Nat := ⟨add⟩
axiom add_zero (m: Nat) : m + 0 = m
axiom add_succ (m n: Nat) : m + succ n = succ (m + n)
{% endhighlight %}

### Subtraction

Subtraction is an interesting operation, because natural numbers do not include negative numbers. The subtraction defined below is a truncated subtraction meaning that if $a \le b$, then $a - b = 0$ [7].

{% highlight lean %}
def sub : Nat → Nat → Nat
  | (succ a) (succ b) := sub a b
  | a b := a

instance : has_sub Nat := ⟨sub⟩
{% endhighlight %}

We can derive several lemmas from this definition, but since they're not the focus of our post, let's treat them as axioms.

{% highlight lean %}
axiom sub_zero (a: Nat) : a - 0 = a
axiom sub_eq (a: Nat) : a - a = 0
axiom sub_assoc (a b c: Nat): a - b + c = a - (b - c)
{% endhighlight %}

### Multiplication

{% highlight lean %}
def mul : Nat → Nat → Nat
| m 0 := 0
| m (succ n) := m + (mul m n)

instance : has_mul Nat := ⟨mul⟩

axiom mul_zero (n: Nat) : n * 0 = 0
axiom mul_succ (n m: Nat): m * (succ n) = m + (m * n)
{% endhighlight %}

Similar to above, these are all lemmas that can be derived, but we'll treat them as axioms here.

{% highlight lean %}
axiom one_mul (n: Nat) : 1 * n = n

axiom mul_dist_add (a b c: Nat) : a * ( b + c) = a * b + a * c
axiom mul_dist_sub (a b c: Nat) : a * ( b - c) = a * b - a * c
axiom mul_assoc (a b c: Nat) : a * ( b * c) = a * b * c
axiom mul_comm (a b: Nat) : a * b = b * a
{% endhighlight %}


### Divisibility

Divisibility is a function that determines whether a number $a$ divides a number $b$ or more precisely if there exists $c \in \mathbb{N}$ such that $b = a * c$.

This is exactly the definition of `dvd` and our first encounter with the universal quantifier `∃`:

{% highlight lean %}
def dvd (a b : Nat) : Prop :=
  ∃ (c : Nat), b = a * c

instance : has_dvd Nat := ⟨dvd⟩
{% endhighlight %}

It's instructive to prove a simple lemma stating that 1 divides every natural number [3].

{% highlight lean %}
lemma one_dvd_n (n : Nat) : 1 ∣ n :=
begin -- ∃ (c : Nat), n = 1 * c
use n, -- n = 1 * n
rw one_mul, -- n = n
end
{% endhighlight %}

When we say `1 | n` we call the function `dvd 1 n` which in turn is `∃ (c : Nat), n = 1 * c`. The tactic `use n` uses `n` as a possible candidate for `c`. It's then trivial to show it's a valid candidate.

## Greatest Common Divisor

Given natural numbers $a$, $b$ and $g$, we say that $g$ is the greatest common divisor of $a$ and $b$ if it's the largest number that divides both $a$ and $b$.

Expressing "largest" is complicated, so we'll use an alternative definition, which says that $g$ is the greatest common divisor of $a$ and $b$ if every other number that divides both $a$ and $b$ also divides $g$ [4].

{% highlight lean %}
def is_gcd (a b g: Nat) : Prop :=
  (g ∣ a) ∧ (g ∣ b) ∧ (∀ x: Nat, (x ∣ a) ∧ (x ∣ b) → (x ∣ g))
{% endhighlight %}

### GCD For Euclid

We'll now ready to state our main proposition:

**Lemma 1.** Given $a, b, q \in \mathbb{N}$ then $gcd(a, b) = gcd(b, a - q * b)$.

In terms of `is_gcd`, we can state that as `is_gcd a b g = is_gcd b (a - q * b) g`, which is what we'll prove. The Lean code follows below as a whole, but we'll comment on specific parts.

{% highlight lean %}
lemma is_gcd_for_euclid (a b q g: Nat):
  is_gcd b (a - q * b) g -> is_gcd a b g :=
begin
-- (1) replace by definition
repeat {rw is_gcd},

-- (2) add to hypothesis
intro H,
cases H with H1 hrest,
cases hrest with H2 H3,

-- (3) break goal into 3
repeat {apply and.intro},

-- Goal 1: d | a
  cases H2 with k1 eq1,
  cases H1 with k2 eq2,
  use k1 + k2 * q,
  rw mul_dist_add,
  rw← eq1,
  rw mul_assoc,
  rw← eq2,
  rw mul_comm,
  rw sub_assoc,
  rw sub_eq,
  rw sub_zero,

-- Goal 2: d | b
  exact H1,

-- Goal 3: ∀ (x : Nat), (x ∣ a) ∧ (x ∣ b) → (x ∣ d)
  intro x1,
  -- (4) assume (x1 ∣ a) ∧ (x1 ∣ b)
  intro G3H,
  cases G3H with G3H1 G3H2,

  -- (5) use x1 in
  -- ∀ (x : Nat), x ∣ b ∧ x ∣ a - q * b → x ∣ g,
  specialize H3 x1,

  -- (6) turn (x1 ∣ g) into (x1 ∣ b) ∧ (x1 ∣ a - q * b)
  apply H3,
  repeat {apply and.intro},

    -- Goal 3.1: (x1 ∣ b)
    exact G3H2,

    -- Goal 3.2: (x1 ∣ a - q * b)
    cases G3H1 with k3 eq3,
    cases G3H2 with k4 eq4,
    use k3 - q * k4,
    rw mul_dist_sub,
    rw← eq3,
    rw mul_comm q k4,
    rw mul_assoc,
    rw← eq4,
    rw mul_comm,
end
{% endhighlight %}

In (1) we first replace both instances of `gcd` with their definition, which are in the form of 3 ANDed propositions, so we have `H1 ∧ H2 ∧ H3 → Q1 ∧ Q2 ∧ Q3`, where:

* `H1 = (g ∣ b)`
* `H2 = (g ∣ a - q * b)`
* `H3 = ∀ x: Nat, (x ∣ a) ∧ (x ∣ a - q * b) → (x ∣ g)`
* `G1 = (g ∣ a)`
* `G2 = (g ∣ b)`
* `G3 = ∀ x: Nat, (x ∣ a) ∧ (x ∣ b) → (x ∣ g)`

At (2) we assume the left side of the implication is true and make that our hypothesis `h1 = H1 ∧ H2 ∧ H3`. We want to show that if `h1` is true, `G1 ∧ G2 ∧ G3` is true too. We further split `h1` into each a hypothesis for each operand, `h1b = H1`, `h1a = H2` and `h1g = H3`. We can do this because if `H1 ∧ H2 ∧ H3` is true, then `H1`, `H2` and `H3` must be true.

In (3) we break the current goal `G1 ∧ G2 ∧ G3` into 3 goals, `G1`, `G2` and `G3`. If we prove each of them is true separately, we can conclude `G1 ∧ G2 ∧ G3` is also true.

To prove *Goal 1*, i.e. that `(g | a)` is true, we just need to find a natural number `n` such that `a = g * n`. Since `(g ∣ b)` (from `H2`), there exists `k1` such that `b = k1 * g` and since `g | (a - q * b)` there exists `k2` such that `a - q * b = k2 * g`. With some algebra it's possible to show that `a = g * (k1 + k2 * q)` so if we use `n = k1 + k2 * q` in `g | a` we'll conclude that it's a true proposition.

Note how we did things in the reverse order. When writing a proof on paper, we'd usually start from `k1` and `k2` and try to show that `a = g * (k1 + k2 * q)`, whereas in Lean we started from this equation and showed it's true.

*Goal 2* is trivial to prove since `G2 = H1`.

*Goal 3* is the hardest to prove. First we give a name to `x` in `∀ (x : Nat), (x ∣ a) ∧ (x ∣ b) → (x ∣ d)`, say `x1`. We'll make no special assumption on `x1` so if we prove that `(x1 ∣ a) ∧ (x1 ∣ b) → (x1 ∣ d)` it should hold for all `x : Nat`.

In (4) we start by taking the left hand side of the implication as hypothesis and breaking them down into the constituent parts,

* `G3H1 = (x1 ∣ a)`
* `G3H2 = (x1 ∣ b)`

Our goal now is to show `(x1 ∣ d)`. (5) We start off by using `x1` in `H3` which is valid since the hypothesis holds for all `x`, so `H3` is now *specialized* to `(x1 ∣ b) ∧ (x1 ∣ a - q * b) → (x1 ∣ g)`.

If we have a hypothesis of the form `H: P1 → P2` and a goal `P2`, `apply H` turns the goal into `P1`. If we prove `P1` is true, `P2` is trivially true from `H`. That's what we do in (6), and since the new goal is a *AND* we can split into 2 goals like in (3).

*Goal 3.1*, `(x1 ∣ b)`, is trivial since it's exactly `G3H2`.

*Goal 3.2* is `(x1 ∣ a - q * b)`. We can use the same process we used to prove *Goal 1*, by noting that `a - q * b = (k3 - q * k4) * x1`, where `k3` and `k4` are such that `a = k3 * x1` and `b = k4 * x2`.

### Euclidean Algorithm

We can restate **Lemma 1** by using the definition of the modulus operator, `mod`. If `r = a mod b`, then there is `q` such that `a = p + q * b`. Thus `p = a - q * b` we have:

**Corollary 2.** Given $a, b \in \mathbb{N}$ then $gcd(a, b) = gcd(b, a \mod b)$.

If $b = 0$, then $a \mod b = a$ and since any number divides 0, we have the base case $gcd(a, b) = gcd(0, a) = a$ if $b = 0$.

This allows us implementing a recursive algorithm to compute the GCD of 2 natural numbers, known as the Euclidean Algorithm. We'll fallback to using `nat` instead of our own `Nat` because otherwise we will have to introduce more definitions like the `%` operator.

A first attempt is:

{% highlight lean %}
def gcd : nat → nat → nat
| a 0 := a
| a b := gcd b (a % b)
{% endhighlight %}

But this causes an error:

> failed to prove recursive application is decreasing, well founded relation @has_well_founded

When I looked up this error it led me to [5], which uses `gcd` as an example for this error! The key observation is that Lean doesn't know that `(a % b) < b` and that this recursion eventually ends.

The proposed fix in [5] is:

{% highlight lean %}
def gcd : nat → nat → nat
| 0        a := a
| (succ b) a := have a % succ b < succ b, from mod_lt _ $ succ_pos _,
                gcd (a % succ b) (succ b)
{% endhighlight %}

Let's parse what is happening here. The expression `have <Prop>, from <Lemma>` is making an assertion based on a Lemma. We can inspect what `mod_lt` and `succ_pos` are via `#print`:

{% highlight lean %}
theorem mod_lt (x : ℕ) {y : ℕ} (h : 0 < y) : x % y < y := sorry
theorem succ_pos : ∀ (n : ℕ), 0 < n.succ := sorry
{% endhighlight %}

`succ_pos` says that the successor of any natural number is positive. If we "call" it with `b`, `succ_pos b`, we get `0 < (succ b)`.

`mod_lt` says that `a % b < b` as long as `b` is positive. If we "call" it with `a` and `succ_pos b` (i.e. a proof that `succ b` is positive), we get `a % (succ b) < (succ b)`, which is exactly what we're stating.

The `_` tells Lean to infer which variable to use and `expr1 $ expr2` is equivalent to `expr1 (expr2)` . We can make this explicit to make it more readable:

{% highlight lean %}
def gcd : nat → nat → nat
| 0        a := a
| (succ b) a := have a % succ b < succ b, from mod_lt a (succ_pos b),
                gcd (a % succ b) (succ b)
{% endhighlight %}

Note that we pattern match against `succ b` as opposed to `b` because we pass `b` to `succ_pos` but `succ b` to the recursive call. We also inverted the order of `a` and `b` in the pattern match, since doing otherwise makes Lean complain [5].

The full code is available on [Github]({{github}}/gcd.lean).

## Conclusion

I struggled a lot to write the proof for **Lemma 1**. I knew exactly how to write the proof on paper but translating it into Lean was very difficult. It's like trying to communicate an idea in a language you're not fluent in!

One thing that really helped was to write down the proof in pseudo-code and then look up tactics [6] that performed the steps I had written down. In a few cases I had to think backwards to make it work. It was very rewarding to finally figure it out.

The definition of the `gcd` is very interesting. It's the first time I run into this scenario where we need to prove an invariant in the function definition, and using theorems for that. I have a vague understanding that there's a strong relationship between functions and theorems ([Curry-Howard Isomorphism](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence
)), but I'm still wrapping my head around this.

## References

* [[1](https://en.wikipedia.org/wiki/Euclid)] Wikipedia - Euclid
* [[2]({{blog}}/2022/01/26/peano_axioms_lean.html)] NP-Incompleteness: Peano's Axioms in Lean
* [[3](https://exlean.org/divisibility-and-primes-i/)] exlean: Divisibility and Primes I
* [[4](https://cse.buffalo.edu/~knepley/classes/cse191/ClassNotes.pdf)] Lecture Notes for CSE 191, M, Knepley and F, Tsai.
* [[5](https://leanprover-community.github.io/extras/well_founded_recursion.html
)] The equation compiler and using_well_founded
* [[6](https://leanprover.github.io/reference/tactics.html)] The Lean Reference Manual, Chapter 6: Tactics
* [[7](https://coq.inria.fr/library/Coq.Arith.Minus.html)] Library Coq.Arith.Minus
