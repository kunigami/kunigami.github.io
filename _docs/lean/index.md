---
layout: doc
title: "Lean Cheatsheet"
---

{% include blog_vars.html %}

## Index
{:.no_toc}

1. TOC
{:toc}

## Syntax

### Inductive Type

{% highlight lean %}
inductive Nat
| zero : Nat
| succ (n : Nat) : Nat
{% endhighlight %}

### Function

{% highlight lean %}
def inc (x: nat): nat := x + 1
{% endhighlight %}

Alternative: pattern matching.

{% highlight lean %}
def add : nat → nat → nat
| m 0 := m
| m (succ n) := succ (add m n)
{% endhighlight %}

**Function Composition.**

{% highlight lean %}
def inc (x: nat): nat := x + 1
def dbl (x: nat): nat := 2 * x
def f := dbl ∘ inc
#eval f 3 -- prints 8
{% endhighlight %}

## Inference

Typing an underscore in an expression asks Lean to infer a suitable value for the expression and fill it in automatically.

## Symbols

For copy and pasting:

{% highlight lean %}
Subscripts: ₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉
Left-arrow: ←
{% endhighlight %}


## Tactics

### apply

Given goal `b` and hypothesis `h: a -> b`, `apply h` turns goal `b` into `a`.

### assumption

The assumption tactic looks through the assumptions in context of the current goal, and if there is one matching the conclusion, it applies it.

{% highlight lean %}
variables x y z w : ℕ

example (h₁ : x = y) (h₂ : y = z) (h₃ : z = w) : x = w :=
begin
  apply eq.trans h₁, -- y = w
  apply eq.trans h₂, -- z = w
  assumption         -- applied h₃
end
{% endhighlight %}

### cases

Split a hyposesis of the form a ^ b into two hypothesis a and b.

### have

have q := f p,
https://www.ma.imperial.ac.uk/~buzzard/xena/natural_number_game/?world=6&level=7
### intro

If the current goal is `a → b`, `intro h` assumes `a` is true (adding as hypothesis `h`) and the goal becomes `b`.

### revert

If there's a hypothesis `h: a → b` and the current goal is `b`, `revert h`, turns the goal into `a → b`.

### rewrite (rw)

* You give an axiom/lemma that states an equality of the form `expr1 = expr2`.
* It will try to find the first expression that matches `expr1`
* It will replace that expression everywhere in the current state.

Syntax:

{% highlight lean %}
axiom foo : a = b
...
rw foo -- replaces a with b
{% endhighlight %}

Example:

{% highlight lean %}
axiom add_succ (m n: nat) : m + nat.succ n = nat.succ (m + n)

lemma my_add_assoc (a b c : nat) : (a + b) + c = a + (b + c) :=
begin
  induction c with d hd,
  -- ⊢ a + b + 0 = a + (b + 0)
  rw add_zero,
  -- ⊢ a + b = a + (b + 0)
  rw add_zero,
  -- ⊢ a + b = a + b
  -- ...

end
{% endhighlight %}

**reversed rewrite.**

Replace `expr2` with `expr1` instead:

Syntax:

{% highlight lean %}
axiom foo : a = b
...
rw ←foo -- replaces b with a
{% endhighlight %}

## split

If the current goal is `a ∧ b`, `split` generates 2 goals `a` and `b`. (Same as `apply and.intro`)

## Tactic Combinators

### repeat

Executes a tactic while it's applicable. Example:

{% highlight lean %}
repeat {rw h}
{% endhighlight %}

## How To?

### Assume the left-hand side of an implication in the goal

If the goal is of the form `⊢ x → y` we add `x` to our hypothesis and prove `y`:

{% highlight lean %}
-- ⊢ x → y
intro h
-- h: x
-- ⊢ y
{% endhighlight %}


### Bind a value to ∀ in the goal

If the goal is of the form `⊢ ∀ x, f(x)` we can bind `x` to some specific variable `y` and prove the goal for it:

{% highlight lean %}
-- ⊢ ∀ x, f(x)
intro y
-- y
-- ⊢ f(y)
{% endhighlight %}


### Bind a value to ∃ in the goal

If the goal is of the form `⊢ ∃ x, f(x)` we can replace with a specific instance via `existsi`:

{% highlight lean %}
-- ⊢ ∃ x, f(x)
existsi y
-- ⊢ y, f(y)
{% endhighlight %}


### Bind a value to ∀ in a hypothesis

If we have a hypothesis `H: ∀ x, f(x)`, we can create a new hypothesis binding `x` with some instance via `have Hy := H y`:


{% highlight lean %}
-- : H: ∀ x, f(x)
have H2 := H y
-- Hy: y, f(y)
{% endhighlight %}

### Bind a value to ∃ in a hypothesis

If we have a hypothesis `H: ∃ x, f(x)`, we can introduce some `y` and create a new hypothesis that is `H` applied to `y`:


{% highlight lean %}
-- : H: ∃ x, f(x)
cases H with y Hy,
-- Hy: y, f(y)
-- y
{% endhighlight %}

### Split a hypothesis in the form H1 ∧ H2 into two hypothesis

If we have a hypothesis `H: H1 ∧ H2`, we can get two hypothesis `H1` and `H2`:

{% highlight lean %}
-- : H: H1 ∧ H2
cases H with H1 H2,
-- H1
-- H2
{% endhighlight %}


## References

* https://leanprover.github.io/theorem_proving_in_lean/tactics.html
* https://leanprover.github.io/reference/tactics.html
* https://leanprover-community.github.io//img/lean-tactics.pdf
