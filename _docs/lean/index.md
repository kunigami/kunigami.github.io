---
layout: doc
title: "Lean Cheatsheet"
---

{% include blog_vars.html %}

# Index
{:.no_toc}

1. TOC
{:toc}

# Tactics

## rewrite (rw)

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

### Reverse

Replace `expr2` with `expr1` instead:

Syntax:

{% highlight lean %}
axiom foo : a = b
...
rw ←foo -- replaces b with a
{% endhighlight %}

# Syntax

## Inductive Type

{% highlight lean %}
inductive Nat
| zero : Nat
| succ (n : Nat) : Nat
{% endhighlight %}

## Function

{% highlight lean %}
def add : Nat → Nat → Nat
| m 0 := m
| m (succ n) := succ (m + n)
{% endhighlight %}
