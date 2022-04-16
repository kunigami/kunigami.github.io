---
layout: doc
title: "Lean Proofs"
---

{% include blog_vars.html %}

# Index
{:.no_toc}

1. TOC
{:toc}

# Natural Numbers

{% highlight lean %}
inductive Nat
| zero : Nat
| succ (n : Nat) : Nat

open Nat

instance : has_zero Nat := ⟨zero⟩
axiom zero_is_nat : zero = 0

instance : has_one Nat := ⟨succ zero⟩
axiom one_is_nat : succ zero = 1
{% endhighlight %}

# Addition

{% highlight lean %}
def add : Nat → Nat → Nat
| m 0 := m
| m (succ n) := succ (add m n)

instance : has_add Nat := ⟨add⟩
axiom add_zero (m: Nat) : m + 0 = m
axiom add_succ (m n: Nat) : m + succ n = succ (m + n)
{% endhighlight %}

**Lemma:** Let $n \in \mathbb{N}$. Then $0 + n = n$.

{% highlight lean %}
lemma zero_add (n: Nat) : 0 + n = n :=
begin
induction n with k hk,
rw zero_is_nat,
rw add_zero,

rw add_succ,
rw hk,
end
{% endhighlight %}

**Lemma:** Let $n, m \in \mathbb{N}$. Then $S(n) + m = S(n + m)$.

{% highlight lean %}
lemma succ_add (n m: Nat) : (succ n) + m = succ (n + m) :=
begin
induction m with k hk,
rw zero_is_nat,
repeat {rw add_zero},

repeat {rw add_succ},
rw hk,
end
{% endhighlight %}

## Commutativity

**Lemma:** Let $n, m \in \mathbb{N}$. Then $n + m = m + n$.

{% highlight lean %}
lemma add_commutativity (n m: Nat): n + m = m + n :=
begin
induction n with k hk,
rw zero_is_nat,
rw add_zero,
rw zero_add,

rw add_succ,
rw succ_add,
rw hk,
end
{% endhighlight %}

# Subtraction

Truncated subtraction: if $n < m$ we assume $n = m = 0$.

{% highlight lean %}
def sub : Nat → Nat → Nat
  | (succ n) (succ m) := sub n m
  | n m := n

instance : has_sub Nat := ⟨sub⟩
{% endhighlight %}

# Multiplication

{% highlight lean %}
def mul : Nat → Nat → Nat
| m 0 := 0
| m (succ n) := m + (mul m n)

instance : has_mul Nat := ⟨mul⟩

axiom mul_zero (n: Nat) : n * 0 = 0
axiom mul_succ (n m: Nat): m * (succ n) = m + (m * n)
{% endhighlight %}

**Lemma:** One is the identity element for multiplication

{% highlight lean %}
lemma one_mul (n: Nat) : 1 * n = n :=
begin
induction n with k hk,

-- base
rw zero_is_nat,
rw mul_zero,

-- step
rw mul_step,
rw hk,
rw ←one_is_nat,
rw add_commutativity,
rw add_succ,
rw zero_is_nat,
rw add_zero,

end
{% endhighlight %}

# Divisibility

{% highlight lean %}
def dvd (a b : Nat) : Prop :=
  ∃ (c : Nat), b = a * c

instance : has_dvd Nat := ⟨dvd⟩
{% endhighlight %}

**Lemma:** One divides every natural number

{% highlight lean %}
-- 1 | n is equivalent to dvd 1 n
-- which is equivalent to ∃ (c : Nat), n = 1 * c
theorem one_dvd_n (n : Nat) : 1 ∣ n :=
begin
use n,
rw one_mul,
end
{% endhighlight %}
