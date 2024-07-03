---
layout: doc
title: "Limits Cheat Sheet"
---

# Limit

## Definition

The limit of $f(x)$ as $x$ approaches $a$, denoted as:

$$\lim_{x \rightarrow a} f(x) = L$$

exists if for every $\epsilon \gt 0$, there exists $\delta \ge 0$ such that when $0 \lt \abs{x - a} \lt \delta$ we have $\abs{f(x) - L} \lt \epsilon$. For a limit to exist, $L$ must be finite and unique.

A way to interpret this: $f(x)$ can get arbitrarily close to $L$ if we choose $x$ close enough to $a$.

**Limit at infinity.** The limit of $f(x)$ as $x$ approaches infinity, denoted as:

$$\lim_{x \rightarrow \infty} f(x) = L$$

exists if for every $\epsilon \gt 0$, there exists $c \gt 0$ such that for all $x \gt c$, $\abs{f(x) - L} \lt \epsilon$.


## Properties

Let $\lim_{x \rightarrow a} f(x) = L_f$ and $\lim_{x \rightarrow a} g(x) = L_g$.

**Addition:**

$$\lim_{x \rightarrow a} \paren{f(x) + g(x)} = L_f + L_g$$

Note that this doesn't always hold for infinite sums.

**Subtraction:**

$$\lim_{x \rightarrow a} \paren{f(x) - g(x)} = L_f - L_g$$

**Multiplication:**

$$\lim_{x \rightarrow a} \paren{f(x)  g(x)} = L_f L_g$$

**Division:**

$$\lim_{x \rightarrow a} \paren{\frac{f(x)}{g(x)}} = \frac{L_f}{L_g}$$

As long as $L_g \ne 0$.

## Infimum and Supremum

**Infimum** and **supremum** are in a way generalizations of the concepts of minimum and maximum and encode the notion of lower and upper bound, respectively.

More formally, let $P$ be a set and $S$ a subset of it. Then

* *Infimum.* is the the largest $a \in P$ such that $a \le x \in S$.
* *Supremum.* is the the smallest $a \in P$ such that $a \le x \in S$.

Example:

Let $P = \mathbb{R}$ and $S = \curly{1/x \mid x \gt 0}$. There's no minimum element in $S$ because for every $y = 1/x$, we can always find $y' = 1/(x + 1) \lt y$. Infimum allows us to choose an element outside of $S$ (but in $P$), in this case $0$.

## Limits inferior and superior

Let $x_r$ be an infinite sequence. If this sequence is not convergent, the limit doesn't exist. However, it's lower/upper bounds might be finite (e.g. a series that oscilates, for example $\sin$).

For this case, we can use limits inferior and superior. Formal definition:

$$\liminf_{r \rightarrow \infty} x_r = \lim_{r \rightarrow \infty} \left( \inf_{m \ge r} x_m \right)$$

and

$$\limsup_{r \rightarrow \infty} x_r = \lim_{r \rightarrow \infty} \left( \sup_{m \ge r} x_m \right)$$

We may ask, why not just $\sup \curly {x_r, r \in \mathbb{N}}$? It maybe be that first terms are big but then the terms get smaller as $r \rightarrow \infty$ so $\sup$ would be dominated by the initial terms, but we want the behavior of terms as $r \rightarrow \infty$.
