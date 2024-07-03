---
layout: doc
title: "Series Cheat Sheet"
---

{% include blog_vars.html %}

## Definition

Let $(a_k)$ be a [sequence]({{docs}}/math/sequence.html). A series is the (infinite) sum of these values:

$$\sum_{k = 0}^{\infty} a_k$$

The **partial sum** is defined as:

$$s_n = \sum_{k = 0}^{n} a_k$$

### Convergence

A series **converges** to a finite $L \in \mathbb{R}$ if the [sequence]({{docs}}/math/sequence.html) of partial sums, $(s_n)$, converges.

A series **converges absolutely** if the series of its absolute summands, i.e.

$$\sum_{k = 0}^{\infty} \abs{a_k}$$

converges.

## Series of functions

Let $f_k(x)$ be a [sequence of functions]({{docs}}/math/sequence.html). A series of functions is defined as:

$$\sum_{k = 0}^{\infty} f_k(x)$$

### Convergence

Define the partial sum:

$$s_n = \sum_{k = 0}^{n} f_k(x)$$

A series of functions [converges uniformily]({{docs}}/math/sequence.html) (or pointwise) if the sequence $(s_n)$ converges uniformily (or pointwise). Note that uniform and pointwise convergence only makes sense for series/sequences of functions.

A series of functions **converges absolutely** if the series

$$\sum_{k = 0}^{\infty} \abs{f_k(x)}$$

converges, either uniformily or pointwise.

**Weierstrass M-test.** If $\abs{f_k(x)} \le M_k$ for all $k$ and $x \in A$ and the series $\sum_{n = 0}^\infty M_n$ converges, then

$$\sum_{k = 0}^{n} f_k(x)$$

converges absolutely and uniformly [1].


## References

* [1] Real Analysis (2nd edition) - Jay Cummings
