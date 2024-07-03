---
layout: doc
title: "Sequence Cheat Sheet"
---

{% include blog_vars.html %}

## Sequence

A **sequence** is a function $\mathbb{N} \rightarrow \mathbb{R}$. Informally, it's a infinite "list" of ordered values. The formal definition essentially maps a position in this list to actual values. A sequence is denoted as $a_0, a_1, \dots$ or by $(a_n)$.

For example the sequence $1, 4, 9, 25, \dots$ is defined by the function $f(n) = n^2$.

### Convergence

A sequence $(a_n)$ **converges** to a finite $L \in \mathbb{R}$ if:

$$\lim_{n \rightarrow \infty} a_n = L$$

## Sequence of functions

Let $f_k: A \rightarrow \mathbb{R}$ for $k \in \mathbb{N}$. A sequence of functions is the sequence $f_1, f_2, f_3, \dots$ and denoted by $(f_k)$. Note that all functions have the same domain and image.

### Convergence

A sequence of function $(f_k)$ **converges pointwise** to a function $f: A \rightarrow \mathbb{R}$ if, for every $x \in A$:

$$\lim_{k \rightarrow \infty} f_k(x) = f(x)$$

A sequence of function $(f_k)$ **converges uniformily** to a function $f: A \rightarrow \mathbb{R}$ if, for every $x \in A$ and $\epsilon \gt 0$, there's $N$ such that for $k \ge N$:

$$\abs{f_k(x) - f(x)} \lt \epsilon$$

### Properties

Let $(f_k)$ be a sequence that converges uniformily to $f: A \rightarrow \mathbb{R}$. Then

$$\lim_{k \rightarrow \infty} \int_{a}^{b} f_k(x)dx = \int_{a}^{b} f(x)dx$$
