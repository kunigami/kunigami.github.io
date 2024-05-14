---
layout: doc
title: "Limits Cheat Sheet"
---

## Definition

The limit of $f(x)$ as $x$ approaches $a$, denoted as:

$$\lim_{x \rightarrow a} f(x) = L$$

exists if for every $\epsilon \gt 0$, there exists $\delta \ge 0$ such that when $0 \lt \abs{x - a} \lt \delta$ we have $\abs{f(x) - L} \lt \epsilon$. For a limit to exist, $L$ must be finite and unique.

A way to interpret this: $f(x)$ can get arbitrarily close to $L$ if we choose $x$ close enough to $a$.

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
