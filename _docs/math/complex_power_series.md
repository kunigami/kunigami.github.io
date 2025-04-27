---
layout: doc
title: "Power Series Cheat Sheet"
---


### Radius of convergence

Let a power series be defined as:

$$f(z) = \sum_{n = 0}^{\infty} {c_n} (z - a)^n$$

For complex coefficients $c_n$, contant $a$ and variable $z$. The radius of convergence is a non-negative real $r$ or $\infty$ such that:

* If $\abs{z - a} > r$, the power series diverges
* If $\abs{z - a} < r$, the power series converges

The name *radius of convergence* alludes to the fact that $\abs{z - a} = r$ is a circle of radius $r$ in the complex plane, and that inside that circle the power series converges.

### Laurent Series

A function can be decomposed into two, based on its Laurent series. The series containing the terms for $n < 0$ are called the **singular** (or **principal**) part of $f(z)$ and those for $n \ge 0$ are the **analytic** part.

For analytic functions (which are holomorphic) the singular part is 0. Conversely, if its singular part is non-zero, we know it contains a singularity.

Reference: [Zeros and Poles]({{blog}}//2024/11/02/poles.html).
