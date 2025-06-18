---
layout: doc
title: "Holomorphic Functions Cheat Sheet"
---

## Definition

A complex function $f(z)$ is **holomorphic** at point $a$ if it's complex differentiable at $a$. More formally the limit:

$$
\lim_{h \rightarrow 0} \frac{f(a + h) - f(a)}{h} = f'(a)
$$

for $h \in \mathbb{C}$ exists. Without any qualifications, a *holomorphic function* is a function that is holomorphic at every point on its domain.

## Properties

Addition, subtraction and multiplication preserve holomorphism. Division preserves holomorphism as long as the divisor is non-zero [1].

Differetiation preserves holomorphism. In fact holomorphic functions are infinitely differentiable [2].

## Analyticity

Let $f(z)$ be a holomorphic function in the disk $\abs{z - a} \lt r$. Then, for $z$ in that circle it can be written as a Taylor series expansion around $a$:

$$f(z) = \sum_{n=0}^{\infty} \frac{f^{(n)}(a)}{n!} (z - a)^n$$

If we denote $c_n = f^{(n)}(a) / n!$, it looks more like a polynomial:

$$f(z) = \sum_{n=0}^{\infty} c_n (z - a)^n$$

## Cauchy's Estimate

Let $f(z)$ be holomorphic and bounded by a finite $M$ in a region $\Omega$ (i.e. , $\abs{f(z)} \le M$ for all $z \in \Omega$). Let $C$ be a circle of radius $r$ centered in $a$ ($C$ is inside $\Omega$). Then:

$$\abs{f^{(n)}(a)} \le \frac{n! M}{r^{n}}$$

In terms of the Taylor series coefficient:

$$\abs{c_n} \le \frac{M}{r^n}$$

## Liouville’s Theorem

If $f(z)$ is holomorphic and bounded on the whole plane, then it's a constant function [2].



## References

* [[1]({{blog}}/2023/12/21/holomorphic-functions.html)] Holomorphic Functions
* [[2]({{blog}}/2024/06/06/cauchy-integral-formula.html)] Cauchy's Integral Formula
* [[3]({{blog}}/2024/07/02/holomorphic-functions-are-analytic.html)] Holomorphic Functions are Analytic
