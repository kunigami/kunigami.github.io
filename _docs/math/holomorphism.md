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

## References

* [[1]({{blog}}/2023/12/21/holomorphic-functions.html)] Holomorphic Functions
* [[2]({{blog}}/2024/06/06/cauchy-integral-formula.html)] Cauchy's Integral Formula
