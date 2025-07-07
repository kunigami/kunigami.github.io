---
layout: doc
title: "Harmonic Functions (Complex) Cheat Sheet"
---

{% include blog_vars.html %}

This documents harmonic functions in the complex domain.

## Definition

Let $f(z): \mathbb{C} \rightarrow \mathbb{R}$ be a function that takes in a complex number and returns a real one. We can see $f(z)$ as a function of $(x, y) \in \mathbb{R}^2$ since $z = x + iy$.

Then we say it is **harmonic** if it satisfies the Laplace equation:

$$
\Delta f = \frac{\partial^2 f}{\partial x^2} + \frac{\partial^2 f}{\partial y^2} = 0
$$

## Holomorphic Functions

A function $f(z): \mathbb{C} \rightarrow \mathbb{C}$ can be expressed as two functions $u(z), v(z): \mathbb{C} \rightarrow \mathbb{R}$, one for the real part and one for the imaginary part:

$$
f(z) = u(z) + i v(z)
$$

If $f(z)$ is holomorphic, then its real part $u(z)$ and imaginary part $v(z)$ are harmonic. They're also **conjugate harmonic** because they're connected by the *Cauchy-Riemann* equations:

$$
\frac{\partial u}{\partial x} = \frac{\partial v}{\partial y}, \quad \frac{\partial u}{\partial y} = \frac{\partial v}{\partial x}
$$

## References

* [[1]({{blog}}/2023/12/21/holomorphic-functions.html)] Holomorphic functions
