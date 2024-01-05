---
layout: doc
title: "Complex Numbers Cheat Sheet"
---

## Real and Imaginary parts

Let $z = x + iy$ for $x, y \in \mathbb{R}$. $x$ is the real part of $z$ and denoted by $\Re(z)$. $y$ is the imaginary part of $z$ and denoted by $\Im(z)$.

The **modulus** is defined as $\abs{z} = \sqrt{x^2 + y^2}$.

## Polar notation

$z = r e^{i \theta} = r (\cos \theta + i \sin \theta)$, for $r = \abs{z}$ and $\theta = \tan^{-1}(y/x)$. $\theta$ is called the **argument** of $z$ and denoted by $\mbox{arg}(z)$.

## Conjugate

Let $z = a + ib$ for $a, b \in \mathbb{R}$. The conjugate, denoted by $\overline{z}$ is defined as $z = a - ib$. Use `\overline{z}` instead of `\bar{z}` in LaTeX.

### Identities

Real (LaTex `\Re`) and imaginary (LaTeX `\Im`) parts:

$$\Re(z) = \frac{z + \overline{z}}{2}, \qquad \Im(z) = \frac{z - \overline{z}}{2i}$$

Modulus:

$$\abs{z}^2 = z \overline{z}$$

### Properties

Inverse of conjugate is the conjugate of the inverse:

$$\overline{\left(\frac{1}{z}\right)} = \frac{1}{\overline{z}} = \frac{z}{\abs{z}^2}$$

Product of conjugates is the conjugate of the product:

$$\overline{z \cdot w} = \overline{z} \cdot \overline{w}$$
