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

Conjugate is invariant with the arithmetic operations of addition, subtraction, multiplication and division:

$$
\begin{align}
\overline{a + b} &= \overline{a} + \overline{b} \\
\overline{a - b} &= \overline{a} - \overline{b} \\
\overline{a * b} &= \overline{a} * \overline{b} \\
\overline{a / b} &= \overline{a} / \overline{b}
\end{align}
$$

## Modulus

### Properties

Triangle inequality:

$$\abs{a + b} \le \abs{a} + \abs{b}$$

Multiplication:

$$\abs{a \cdot b} = \abs{a} \cdot \abs{b}$$

## Expontential

If $z = x + i y$

$$
\abs{e^z} = e^{x}
$$

<proof>
$$\abs{e^z} = \abs{e^x} \abs{e^{i y}}$$

The second factor is a complex number in polar form with $r = 1$, so the only portion that contributes to resulting modulus is the first factor, which is real.
</proof>
