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

**Euler's identity**:

$$
e^{i\pi} + 1 = 0
$$

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

**Triangle inequality.**

$$\abs{a + b} \le \abs{a} + \abs{b}$$

Strict triangle inequality. If

$$\abs{a + b} = \abs{a} + \abs{b}$$

then $a = kb$ for some real $k$.

<proof>
This is trivial for $a = 0$ or $b = 0$, so assume $a, b \ne 0$. Consider the square of the left side and use that $\abs{z}^2 = z \overline{z}$

$$
\abs{a + b}^2 = (a + b)(\overline{a + b}) = (a + b)(\overline{a} + \overline{b}) = \abs{a}^2 + \abs{b}^2 + a\overline{b} + \overline{a}b
$$

We have that $a\overline{b} + \overline{a}b = a\overline{b} + \overline{a\overline{b}}$, so using $2\Re(z) = z + \overline{z}$, we have:

$$
\abs{a + b}^2 = \abs{a}^2 + \abs{b}^2 + 2\Re(a\overline{b})
$$

expanding and using $\abs{z} = z \overline{z}$:

$$
(a + b)(\overline{a + b}) = \abs{a}^2 + \abs{b}^2 + 2
$$

Now squaring the right side:

$$(\abs{a} + \abs{b})^2 = \abs{a}^2 + \abs{b}^2 + 2\abs{a}\abs{b}$$

which implies that

$$\abs{a}\abs{b} = \Re(a\overline{b})$$

we have that $\abs{a}\abs{b} = \abs{a}\abs{\overline{b}} = \abs{a\overline(b)}$ and thus:

$$\abs{a\overline(b)} = \Re(a\overline{b})$$

which implies $a\overline(b)$ is real. Let $t = a\overline(b)$, so that:

$$
a = \frac{t}{\overline(b)}
$$

multiply the fraction by $b/b$ to obtain:

$$
a = \frac{t}{\abs{b}^2} b
$$

$k = t / \abs{b}^2$ is real.

</proof>

**Multiplication.**

$$\abs{a \cdot b} = \abs{a} \cdot \abs{b}$$

**Division.**

If $b \ne 0$:

$$\abs{\frac{a}{b}} = \frac{\abs{a}}{\abs{b}}$$

**Exponential.** Let $x$ be a real value and $z$ a complex number. Then:

$$
\abs{x^z} = x^{\Re(z)}
$$

<proof>

Rebase $x^z$ as $e^{z \log x}$ and let $z = a + i b$ for $a, b \in \mathbb{R}$. Then:

$$x^z = e^{a \log x} e^{i b \log x}$$

Apply modulus:

$$\abs{x^z} = \abs{e^{a \log x}} \abs{e^{i b \log x}}$$

The second term is a complex number in polar notation with $r = 1$ and $\theta = b \log x$, so $\abs{e^{i b \log x}} = 1$ and hence:

$$\abs{x^z} = \abs{e^{a \log x}}$$

Since all terms are reals,

$$\abs{e^{a \log x}} = e^{a \log x}$$

Rebasing back:

$$= x^{a} = x^{\Re(z)}$$

</proof>
