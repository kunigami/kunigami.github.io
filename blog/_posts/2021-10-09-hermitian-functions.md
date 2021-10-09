---
layout: post
title: "Hermitian Functions"
tags: [calculus]
vanity: "2021-10-09-hermitian-functions"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
    <img src="{{resources_path}}/charles-hermite.jpeg" alt="Charles Hermite thumbnail" />
</figure>


In this post we’ll study even and odd functions, the even-odd function decomposition, Hermitian functions and prove some results for Fourier transforms.

Hermitian functions are named after the 19th century French mathematician [Charles Hermite](https://en.wikipedia.org/wiki/Charles_Hermite). Hermitian matrices are also named after hin. Charles was the advisor of Henri Poincaré.

<!--more-->

<br />

## Even and Odd functions

Let $f: \mathbb{R} \rightarrow \mathbb{R}$ be a function with a real domain and image, where $F$ is either the real numbers $\mathbb{R}$ or the complex numbers $\mathbb{C}$.

An **even function** is defined as:

$$f(x) = f(-x)$$

An example of even function is $f(x) = x^2$, since $x^2 = (-x)^2$.

Conversely, an **odd function** is defined as:

$$f(x) = - \overline{f(-x)}$$

An example of odd function is $f(x) = x^3$, since $x^3 = - (-x)^3$.

### Odd Naming

The names *odd* and *even* comes from the fact that if $f(x)$ is of the form $f(x) = x^n$, then $f(x)$ is even when $n$ is even, and odd when $n$ is odd.

I find this terminology confusing, since $f(x) = \abs{x}$ is also even. Perhaps a better name could be *symmetric* and *anti-symmetric*, but unfortunately symmetric functions mean [something else](https://en.wikipedia.org/wiki/Symmetric_function).

By calling functions this way also gives the idea that all functions are either even and odd, which is not true. Some functions are neither.

## Hermitian Functions

Hermitian functions are in a way a generalization of Even and Odd functions, in which we allow complex numbers as the image. The analogous of *even function* is the **(symmetric) Hermitian function** as:

$$f(x) = \overline{f(-x)}$$

An example of Hermitian function is $f(x) = x^2+ ix$, since $f(-x)= x^2 - ix = \overline{f(x)}$.

Conversely, the analogous to **odd function** is the **antisymmetric Hermitian function**:

$$f(x) = - \overline{f(-x)}$$

An example of antisymmetric Hermitian function is $f(x) = x + ix^2$, since $f(-x)= -x + ix^2 = - (x - ix^2) = -\overline{f(x)}$.

Note that a function $f$ is a Hermitian function if and only if $Re\curly{f}$ is even and $Im\curly{f}$ is odd.

### Generalizing to $\mathbb{R}^n$

We can generalize the concept of Hermitian functions to the $\mathbb{R}^n$ domain, so if $f: \mathbb{R}^n \rightarrow \mathbb{C}$, then:

$$f(\vec{x}) = \overline{f(-\vec{x})}$$

Where $\vec{x} = x_0, \cdots, x_{n-1}$ and $-\vec{x} = -x_0, \cdots, -x_{n-1}$.

## Hermitian Decomposition

Let $f: \mathbb{R} \rightarrow \mathbb{C}$ be a function, $f_s$ a symmetric Hermitian function and $f_a$ an antisymmetric Hermitian. If $f$ can be written as a sum of $f_s$ and $f_a$, that is:

$$(1) \qquad f(x) = f_s(x) + f_a(x)$$

then it has a **Hermitian decomposition**.

**Proposition 1.** Every function $f: \mathbb{R} \rightarrow \mathbb{C}$ has a Hermitian decomposition.

*Proof:*

We can define

$$f_s(x) = \frac{f(x) + \overline{f(-x)}}{2}$$

and

$$f_a(x) = \frac{f(x) - \overline{f(-x)}}{2}$$

Which clearly satisfies the identity (1). We now claim that $f_s$ is symmetric, i.e. $f_s(x) = \overline{f_s(-x)}$. Consider some $x_p \ge 0$, and let $x_n = - x_p$. We can show that $f_s(x_p) = \overline{f_s(x_n)}$ by:

$$f_s(x_p) = \frac{f(x_p) + \overline{f(-x_p)}}{2} = \frac{f(x_p) + \overline{f(x_n)}}{2} = \overline{\frac{\overline{f(-x_n)} + f(x_n)}{2}} = \overline{f_s(x_n)}$$

which proves our claim. We can show that $f_a(x) = - \overline{f_a(-x)}$ through an analogous process.
*QED*.

**Proposition 2.** The Hermitian decompositions is unique.

*Proof:* See *Appendix*.

Our definition of Hermitian decomposition and the two properties above can be trivially generalized for $f: \mathbb{R}^n \rightarrow \mathbb{C}$ since we don't work directly with $\vec{x}$ but rather their scalar image.

## Fourier Transform is Hermitian

We'll now show that the [DTFT]({{blog}}/2021/07/31/discrete-fourier-transform.html) is Hermitian if it's applied to $\vec{x} \in \mathbb{R}^N$ is:

$$(2) \quad \lambda(\omega) = \sum_{t = 0}^{N-1} x_t e^{-i \omega t} \quad -\pi \le \omega \le \pi$$

Let's expand $e^{-i \omega t}$ using Euler's formula:

$$e^{-i \omega t} = \cos(- \omega t) + i \sin( - \omega t)$$

Since $\cos(\theta) = \cos(-\theta)$ and $\sin(\theta) = -\sin(-\theta)$,

$$e^{-i \omega t} = \cos(\omega t) - i \sin(\omega t) = \overline{e^{i \omega t}}$$

If we call each term in the sum (2), $c_{t,\omega} = x_t e^{-i \omega t}$, then by the results above, $c_{t,\omega} = \overline{c_{t, -\omega}}$, and it's easy to see this property is preserved over sums, so:

$$\lambda(\omega) = \overline{\lambda(-\omega)}$$

Which shows the Fourier transform, $\lambda: \mathbb{R} \rightarrow \mathbb{C}$ is a symmetric Hermitian function.

## Conclusion

I found the even-odd decomposition counter-intuitive at first, but making analogies to how a complex number can be uniquely decomposed into a sum of a purely real and purely imaginary parts makes it a bit intuitive.

I initially intended to study even and odd functions but was interested in the results of for Fourier transforms, which led me to the more general Hermitian functions.

Interestingly, the Wikipedia entry for [even and odd functions](https://en.wikipedia.org/wiki/Even_and_odd_functions#Complex-valued_functions) does not mention Hermitian functions even though it presents the generalized version for complex images.

## Related Posts

In regards of decomposition, I was reminded of the [Shor's Prime Factoring Algorithm]({{blog}}/2020/12/26/shors-prime-factoring.html) post, where we saw that $\ket{1}$ can be decomposed into some a linear combination of eigenvectors:

$$\frac{1}{\sqrt{r}} \sum_{s=0}^{r-1} \ket{u_s} = \ket{1}$$

in and [Discrete Fourier Transforms]({{blog}}/2021/07/31/discrete-fourier-transform.html) we also see that a signal can be decomposed into a sum of sinusoids.

Decomposing values into "basic" parts, is prevalent in linear algebra via bases of vector spaces.

## Appendix

*Proof of Proposition 2:*

We'll show by contradiction and some tedious algebra that there's only one way the do a Hermitian decomposition.

Suppose there is some symmetric function $f'_s$ that is different than $f_s$. For this to be true, there must exist at least one $x'$ such that $f'_s(x') \ne f_s(x')$.

Since $f'_s(x')$ is a complex number, either its real part or imaginary part, or both, must be different from that of $f_s(x')$. First let's assume at least the real part is different and that, without loss of generality, $Re\curly{f'_s(x')} > Re\curly{f_s(x')}$.

Because it has to satisfy (1) and we know that $f_a(x') \ne f(x') - f'_s(x')$, so there must also be an antisymmetric function $f'_a \ne f_a$ such that $f'_a(x') = f(x') - f'_s(x')$. If we only consider the real part, $Re\curly{f'_a(x')} = Re\curly{f(x')} - Re\curly{f'_s(x')} < Re\curly{f(x')} - Re\curly{f_s(x')} = Re\curly{f_a(x')}$, that is

$$(3) \quad Re\curly{f'_a(x')} < Re\curly{f_a(x')}$$

Since $f'_s$ is symmetric, we have $f'_s(x') = \overline{f'_s(-x')}$ or $Re\curly{f'_s(x')} = Re\curly{f'_s(-x')}$. Since $f'_s(x')$ is also symmetric, we have

$$(4) \quad Re\curly{f'_s(-x')} > Re\curly{f_s(-x')}$$

Applying (1) for $-x'$ and considering only the real part we have $Re\curly{f'_a(-x')} = Re\curly{f(-x')} - Re\curly{f'_s(-x')}$ By (4), $Re\curly{f'_a(-x')} < Re\curly{f(-x')} - Re\curly{f_s(-x')} = Re\curly{f_a(-x')}$, that is

$$Re\curly{f'_a(-x')} < Re\curly{f_a(-x')}$$

or

$$(5) \quad -Re\curly{f'_a(-x')} > -Re\curly{f_a(-x')}$$

Since $f'_a$ is antisymmetric, $Re\curly{f'_a(x')} = -Re\curly{f'_a(-x')}$, then by (5) $Re\curly{f'_a(x')} > -Re\curly{f_a(-x')} = Re\curly{f_a(x')}$, which contradicts (3).

For the second part, let's assume the imaginary part is different and that $Im\curly{f'_s(x')} > Im\curly{f_s(x')}$. It will be pretty similar to the first part, but we have to be careful with the signs. Because it has to satisfy (1), by an analogous reasoning we'll get

$$(6) \quad Im\curly{f'_a(x')} < Im\curly{f_a(x')}$$

Since $f'_s$ and $f_s$ are symmetric, we have $Im\curly{f'_s(x')} = -Im\curly{f'_s(-x')}$ and $Im\curly{f_s(x')} = -Im\curly{f_s(-x')}$. We started with $Im\curly{f'_s(x')} > Im\curly{f_s(x')}$ which implies $-Im\curly{f'_s(x')} < -Im\curly{f_s(x')}$ and thus

$$(7) \quad Im\curly{f'_s(-x')} < Im\curly{f_s(-x')}$$

Applying (1) for $-x'$ we have $Im\curly{f\'_a(-x\')} = Im\curly{f(-x\')} - Im\curly{f\'_s(-x\')}$. From (7) $Im\curly{f\'_a(-x\')} > Im\curly{f(-x\')} - Im\curly{f_s(-x\')} = Im\curly{f_a(-x\')}$, that is

$$(8) \quad Im\curly{f'_a(-x')} > Im\curly{f_a(-x')}$$

Since $f'_a$ is antisymmetric, $Im\curly{f'_a(x')} = Im\curly{f'_a(-x')}$ (negation + conjugate), then by (8) $Im\curly{f'_a(x')} > Im\curly{f_a(-x')} = Im\curly{f_a(x')}$, which contradicts (6).

In either case, we reach a contradiction if we assume there exists a different way of decomposing $f$ into symmetric and antisymmetric Hermitian functions, so it must be it's unique.

*QED*.

## References

* [[1](https://en.wikipedia.org/wiki/Even_and_odd_functions)] Even and odd functions
* [[2](https://en.wikipedia.org/wiki/Hermitian_function)] Hermitian function
