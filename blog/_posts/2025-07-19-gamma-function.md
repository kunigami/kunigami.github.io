---
layout: post
title: "The Gamma Function"
tags: [analysis]
excerpt_separator: <!--more-->
vanity: "2025-07-19-gamma-function"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/gamma-func.svg" alt="3D plot of the absolute value of the gamma function" />
</figure>

There's evidence that the factorial function has been used in some form since at least 1150 by the Indian mathematician Bhāskara II [3]. In the 18th century, European mathematicians attempted to generalize this function beyond the natural numbers. Daniel Bernoulli, Christian Goldbach and Leonhard Euler were involved in the early attempts and came up with different definitions.

In the 19th century Carl Friedrich Gauss, Karl Weierstrass and Adrien-Marie Legendre further contributed to it, the latter naming the function and its equivalent definitions as the **gamma function**.

Weierstrass in particular provided an alternative definition as an infinite product. He later generalized this result to the [Weierstrass Factorization Theorem]({{blog}}/2025/07/02/weierstrass-factorization-theorem.html). It's within this context that we'll study the gamma function in this post.

<!--more-->

## Development

In the post [Weierstrass Factorization Theorem]({{blog}}/2025/07/02/weierstrass-factorization-theorem.html) we used the theorem to derive a power series for $\sin(\pi z)$, based on the fact that its zeros are the natural numbers. It turns out $\sin (\pi z)$ is the simplest function that has all integers as zeros.

We now aim to decompose it a bit. Suppose we have a function that only has the negative numbers as zeros. Using Weierstrass theorem we can show that one such a function is:

$$
G(z) = \prod_{n = 1}^\infty \left(1 + \frac{z}{n}\right) e^{-z/n}
$$

via a similar reasoning as done for $\sin (\pi z)$. Conversely, the function $G(-z)$ has the positive numbers as zeros, so their product has all integers except zero as zeros:

$$
G(z) G(-z) = \prod_{n = 1}^\infty \left(1 - \frac{z^2}{n^2}\right)
$$

Noting the terms $e^{-z/n}$ cancel out. In [2] we had shown that:

$$
\sin (\pi z) = z \pi \prod_{n \ge 1} \left(1 - \frac{z^2}{n^2}\right)
$$

So we can write

$$
\sin (\pi z) = z \pi G(z) G(-z)
$$

Now consider $G(z - 1)$. If $w + 1$ is a zero for $G(z)$, then $w$ is a zero for $G(z - 1)$, hence we conclude that its zeros are all the non-positive numbers (i.e. the zeros of $G(z)$ plus $0$).

Using Weierstrass, we can conclude that:

$$
G(z - 1) = z e^{\gamma(z)} \prod_{n = 1}^\infty \left(1 + \frac{z}{n}\right) e^{-z/n}
$$

Replacing $G(z)$ we have:

$$
(1) \quad G(z - 1) = z e^{\gamma(z)} G(z)
$$

It's possible to show that $\gamma(z)$ is a constant, which we'll denote by $\gamma$ and known as *Euler's constant*, defined as:

$$
\gamma = \lim_{n \rightarrow \infty} \left(\left(\sum_{k = 1}^n \frac{1}{k}\right) - \log n \right)
$$

and it's approximately 0.57722. Let's define $H(z) = G(z) e^{\gamma z}$. We have that

$$
(2) \quad H(z - 1) = z H(z)
$$

<proof>

$$
H(z - 1) = G(z - 1) e^{\gamma (z - 1)}
$$

Replacing $(1)$

$$
= z e^{\gamma} G(z) e^{\gamma (z - 1)} = z e^{\gamma z} G(z)
$$

Replacing the definition of $H(z)$:

$$
\quad H(z - 1) = z H(z)
$$

</proof>

If we define $\Gamma(z) = 1 / (z H(z))$ we obtain

$$
(3) \quad \Gamma(z + 1) = z \Gamma(z)
$$

<proof>

$$
\Gamma(z - 1) = \frac{1}{(z - 1) H(z - 1)}
$$

Replacing $(2)$:

$$
= \frac{1}{(z - 1) z H(z)} = \frac{1}{z - 1} \frac{1}{z H(z)}
$$

Replacing the definition of $\Gamma(z)$:

$$
\Gamma(z - 1) = \frac{\Gamma(z)}{z - 1}
$$

or that

$$
\Gamma(z + 1) = z \Gamma(z)
$$

</proof>

Which is a more useful form than $G(z)$, and is the famous **Gamma function**. By substituting back the definitions of $H(z)$ and then $G(z)$, we get the explicit formula:

$$
(4) \quad \Gamma(z) = \frac{e^{-\gamma z}}{z} \prod_{n = 1}^\infty \left( 1 + \frac{z}{n}\right)^{-1} e^{z / n}
$$

<proof>

Replacing $H(z)$:

$$
\Gamma(z) = \frac{1}{z e^{\gamma z} G(z)}
$$

Replacing $G(z)$:

$$
\Gamma(z) = \frac{e^{-\gamma z}}{z} \prod_{n = 1}^\infty \left( 1 + \frac{z}{n}\right)^{-1} e^{z / n}
$$

</proof>

We can express our early example in terms of $\Gamma(z)$:

$$
(5) \quad \frac{\pi}{\Gamma(z)\Gamma(1 - z)} = \sin \pi z
$$

This identity is known as **Euler's reflection formula**.

<proof>

First we use replace $z$ with $-z$ in $(3)$:

$$
\Gamma(1 - z) = -z \Gamma(-z)
$$

Expanding $\Gamma(-z)$ via $(4)$:

$$
\Gamma(-z) = \frac{e^{\gamma z}}{-z} \prod_{n = 1}^\infty \left( 1 - \frac{z}{n}\right)^{-1} e^{-z / n}
$$

Multiplying by $\Gamma(z)$:

$$
\Gamma(1 - z) \Gamma(z) = -z \Gamma(-z)\Gamma(z) =
$$

Expanding $\Gamma(z)$ via $(4)$ and grouping similar terms together:

$$
= -z \frac{e^{\gamma z} e^{-\gamma z}}{-z z} \prod_{n = 1}^\infty \left( 1 + \frac{-z}{n}\right)^{-1} \left( 1 - \frac{z}{n}\right)^{-1} e^{-z / n} e^{z / n}
$$

Cancelling terms:

$$
= \frac{1}{z} \prod_{n = 1}^\infty \left( 1 + \frac{-z}{n}\right)^{-1} \left( 1 + \frac{z}{n}\right)^{-1}
$$

So we can write $1 / (\Gamma(z) \Gamma(1 - z))$ as:

$$
\frac{1}{\Gamma(z) \Gamma(1 - z)} = z \prod_{n = 1}^\infty \left( 1 + \frac{-z}{n}\right) \left( 1 + \frac{z}{n}\right) = z \prod_{n = 1}^\infty \left( 1 - \frac{z^2}{n^2}\right)
$$

Multiplying by $\pi$ gives us the formula for $\sin \pi z$:

$$
\frac{\pi}{\Gamma(z) \Gamma(1 - z)} = \sin \pi z
$$

</proof>

The gamma function is a meromorphic function, with the non-positive integers as poles.

## Factorial

We can see the gamma function as a generalization of the factorial. If we plug 1 in $\Gamma(z)$ we get $\Gamma(1) = 1$.

<proof>

Replacing $z = 1$ in $(4)$ gives us:

$$
\Gamma(1) = e^{-\gamma} \prod_{n = 1}^\infty  \left( 1 + \frac{1}{n}\right)^{-1} e^{1 / n}
$$

Taking the logarithm:

$$
\log \Gamma(1) = -\gamma - \sum_{n = 1}^\infty  \log \left( 1 + \frac{1}{n}\right) + \sum_{n = 1}^\infty \frac{1}{n}
$$

We can express this as a limit:

$$
= \lim_{n \rightarrow \infty} -\gamma - \sum_{k = 1}^n  \log \left( 1 + \frac{1}{k}\right) + \sum_{k = 1}^n \frac{1}{k}
$$

<i>Lemma 1</i> tells us that

$$
\sum_{k = 1}^n  \log \left( 1 + \frac{1}{k}\right) = \log (n + 1)
$$

and <i>Lemma 2</i> that

$$
\lim_{n \rightarrow \infty} \sum_{k = 1}^n  \log \left( 1 + \frac{1}{k}\right) = \lim_{n \rightarrow \infty} \log (n)
$$

Going back to the development, we have:

$$
\log \Gamma(1) = \lim_{n \rightarrow \infty} -\gamma - \log (n) + \sum_{k = 1}^n \frac{1}{k}
$$

But recall that

$$
\gamma = \lim_{n \rightarrow \infty} \sum_{k = 1}^n \frac{1}{k} - \log (n)
$$

Meaning that

$$
\log \Gamma(1) = 0
$$

and hence $\Gamma(1) = 1$
</proof>

Thus, if $z \in \mathbb{N}$, the recurrence $\Gamma(z + 1) = z \Gamma(z)$ tells us that $\Gamma(z) = (z - 1)!$. The gamma function is more general than the factorial because we can plug in real numbers. For example, $\Gamma(1/2) = \sqrt{\pi}$.

<proof>
We can plug $z = 1/2$ in $(5)$ to obtain:

$$
\frac{\pi}{\Gamma(1/2)^2} = \sin \pi /2 = 1
$$

Thus:

$$
\Gamma(1/2)^2 = \pi
$$

</proof>

There's also a formula connecting $\Gamma(2z)$ and $\Gamma(z)$, known as *Legendre's duplication formula*, which we'll not prove here:

$$
\Gamma(2z) = \frac{\Gamma(z) \Gamma(z + 1/2) 2^{2z - 1}}{\sqrt{\pi}}
$$

## Conclusion

Like with [Weierstrass Factorization Theorem]({{blog}}/2025/07/02/weierstrass-factorization-theorem.html), this chapter was also one of the most enjoyable to read from Ahlfors [1]. The connection betweem seemingly unrelated things in math always makes me appreciate its beauty.

## Appendix

**Lemma 1.**

$$
\log (n + 1) = \sum_{k = 1}^{n} \log \left(1 + \frac{1}{k} \right)
$$

<proof>

We have

$$
\log \left(1 + \frac{1}{k} \right) = \log \left(\frac{k + 1}{k} \right) = \log (k + 1) - \log (k)
$$

So when we sum these terms, all intermediate terms will cancel (i.e. this is a telescopic sum), so:

$$
\sum_{k = 1}^{n} \log \left(1 + \frac{1}{k} \right) = \log (n + 1) - \log(1) = \log (n + 1)
$$

</proof>

**Lemma 2.**

$$
\lim_{n \rightarrow \infty} \log (n + 1) = \log(n)
$$

<proof>

We can use this identity from the proof of <i>Lemma 1</i>:

$$
\log \left(1 + \frac{1}{n} \right) = \log (n + 1) - \log (n)
$$

or:

$$
\log (n + 1) = \log \left(1 + \frac{1}{n} \right) + \log(n)
$$

As $n \rightarrow \infty$, $1/n \rightarrow 0$, so:

$$
\lim_{ n \rightarrow \infty} \log (n + 1) = \lim_{ n \rightarrow \infty} \log \left(1 + \frac{1}{n} \right) + \log(n) = \lim_{ n \rightarrow \infty} \log(n)
$$


</proof>

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2](({{blog}}/2025/07/02/weierstrass-factorization-theorem.html))] NP-Incompleteness: Weierstrass Factorization Theorem
* [[3](https://en.wikipedia.org/wiki/Gamma_function)] Wikipedia: Gamma function
