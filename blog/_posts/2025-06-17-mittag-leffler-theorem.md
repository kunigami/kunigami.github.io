---
layout: post
title: "Mittag-Leffler's Theorem"
tags: [analysis]
vanity: "2025-06-17-mittag-leffler-theorem"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/leffler.jpg" alt="Mittag-Leffler" />
</figure>

Magnus Gustaf "Gösta" Mittag-Leffler was a Swedish mathematician. After earning a PhD at Uppsala University, Mittag-Leffler attended lectures on elliptic functions from Charles Hermite in Paris and Karl Weierstrass in Berlin, which influenced his works.

Mittag-Leffler was an advocate of women's rights. He helped Sofia Kovalevskaya become full professor of mathematics in Stockholm, the first woman to do so in Europe. As a member of the Nobel Prize Committee, Mittag-Leffler was responsible for convincing the committee to include Marie Curie in the Nobel prize in physics, instead of just Pierre Curie.

In this post we'd like to study the *Mittag-Leffler's Theorem*.

<!--more-->

The theorem says that if we have a given set of numbers, there exists a function that is holomorphic everywhere except at the poles corresponding to these numbers. More precisely:

**Theorem 1.** Let $\Omega$ be an open set in $\mathbb{C}$ and $E$ a subset of $\Omega$ without limit points. Let $Q_a$ be a polynomial of the form:

$$Q_a(z) = \sum_{n = 1}^{N_a} \frac{c_{a, n}}{(z - a)^n}$$

Then there exists a meromorphic function $f(z)$ in $U$ defined as:

$$f(z) = \left(\sum_{k = 1}^{\infty} Q_{p_k}(z) - q_k\right) + g(z)$$

for $p_k \in E$, polynomials $q_k$ and a holomorphic function $g(z)$. The poles of this function corresponds to $E$. Additionally, $Q_{p_k}(z)$ is the singular part of the Laurent series expansion of $f(z)$ around $p_k$.

<proof>

Let's first get the easy cases out of the way. If $E$ is finite, then

$$(1.1) \quad \sum_{a \in E} Q_a(z)$$

is a function where the poles correspond to $a \in E$, since each $Q_a$ has $a$, and only $a$, as a pole. For $z \ne a$, $Q_a(z)$ does not have poles, so $Q_a(z)$ corresponds to the singular part of the Laurent series expansion of $f(z)$ around $a$.
<br /><br />
Now, if we assume $E$ is infinite, we can't use the same argument as before because $(1.1)$ is an infinite series which might not converge, so we need more care.
<br /><br />
To make the discussion easier, we label its elements $p_1, p_2, \cdots$ such that $\abs{p_1} \le \abs{p_1} \le \cdots$. Let's consider a given $p_k$. For $\abs{z} \lt \abs{p_k}$, we have that $Q_{k}$ is holomorphic, because $z \ne p_k$ and hence it has no poles. Thus it has a convergent Taylor series. Let $q_{k}$ be the first $d_k$ terms of the Taylor series expansion around 0.
<br /><br />
Define $M_k$ as the upper bound of $Q_{p_k}$ for $\abs{z} \lt \abs{p_k} / 2$, that is:

$$M_{k} = \sup_{\abs{z} \lt \abs{p_k} / 2} \abs{Q_{p_k}(z)}$$

According to <i>Lemma 2</i>, If we subtract $Q_{p_k}(z)$ by $q_k$, we get the following upperbound for $\abs{z} \lt \abs{p_k} / 4$:

$$\abs{Q_{p_k}(z) - q_k} \le M_k 2^{-d_k}$$

Since we have freedom to choose $d_k$, we can choose one such that $M_k 2^k \lt 2^{d_k}$ since $M_k$ and $k$ are independent from our choice of $d_k$. Thus we have:

$$\abs{Q_{p_k}(z) - q_k} \lt M_k 2^{-k}$$

Now define

$$(1.2) \quad f(z) = \sum_{k = 1}^{\infty} Q_{p_k}(z) - q_k$$

This is the meromorphic function we're looking for, but we need to prove it exists by showing that the series on the right hand side converges.
<br /><br />
Let $K$ be any compact set in $\Omega$ that doesn't contain any points of $E$. We can find $N$ such that the disk $\abs{z} \lt \abs{p_N} / 4$ contains $K$. Here we need to use the hypothesis that $E$ doesn't have a limit point, because we need $\lim_{k \rightarrow \infty} p_k = \infty$ to be able to find such $N$.
<br /><br />
For $z \in K$ we have that

$$\abs{f_n(z)} \le \sum_{k = n}^{\infty} \abs{Q_{p_k}(z) - q_k} \lt \sum_{k = n}^{\infty} 2^{-k}$$

converges for all $n \ge N$, which passes <i>Weierstrass M-test</i> [2]. Since $N$ is finite, the series

$$\sum_{k = 1}^{N} Q_{p_k}(z) - q_k$$

also converges. We conclude that $f(z)$ exists for any compact set in $\Omega$ that doesn't contain any points of $E$. For $p_k \in E$, we don't need to show that the series converges to $f(z)$. In fact $p_k$ is a pole for $Q_{p_k}(z)$, so the value of the series is infinity anyways.
<br /><br />
For any $p_k$, we have that the series form a Laurent series around $p_k$. Since $q_k$ are parts of a Taylor series, the only pole is from $Q_{p_k}(z)$ and thus it forms the singular / principal part (i.e. terms with negative powers) of the Laurent series around $p_k$.
<br /><br />
We can thus conclude that $f(z)$ is a meromorphic function in $\Omega$ with poles in $E$. If we add any holomorphic function $g(z)$ to $f(z)$, it preserves these properties.
</proof>

Note that we cannot combine the sum of $q_k$ (which are polynomials and holomorphic on their own) with $g(z)$ because the series of $q_k$ might not converge.

Intuitively the terms $q_k$ are used to keep the growth of $Q_{p_k}$ under control and make sure it converges. In the special case in which

$$\sum_{k = 1}^{\infty} Q_{p_k}(z)$$

is convergent in $\Omega \setminus E$, we can assume $f(z)$ takes the simpler form:

$$f(z) = \left(\sum_{k = 1}^{\infty} Q_{p_k}(z) \right) + g(z)$$


## Example

Suppose we want to find the series expansion of $f(z) = \pi^2 / \sin^2(\pi z)$. We can first find the poles of this expression and use the Mittag-Leffler theorem to find a series that converges to it given those poles.

The poles for this function are the points for which $\sin(\pi z) = 0$, which is any integer $n \in \mathbb{Z}$. So the set of poles is $\mathbb{Z}$, which does not have a limit point, a condition for us to use the Mittag-Leffler theorem.

From *Lemma 3*, the singular part of the Laurent expansion of $\pi^2 / \sin^2(\pi z)$ around a pole $n \in \mathbb{Z}$ is $1 / (z - n)^2$, so we have $Q_n(z) = 1 / (z - n)^2$. And we have that:

$$
\sum_{n \in \mathbb{Z}} Q_n(z) = \sum_{n \in \mathbb{Z}} \frac{1}{(z - n)^2}
$$

is uniformily convergent, which allows us to use the special case of the Mittag-Leffler's Theorem to conclude that:

$$
\frac{\pi^2}{\sin^2(\pi z)} = \left(\sum_{n \in \mathbb{Z}} \frac{1}{(z - n)^2}\right) + g(z)
$$

We now wish to show $g(z) = 0$. It's possible to show that if $z = x + iy$, then for $\abs{y} \rightarrow \infty$, both $\pi^2 / \sin^2(\pi z)$ and $\sum_{n \in \mathbb{Z}} 1 / (z - n)^2$ tend uniformily to 0 and thus if $x$ is bounded, say $0 \le x \le 1$, then $\abs{g(z)}$ is also bounded.

Since both $\pi^2 / \sin^2(\pi z)$ and $\sum_{n \in \mathbb{Z}} 1 / (z - n)^2$ are periodic with period 1, $\abs{g(z)}$ is bounded for any "strip" $n \le x \le n + 1$, and thus bounded in the whole plane. By Liouville's Theorem [1], this means $g(z)$ is a constant. And since we know $g(z)$ assumes the value $0$ for $\abs{y} \rightarrow \infty$, that constant must be 0.

We can thus conclude that:

$$
\frac{\pi^2}{\sin^2(\pi z)} = \sum_{n \in \mathbb{Z}} \frac{1}{(z - n)^2}
$$

## Appendix

**Lemma 2.** Let $Q_{p_k}$, $q_k$, $d_k$ and $M_k$ be as defined in the proof of *Theorem 1*. Then if $\abs{z} \lt \abs{p_k} / 4$:

$$\abs{Q_{p_k}(z) - q_k} \le M_k 2^{-d_k}$$

<proof>

Let's define $g_{p_k}(z)$ as $Q_{p_k}(z) - q_k(z)$ with domain $\abs{z} \lt \abs{p_k} / 4$.
<br /><br />
We already established that if $\abs{z} \lt \abs{p_k}$ then $Q_{p_k}(z)$ is holomorphic, so it has a Taylor series expansion and that $q_k$ is the first $d_k$ terms of it, so if we subtract, we're left with the remaining terms:

$$g_{p_k}(z) = \sum_{j = d_k + 1}^\infty c_j z^{j}$$

By Cauchy’s Estimate [1], we have that since $g_{p_k} \lt M_k$ in its domain and thus:

$$\abs{c_j} \le \frac{M_k}{r^j}$$

Where $r$ is the radius of a circle around the origin on which $g_{p_k}$ is holomorphic, so we can pick $r = \abs{p_k} / 2$, yielding:

$$\abs{c_j} \le \frac{2^j M_k}{p_k^j}$$

Since $\abs{z} \lt \abs{p_k} / 4$ we have:

$$\abs{c_j z^j} \le \frac{2^j M_k \abs{p_k}^j}{\abs{p_k}^j 4^j} = M_k 2^{-j}$$

We can sum the terms from $d_k + 1$ to infinity, which forms a geometric series:

$$S = 2^{-(d_k + 1)} + 2^{-(d_k + 2)} + 2^{-(d_k + 3)} + \cdots$$

and

$$2S = 2^{-d_k} + 2^{-(d_k + 1)} + 2^{-(d_k + 2)} + 2^{-(d_k + 3)} + \cdots$$

So $S = 2^{-d_k}$ and

$$\abs{g_{p_k}(z)} \le \sum_{j = d_k + 1}^\infty \abs{c_j z^{j}} \le M^k \sum_{j = d_k + 1}^\infty 2^{-j} = M^k 2^{-d_k}$$

</proof>

**Lemma 3.** The singular part of the Laurent expansion of $\pi^2 / \sin^2(\pi z)$ around $n \in \mathbb{Z}$ is $1 / (z - n)^2$ for $n \in \mathbb{Z}$.

<proof>

First we consider some point around the pole $n$, $z = n + \delta$. We have that [3]:

$$\sin(\pi z) = \sin(\pi + \pi \delta) = \sin(\pi n) \cos(\pi \delta) + \cos(\pi n) \sin(\pi \delta)$$

Since $\sin(\pi n) = 0$ and $\cos(\pi n) = (-1)^n$,

$$\sin(\pi z) = (-1)^n \sin(\pi \delta)$$

Squaring:

$$(3.1) \quad \sin^2(\pi z) = \sin^2(\pi \delta)$$

We consider the Taylor series expansion of $\sin^2(\pi \delta)$ around 0:

$$
\sin^2(\pi \delta) = \sum_{k = 0}^{\infty} (-1)^{k+1} \frac{2^{2k-1}(\pi \delta)^{2k}}{(2k)!} = (\pi \delta)^2 - \frac{2}{3}(\pi \delta)^4 + \frac{4}{45}(\pi \delta)^6 + \cdots
$$

We can rewrite the right hand side as:

$$
= (\pi \delta)^2 \left(1 - \frac{2}{3}(\pi \delta)^2 + \frac{4}{45}(\pi \delta)^4 + \cdots \right)
$$

Define

$$
A(\delta) = \frac{2}{3}(\pi \delta)^2 + \frac{4}{45}(\pi \delta)^4 + \cdots
$$

We have

$$
\sin(\pi \delta) = (\pi \delta)^2 (1 + A(\delta))
$$

Inverting:

$$\frac{1}{\sin^2(\pi \delta)} = \frac{1}{(\pi \delta)^2} \frac{1}{1 + A(\delta)}
$$

Since $\delta$ is small $A(w) \lt 1$ and we can write

$$
\frac{1}{1 + A(\delta)} = 1 - A(\delta) + A(\delta)^2 - A(\delta)^3 + \cdots
$$

The key part here is that $A(\delta)$ has a factor of $(\pi \delta)^2$,  so if we replace back:

$$
\frac{1}{\sin^2(\pi \delta)} = \frac{1}{(\pi \delta)^2} (1 - A(\delta) + A(\delta)^2 - A(\delta)^3 + \cdots)
$$

Only the first term has $\delta$ in the denominator, so we can assume the rest is some polynomial of $\delta$:

$$
\frac{1}{\sin^2(\pi \delta)} = \frac{1}{(\pi \delta)^2} + P(\delta)
$$

Replacing back $\delta = z - n$ and using $(3.1)$:

$$
\frac{1}{\sin^2(\pi z)} = \frac{1}{(\pi (z - n))^2} + P(z - n)
$$

multiplying by $\pi^2$:

$$
\frac{\pi^2}{\sin^2(\pi z)} = \frac{1}{(z - n)^2} + P(z - n)
$$

So we conclude that the singular part of $\pi^2 / \sin^2(\pi z)$ is $1/(z - n)^2$.
</proof>

## References

* [[1](https://www.kuniga.me/docs/math/holomorphism.html)] NP-Incompleteness - Holomorphic Functions Cheat Sheet
* [[2](https://www.kuniga.me/docs/math/series.html)] NP-Incompleteness - Series Cheat Sheet
* [[3](https://www.kuniga.me/docs/math/trigonometry.html)] NP-Incompleteness - Trigonometry Cheat Sheet
