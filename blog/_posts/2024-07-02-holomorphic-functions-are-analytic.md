---
layout: post
title: "Holomorphic Functions are Analytic"
tags: [analysis]
vanity: "2024-07-02-holomorphic-functions-are-analytic"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/weierstrass.png" alt="Portrait of Weierstrass" style="width: 100px;" />
</figure>

This is our sixth post in the series with my notes on complex integration, corresponding to *Chapter 4* in Ahlfors' Complex Analysis.

In this post however, we won't follow Ahlfors' book, but rather Terry Tao's notes on Complex Analysis [2] to show that holomorphic functions are analytic.

<!--more-->

The previous posts from the series:

1. [Complex Integration]({{blog}}/2024/04/05/complex-integration.html)
1. [Path-Independent Line Integrals]({{blog}}/2024/04/13/path-independent-line-integrals.html)
1. [Cauchy Integral Theorem]({{blog}}/2024/04/26/cachy-integral-theorem.html)
1. [The Winding Number]({{blog}}/2024/05/09/the-winding-number.html)
1. [Cauchy's Integral Formula]({{blog}}/2024-06-06-cauchy-integral-formula)

## Analytic Functions

An analytic function is a function that can be locally represented by a convergent power series. *Definition 1* provides a more formal definition:

**Definition 1.** Let $f: \Omega \rightarrow \mathbb{C}$ be a function. We say $f$ is **analytic in** $a$ if there's an open disk $D$ centered in $a$ such that for $z \in D$:

$$(1) \quad f(z) = \sum_{n = 0}^{\infty} {c_n} (z - a)^n$$

where $c_n \in \mathbb{C}$. For $f(z)$ to be defined, $(1)$ must converge.

**Definition 2.** Let $r$ be the radius of disk $D$, i.e. $D = \curly{x \in \Omega \mid \abs{z - a} < r}$. If $(1)$ converges when $\abs{z - a} < R$ and diverges when $\abs{z - a} > R$, we call $R$ the **radius of convergence**.

*Lemma 3* provides an explicit formula for the radius of convergence of $(1)$:

**Lemma 3.** Let $f(z)$ be a power series as in $(1)$. Then the radius of convergence is:

$$(3.1) \quad R = \liminf_{n \rightarrow \infty} \abs{c_n}^{-1/n}$$

assuming that $\abs{c_n}^{-1/n} = \infty$ if $c_n = 0$.

<proof>

Assume $\abs{z - a} \gt R$. By $(3.1)$, we have

$$\lim_{r \rightarrow \infty} \paren{\inf_{n \ge r} \abs{c_n}^{-1/n}} = R$$

So for all $n \ge r$, $\abs{c_n}^{-1/n} \le R \lt \abs{z - a}$, or that

$$\abs{c_n}^{-1/n} \lt \abs{z - a}$$

Multiplying by $\abs{c_n}^{1/n}$:

$$1 \lt \abs{c_n}^{1/n} \abs{z - a}$$

Since these are all positive terms, we can raise to the $n$-th power:

$$1 \lt \abs{c_n} \abs{z - a}^n = \abs{c_n (z - a)^n}$$

Which means as $r$ tends to infinity, $\abs{c_n (z - a)^n}$ is greater than 0, so if $\abs{z - a} \gt R$, then the power series $(1)$ does not converge.
<br /><br />
Now suppose $\abs{z - a} \lt R$. Choose $\abs{z - a} \lt R' \lt R$. Then we claim that as $r \rightarrow \infty$, $\abs{c_n}^{-1/n} \gt R'$. Otherwise, we'd have $\abs{c_n}^{-1/n} \le R' \lt R$ which contradicts the hypothesis that $R$ is a lower bound for this term. So we have:

$$\abs{c_n}^{-1/n} \gt R'$$

Inverting this expression gives us:

$$\abs{c_n}^{1/n} \lt \frac{1}{R'}$$

Multiplying by the positive term $\abs{z - a}$:

$$\abs{c_n}^{1/n} \abs{z - a} \lt \frac{\abs{z - a}}{R'}$$

Raising to the $n$-th power:

$$\abs{c_n} \abs{z - a}^n \lt \paren{\frac{\abs{z - a}}{R'}}^n$$

Putting everything inside the modulus:

$$\abs{c_n (z - a)}^n \lt \paren{\frac{\abs{z - a}}{R'}}^n$$

Because $R' \gt \abs{z- a}$ the geometric series

$$\sum_{n = 0}^{\infty} \paren{\frac{\abs{z - a}}{R'}}^n$$

is absolutely convergent and hence

$$\sum_{n = 0}^{\infty} \abs{c_n (z - a)}^n$$

is also absolutely convergent. Summarizing, we proved that if $\abs{z - a} < R$, the power series $(1)$ converges and if $\abs{z - a} > R$ it diverges, thus $R$ is the radius of convergence by definition.

</proof>

Intuitively speaking, for the series to converge, each term $c_n \abs{z - a}^n$ must tend towards 0, so $\abs{z - a}$ must be smaller than $1/c_n^{1/n}$, otherwise the series either goes to infinity or "oscilates" around some value. So the necessity of this makes sense. The sufficiency is less obvious though!

## Holomorphic Functions

*Theorem 4* proves one of the most important results in complex analysis, that holomorphic functions are analytic:

**Theorem 4.** Let $f: \Omega \rightarrow \mathbb{C}$ be a holomorphic function. Let $r \gt 0$ such that the closed disk $D$, $\abs{z - a} \le r$, is contained in $\Omega$. Let $C$ be a circle of radius $r$ centered in $a$. Define $c_n$ as:

$$(4.1) \quad c_n = \frac{1}{2\pi i} \int_C \frac{f(z)}{(z - a)^{n+1}} dz$$

Then the power series

$$(4.2) \quad \sum_{n = 0}^{\infty} c_n (z - a)^n$$

converges to $f(z)$ if $\abs{z - a} \lt r$ and hence is analytic in $a$.

**Outline of the proof.** First we prove that the radius of convergence of $c_n$ is $r$. Then we pick a point $w$ inside the circle $\abs{z - a} \lt r$ and show that $(4.2)$ converges to $f(w)$.

To do so, we express $f(w)$ as a function of $f(z)$, $z \in C$, using Cauchy's integral formula [6],

$$f(w) = \frac{1}{2\pi i} \int_{C} \frac{f(z)}{z - w} dz$$

The most clever step on the proof is to expand the denominator $1/(z - w)$ into an infinite geometric series. This makes $f(w)$ almost look like $(4.2) + (4.1)$:

$$f(w) = \frac{1}{2\pi i} \int_{C} \sum_{n = 0}^\infty \frac{f(z) (w - a)^n}{(z - a)^{n+1}} dz$$

Except that the integral and sum are in a different order. Swaping sum and integrals are commutative when they're finite, but since we have an infinite sum, proving we can swap them is non-trivial, and is actually the most complicated part of the proof! But once we do, rearranging terms gives us what we want!

<proof>

Since $D$ is a closed disk, it's a compact set [4] and because holomorphic functions are continuous, it can be shown that $f$ itself is also bounded in $D$ [4]. Thus, let $\abs{f(z)} \le M$ for some finite $M \in \mathbb{R}$. For points $z \in C$, $\abs{z - a} = r$, so

$$(4.3) \quad \abs{\frac{f(z)}{(z - a)^{n+1}}} \le \frac{M}{r^{n + 1}}$$

Taking the modulus of $(4.1)$:

$$\abs{c_n} = \frac{1}{2\pi} \abs{\int_C \frac{f(z)}{(z - a)^{n+1}} dz}$$

Using <i>Lemma 5 (Appendix)</i> and the fact that $\abs{C}$ is the circumference of the circle of radius $r$, that is $2\pi r$,

$$\abs{c_n} \le \frac{1}{2\pi} 2\pi r \frac{M}{r^{n + 1}} = \frac{M}{r^n}$$

From <i>Lemma 3</i>, the radius of convergence $R$ is:

$$R = \liminf_{n \rightarrow \infty} \abs{c_n}^{-1/n} = \liminf_{n \rightarrow \infty} \frac{r}{M^{1/n}}$$

As $n$ approaches infinity, $M^{1/n}$ tends to 1, and since $r$ is constant, we have $R = r$. So we proved the radius of convergence of $(4.2)$ is $r$. It remains to show it converges to $f(z)$ when $\abs{z - a} \lt r$, i.e. for any point inside $D$.
<br /><br />
Let $w$ be such a point inside $D$. Since the circle $C$ is $n(C, w) = 1$ we can apply Cauchy's integral formula [6] to $w$:

$$(4.4) \quad f(w) = \frac{1}{2\pi i} \int_{C} \frac{f(z)}{z - w} dz$$

Since $w$ is a point in the interior of $C$ and $z$ is on the circumference of $C$, we have $\abs{w - a} \lt \abs{z - a}$ and we can use <i>Lemma 6</i> (<i>Appendix</i>) to conclude:

$$\frac{1}{z - w} = \sum_{n = 0}^\infty \frac{(w - a)^n}{(z - a)^{n+1}}$$

Replacing this in $(4.4)$:

$$f(w) = \frac{1}{2\pi i} \int_{C} \sum_{n = 0}^\infty \frac{f(z) (w - a)^n}{(z - a)^{n+1}} dz$$

We are allowed to move $f(z)$ inside the sum because it's invariant with $n$. From $(4.3)$, we have that the integrant is bounded, more precisely:

$$\abs{\frac{f(z)(w - a)^n}{(z - a)^{n+1}}} \le \frac{M \abs{(w - a)}^n}{r^{n+1}}$$

Since $\abs{w - a} \lt r$, $\abs{(w - a)}^n / r^{n+1} \lt 1$ and thus the geometric series:

$$\sum_{n = 0}^{\infty} \frac{M \abs{(w - a)}^n}{r^{n+1}}$$

is finite. Then, by *Lemma 9 (Appendix)* we can swap the integral and the sum we get:

$$f(w) = \frac{1}{2\pi i} \sum_{n = 0}^\infty \int_{C} \frac{f(z) (w - a)^n}{(z - a)^{n+1}} dz$$

Since $(w - a)^n$ is independent of $z$ we can move it out of the integral and move the outer constant inside:

$$f(w) = \sum_{n = 0}^\infty (w - a)^n \paren{\frac{1}{2\pi i} \int_{C} \frac{f(z)}{(z - a)^{n+1}} dz}$$

From $(4.1)$, the second factor on the summand is $c_n$:

$$f(w) = \sum_{n = 0}^\infty (w - a)^n c_n$$

</proof>

## Taylor series

In *Cauchy's Integral Formula*, we showed that a holomorphic function is infinitely differentiable and from *Theorem 4* so are analytic functions. The explicit formula is:

$$f^{(n)}(a) = \frac{n!}{2\pi i} \int_\gamma \frac{f(z)}{(z - a)^{n+1}} dz$$

It looks very similar to the definition $c_n$ $(4.1)$ except for the factorial factor. Indeed we can see that:

$$f^{(n)}(a) = n!c_n$$

or

$$c_n = \frac{f^{(n)}(a)}{n!}$$

Replacing this in $(4.2)$ we get:

$$f(z) = \sum_{n=0}^{\infty} \frac{f^{(n)}(a)}{n!} (z - a)^n$$

Which is the exact definition of the [Taylor series](https://en.wikipedia.org/wiki/Taylor_series)!

## Conclusion

My goal was to write about [removable singularities](https://en.wikipedia.org/wiki/Removable_singularity) but I got stuck in understanding the proofs in Ahlfors. I found Tao's notes more clear, but they follow different approaches. Tao builds the concept of removable singularities on top of *analytic functions*, while Ahlfors delays discussing them until much later.

Thus, in order to continue my progression on Complex Analysis, I decided to write about analytic functions first, which builds on top of *Cauchy's Integral Formula*, so I think it's a reasonable time to do so.

I found that the proofs from Tao's notes rely a lot more on results from real analysis applied to the complex domain. An indication of this is the number of items in *References*, I had to refresh a bunch of concepts, especially regarding series and convergence.

## Appendix

Here we prove general results used by the main proofs in the post, but that would get in the way if inline with the proof itself.

**Lemma 5.** Let $\gamma$ be a curve and $\abs{f(z)} \le M$ for $z \in \gamma$. Then

$$\abs{\int_{\gamma} f(z) dz} \le M \abs{\gamma}$$

<proof>

This is almost a direct consequence of <i>Theorem 4</i> in [3], which claims:

$$\abs{\int_{\gamma} f(z) dz} \le \int_{\gamma} \abs{f(z)} \abs{dz}$$

Using the hypothesis $\abs{f(z)} \le M$,

$$\le \int_{\gamma} M \abs{dz} = M \int_{\gamma} \abs{dz}$$

The last integral corresponds to the length of the curve, $\abs{\gamma}$.

</proof>

**Lemma 6.** Let $a, z, w \in \mathbb{C}$ be distinct values, with $\abs{w - a} \lt \abs{z - a}$. Then:

$$\frac{1}{z - w} = \sum_{n = 0}^\infty \frac{(w - a)^n}{(z - a)^{n+1}}$$

<proof>

Let $v = (w - a)/(z - a)$. Since $\abs{v} \lt 1$, the geometric series

$$S_n = \sum_{n = 0}^\infty v^n$$

Converges to:

$$S_n = \frac{1}{1 - v}$$

Replacing $v$:

$$S_n = \frac{1}{1 - \frac{w - a}{z - a}} = \frac{1}{\frac{z - a - (w - a)}{z-a}} = \frac{z - a}{z -w}$$

Thus

$$\sum_{n = 0}^\infty \frac{(w - a)^n}{(z - a)^{n}} = \frac{z - a}{z -w}$$

or that

$$\frac{1}{z - w} = \sum_{n = 0}^\infty \frac{(w - a)^n}{(z - a)^{n+1}}$$

</proof>

**Lemma 7.** Let $(f_k(z))$ be a sequence of Darboux integrable functions that converges uniformily to $f(z)$. Then $\int_{\gamma} f(z)dz$ is Darboux integrable and:

$$\lim_{k \rightarrow \infty} \int_{\gamma} f_k(z)dz = \int_{\gamma} f(z)dz$$

<proof>

Since $(f_k(z))$ converges uniformily to $f(z)$, for any $\epsilon$, there is $N$ such that for all $k \ge N$,

$$\abs{f_k(z) - f(z)} \lt \epsilon$$

from this we can say that:

$$\abs{f_k(z)} - \epsilon \lt \abs{f(z)} \lt \abs{f_k(z)} + \epsilon$$

since each $f_k$ is Darboux integrable, the upper and lower Darboux integrals, $U_k$ and $L_k$, exist and are equal to $\int_{\gamma} f_k(z)dz$. We have the <i>upper and lower Darboux sums</i> for $f(z)$ and some partition $P$ as:

$$\begin{align}
U(f,P) &= \sum_{i = 1}^n M_{i} (\abs{z(t_{i-1}) - z(t_{i})}) \\
L(f,P) &= \sum_{i = 1}^n m_{i} (\abs{z(t_{i-1}) - z(t_{i})})
\end{align}
$$

with

$$\begin{align}
M_{i} &= \sup \curly{\abs{f(z(t))} : t \in [t_{i-1}, t_{i}]} \\
m_{i} &= \inf \curly{\abs{f(z(t))} : t \in [t_{i-1}, t_{i}]}
\end{align}
$$

Since $f(z)$ is within $\epsilon$ of $f_k(z)$, so $\abs{M_{k,i} - M_{i}} \lt \epsilon$ and $\abs{m_{k,i} - m_{i}} \lt \epsilon$ and thus $\abs{U(f_k,P) - U(f,P)} \lt \abs{\gamma} \epsilon$ and $\abs{L(f_k,P) - L(f,P)} \lt \abs{\gamma} \epsilon$ for any partition $P$.

Thus $\abs{U - U_k} \lt \abs{\gamma} \epsilon$ and $\abs{L - L_k} \lt \abs{\gamma} \epsilon$. Since we can pick arbitrarily small $\epsilon$, we have that

$$U = \lim_{k \rightarrow \infty} U_k = \lim_{k \rightarrow \infty} L_k = L$$

and thus $f(z)$ is Darboux integrable and equal to $\lim_{k \rightarrow \infty} \int_{\gamma} f_k(z)dz$.

QED.

</proof>

**Lemma 8.** Let $f_k(z)$ be integrable functions such that

$$\abs{f_k(z)} \le M_k$$

Such that $\sum_{n = 0}^{\infty} M_k$ converges. Then

$$\abs{\int_{\gamma} f_k(z)} \lt N_k$$

Where $\sum_{n = 0}^{\infty} N_k$ also converges.

<proof>

From <i>Lemma 5</i> we have that

$$\abs{\int_{\gamma} f_k(z)} \le M_k \abs{gamma}$

Thus setting $N_k = M_k \abs{gamma}$ satisfies the first constaint and that

$$\sum_{n = 0}^{\infty} N_k = \abs{\gamma} \sum_{n = 0}^{\infty} M_k$$

So if $\sum_{n = 0}^{\infty} M_k$ converges to a limit $L$, $\sum_{n = 0}^{\infty} N_k$ comverges to $LM$.

</proof>


**Lemma 9.** Let $f_k(z)$ be Darboux integrable functions such that

$$\abs{f_k(z)} \lt M_k$$

and that $\sum_{n = 0}^{\infty} M_k$ converges. Then we can exchange the order of the integral and the sum:

$$\sum_{k=0}^\infty \int_\gamma f_k(z) dz = \int_\gamma \sum_{k=0}^\infty f_k(z) dz$$

<proof>

Since

$$\abs{f_k(z)} \lt M_k$$

and that $\sum_{n = 0}^{\infty} M_k$ converges, we can use <a href="https://en.wikipedia.org/wiki/Weierstrass_M-test">Weierstrass M-test</a> to conclude that the series

$$\sum_{k=0}^\infty f_k(z)$$

converges uniformly. In other words, if we have the partial sum:

$$(9.E) \quad s_n(z) = \sum_{k=0}^n f_k(z)$$

then the M-test says that the sequence $(s_n(z))$ converges uniformily to some function $s(z)$. Uniform convergence implies pointwise convergence and hence:

$$(9.F) \quad \lim_{n \rightarrow \infty} s_n(z) = s(z) = \sum_{k=0}^\infty f_k(z)$$

Since each $f_k(z)$ is integrable, their finite sum is also integrable, so we can conclude by <i>Lemma 7</i> that $s(z)$ is integrable and that:

$$\lim_{n \rightarrow \infty} \int_{\gamma} s_n(z)dx = \int_{\gamma} s(z)dz$$

Replacing $s_k$ with $(9.E)$ and $s$ with $(9.F)$:

$$(9.C) \quad \lim_{n \rightarrow \infty} \int_{\gamma} \sum_{k=0}^n f_k(z) dz = \int_{\gamma} \sum_{k=0}^\infty f_k(z) dz$$

For finite terms we can exchange the order of an integral and sum, so we have:

$$(9.A) \quad \int_\gamma \sum_{k=0}^n f_k(z) dz = \sum_{k=0}^n \int_\gamma f_k(z) dz$$

Replacing in $(9.C)$:

$$(9.D) \quad \lim_{n \rightarrow \infty} \sum_{k=0}^n \int_{\gamma} f_k(z) dz = \int_{\gamma} \sum_{k=0}^\infty f_k(z) dz$$

By <i>Lemma 8</i> the series of the integrals of $f_k$ also satisfies the Weierstrass M-test and hence converges uniformily, so the limit from $(9.D)$ exists:

$$\lim_{n \rightarrow \infty} \sum_{k=0}^n \int_{\gamma} f_k(z) dz = \sum_{k=0}^\infty \int_{\gamma} f_k(z) dz$$

Replacing in $(9.D)$:

$$\sum_{k=0}^\infty \int_\gamma f_k(z) dz = \int_{\gamma} \sum_{k=0}^\infty f_k(z) dz$$

QED.

</proof>

*Lemma 9* shows that we can swap a series and an integral if the functions $f_k(z)$ satisfy the Weierstrass M-test. Tao's notes and many sites I found suggest it's possible to show the same if $f_k(z)$ are uniformily convergent.

Satisfying Weierstrass M-test implies uniform convergence but not the other way around. So *Lemma 9* is a weaker version of the result based on uniform convergence, but I couldn't prove or find a clear proof ([10] has an answer but I don't understand why that proves the claim).

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2](https://terrytao.wordpress.com/2016/10/02/math-246a-notes-3-cauchys-theorem-and-its-consequences/)] What's new - Math 246A, Notes 3: Cauchy’s theorem and its consequences
* [[3]({{blog}}/2024/04/05/complex-integration.html)] NP-Incompleteness: Complex Integration
* [[4]({{docs}}/math/topology.html)] NP-Incompleteness: Topology Cheat Sheet
* [[5]({{docs}}/math/complex_power_series.html)] NP-Incompleteness: Complex Power Series Cheat Sheet
* [[6]({{blog}}/2024/06/06/cauchy-integral-formula.html)] NP-Incompleteness: Cauchy's Integral Formula
* [[7]({{docs}}/math/series.html)] NP-Incompleteness: Series Cheat Sheet
* [[8]({{docs}}/math/sequences.html)] NP-Incompleteness: Sequences Cheat Sheet
* [[9]({{blog}}/2023/12/21/holomorphic-functions.html)] NP-Incompleteness: Holomorphic Functions
* [[10](https://math.stackexchange.com/questions/3372373/proof-that-uniform-convergence-allows-the-sum-and-integral-signs-to-be-exchanged)] Mathematics: Proof that uniform convergence allows the sum and integral signs to be exchanged
