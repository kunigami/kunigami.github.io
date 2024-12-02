---
layout: post
title: "Cauchy's Integral Formula"
tags: [analysis]
vanity: "2024-06-06-cauchy-integral-formula"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}


<figure class="image_float_left">
  <img src="{{resources_path}}/thumbnail.jpeg" alt="A still life painting of a beaker" />
</figure>

This is our fifth post in the series with my notes on complex integration, corresponding to *Chapter 4* in Ahlfors' Complex Analysis.

We'll focus on Cauchy's Integral Formula, which is not only a tool on itself for solving some types of integrals but also a stepping stone for several other results including: *Morera's theorem*, *Cauchy's Estimate* and *Liouville's Theorem*.

<!--more-->

The previous posts from the series:

1. [Complex Integration]({{blog}}/2024/04/05/complex-integration.html)
1. [Path-Independent Line Integrals]({{blog}}/2024/04/13/path-independent-line-integrals.html)
1. [Cauchy Integral Theorem]({{blog}}/2024/04/26/cachy-integral-theorem.html)
1. [The Winding Number]({{blog}}/2024/05/09/the-winding-number.html)

## Recap

In the previous post, [The Winding Number]({{blog}}/2024/05/09/the-winding-number.html) [3], we explored the winding number as:

$$n(\gamma, a) = \frac{1}{2\pi i} \int_{\gamma} \frac{dz}{z - a}$$

Which can be interpreted as the number of revolutions a closed curve $\gamma$ performs around a point $a$ not on it. Of particular interest is the case where $n(\gamma, a) = 1$.

In the post before that, [Cauchy Integral Theorem
]({{blog}}/2024/04/26/cachy-integral-theorem.html) [2], we learned that if $f(z)$ is holomorphic, then, under some conditions

$$\int_{\gamma} f(z)dz = 0$$

These results will be crucial to the main topic of this post: *Cauchy's Integral Formula*.

## Definition

What happens if we try to apply the Cauchy integral theorem to this function:

$$(1) \quad F(z) = \frac{f(z) - f(a)}{z - a}$$

for some $a$ not on the curve $\gamma$? We can interpret this function as a rate of change (how much f(z) changes when $z$ does). In fact, notice that $\lim_{z \rightarrow a} F(z)$ is essentially $f'(a)$.

We'll see that $F(z)$ is holomorphic except at $z = a$, but this "singularity" is acceptable under the Cauchy integral theorem and we can still conclude that

$$\int_{\gamma} F(z)dz = 0$$

from this we're be able to derive *Cauchy's Integral Formula*.

**Lemma 1 (Cauchy's Integral Formula).** Let $f(z)$ be holomorphic in an open disk $\Delta$, and a closed curve $\gamma$ in $\Delta$ and a point $a \in \Delta$ not on $\gamma$ and such that $n(\gamma, a) = 1$. Then:

$$(2) \quad f(a) = \frac{1}{2\pi i} \int_{\gamma} \frac{f(z)}{z - a} dz$$

<proof>
Consider the following function:

$$(1.1) \quad F(z) = \frac{f(z) - f(a)}{z - a}$$

For $z \in \Delta$. Since $f(z)$ and $f(a)$ are holomorphic and holomorphism is invariant with subtraction and division by a non-zero value, $F(z)$ is holomorphic in $\Delta$ except at $a$.

However, we have that:

$$\lim_{z \rightarrow a} F(z) (z - a) = \lim_{z \rightarrow a} f(z) - f(a) = 0$$

Which allows us to use Cauchy Integral Theorem (<i>Theorem 4</i> in [2]). To conclude that:

$$\int_{\gamma} F(z)dz = 0$$

Replacing by $(1.1)$:

$$\int_{\gamma} \frac{f(z) - f(a)}{z - a} dz = 0$$

Separating into two integrals:

$$\int_{\gamma} \frac{f(z)}{z - a} dz = \int_{\gamma} \frac{f(a)}{z - a} dz$$

Since $a$ is not in $\gamma$, $f(a)$ is constant, and we can do:

$$\int_{\gamma} \frac{f(z)}{z - a} dz = f(a) \int_{\gamma} \frac{1}{z - a} dz$$

The integral on the right hand side is the winding number [3], $n(\gamma, a)$, multiplied by $2\pi i$, so we can re-arrange as:

$$(1.2) \quad f(a) n(\gamma, a) = \frac{1}{2\pi i}\int_{\gamma} \frac{f(z)}{z - a} dz$$

If $a$ is outside $\Delta$, then by <i>Lemma </i> in [3], we have $n(\gamma, a) = 0$. We also have that $z \ne a$ since $z \in \Delta$ and thus

$$\frac{f(z)}{z - a}$$

is holomorphic in $\Delta$ and by <i>Theorem 3</i>,

$$\int_{\gamma} \frac{f(z)}{z - a} dz = 0$$

Which means $(1.2)$ still holds even if we drop our initial assumption that $a \in \Delta$.

In the particular case that $\gamma$ winds around $a$ exactly once and thus $n(\gamma, a) = 1$, we have:

$$f(a) = \frac{1}{2\pi i} \int_{\gamma} \frac{f(z)}{z - a} dz$$

</proof>

This formula is useful because it enables us to compute the value of $f$ at any point $a$ inside a simple curve $\gamma$ if we know how to compute $f(z)/(z - a)$ at its boundaries!

### Example

We can also compute the integral $f(z)/(z - a)$ on the boundary of a simple curve from a point inside it. Let's look at an example. Suppose we want to compute:

$$\int_{\abs{z} = 1} \frac{e^z}{z} dz$$

Let $f(z) = e^z$ and $a = 0$. Then $F(z)$ as in $(1)$ is holomorphic in the disk $\abs{z} \lt 2$ except at $a$. This allows us to use $(2)$ where $\gamma$ is $\abs{z} = 1$ (since $n(\gamma, a) = 1$ for any point inside a circle):

$$e^{0} = 1 = \frac{1}{2\pi i}\int_{\abs{z} = 1} \frac{e^z}{z} dz$$

Thus:

$$\int_{\abs{z} = 1} \frac{e^z}{z} dz = 2\pi i$$

## Infinite Differentiability

Recall from [6], that a complex derivative is given by:

$$f'(z) =  \lim_{h \rightarrow 0} \frac{f(z + h) - f(z)}{h}$$

where $h$ is a complex number. We want to prove that if $f'(z)$ exists, then $f^{''}(z)$ exists, i.e. it's infinitely differentiable. We'll go further and provide an explicit formula for the $n$-th derivative  (equation $(4)$). The proof can be derived from Cauchy's Integral Formula.

Before that however, we'll need an auxiliary lemma:

**Lemma 2.** Let $f(z)$ be a continuous function on the closed curve $\gamma$. Then

$$(3) \quad F_n(a) = \int_{\gamma} \frac{f(z)}{(z - a)^n} dz$$

<p>
is holomorphic in each of the regions determined by $\gamma$ and the derivative is $F'_n(a) = n F_{n + 1}(a)$.
</p>

<proof>
We'll prove by induction, starting with the base $n = 1$. So our goal is to to show $F_1$ is holomorphic and that $F'_1(a) = F_2(a)$ for $a \not\in \gamma$.<br /><br />
We'll first show that $F_1$ is continuous at any point $a_0$. One way to show this is that for every $\epsilon > 0$, we can find a neighborhood around $a_0$, $\abs{a - a_0} \lt \delta$, for which $\abs{F_1(a) - F_1(a_0)} \lt \epsilon$. Let's compute $F_1(a) - F_1(a_0)$, by first replacing $(3)$:

$$F_1(a) - F_1(a_0) = \int_{\gamma} \frac{f(z)}{z - a} dz - \int_{\gamma} \frac{f(z)}{z - a_0} dz$$

Grouping under one integral and normalizing by a common denominator:

$$ = \int_{\gamma} \frac{f(z) ((z - a_0) - (z - a))}{(z - a)(z - a_0)} dz $$

Cancelling terms and moving constant factors out:

$$(2.1) \quad F_1(a) - F_1(a_0) = (a - a_0) \int_{\gamma} \frac{f(z)}{(z - a)(z - a_0)} dz $$

We'll find a relationship between $\epsilon$ and $\delta$, so then for any $\epsilon$ we're given, we'll know how to pick $\delta$. We start by choosing $\delta \gt 0$ such that the open disk around $a_0$, $\abs{a - a_0} \lt \delta$ doesn't cross with $\gamma$. Now consider the inner circle $\abs{a - a_0} \lt \delta / 2$. If we restrict $a$ to be in there, we have:

$$(2.2) \quad \abs{z - a} \gt \delta / 2$$

for $z \in \gamma$. To see why, first check <i>Figure 2.1</i>. The closest point $z$ to $a_0$ must still be closer than $\delta$ because we said the circle doesn't cross $\gamma$. It thus cannot be closer than $\delta / 2$ to any point $a$ on the inner circle. By a similar observation we can conclude that:

$$(2.3) \quad \abs{z - a_0} \gt \delta$$

<figure class="center_children">
  <img src="{{resources_path}}/neighbor.svg" alt="See caption."  style="width: 300px;" />
  <figcaption>Figure 2.1</figcaption>
</figure>

Let's find $\abs{F_2(a_0) - F_1'(a_0)}$, by replacing $F_2$ by $(3)$ and $F_1'$

We now compute $\abs{F_1(a) - F_1(a_0)}$ by replacing $(2.B)$:

$$\abs{F_1(a) - F_1(a_0)} = \abs{(a - a_0) \int_{\gamma} \frac{f(z)}{(z - a)(z - a_0)} dz}$$

Modulus being invariant with multiplication:

$$= \abs{a - a_0} \abs{\int_{\gamma} \frac{f(z)}{(z - a)(z - a_0)} dz}$$

We use <i>Theorem 1</i> from [4] to obtain:

$$\le \abs{a - a_0} \int_{\gamma} \abs{\frac{f(z)}{(z - a)(z - a_0)}} \abs{dz}$$

Again, modulus being invariant with multiplication and division:

$$= \abs{a - a_0} \int_{\gamma} \frac{\abs{f(z)}}{\abs{z - a}\abs{z - a_0}} \abs{dz}$$

We now use $(2.2)$, $\abs{z - a} \gt \delta / 2$ or that

$$\frac{1}{\abs{z-a}} \lt \frac{2}{\delta}$$

and from $(2.3)$, $\abs{z - a_0} \gt \delta$ or that

$$\frac{1}{\abs{z-a_0}} \lt \frac{1}{\delta}$$

Going back to the main equation, we get:

$$\abs{F_1(a) - F_1(a_0)} \lt \abs{a - a_0} \frac{2}{\delta^2} \int_{\gamma} \abs{f(z)} \abs{dz}$$

Since $\abs{a - a_0} \lt \delta / 2$,

$$\abs{F_1(a) - F_1(a_0)} \lt \frac{1}{\delta} \int_{\gamma} \abs{f(z)} \abs{dz}$$

Since the curve $\gamma$ is fixed, the integral is a constant $k$, so if we call:

$$\epsilon = \frac{1}{\delta} \int_{\gamma} \abs{f(z)} \abs{dz} = \frac{k}{\delta}$$

Then we have that for any $\epsilon \gt 0$, we can choose $\delta$ as:

$$\delta = \frac{k}{\epsilon}$$

such that for $\abs{a - a_0} \lt \delta / 2$ (i.e. points in the inner circle), we'll get $\abs{(F_1(a) - F_1(a_0))} \lt \epsilon$, which means $F_1(a_0)$ is continuous.

Now, let $g(z) = f(z) / (z - a_0)$, with

$$G_n(a) = \int_\gamma \frac{g(z)}{(z - a)^n} dz$$

For $n = 1$, $G_1(a)$ is the integral in $(2.1)$, so we can write:

$$F_1(a) - F_1(a_0) = (a - a_0) G_1(a)$$

If we divide by $(a - a_0)$ and take the limit of $a$ to $a_0$, we get $F'_1(a_0)$:

$$F'_1(a_0) = \lim_{a \rightarrow a_0} \frac{F_1(a) - F_1(a_0)}{a - a_0} = \lim_{a \rightarrow a_0} G_1(a)$$


Since we didn't specify what $f(z)$ was when showing $F_1$ is continuous, we can use $g(z)$ instead and conclude that $G_1$ is also continuous. This means that

$$\lim_{a \rightarrow a_0} G_1(a) = G_1(a_0) = \int_\gamma \frac{g(z)}{(z - a_0)} dz = \int_\gamma \frac{f(z)}{(z - a_0)^2} dz = F_2(a_0)$$

So we conclude that $F'_1(a_0) = F_2(a_0)$. Note, again, that since we didn't specify what $f(z)$ was, we can use $g(z)$ instead and conclude that $G'_1(a_0) = G_2(a_0)$.
<br /><br />

This was the inductive basis. The inductive hypothesis is that we'll assume $F_{n-1}$ is holomorphic and $F'_{n-1} = n F_{n}$ (this hypothesis is applicable to $G_n$ as well). We need to prove that $F_{n}$ is holomorphic and $F'_{n} = n F_{n + 1}$.

Let's compute $F_n(a) - F_n(a_0)$ by replacing their definition $(3)$:

$$(2.4) \quad F_n(a) - F_n(a_0) = \int_\gamma \frac{f(z)}{(z - a)^n}dz - \int_\gamma \frac{f(z)}{(z - a_0)^n}dz$$

We'll use the follow identity:

$$\frac{1}{(z - a)^n} = \frac{1}{(z - a)^{n-1} (z - a_0)} + (a - a_0) \frac{1}{(z - a)^n (z - a_0)}$$

Replacing this in $(2.4)$:

$$ = \paren{\int_\gamma \frac{f(z)}{(z - a)^{n-1} (z - a_0)}dz - \int_\gamma \frac{f(z)}{(z - a_0)^n}dz} + (a - a_0) \int_\gamma \frac{f(z)}{(z - a)^n (z - a_0)} dz$$

The first integral is $G_{n-1}(a)$, the second is $G_{n-1}(a_0)$:

$$(2.5) \quad F_n(a) - F_n(a_0) = G_{n-1}(a) - G_{n-1}(a_0) + (a - a_0) \int_\gamma \frac{f(z)}{(z - a)^n (z - a_0)} dz$$

We wish to show $F_n$ is continuous at $a_0$. In other words, that there exists $\delta \gt 0$ such that $\abs{a - a_0} \lt \delta$ implies $\abs{F_n(a) - F_n(a_0)} \lt \epsilon$ for any $\epsilon \gt 0$. As before, we'll do backwards and find a $\delta$ that makes

$$\abs{F_n(a) - F_n(a_0)} = \abs{G_{n-1}(a) - G_{n-1}(a_0)} + \abs{a - a_0} \int_\gamma \frac{\abs{f(z)}}{\abs{z - a}^n \abs{z - a_0}} \abs{dz} \lt \epsilon$$

Since $G_{n-1}$ is differentiable at $a_0$ (by hypothesis), it's continuous and thus there is $\delta_1 \gt 0$ such that $\abs{a - a_0} \lt \delta_1$ implies

$$\abs{G_{n-1}(a) - G_{n-1}(a_0)} \lt \epsilon_1$$

Using $(2.2)$ and $(2.3)$ as before, we can conclude that:

$$\int_\gamma \frac{\abs{f(z)}}{\abs{z - a}^n \abs{z - a_0}} \abs{dz} \lt \frac{2^{n}}{\delta^{n+1}} \int_\gamma \abs{f(z)}\abs{dz} = \frac{2^{n}}{\delta^{n+1}} k$$

If we call $\epsilon_2 = (2^{n}k)/\delta^{n+1}$ we know how to pick $\delta$ to obtain that bound. Choosing $\epsilon_1 = \epsilon_2 = \epsilon / 2$, and using $\abs{a - a_0} \lt \min(\delta_1, \delta)$ should give us $\abs{F_n(a) - F_n(a_0)} \lt \epsilon_1 + \epsilon_2 = \epsilon$.
<br /><br />
So $F_n$ is continuous and so is $G_n$. The remaining integral in $(2.5)$ is $G_n(a)$, so we have:

$$F_n(a) - F_n(a_0) = G_{n-1}(a) - G_{n-1}(a_0) + (a - a_0) G_n(a)$$

Dividing by $(a - a_0)$ and taking the limit $a \rightarrow a_0$ gives us $F'_{n}$:

$$F'_n(a_0) = \lim_{z \rightarrow a} \paren{\frac{G_{n-1}(a) - G_{n-1}(a_0)}{a - a_0} + G_n(a)}$$

or since limit is invariant with sum:

$$= \lim_{z \rightarrow a} \paren{\frac{G_{n-1}(a) - G_{n-1}(a_0)}{a - a_0}} + \lim_{z \rightarrow a}  G_n(a)$$

The first limit is $G'_{n-1}(a_0)$ and by hypothesis equal to $(n-1) G_n(a_0)$. Since $G_n(a)$ is continuous, it equal $G_n(a_0)$ as $a \rightarrow a_0$:

$$= (n - 1) G_n(a_0) + G_n(a_0) = n G_n(a_0)$$

Finally, we have that $G_{n}(a_0) = F_{n+1}(a_0)$, so:

$$F'_n(a_0) = n F_{n+1}(a_0)$$

<i>QED</i>
</proof>

We note that we can write $(2)$ as:

$$f(a) =\frac{1}{2\pi i} F_1(a)$$

We can now use *Lemma 2* to compute the $n$-th derivative of $f(a)$:

**Lemma 3.** Let $f(z)$ be a holomorphic in $\Delta$ and let $\gamma$ a closed curve in $\Delta$, and $a$ such that $n(\gamma, a) = 1$. Then

$$(4) \quad f^{(n)}(a) = \frac{n!}{2\pi i} \int_\gamma \frac{f(z)}{(z - a)^{n+1}} dz$$

<proof>

We start with $(2)$:

$$f(a) = \frac{1}{2\pi i} \int_{\gamma} \frac{f(z)}{z - a} dz$$

Then replace it by $(3)$ from <i>Lemma 2</i>:

$$f(a) =\frac{1}{2\pi i} F_1(a)$$

We can compute $f'(a)$ as:

$$f'(a) = \frac{1}{2\pi i} F'_1(a)$$

Which, from <i>Lemma 2</i> is:

$$(2.6) \quad f'(a) = \frac{1}{2\pi i} F_2(a)$$

Similarly, for $f^{(3)}(a)$:

$$f^{(3)}(a) = \frac{f'}{da}(a)$$

Replacing by $(2.6)$:

$$= \frac{1}{2\pi i} F'_2(a)$$

Using <i>Lemma 2</i> again:

$$f^{(3)}(a) = \frac{1}{2\pi i} F_3(a)$$

For $f^{(4)}$:

$$f^{(4)}(a) = \frac{f^{(3)}}{da}(a) = \frac{2}{2\pi i} F'_3(a) = \frac{6}{2\pi i} F_4(a)$$

you get the idea. For the $n$-th derivative we have:

$$f^{(n)}(a) = \frac{df^{(n-1)}}{da}(a) = \frac{n!}{2\pi i} F_{n+1}(a) =  \frac{n!}{2\pi i} \int_\gamma \frac{f(z)}{(z - a)^{n+1}} dz$$

</proof>

Summarizing, *Lemma 2* proves that line integrals are infinitely differentiable and *Lemma 1* allows us to express a holomorphic function $f$ at any point $a$ as a function of a line integral. Combining both gives us that a function $f$ is also infinitely differentiable.

Suppose $f(z)$ is holomorphic in a region $\Omega$ and $a \in \Omega$. Since $\Omega$ is open, we can always find an open disk $\Delta$ as a neighborhood of $a$, $\abs{z - a} \lt \delta$, and inside it a circle $C$ containing $a$. Since a circle winds exactly once around points on its interior, $n(C, a) = 1$.

In these conditions we can apply *Lemma 3* and thus conclude that $f(a)$ is infinitely differentiable. For each $a$ on the domain of $f$, we can always choose a suitable $C$, so this leads us to the following high-level corollary:

**Corollary 4.** Holomorphic functions are infinitely differentiable.

Another consequence is that if $f(a)$ is the derivartive of a holomorphic function, then $f(a)$ itself is holomorphic. Let's revisit *Corollary 1* in *Cauchy Integral Theorem* [2]:

> Let $f(z)$ be a function defined in $\Omega$. Then $\int_\gamma f(z)dz = 0$ if and only if $f$ is the derivative of some holomorphic function $F$ in $\Omega$.

So one direction says that if $\int_\gamma f(z)dz = 0$ then $f$ is the derivative of a holomorphic function. But now we know $f$ is also holomorphic. This leads to a famous result:

**Theorem 5 (Morera's theorem)** If $f(z)$ is defined and continuous in a region $\Omega$, and $\int_\gamma f(z)dz = 0$ for any closed cuver $\gamma$, then $f(z)$ is holomorphic.

## Cauchy's Estimate

Suppose $f(z)$ is holomorphic and bounded. We can then obtain an upperbound for $\abs{f^{(n)}(a)}$ via *Cauchy's Estimate*.

**Lemma 6 (Cauchy's Estimate).** Let $f(z)$ be holomorphic and bounded by a finite $M$ in a region $\Omega$ (i.e. , $\abs{f(z)} \le M$ for all $z \in \Omega$). Let $C$ be a circle of radius $r$ centered in $a$ ($C$ is inside $\Omega$). Then:

$$(5) \quad \abs{f^{(n)}(a)} \le \frac{n! M}{r^{n}}$$

<proof>

By $(4)$ of <i>Lemma 3</i> we have:

$$\abs{f^{(n)}(a)} = \abs{\frac{n!}{2\pi i} \int_C \frac{f(z)}{(z - a)^{n+1}} dz}$$

Using <i>Theorem 1</i> in [4]:

$$ = \frac{n!}{\abs{2\pi i}} \int_C \frac{\abs{f(z)}}{\abs{(z - a)}^{n+1}} \abs{dz}$$

We have that $\abs{z - a} = r$ and  $\abs{f(z)} \le M$, so

$$ \le \frac{n! M}{2\pi r^{n+1}}  \int_C \abs{dz}$$

The left integral is the circumference of a circle of radius $r$, $2\pi r$, so:

$$\abs{f^{(n)}(a)} \le \frac{n! M}{r^{n}}$$

</proof>

We can use this result to prove another famous result, *Liouville's Theorem*:

**Theorem 7 (Liouville's Theorem).** If $f(z)$ is holomorphic and bounded on the whole plane, then it's a constant function.

<proof>

We'll use Cauchy's estimate to find an upper bound of $f'(a)$. By hypothesis $\abs{f(z)} \le M$, so we have from $(5)$ and $n = 1$:

$$\abs{f'(a)} \le \frac{M}{r}$$

Since $f(z)$ is holomorphic on the whole plane, we can choose an arbitrarily large circle, with $r \rightarrow \infty$, such that

$$\abs{f'(a)} = 0$$

Since the derivative is 0, the function cannot vary and is thus constant.
</proof>

Liouville's Theorem can on its turn be used to prove the *Fundamental Theorem of Algebra*.

**Theorem 8 (Fundamental Theorem of Algebra).** If $P(z)$ is a single-variable polynomial of complex coefficients and degree greater than 0, then it has at least one complex root.

<proof>

Let's prove by contradiction. Suppose that $P(z)$ has no roots, that is, there's no $z$ such that $P(z) = 0$. So we assume $P(z) \ne 0$, and then $1/P(z)$ is holomorphic on the entire plane.
<br /><br />
$1/P(z)$ is also bounded since besides having $P(z) \ne 0$, as $z \rightarrow \infty$, $1/P(z)$ tends to 0. Thus, according to Liouville's Theorem, $1/P(z)$ must be a constant function, and so is $P(z)$, implying it has degree 0, a contradiction.

</proof>

## Conclusion

We derived Cauchy's integral formula from Cauchy's integral theorem applied to the rate of change of $f(z)$, which we called $F(z)$.

We also simplified our lives by only considering cases where $n(\gamma, a) = 1$ to get rid of this factor. It turns out not to be a big problem: we can choose our closed curve to have that property and still get Morera's and Liouville's theorems.

To make sure we could build on top of results from previous posts, we had to make sure to look at a neighborhood of each point $a$ (by choosing the circle $C$), small enough to guarantee for example that $f(z)$ is holomorphic there. In this sense, these properties we have proved, such as that holomorphic functions are infinitely differentiable, are "local" properties.

Finally, none of the famous results (*Theorems 5, 7, 8*) made full use of *Lemma 3*. *Morera's theorem* only used the fact that the derivative of a holomorphic function is holomorphic and *Liouville's theorem* only used it for $n = 1$. Fuller use of *Lemma 3* will be left for future posts.

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2]({{blog}}/2024/04/26/cachy-integral-theorem.html)] NP-Incompleteness: Cauchy Integral Theorem
* [[3]({{blog}}/2024/05/09/the-winding-number.html)] NP-Incompleteness: The Winding Number
* [[4]({{blog}}/2024/04/05/complex-integration.html)] NP-Incompleteness: Complex Integration
* [[5]({{blog}}/2024/04/13/path-independent-line-integrals.html)] NP-Incompleteness: Path-Independent Line Integrals
* [[6]({{blog}}/2023/12/21/holomorphic-functions.html)] NP-Incompleteness: Holomorphic Functions
