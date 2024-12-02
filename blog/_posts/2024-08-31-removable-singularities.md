---
layout: post
title: "Removable Singularities"
tags: [analysis]
vanity: "2024-08-31-removable-singularities"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/riemann.jpeg" alt="Portrait of Bernhard Riemann" />
</figure>

This is the 7-th post in the series with my notes on complex integration, corresponding to *Chapter 4* in Ahlfors' Complex Analysis.

In this post we'll cover *Removable Singularities* in the context of holomorphic functions.

<!--more-->

The previous posts from the series:

1. [Complex Integration]({{blog}}/2024/04/05/complex-integration.html)
1. [Path-Independent Line Integrals]({{blog}}/2024/04/13/path-independent-line-integrals.html)
1. [Cauchy Integral Theorem]({{blog}}/2024/04/26/cachy-integral-theorem.html)
1. [The Winding Number]({{blog}}/2024/05/09/the-winding-number.html)
1. [Cauchy's Integral Formula]({{blog}}/2024/06/06/cauchy-integral-formula.html)
1. [Holomorphic Functions are Analytic]({{blog}}/2024/07/02/holomorphic-functions-are-analytic.html)

## Definitions

Let $f$ be a holomorphic function in $\Omega$. A **singularity** is is a point $a$ where a function is not defined, that is, $a \not\in \Omega$.

A singularity is "isolated" if nothing around it is a singularity. More formally, point $a$ is an **isolated singularity** if $a \not\in \Omega$ and there exists a radius $r \gt 0$ such that $f$ is holomorphic in the punctured disk $0 \lt \abs{z - a} \lt r$.

A **removable singularity** is a special type of isolated singularity with the property that $\lim_{z \rightarrow a} (z - a) f(z) = 0$.

One nice thing about removable singularities is that it's possible to "extend" the function $f$ such it is made holomorphic there as well (*Theorem 1*), so that it's not a singularity anymore, effectively "removing" it, hence the terminology.

**Theorem 1.** Let $f(z)$ be a holomorphic function in $\Omega \setminus \curly{a}$. Then

$$(1) \quad \lim_{z \rightarrow a} f(z)(z - a) = 0$$

if and only if there exists an holomorphic function $g(z)$ in $\Omega$ that coincides with $f(z)$ in $\Omega \setminus \curly{a}$.

<proof>

First we prove that if there exists an holomorphic function in $\Omega$ that coincides with $f(z)$ in $\Omega \setminus \curly{a}$ then $(1)$ holds. Let $g$ be such a function.

By hypothesis $g$ is holomorphic in $a$, so it's also continuous in $a$ and by definition $\lim_{z \rightarrow a} g(z) = g(a)$. Since $z$ is always different from $a$ inside the limit, we can substitute $g$ for $f$ and obtain:

$$(1.1) \quad \lim_{z \rightarrow a} f(z) = g(a)$$

in other words the limit of $f(z)$ as $z$ approaches $a$ is finite and unique. Consider the $\lim_{z \rightarrow a} (z - a) f(z)$. First we consider the limit of each factor:

$$= \paren{\lim_{z \rightarrow a} (z - a)} \paren{\lim_{z \rightarrow a} f(z)}$$

Replacing $(1.1)$:

$$= \paren{ \lim_{z \rightarrow a} (z - a) } g(a)$$

Since $g(a)$ is defined, it must be finite and thus a constant, and the limit above goes to 0 and we conclude that:

$$\lim_{z \rightarrow a} (z - a) f(z) = 0$$

On the other direction, assume $(1)$ holds. Define the function $h(z)$ as follows:

$$
\begin{equation}
  (1.B) \quad h(z)=\left\{
  \begin{array}{@{}ll@{}}
    (z-a)^2 f(z), & z \in \Omega \setminus \curly{a} \\
    0, & z = a
  \end{array}\right.
\end{equation}
$$

Since $f(z)$ is holomorphic when $z \ne a$ and that polynomials like $(z - a)^2$ are also holomorphic, then $h(z)$ is holomorphic when $z \ne a$.

Now consider $h'(a)$:

$$h'(a) = \lim_{z \rightarrow a} \frac{h(z) - h(a)}{z - a}$$

Since $z \ne a$, $h(z) = (z-a)^2 f(z) $ and $h(a) = 0$ by $(1.B)$:

$$= \lim_{z \rightarrow a} \frac{(z-a)^2 f(z) - 0}{z - a}$$

Which simplifies to:

$$= \lim_{z \rightarrow a} (z-a)f(z)$$

Which by hypothesis is 0, so:

$$(1.2) \quad h'(a) = 0$$

Thus the limit exists and hence $h'(a)$ is defined and $h$ is holomorphic in $a$ as well. As we've shown in [2], holomorphic functions are analytic, so $h(z)$ can be expressed as a power series:

$$(1.3) \quad h(z) = c_0 + c_1 (z - a) + c_2 (z - a)^2 + \dots$$

We can use some identities to figure out the coefficients. By setting $z = a$, we get $c_0 = h(a)$ which by $(1.B)$ is 0. Differentiating $(1.3)$ gives us:

$$h'(z) = c_1 + c_2 2 (z - a) + c_3 3 (z - a)^2 + \dots$$

Again, setting $z = a$, we get $c_1 = h'(a)$ which by $(1.2)$ is also 0. So $h(z)$ is:

$$(1.2) \quad  h(z) = c_2 (z - a)^2 + c_3 (z - a)^3 + \dots$$

If $z \ne a$, we can rewrite $(1.B)$ as

$$f(z) = \frac{h(z)}{(z-a)^2}$$

Replacing $(1.2)$:

$$= c_2 + c_3 (z - a) + c_4 (z - a)^2 + \dots$$

So this is the power series expansion of $f(z)$ when $z \ne a$. Since this is a convergent power series, by <i>Theorem 4</i> in [2] there exists some corresponding holomorphic function $g(z)$. For $z \ne a$ we just saw it equals to $f(z)$. For $g(a)$ we get $c_2$. Summarizing:

$$
\begin{equation}
  g(z)=\left\{
  \begin{array}{@{}ll@{}}
    f(z), & z \in \Omega \setminus \curly{a} \\
    c_2, & z = a
  \end{array}\right.
\end{equation}
$$

We can actually calculate $c_2$. Recall that it is the first term of the power series expansion of $f(z)$, which from <i>Theorem 4</i> in [2] is:

$$c_2 = \frac{1}{2\pi i} \int_{C} \frac{f(w)}{w - a}dw$$

For some circle $C$ centered in $a$ and contained in $\Omega$.

QED

</proof>

According to Wikipedia this is part of *Riemman's Theorem* on removable singularities [6]. The theorem proves other equivalences too.

In the proof of *Theorem 1*, we saw that even if $f(a)$ doesn't exist, we can compute $g(a)$ via Cauchy's Integral Formula:

$$(2) \quad g(a) = \frac{1}{2\pi i} \int_{C} \frac{f(z)}{z - a}dz$$

However, an alternative approach is to simply compute $g(a)$ as:

$$g(a) = \lim_{z \rightarrow a} f(z)$$

Since we know $g(a)$ exists, is holomorphic and thus continuous.

### Example

Wikipedia provides the example of the unormalized $\mbox{sinc}$ function:

$$
\mbox{sinc}(z) = \frac{\sin z}{z}
$$

which is holomorphic everywhere except (in principle) at $z = 0$. We can use *Theorem 1* to show $\mbox{sinc}(0)$ can be computed as $\lim_{z \rightarrow 0} \mbox{sinc}(z)$ which from *Lemma 2* equals to 1.

**Lemma 2.** Let $z \ne 0$ be a complex number. Then:

$$\lim_{z \rightarrow 0} \frac{\sin z}{z} = 1$$

<proof>
The real case can be proved using Euclidean geometry and the squeeze theorem [7]. For the complex case we can use the Taylor series expansion of $\sin z$:

$$
\sin z = z - \frac{z^3}{3!} + \frac{z^5}{5!} - \frac{z^7}{7!} + \cdots
$$

Dividing by $z \ne 0$:

$$
\frac{\sin z}{z} = 1 - \frac{z^2}{3!} + \frac{z^4}{5!} - \frac{z^6}{7!} + \cdots
$$

Taking the limit $z \rightarrow 0$:

$$
\lim_{z \rightarrow 0} \frac{\sin z}{z} = 1
$$

QED.

</proof>

We can also compute it directly from $(2)$:

$$\mbox{sinc}(0) = \frac{1}{2\pi i} \int_{C} \frac{\sin z}{z^2}dw$$

by using the [residue theorem](https://en.wikipedia.org/wiki/Residue_theorem) (which we haven't studied yet) to arrive at the same conclusion. Thus $\mbox{sinc}(z)$ is holomorphic on the entire complex plane.

## Finite Taylor Series

Consider the function:

$$(3) \quad F(z) = \frac{f(z) - f(a)}{z - a}$$

which is holomorphic everywhere $f(z)$ is except at $a$, but we have at least that $\lim_{z \rightarrow a} F(z) (z - a) = 0$, so we can apply *Theorem 1* for $F(z)$ to conclude that there exists some function $G(z)$ which is holomorphic on $f$'s domain.

Since $G$ is holomorphic at $a$, it's also continuous in $a$ and thus we can express it as

$$G(a) = \lim_{z \rightarrow a} F(z)$$

Which when replacing with $(3)$, gives us the definition of $f'(a)$, so:

$$
\begin{equation}
  G(z)=\left\{
  \begin{array}{@{}ll@{}}
    F(z), & z \in \Omega \setminus \curly{a} \\
    f'(a), & z = a
  \end{array}\right.
\end{equation}
$$

We can write:

$$f(z) = f(a) + (z - a) G(z)$$

Which holds for $z \ne a$ from $(3)$. For $z = a$ this is trivally correct since $G(a)$ is finite, and we get $f(a) = f(a)$. Renaming $G$ to $f_1$:

$$f(z) = f(a) + (z - a) f_1(z)$$

Repeating for $f_1$ and naming the corresponding function defined in $(3)$ as $f_2$:

$$f_1(z) = f_1(a) + (z - a) f_2(z)$$

If we do it $n$ times we get the recurrence:

$$
f_{n - 1}(z) = f_{n - 1}(a) + (z - a) f_n(z)
$$

We can "expand" these and write $f(z)$ as a function of $f_n$:

$$
(4) \quad f(z) = f(a) + (z - a) f_1(a) + (z - a)^2 f_2(a) + \cdots \\ + (z - a)^{n-1} f_{n-1}(a) +  (z - a)^n f_n(z)
$$

We can now use *Lemma 3* to express $f_k$ as the $k$-th derivative of $f$.

**Lemma 3.** Let $f$ be a holomorphic function in $\Omega$ and $f_k$ be defined as

$$f_k(z) = \frac{f_{k-1}(z) - f_{k-1}(a)}{z - a}$$

Then

$$(5) \quad f_n(a) = \frac{f^{(n)}(a)}{n!}$$

<proof>

We already have the identity $(4)$. If we differentiate it we get:

$$
f'(z) = f_1(a) + 2(z - a) f_2(a) + \cdots + (n - 1)  (z - a)^{n-2} f_{n-1}(a) \\ + n(z - a)^{n - 1} f_n(z) + (z - a)^n f_n'(z)
$$

Differentiating again:

$$
f''(z) =  2 f_2(a) + \cdots + (n - 1)(n - 2)  (z - a)^{n-3} f_{n-1}(a) \\ + n(n - 1)(z - a)^{n - 2} f_n(z) + (z - a)^{n-1} f_n'(z) + n (z - a)^{n - 1} f_n'' + (z - a)^n f_n''(z)
$$

It's tedious work, but differentiating it $n$ times, we'll see that the terms with $f_k$ for $k \lt n$ will vanish and the other terms besides $f_n(z)$ will have a factor $(z - a)$, so when we set $z = a$ we obtain:

$$
f^{(n)}(a) = n!f_n(a)
$$

QED

</proof>

Using $(5)$ we can now rewrite $(4)$ using only the derivarites of $f$ and $f_n$:

$$
f(z) = f(a) + \frac{f'(a)}{1!} (z - a) +  \frac{f''(a)}{2!} (z - a)^2 + \cdots \\
+ \frac{f^{n-1}(a)}{(n - 1)!} (z - a)^{n - 1} + f_n(z) (z - a)^n
$$

or more concisely:

$$
(6) \quad f(z) = f_n(z) (z - a)^n + \sum_{k = 0}^{n - 1} \frac{f^{(k)}(a)}{k!} (z - a)^k
$$

Noting that if $f(z)$ is holomorphic in $\Omega$, so is $f_n(z)$. We can use *Lemma 4* to express $f_n$ as a function of $f$ as well.

**Lemma 4.** Let $f_n$ be a holomorphic function defined as:

$$(7) \quad f_n(z) = \frac{f(z)}{(z - a)^n} - \sum_{k = 0}^{n - 1} \frac{f^{(k)}(a)}{k!(z - a)^{n-k}}$$

For $n \ge 1$. Then

$$
(8) \quad f_n(z) = \frac{1}{2\pi i} \int_{C} \frac{f(w)dw}{(w - a)^n(w - z)}
$$

<proof>

Because $f_n$ is holomorphic, we can use Cauchy's Integral Formula (Lemma 1 in [3]):

$$
(4.1) \quad f_n(z) = \frac{1}{2\pi i} \int_{C} \frac{f_n(w)dz}{w - z}
$$

We can then express $f_n(w)$ via $(7)$:

$$f_n(w) = \frac{f(w)}{(w - a)^n} - \sum_{k = 0}^{n - 1} \frac{f^{(k)}(a)}{k!(w - a)^{n-k}}$$

Plugging this into $(4.1)$ we get:

$$
f_n(z) = \frac{1}{2\pi i} \paren{ \int_{C} \frac{f(w)dw}{(w - a)^n(w - z)} - \int_C \sum_{k = 0}^{n - 1} \frac{f^{(k)}(a)}{k!(w - a)^{n-k}(w - z)} dw}
$$

Exchanging the integral and the finite sum, and moving the terms that do not depend on $w$ out of the integral:

$$
= \frac{1}{2\pi i} \paren{ \int_{C} \frac{f(w)dw}{(w - a)^n(w - z)} - \sum_{k = 0}^{n - 1} \frac{f^{(k)}(a)}{k!} \int_C  \frac{dw}{(w - a)^{n-k}(w - z)}}
$$

We'll now show that the integral inside the sum is actually 0. Let's define:

$$
(4.2) \quad  F_v(a) = \int_C \frac{dw}{(w - a)^{v}(w - z)}
$$

For $v = 1, \dots, n$ (noting this is the same index range $n-k$ has for $k = 0, n-1$).

For $v = 1$ we have:

$$
F_1(a) = \int_C \frac{dw}{(w - a)(w - z)}
$$

We can use the identity:

$$\frac{1}{(w - a)(w - z)} = \frac{1}{z - a} \paren{\frac{1}{w - a} - \frac{1}{w - z}}$$

to obtain:

$$
F_1(a) = \frac{1}{z - a}  \int_C \frac{1}{w - a} - \frac{1}{w - z} dw
$$

Since points $a$ and $z$ are inside the circle and $w$ is on the circumference, $w \ne a$, $w \ne z$ and thus the integrand is a holomorphic function and we can use *Cauchy Integral Theorem* [4] to conclude the integral is 0 and thus:

$$F_1(a) = 0$$

is a constant function. Hence all its $n$-th derivatives are also zero, $F_1^{(n)}(a) = 0$.

Now let $g(w) = \frac{1}{w - z}$. We can write $(4.2)$ as:

$$
F_v(a) = \int_C \frac{g(w) dw}{(w - a)^{v}}
$$

Since $g(w)$ is continuous in $C$ (again, $z \ne w$), we can use *Lemma 2* from [3] to obtain $F_v'(a) = v F_{v+1}(a)$, or expanding it $v$ times:

$$
F_{v + 1}(a) = \frac{F^{(n)}_1(a)}{v!}
$$

Which implies that $F_v(a) = 0$ for all $v$. Going back to $f_n(z)$, we can now conclude:

$$
f_n(z) = \frac{1}{2\pi i} \int_{C} \frac{f(w)dw}{(w - a)^n(w - z)}
$$

QED
</proof>

We can replace $(8)$ in $(6)$ and obtain the final form of the finite Taylor series:

**Corollary 5.** Let $f(z)$ be a holomorphic function in $\Omega$, with $a \in \Omega$. Then

$$
f(z) = \frac{(z - a)^n}{2\pi i} \int_{C} \frac{f(w)dw}{(w - a)^n(w - z)} + \sum_{k = 0}^{n - 1} \frac{f^{(k)}(a)}{k!} (z - a)^k
$$

For a circle $C$ centered in $a$ and contained within $\Omega$ and $n \ge 1$.

I'm not sure how this result is useful though. It seems to be that computing $f_n$ via $(8)$ is strictly harder than just computing $f(z)$ directly:

$$
f(z) = \frac{1}{2\pi i} \int_{C} \frac{f(w)dw}{(w- z)}
$$

## Alternative Characterizations

It's useful to consider other definitions of *removable singularity*, because they might be used to contrast with other types of isolated singularities.

**Corollary 6.** A point $a$ is a removable singularity if and only if $\lim_{z \rightarrow a} f(z)$ exists.

<proof>

One direction follows from <i>Theorem 1</i>: we know that $g(a)$ is holomorphic and hence continuous and that $g(a) = \lim_{z \rightarrow a} f(z)$, so the limit exists.
<br /><br />
If $\lim_{z \rightarrow a} f(z)$ exists, then $f(z)$ is bounded in a neighborhood of $a$ by some $L$, so

$$
\lim_{z \rightarrow a} \abs{z - a}\abs{f(z)} \lt  \lim_{z \rightarrow a} \abs{z - a} L = 0
$$

and thus $\lim_{z \rightarrow a} (z - a) f(z) = 0$ which is the definition of removable singularity.
</proof>




## Conclusion

In this post we learned that if a function is holomorphic in a domain $\Omega$ except at a point $a$, we can extend it to be holomorphic in $a$, as long as $\lim_{z \rightarrow a}f(z) (z - a) = 0$.

We've seen a similar property before with Cauchy Integral Theorem in a disk (*Theorem 4* in [4]) states that:

> Let $f(z)$ be holomorphic in the region $\Delta'$ obtained by omitting a finite number of points $\xi_j$ from the open disk $\Delta$. If $f(z)$ is such that
>
> $$\lim_{z \rightarrow \xi_j} (z - \xi_j) f(z) = 0$$
>
> then
>
> $$\int_{\gamma} f(z) dz = 0$$
>
> for any closed curve $\gamma$ in $\Delta'$.

So these $\xi_j$ are removable singularities. With *Theorem 1*, we wouldn't need to prove *Theorem 4* in [4], we could just show that if $f(z)$ is holomorphic in $\Delta'$, it's also holomorphic in $\Delta$ and use *Theorem 3* in [4] which is the version without removable singularities.

We must however be careful with circular arguments: *Theorem 1* relies on the fact that holomorphic functions are analytic, whose proof depends on Cauchy's Integral Formula, which in turn relies on *Theorem 4*.

That's one concern I have in using multiple sources to complement Alfhors' textbook: By relying on results out of order I might risk running into circular arguments. Alfhors proves *Theorem 1* by claiming that Cauchy's Integral Formula allows holomorphic functions to have missing points as long as $\lim_{z \rightarrow a}f(z) (z - a) = 0$, but it doesn't provide an explicit proof of that and I couldn't prove it myself.

I relied on Tao's blog [4] and Wikipedia [6] that prove *Theorem 1* by using that holomorphic functions are analytic.

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2]({{blog}}/2024/07/02/holomorphic-functions-are-analytic.html)] NP-Incompleteness: Holomorphic Functions are Analytic
* [[3]({{blog}}/2024/06/06/cauchy-integral-formula.html)] NP-Incompleteness: Cauchy's Integral Formula
* [[4]({{blog}}/2024/04/26/cachy-integral-theorem.html)] NP-Incompleteness: Cauchy Integral Theorem
* [[5](https://terrytao.wordpress.com/2016/10/11/math-246a-notes-4-singularities-of-holomorphic-functions/)] What’s new - Math 246A, Notes 4: singularities of holomorphic functions
* [[6](https://en.wikipedia.org/wiki/Removable_singularity)] Wikipedia: Removable singularity
* [[7](https://www.youtube.com/watch?v=T36uC2pxwR4)] Youtube: Proof: Limit of $\sin x/x$ as $x$ approaches 0 with Squeeze Theorem \| Calculus 1 (Wrath of Math)
