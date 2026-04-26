---
layout: post
title: "Functionals"
tags: [functional analysis]
vanity: "2026-04-26-functionals"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/riesz.png" alt="Thumbnail of Frigyes Riesz" />
</figure>

I started reading the book *The Theoretical Minimum* by Leonard Susskind and George Hrabovsky. A lot of the math from the early chapters looked familiar, but in *Chapter 6: The Principle of Least Action*, they describe and derive the *Euler-Lagrange equation*, which I don't recall seeing before.

I wanted to explore these equations and their derivation, but from a more mathematical point of view. This led me to a short rabbit hole around functionals and Sobolev spaces and since I like learning things from first principles, I decided to cover functionals first.

<!--more-->

The thumbnail features the Hungarian mathematician Frigyes Riesz. He was first featured in the post [Subharmonic Functions]({{blog}}/2025/12/14/subharmonic-functions.html) and is back here because he's considered one of the founders of functional analysis and we'll cover one theorem named after him, the *Riesz Representation Theorem*.

## Functionals

In the branch of math called functional analysis the core object is the *functional*. Without qualifiers it's implicitly assumed that a functional is a linear one. In this post however, since we want to cover functionals in general, we'll qualify linear functions explicitly and assume the general case when saying *functionals*.

A **functional** is essentially a function $f$ where the domain is a vector space $H$ and the image is a field, either the reals or the complex numbers, generically denoted by $\mathbb{F}$:

$$
f : H \rightarrow \mathbb{F}
$$

More intuitively, a functional is a function that takes a function as input and returns a scalar. Much like functional programming which operates over functions as objects.

One may ask: what do functions have to do with vector spaces? In a vector space the *vector* is a concept more broad than, say, a tuple of scalars like $\mathbb{R}^3$. It just means it's some object that satisfies a set of axioms (e.g. addition, scalar multiplication). An example of a function vector space is the set of continuous functions.

If this function (or map) satisfies additivity and scalar multiplication, then it's called a **linear functional**. In other words if $x, y$ are members of a vector space $V$, and $\lambda \in \mathbb{F}$ and $f$ a linear map $f: V \rightarrow \mathbb{F}$:

$$
f(x + y) = f(x) + f(y) \\
f(\lambda x) = \lambda f(x)
$$

We'll now cover some related concepts and properties of functionals.

## Properties

### Norm

The **norm** or *operator norm* of a functional $f$ and denoted by $\norm{f}$ is defined as:

$$
\norm{f} = \sup_{\norm{x} \ne 0} \frac{\abs{f(x)}}{\norm{x}}
$$

That is, it's the supremum of the value of $f$ but normalized by its input size. Note that it doesn't make sense to talk about $\abs{f}$, even though its image is a scalar, since it requires a specific input for it to spit out a scalar.

### Continuity

For continuity to make sense for functionals, the domain must be a [topological space]({{blog}}/2022/11/03/topological-equivalence.html), i.e. it must have the notion of open sets, because [continuity](https://www.kuniga.me/docs/math/topology.html#continuity) depends on these.

In a more specific case, if we assume the domain is a normed vector space, i.e. it has the notion of distance between its elements, then we can use the $\epsilon-\delta$ definition of continuity for the functional, that is, a functional is **continuous** at $x_0$ if for every $\epsilon \gt 0$, there exists $\delta \gt 0$:

$$
\norm{x - x_0} \lt \delta \implies \norm{f(x) - f(x_0)} \lt \epsilon
$$

For a linear functional $f$ in particular, there's an alternative characterization: if there's an upper bound on how much bigger $f$ is compared to its input, then it's continuous:

**Lemma 1.** The linear functional $f: H \rightarrow \mathbb{F}$ is continuous if and only if

$$
\norm{f} \le C \norm{x}
$$

for all $x \in H$ and some constant $C$.

<proof>

Assume first that $f$ is bounded. Because $f$ is linear, we have $f(0) = 0$. Consider continuity at $0$. We need to show that for every $\epsilon \gt 0$, there exists $\delta \gt 0$:

$$
\norm{x} \lt \delta \implies \norm{f(x)} \lt \epsilon
$$

since we have $\norm{f} \le C \norm{x}$ we just choose $\delta = \epsilon/C$ if $C \gt 0$. Otherwise we have $\norm{f} = 0$, which is continuous.

<br /><br />
Now for a general point $x_0$, we use the linearity of $f$ to show:

$$
f(x) - f(x_0) = f(x - x_0)
$$

using the hypothesis:

$$
\abs{f(x) - f(x_0)} = \abs{f(x - x_0)} \le C \norm{x - x_0}
$$

If $C \gt 0$ we again take $\delta = \epsilon/C$. So if $\norm{x - x_0} \lt \delta$, then:

$$
\abs{f(x) - f(x_0)} \le C \norm{x - x_0} \lt C \delta = \epsilon
$$

so $f$ is also continuous at $x_0$.
<br /><br />
Now consider the other direction, that assumes $f$ is continuous. Consider the case at 0, that gives us, for all $\epsilon \gt 0$, there exists $\delta \gt 0$:

$$
\norm{x} \lt \delta \implies \norm{f(x)} \lt \epsilon
$$

we can then take $\epsilon = 1$ and $\delta_1$ is the corresponding constant. Let $x$ be any vector from the domain. We can re-scale it as:

$$
y = \frac{\delta_1}{2 \norm{x}} x
$$

and we have that $\norm{y} = \delta_1 / 2$. So now $y$ is a point in the neighborhood of $0$ (defined by $\norm{y} \lt \delta_1$) and thus $\abs{f(y)} \lt 1$. By linearity we have:

$$
f(y) = f\left(\frac{\delta_1}{2 \norm{x}} x \right) = \frac{\delta_1}{2 \norm{x}} f(x)
$$

and since $\abs{f(y)} \lt 1$:

$$
\abs{\frac{\delta_1}{2 \norm{x}} f(x)} \lt 1 \implies \abs{f(x)} \lt \frac{2}{\delta_1} \norm{x}
$$

we can thus choose $C = 2 / \delta_1$ to show that

$$
\norm{f} \le C \norm{x}
$$

so intuitively $C$ is the scaling factor that brings every point $x$ in the domain inside the neighborhood of $0$ where continuity holds.
</proof>


### Differentiation

Let $X, Y$ be vector spaces equipped with a norm and $f : X \rightarrow Y$. We say that $f$ is **Fréchet differentiable** if there exists a linear map $A: X \rightarrow Y$ (Fréchet derivative) such that:

$$
(1) \quad \lim_{\norm{h} \rightarrow 0} \frac{\norm{f(x + h) - f(x) - A(h)}}{\norm{h}} = 0
$$

For $h \in X$. Note that $f$ is not necessarily a functional, only if $Y = \mathbb{R}$ or $Y = \mathbb{C}$, and even if it is, it's not necessarily linear. However if $f$ is a functional, then the Fréchet derivative is a linear function because it's a linear map from a vector space to a field.


This is a general definition of the differential we see in real analysis. If we take $X = Y = \mathbb{R}$, then we can simplify $(1)$ to:

$$
(2) \quad \lim_{h \rightarrow 0} \frac{f(x + h) - f(x) - f'(x)h}{h} = 0
$$

By having $A(h) = f'(x)h$. If we add $f'(x)h/h$ to both sides of the equation, we get the more familiar:

$$
(3) \quad f'(x) = \lim_{h \rightarrow 0} \frac{f(x + h) - f(x)}{h}
$$

One might ask why we use this other form. The expression $f(x + h) - f(x)$ is an element of $Y$ and $h \in X$, so in order for $(3)$ to make sense, we'd need to define multiplication or division between $X$ and $Y$.

The Fréchet derivative also generalizes the [complex derivatives]({{blog}}/2023/12/21/holomorphic-functions.html), the one that defines holomorphic functions and underpins complex analysis. We can also have the form $(2)$ but the implicit assumption is that in $f'(x)h$ the multiplication operator is the complex one.

Note how the Fréchet derivative goes one abstraction layer above by "wrapping" $f'(x)h$ as some function $A(h)$. This is similar to how topological spaces abstract normed spaces by working with open sets instead of norms (open set is a higher object than norms because they can be defined from norms but not the other way around).

## Linear Functionals

Now we focus on properties that are only applicable if the functional is linear.

### Kernel

Let $X, Y$ be vector spaces and $f$ a linear map between them. The **kernel** is the subspace of $X$ defined as:

$$
\ker f = \curly{x \in X : f(x) = 0}
$$

In other words, all elements in the domain that map to $0$ in the image. Note that $0$ here is not necessarily the scalar number $0$, but the $0$ element in the vector space $Y$.

We can verify that the kernel is indeed a subspace of $X$. It contains the $0$-th element because $f$ is linear and thus $f(0) = 0$. If $x, y \in \ker f$, then $x + y \in \ker f$ again because $f$ is linear and $f(x + y) = f(x) + f(y) = 0 + 0$. If $x \in \ker f$ and $\lambda$ is a scalar, then $\lambda x \in \ker f$ because $f(\lambda x) = \lambda f(x) = 0$.

If $f$ is continuous, then $\ker f$ is closed. We can show this by using one of the topological definitions of [continuity](https://www.kuniga.me/docs/math/topology.html#continuity): A function $f: X \rightarrow Y$ is continuous if and only if for every $U$ that is a closed set in $Y$, $f^{-1}(U)$ is a closed set in $X$. Since $\curly{0}$ is a closed set in $Y$ and $\ker f$ is the pre-image of $\curly{0}$, $\ker f$ is closed.

So we know the kernel is a subspace of $X$, but it doesn't necessarily inherit the same properties of the vector space $X$. If the domain is a [Hilbert space]({{blog}}/2021/06/26/hilbert-spaces.html) however, then it's possible to show that $\ker f$ is also a Hilbert space.

### Dual Space

Intuitively things that have linear properties can form a vector space, because a lot of its axioms are about linear combination of vectors. Since linear functionals have linear properties, they can also form a vector space! This vector space is called the **dual** of the domain vector space of the functionals.

More specifically, the dual space of a vector space $V$ is the set of all linear functionals that have $V$ as domain, denoted with a superscript asterisk:

$$
V^{*} = \curly{f : V \rightarrow \mathbb{R}}
$$

where $f$ is a linear functional. The intuition here is that $f$ associates a measure to the vectors of $V$. For Hilbert spaces in particular, we have a nice identity between a vector space and its dual. Before we show that, we need the following result:

**Theorem 2.** (Riesz Representation Theorem) Let $H$ be a Hilbert space and $f: H \rightarrow \mathbb{F}$ a continuous linear functional. Then, there exists a unique vector $y \in H$ such that:

$$
f(x) = \langle x, y\rangle \quad \forall x \in H
$$

and with $\norm{f} = \norm{y}$

<proof>

We first prove for the case where $f(x) = 0$. In that case we choose $y = 0$ and we're done. Otherwise, consider the kernel of $f$, $\ker f$, which we've seen is a vector space and closed because $f$ is continuous. Note that there is $z$ for which $f(z) \ne 0$ and thus $z \notin \ker f$.
<br /><br />
Recall the <a href="{{blog}}/2021/06/26/hilbert-spaces.html">The Projection Theorem</a> which shows that given a subspace $S$ of a Hilbert space $H$ we can decompose into $S$ and $S^\perp$. More specifically, for each $x \in H$, we can find $x_S \in S$ and $x_{S^{\perp}} \in S^\perp$ such that $x = x_S + x_{S^{\perp}}$ and that $x_S, x_{S^{\perp}}$ are orthogonal, i.e. $\langle x_S, x_{S^{\perp}} \rangle = 0$. This is denoted as $H = S \oplus S^\perp$.
<br /><br />
Since $\ker f$ is a subspace of $H$, we can use such decomposition. We wish to show that $\ker f^\perp$ is one-dimensional, which means it has a base of size 1 (not necessarily that it has a single element). Now choose $u, v \in \ker f^\perp$. Define $w = \alpha u - \beta v$, with $\alpha = f(v)$ and $\beta = f(u)$ as scalars. Since this is a linear combination and $\ker f^\perp$ is a vector space, $w \in \ker f^\perp$.
<br /><br />
Now do $f(w) = \alpha f(u) - \beta f(v)$ and replace the scalars:  $f(w) = f(v) f(u) - f(u) f(v) = 0$. Thus $w \in \ker f$. The only element that can belong to both a set and its orthogonal complement is $0$. This means that $w = 0$ and thus: $\alpha u = \beta v$, which means $u$ and $v$ are the same vector up to a scalar and that $\ker f^\perp$ has a base of size 1. Let's call that base $u$.
<br /><br />
So from the projection theorem, every $x \in H$ can be written as $x = k + \alpha u$ where $k \in \ker f$. Applying $f(x)$ gives us $f(x) = f(k) + \alpha f(u)$. Since $k \in \ker f$, $f(k) = 0$ and thus $f(x) = \alpha f(u)$.
<br /><br />
We have that $\langle x, u \rangle = \langle k + \alpha u, u \rangle =  \langle k, u \rangle + \alpha \langle u, u \rangle$. $k$ and $u$ are orthogonal by definition and $\langle u, u \rangle = \norm{u}^2$, so $\langle x, u \rangle = \alpha \norm{u}^2$ or that $\alpha = \langle x, u \rangle / \norm{u}^2$.
<br /><br />
Replace in $f(x) = \alpha f(u)$ gives us

$$
f(x) = \frac{f(u)}{\norm{u}^2} \langle x, u \rangle
$$

Now choose

$$
y = \frac{\overline{f(u)}}{\norm{u}^2} u
$$

The fraction above is just a scalar factor so $y \in H, y \in \ker f^\perp$. We choose the conjugate of $f(u)$ because the identity for inner product is $\langle x , \alpha y \rangle = \overline{\alpha} \langle x , y \rangle$. We have that:

$$
\langle x, y \rangle = \left\langle x, \frac{\overline{f(u)}}{\norm{u}^2} u \right\rangle = \frac{f(u)}{\norm{u}^2} \langle x, u \rangle = f(x)
$$

So we found $y \in H$ such that $f(x) = \langle x, y \rangle$. Since our choice does not depend on $x$, it holds for all $x \in H$.
<br /><br />
Now we need to show this is unique. Suppose we have $y_1, y_2$ and that $\langle x, y_1 \rangle = \langle x, y_2 \rangle$ then $\langle x, y_1 - y_2 \rangle = 0$ for all $x$, including the case where $x = y_1 - y_2$. The only case in which $\langle x, x \rangle = 0$ is for $x = 0$, so $y_1 = y_2$.
<br /><br />
Finally we show that $\norm{f} = \norm{y}$. First by definition:

$$
\norm{f} = \sup_{x \ne 0} \frac{\abs{f(x)}}{\norm{x}}
$$

we have $f(x) = \langle x, y \rangle$ so:

$$
\norm{f} = \sup_{x \ne 0} \frac{\abs{\langle x, y \rangle}}{\norm{x}}
$$

by Cauchy-Schwarz: $\abs{\langle x, y \rangle} \le \norm{x} \norm{y}$ and thus:

$$
\frac{\abs{\langle x, y \rangle}}{\norm{x}} \le \norm{y}
$$

hence $\norm{f} \le \norm{y}$. For the other direction, if $y \ne 0$ we have:

$$
\frac{\abs{f(y)}}{\norm{y}} = \frac{\abs{\langle y, y \rangle}}{\norm{y}} = \frac{\norm{y}^2}{\norm{y}} = \norm{y}
$$

Since $\norm{f}$ is the supremum of all $\abs{f(x)}/\norm{x}$ for $x = y$ it implies it should be at least $\norm{y}$, $\norm{f} \ge \norm{y}$ so $\norm{f} = \norm{y}$.
</proof>

What this theorem is saying is that for any continuous linear functional $f$ over a Hilbert domain, there's exactly one element in that domain that "encodes" $f$ as a dot product with that element.

We can now claim that Hilbert spaces are isomorphic to their dual, $H \cong H^\*$, that is, there exists a bijection between these two sets. In this case the function is defined by:

$$
T(y) = f_y, \quad f_y(x) = \langle x, y \rangle
$$

which is a bijection since for each $f \in H^\*$ there's a unique $y$ for which $f = f_y$. Conversely each $y$ defines a unique function $f_y$. Further since $\norm{f} = \norm{y}$ this is a "length-preserving" bijection, which leads to the more general *isometric isomorphism*.

This is a special identity for Hilbert spaces. It does not hold for example for the slightly more general [Banach space]({{blog}}/2021/06/26/hilbert-spaces.html).

## Conclusion

I thought this would be my first post on functional analysis, but I had forgotten I wrote about Hilbert spaces [before]({{blog}}/2021/06/26/hilbert-spaces.html).

This was another topic I relied entirely on ChatGPT and really liked the interactive process. It's very gratifying to start with a blurry view of it and gradually build a cohesive and more intuitive picture.

One of the most amusing moments was when I asked ChatGPT what happens if we go recursive and build a vector space of functionals, and then it pointed out it's basically the dual space, which I had already studied. It then "clicked".

It was also nice to connect with other parts I have studied in the past such as analysis and [topology](https://www.kuniga.me/docs/math/topology.html).
