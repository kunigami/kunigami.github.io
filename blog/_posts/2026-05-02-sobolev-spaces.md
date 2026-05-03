---
layout: post
title: "Sobolev Spaces"
tags: [functional analysis]
vanity: "2026-05-02-sobolev-spaces"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/sobolev.png" alt="Thumbnail of Sergei Lvovich Sobolev" />
</figure>


Continuing with my exploration of understanding physics from first math principles (see previous [post on functionals]({{blog}}/2026/04/26/functionals.html)), I wanted to learn more about Sobolev spaces.

These are a type of vector space named after the Soviet mathematician Sergei Lvovich Sobolev (1908-1989), featured on the thumbnail.

<!--more-->

## Motivation

Usually when we discuss differentiability, we have functions that are either differentiable or not.

In the real world however, we often need to handle functions that are not strictly differentiable everywhere but are almost there. A simple example is the function $f(x) = \abs{x}$ which is differentiable everywhere except at $x = 0$.

There's a more relaxed version of differentiability called the *weak differentiability* which we'll cover soon, but the idea is that a Sobolev space is the vector space of weakly differentiable functions.

Before we go there, let's lay down some nomenclature.

## Function Spaces

A vector space in which the elements are functions is generally called a **function space**. There's a special notation for a select group of function spaces, which are associated with the properties of the functions that are part of it.

The set of continuous functions with domain $\Omega$ is denoted by $C(\Omega)$. We add a superscript $k$ indicating how many times these functions can be differentiated, so $C^1(\Omega)$ is the set of functions that have a continuous derivative. The set of smooth functions (infinitely differentiable) is denoted by $C^\infty(\Omega)$.

A special property we'll need later is called **compact support**. This basically means that there exists some compact set $K \subset \Omega$ outside of which the function is 0. One example of such a function is:

$$
\begin{equation}
  \varphi(x)=\left\{
  \begin{array}{@{}ll@{}}
    \exp(-\frac{1}{1 - x^2}) & \text{if } \abs{x} \lt 1 \\
    0, & \text{otherwise}
  \end{array}\right.
\end{equation}
$$

These functions have the subscript $_c$, e.g. $C^\infty_c(\Omega)$.

The set of (Lebesgue) integrable functions is denoted by $L^p(\Omega)$. More formally:

$$
L^p(\Omega) = \curly{ f: \Omega \rightarrow \mathbb{F} \mid \int_{\Omega} \abs{f(x)}^p dx \lt \infty}
$$

A more relaxed version is that the function only needs to be integrable in compact sets. This is denoted by $L_{LOC}$:

$$
L_{LOC}^p(\Omega) = \curly{ f: \Omega \rightarrow \mathbb{F} \mid \int_{K} \abs{f(x)}^p dx \lt \infty, \quad \forall \mbox{compact set }K \in \Omega}
$$

Note that $L^p(\Omega) \subseteq L_{LOC}^p(\Omega)$. One example is $f(x) = 1$ for $\Omega = \mathbb{R}$. In this case compact set means a bounded closed interval $[a, b]$. We have:

$$
\int_{a}^{b} f(x) dx = b - a \lt \infty
$$

So $f \in L_{LOC}^p(\Omega)$. But if the interval is unbounded, then the integral is not defined so $f \not \in L^p(\Omega)$.

We also have $W^{k, p}$ which is a vector space of the functions in $L^p$ such that all their *weak* derivatives up to order $k$ are also in $L^p$:

$$
W^{k, p}(\Omega) = \curly{ f \in L^p : f\mbox{'s } n\mbox{-th weak derivative} \in L^p \mbox{ for } n \le k}
$$

We haven't defined weak derivatives yet though, but we'll do so shortly. For now it suffices to say this vector space is what we call a **Sobolev space**.


## Weak Derivatives

Let's start with the formal definition. Let $\Omega$ be an open subset of $\mathbb{R}^n$ and $u \in L_{LOC}^1(\Omega)$. We say that $v \in L_{LOC}^1(\Omega)$ is **a weak derivative of $u$ in the $i$-th direction** (denoted as $v = \partial_i u$) if:

$$
(1) \quad \int_{\Omega} u(x) \partial_i \varphi(x) dx = -\int_{\Omega} v(x) \varphi(x) dx, \quad \forall \varphi \in C^{\infty}_c(\Omega)
$$

Let's explain what each term means. The integral is over the infinitesimal box on $\mathbb{R}^n$ in the subspace $\Omega$. The function $\varphi(x)$ is called a *test function* and because it has a compact support, it's only non-zero in that compact set, so we can interpret it as a bitmask in programming or a microscope that "focuses" a function $f$ when we multiply it by $\varphi$.

The notation $\partial_i$ is the partial derivative on the dimension $i$ (i.e. with respect to $x_i$) and is a shorthand for $\partial_i f = \partial f / \partial x_i$. A more compact notation if we omit the function argument:

$$
\int_{\Omega} u \partial_i \varphi dx = -\int_{\Omega} v \varphi dx
$$

### Intuition

I still don't have a good intuition behind why weak derivatives are useful. The least unsatisfying one is this: pointwise derivatives are too strict. The weak derivative averages out $u$ locally (in the compact support of $\varphi$) by multiplying it with $\partial_i \varphi$. Since $\varphi$ is smooth, it helps get rid of the kinks in $u$.

This reminds me of the use of convolution in image processing to smooth out pixels by taking the average of the surroundings.

The use of $\partial_i \varphi$ instead of just $\varphi$ is so that we get what would correspond to $\partial_i u$ on the other side of the equality.

### Example

To get an idea on how to use this formulation, consider the function $f(x) = \abs{x}$ with $x \in \mathbb{R}$, for which we want to find the derivative. It's not differentiable at $x = 0$ because of the "edge".

For the rest of the domain we have $f'(x) = 1$ for $x \gt 0$ and $f'(x) = -1$ for $x \lt 0$. This is the function we'll tentatively take as the function $v$. We just need to show it satisfies $(1)$. Note we didn't specify what $f'(0)$ is like, but it can be anything. We'll call this function $\mbox{sgn}(x)$.

**Lemma 1.** $\mbox{sgn}(x)$ is the weak derivative of $\abs{x}$

<proof>
$$
\int_{-\infty}^{\infty} \abs{x} \varphi'(x) dx = -\int_{-\infty}^{\infty} \mbox{sgn}(x) \varphi(x) dx
$$

We can work on the left side first by splitting the integral into two:

$$
\int_{-\infty}^{\infty} \abs{x} \varphi'(x) dx = \int_{-\infty}^{0} -x \varphi'(x) dx + \int_{0}^{\infty} x \varphi'(x) dx
$$

Integration by parts gives us:

$$
= \int_{-\infty}^{0} \varphi(x) dx - \int_{0}^{\infty} \varphi(x) dx
$$

Replacing $\mbox{sgn}(x) = -1$ in the first and $\mbox{sgn}(x) = 1$ in the second:

$$
= - \int_{-\infty}^{0} \mbox{sgn}(x) \varphi(x) dx - \int_{0}^{\infty} \mbox{sgn}(x) \varphi(x) dx = - \int_{-\infty}^{\infty} \mbox{sgn}(x) \varphi(x) dx
$$

So $\mbox{sgn}$ is indeed a weak derivative of $\abs{x}$.
</proof>

So in practice we do not need to list all the test functions. We just use the specific property that it has a compact support but otherwise don't make any assumption about them.

### Derivation

Let's now understand where this formula comes from. Suppose for now that $u$ has a derivative. Since $\varphi$ is a smooth function, it also has a derivative. Suppose we multiply them and want to take the derivative on dimension $i$. We can use the product rule:

$$
\partial_i (u \varphi) = (\partial_i u) \varphi + u (\partial_i \varphi)
$$

Here we're omitting the $(x)$ parameter for simplicity. Now integrate over the entire domain $\Omega$:

$$
\int_\Omega \partial_i (u \varphi) dx = \int_\Omega (\partial_i u) \varphi dx + \int_\Omega u (\partial_i \varphi) dx
$$

We can think of $\partial_i$ as a gradient if we multiply by $e_i$ (the basis vector):

$$
\partial_i f = \nabla f \cdot e_i
$$

or in our case

$$
\partial_if (u \varphi) = \nabla (u \varphi) \cdot e_i
$$

so

$$
\int_\Omega \partial_i (u \varphi) dx = \int_\Omega  \nabla (u \varphi) \cdot e_i dx = \left(\int_\Omega  \nabla (u \varphi) dx \right) e_i
$$

By the [divergence theorem](https://www.kuniga.me/docs/math/integral.html), we have that

$$
\int_\Omega \nabla (u \varphi) dx = \int_{\delta \Omega} (u \varphi) \cdot \mathbf{n} dS
$$

But because $\varphi$ has compact support, it means outside that region it is 0. This includes the boundary $\delta \Omega$, so each term on the righthand integral is $0$ and thus:

$$
\int_\Omega \nabla (u \varphi) dx = 0
$$

and:

$$
\int_\Omega \partial_i (u \varphi) dx = 0
$$

and finally:

$$
\int_\Omega u (\partial_i \varphi) dx = -\int_\Omega (\partial_i u) \varphi dx
$$

which is the exact form as $(1)$. The only thing is that we assumed that $u$ is differentiable. So instead of requiring that $u$ is differentiable, we just require a function $v$ to exist that satisfies $(1)$. For a differentiable function, $v$ coincides with $\partial_i u$.

The idea being "If it looks like a duck, swims like a duck, and quacks like a duck, then it probably is a duck". We'll look at this from a different angle next.

### Representation, an analogy

Recall that in the post about functionals we covered the *Riesz Representation Theorem* which basically says that if $H$ is a Hilbert space then there exists some element in it, $y$, such that any linear functional $f$ on $H$ can be *represented* as the inner product $f(x) = \langle x, y\rangle$ for all $x$.

We have an analogous case for weak derivatives. For a fixed function $u$, define the functional:

$$
L(\varphi) = \int_\Omega u \partial_i \varphi dx
$$

we can show this functional is linear. So an analogous result to Riesz's is that there exists a function $v$ such that $L(\varphi)$ can be represented as:

$$
L(\varphi) = \int_\Omega v \varphi dx
$$

For all $\varphi$. The difference is that in Riesz it guarantees such a $y$ exists, but here $v$ only exists if $u$ has a *weak derivative*. Also for Hilbert spaces $y$ is from the same domain as $f(x)$, while here the domain of $L(\varphi)$ is not necessarily the same as $v$.

Ok, so now we know where the equation comes from and that it's tied to integration by parts and test functions, but why choose this specific property among all possible properties? We'll now cover examples to help us see why it's useful.

### Properties

In the example, we saw that $f = \mbox{sgn}$ is a weak derivative of $\abs{x}$ but that we can set any value for $f(0)$. At first glance this suggests that weak derivatives are not unique like derivatives are. But there's a slightly weaker result that shows they're the same almost everywhere (more formally, those with non-zero measure), as shown in *Lemma 2*:

**Lemma 2.** Let $v_1$ and $v_2$ be the weak derivative of $u$ at the $i$-th direction. Then $v_1 = v_2$ except at sets with measure $0$.

<proof>
We have that:

$$
\int_\Omega v_1 \varphi dx = \int_\Omega v_2 \varphi dx
$$

so

$$
\int_\Omega (v_1 - v_2) \varphi dx = 0
$$

Let $g = v_1 - v_2$. We want to prove that

$$
\int_\Omega g \varphi dx = 0, \quad \forall \varphi \in C^\infty_c(\Omega)
$$

implies $g = 0$ for any set with non-zero measure. Let's prove by contradiction. Suppose $\epsilon \gt 0$ and define

$$
E = \curly{ x \in \Omega: g(x) \gt \epsilon}
$$

and that $E$ has positive measure. It's possible to show that there exists a compact set $K \subset E$ with $\abs{K} \gt 0$ (I have no idea why this is true, I haven't studied measure theory). Then there must exist $\varphi(x) = 1$ with support in $K$, so

$$
\int_\Omega g \varphi dx \gt \epsilon \int_\Omega \varphi dx \ge \epsilon \int_K 1 dx = \epsilon \abs{K}
\gt 0$$

which is a contradiction. The same argument applies for negative measure.
</proof>

To say weak derivatives are *stable under limits* means that if we have a sequence of functions that converges to $u$ and their corresponding weak derivatives converge to $g$, then $g$ is a weak derivative of $u$, as shown in *Lemma 3*:

**Lemma 3.** Let $u_n$ be a family of functions in $L^2$ with weak derivatives ($\partial_i u_n$) also in $L^2$ with the following properties:

$$
\lim_{n \rightarrow \infty} u_n = u \\
\lim_{n \rightarrow \infty} \partial_i u_n = g \\
$$

Then $\partial_i u = g$ (weakly).

<proof>
Since $\partial_i u_n$ is the weak derivative of $u_n$ we have:

$$
\int_\Omega u_n \partial_i \varphi = - \int_\Omega (\partial_i u_n) \varphi
$$

We have that $\lim_{n \rightarrow \infty} u_n - u = 0$ and since $\varphi$ is bounded (from being smooth), so

$$
\lim_{n \rightarrow \infty} \int_\Omega (u_n - u) \partial_i \varphi = 0
$$

thus:

$$
\lim_{n \rightarrow \infty} \int_\Omega u_n \partial_i \varphi = \int_\Omega u \partial_i \varphi
$$

a similar line of argument gives us:

$$
\lim_{n \rightarrow \infty} \int_\Omega \partial_i u_n \varphi = \int_\Omega g \varphi
$$

so

$$
\int_\Omega u \partial_i \varphi = \int_\Omega g \varphi
$$
</proof>

This property does not hold for normal derivatives because differentiable functions do not necessarily converge to a differentiable one. An example is:

$$
u_n(x) = \sqrt{x^2 + \frac{1}{n}}
$$

which is differentiable, but it converges to $u(x) = \abs{x}$ which is not.

### Higher-order

So far we've only defined the (partial) weak derivative for a single dimension. We can however extend this to multiple dimensions and cardinality. We define a multi-index as:

$$\alpha = (\alpha_1, \alpha_2, \cdots, \alpha_n), \quad \abs{\alpha} = \sum_{i}^n \alpha_i$$

and the derivative $D^\alpha$ as:

$$
(2) \quad D^\alpha = \frac{\partial^{\abs{\alpha}} f}{\partial {x_1}^{\alpha_1}\partial {x_2}^{\alpha_2} \cdots \partial {x_n}^{\alpha_n}}
$$

the generalization of weak derivatives is then:

$$
\int_\Omega D^\alpha f \varphi = (-1)^{\abs{\alpha}} \int_\Omega f D^\alpha \varphi , \quad \forall \varphi
$$

<proof>
This formula can be obtained by repeated application of integration by parts. Suppose we know $u$ has weak derivatives for dimensions $i$ and $j$ and we want to determine

$$
\int_\Omega u \partial_i \partial_j \varphi dx
$$

Since $\varphi$ is infinitely differentiable, $\partial_j \varphi$ is a valid test function so we can apply $(1)$ to obtain:

$$
\int_\Omega u \partial_i (\partial_j \varphi) dx = -\int_{\Omega} \partial_i u (\partial_j \varphi) dx
$$

now we need to assume the function $\partial_i u$ also has a weak derivative at $j$ so we can do:

$$
\int_\Omega \partial_i u (\partial_j \varphi) dx = -\int_{\Omega} (\partial_j \partial_i u) \varphi dx
$$

putting it all together:

$$
\int_\Omega u \partial_i \partial_j \varphi dx = (-1)^2 \int_{\Omega} (\partial_j \partial_i u) \varphi dx
$$

Note that the order of partials got reversed ($i, j \rightarrow j, i$). However, partial derivatives are commutative so we can write:

$$
\int_\Omega u \partial_i \partial_j \varphi dx = \int_\Omega u \partial_j \partial_i \varphi dx
$$

which will preserve the ordering. Note that each time we apply a partial derivative we multiply the result by $-1$, that's the reason for the factor $(-1)^{\abs{\alpha}}$.

</proof>


## Sobolev Space

Now that we've explored weak derivatives, it's time to make it a proper vector space. The elements of this space are the weakly differentiable functions of order $k$ and in $L^p$. As we introduced in *Function Spaces* above:

$$
W^{k, p}(\Omega) = \curly{ f \in L^p : f\mbox{'s } m\mbox{-th weak derivative} \in L^p \mbox{ for } m \le k}
$$

When we say $m$-th derivative here we mean $D^\alpha$ for $\abs{\alpha} = m$. We can define the norm of a function in this space as:

$$
\norm{f} = \left(\sum_{\abs{\alpha} \le k} \norm{D^\alpha f}^p \right)^{1/p}
$$

Since norms are dependent on specific vector spaces we can include it to make it clearer:

$$
\norm{f}_{W^{k, p}} = \left(\sum_{\abs{\alpha} \le k} \norm{D^\alpha f}_{L^p}^p \right)^{1/p}
$$

So in English, the norm of a vector in a Sobolev space is the "length" of the vector formed by all possible partial derivatives adding up to order $k$. For example, if $p = 2$, then it becomes the Euclidean norm.

With this norm, *Lemma 4* shows that Sobolev spaces are Banach spaces.

**Lemma 4.** Sobolev spaces are Banach spaces.

<proof>
To show this, we need to show that every <a href="https://www.kuniga.me/docs/math/sequence.html">Cauchy sequence</a> converges in this space. In other words, let $(f_k)$ be a sequence of functions. Then for all $\epsilon \gt 0$, there is $N$ such that for all $j, m \ge N$:

$$
\sup_{x \in D} \norm{f_j(x) - f_m(x)}_{W^{k,p}} \lt \epsilon
$$

we want to then show that there exists $f \in W^{k, p}$ such that

$$
\lim_{j \rightarrow \infty} \norm{f_j - f}_{W^{k,p}} = 0
$$

using linear properties we have that

$$
(4.1) \quad \norm{f_j - f_m}_{W^{k,p}} = \left(\sum_{\abs{\alpha} \le k} \norm{D^\alpha f_j - D^\alpha f_m}_{L^p}^p \right)^{1/p}
$$

for a fixed $\beta$ we have:

$$
\norm{D^\beta f_j - D^\beta f_m}_{L^p}^p \le \sum_{\abs{\alpha} \le k} \norm{D^\alpha f_j - D^\alpha f_m}_{L^p}^p
$$

and

$$
\norm{D^\beta f_j - D^\beta f_m}_{L^p} \le \left(\sum_{\abs{\alpha} \le k} \norm{D^\alpha f_j - D^\alpha f_m}_{L^p}^p\right)^{1/p}
$$

so we have

$$
\norm{D^\beta f_j - D^\beta f_m}_{L^p} \le \norm{f_j - f_m}_{W^{k,p}}
$$

But because $D^\beta f_k$ is part of the Banach space $L^p$, it converges to some limit $v_\beta$ in there. In particular, if $\beta = 0$, $f_k \rightarrow v_0$. Let's call it $f = v_0$. We have that $f \in L^p$, but to show it exists in $W^{k, p}$ we need to show it has all the partial weak derivatives $D^\alpha f$ for $\abs{\alpha} \le k$.
<br /><br />
By the definition of the higher order weak derivative we have

$$
\int_\Omega D^\alpha f_j \varphi = (-1)^{\abs{\alpha}} \int_\Omega f_j D^\alpha \varphi
$$

we know that the limit $D^\alpha f_j \rightarrow v^\alpha$ and $f_j \rightarrow f$ in $L^p$, so we can take the limit for the expression above to obtain:

$$
\int_\Omega v_\alpha \varphi = (-1)^{\abs{\alpha}} \int_\Omega f D^\alpha \varphi
$$

which by definition means $v_\alpha$ is the weak derivative $D^\alpha f$. This means $D^\alpha f$ is in $L^p$ and thus $f \in W^{k,p}$. By $(4.1)$ we have:

$$
\norm{f_j - f_m}_{W^{k,p}}^p = \sum_{\abs{\alpha} \le k} \norm{D^\alpha f_j - D^\alpha f_m}_{L^p}^p
$$

since $f$ is in $W^{k,p}$ we can do:

$$
\norm{f_j - f}_{W^{k,p}}^p = \sum_{\abs{\alpha} \le k} \norm{D^\alpha f_j - D^\alpha f}_{L^p}^p
$$

taking $j \rightarrow \infty$, we know that $D^\alpha f_j \rightarrow D^\alpha f = v_\alpha$ since that's how we defined $v_\alpha$, so

$$
\lim_{j \rightarrow \infty} \norm{f_j - f}_{W^{k,p}} = \lim_{j \rightarrow \infty} \sum_{\abs{\alpha} \le k} \norm{D^\alpha f_j - D^\alpha f}_{L^p}^p = 0
$$

so every Cauchy sequence in $W^{k,p}$ converges.

</proof>

For $W^{k, 2}$ we can define the inner product as:

$$
\langle u, v \rangle = \sum_{\abs{\alpha} \le k} \int_\Omega D^\alpha u(x) D^\alpha v(x) dx
$$

The norm induced via $\langle u, u \rangle$ is:

$$
\langle u, u \rangle = \norm{u}^2 = \sum_{\abs{\alpha} \le k} \int_\Omega (D^\alpha u(x))^2 dx
$$

the integral is now the $L^2$ norm for $D^\alpha u(x)$ so

$$
\norm{u} = \left(\sum_{\abs{\alpha} \le k} \norm{D^\alpha u}^2_{L^2} \right)^{1/2}
$$

which is consistent with the norm defined before. This is enough to show that $W^{k, 2}$ is a Hilbert space. In this context it's common to use the notation $H^k = W^{k,2}$.

## Conclusion

I spent a lot of time (with ChatGPT) trying to get an intuition for weak derivatives and am still not truly satisfied. I do get the idea that we relax the conditions for a derivative by defining it via one of its properties instead.

The same idea is used in vector spaces: Hilbert spaces are those with an inner product. Inner products can induce a norm, but norms can be defined independently of inner produts. Thus, Banach spaces only require norm, not inner product, being thus more general than Hilbert spaces.

The same idea in appears in topology e.g. defining things in terms of open sets instead of Euclidean distance. The part I don't understand is why this property specifically. Why was it chosen over any other property? Maybe once I learn more about its applications in physics I'll get a better sense.

I've read about Sobolev spaces on many occasions, especially when reading about physics, so I'm glad to finally understand them a little better.

## References

* [1] ChatGPT
* [[2]({{blog}}/2026/04/26/functionals.html)] NP-Incompleteness - Functionals
