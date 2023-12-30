---
layout: post
title: "Conformal Maps"
tags: [analysis]
excerpt_separator: <!--more-->
vanity: "2023-12-30-conformal-maps"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/thumbnail.png" alt="Mandelbrot fractal. This is a small scale version (different color scheme) of Figure 3." />
</figure>

This is part of the series on complex analysis. I recommend first checking the previous post:

* [Holomorphic Functions]({{blog}}/2023/12/21/holomorphic-functions.html).

In this post we'll discuss conformal maps, in particular understand its definition, provide examples and also make a connection with holomorphic functions.

<!--more-->

## Definition

A function $f: U \rightarrow V$ is called **conformal** (or **angle-preserving**) at a point $z \in V$ if it preserves angles between two directed curves through $z$ [4].

Let's try to gain some intuition by considering an example in 2D. Let $\gamma$ and $\gamma'$ be two arbitrary curves that pass through a point $z$. Consider the tangents of the curves at $z$. Since the curves are directed, we can use that to direct their tangents and find the angle between, which we'll call $\theta$. *Figure 1* (left) depicts an example.

<figure class="center_children">
  <img src="{{resources_path}}/conformal-example.png" alt="See caption." />
  <figcaption>Figure 1: (left) Two curves intersecting at point $p$ with their tangents having an angle $\theta$. (right) The same curves after applying some function $f$. Since their tangents still have an angle at $p' = f(p)$, $f$ is conformal at $p$. Source: hand-made</figcaption>
</figure>

We can apply a function $f$ to $\gamma$ and $\xi$. Let's denoted the resulting curves by $\xi$ and $\xi'$ respectively. Since $z \in \gamma$ and $z \in \xi$, we have that $w = f(z)$ belongs to $\gamma'$ and $\xi'$. Now consider the tangents of $\gamma'$ and $\xi'$ at $w$. If the angle between them is also $\theta$, then this function is conformal at $z$. *Figure 1* (right) shows an example.

## Holomorphic Functions

In *Theorem 1* we show that conformal maps are holomorphic functions at points in which the derivative is non-zero.

<theorem>
<b>Theorem 1.</b> A function $f$ is a conformal in an open set $\Omega$ if and only if it's holomorphic in $\Omega$ and its derivative is non-zero anywhere in $\Omega$.
</theorem>

<proof>
Let's first prove that if a function is holomorphic in $\Omega$ and its derivative is non-zero anywhere in $\Omega$ then it is conformal.

Consider a parametric curve $\gamma_1$ contained in $\Omega$, that is, the points in $\gamma_1$ can be described via a function $z_1(_1)$, $z_1 : \mathbb{R} \rightarrow \mathbb{C}$, for $\alpha \le t \le \beta$. And assume that $\gamma_1$ is differentiable for all $t$.

If we apply $f$ on $\gamma_1$ we'll obtain another curve $\gamma'_1$, where its points are defined by $w_1(t) = f(z_1(t))$ for $\alpha \le t \le \beta$. Now consider the derivative of $w_1(t)$ with respect to $t$. By the chain rule of calculus we have:

$$\frac{d}{dt}w_1(t) = \frac{d}{dz_1}f(z_1(t)) \frac{d}{dt}z_1(t)$$

or with a more compact syntax:

$$(1) \qquad w'_1(t) = f'(z_1(t)) z'_1(t)$$

Since $f$ is holomorphic, $f'(z_1(t))$ exists, $z'_1(t)$ is the tangent of the curve $\gamma$ at the point $z_1(t)$ and $w'(t)$ is the tangent of $\gamma'$ at the point $w(t)$.

All these terms are complex numbers, so we can compute their angles in the complex plane via their argument. Recall that a complex number can be written in polar form, $z = r \cos \theta + i \sin \theta$, where $\theta$ is called the argument of $z$, or $arg(z)$.

It's possible to show that the argument of the products of two complex numbers is the sum of their arguments, that is: $\mbox{arg}(z w) = \mbox{arg}(z) + \mbox{arg}(w)$. Applying this to (1) gives us:

$$(2) \quad \mbox{arg} (w_1'(t)) = \mbox{arg} (f'(z_1(t))) + \mbox{arg} (z_1'(t))$$

Here's where we need the hypothesis of $f'(z) \ne 0$ for $z \in A$, because otherwise $\mbox{arg} (0)$ isn't defined.

Consider another parametric curve $\gamma_2$ contained in $\Omega$ whose points can be described via a function $z_2(t)$, $z_2 : \mathbb{R} \rightarrow \mathbb{C}$, for $\alpha \le t \le \beta$.

Suppose curves $\gamma_1$ and $\gamma_2$ intersect at point $p = z_1(t_1) = z_2(t_2)$. We have that $z_1'(t_1)$ is the tangent of $\gamma_1$ at $p$ and $z_2'(t_2)$ the tangent of $\gamma_2$ at $p$. The angle $\theta$ between them can be found via:

$$\theta = \mbox{arg} (z_1'(t_1)) - \mbox{arg} (z_2'(t_2))$$

Now consider the curves corresponding to applying $f$ to $\gamma_1$ and $\gamma_2$, denoted by $\gamma_1'$ and $\gamma_2'$. These curves contain the point $p' = f(p)$ and the tangents for $\gamma_1'$ and $\gamma_2'$ at $p'$ are $w_1'(t_1)$ and $w_2'(t_2)$, respectively.

The angle $\theta'$ between these two tangents is given by:

$$\theta' = \mbox{arg} (w_1'(t_1)) - \mbox{arg} (w_2'(t_2))$$

We can use (2) for both curves and replace in the equation above:

$$\theta' = (\mbox{arg} (f'(z_1(t_1))) + \mbox{arg} (z_1'(t_1))) - (f'(z_2(t_2))) + \mbox{arg} (z_2'(t_2))$$

Since $z_1(t_1) = z_2(t_2)$, the first term cancels out and we're left with:

$$\theta' = \mbox{arg} (z_1'(t_1)) - \mbox{arg} (z_2'(t_2)) = \theta$$

Which is the definition of a conformal map at $p$. Note we didn't add any restriction other than $p \in A$. Let's prove the other direction, i.e., we're given a conformal function $f$ at any $p \in A$ and we wish to show that $f$ is holomorphic and that $f'(p) \ne 0$.

Using the same setup from the other direction, we have two curves $\gamma_1$ and $\gamma_2$ intersecting at $p$. The implicit assumption is that the tangents at $p$ and $f(p)$ exist for both $\gamma_1$ and $\gamma_2$ so $\mbox{arg} (z_1'(t_1))$, $\mbox{arg} (z_2'(t_2))$, $\mbox{arg} (w_1'(t_1))$ and $\mbox{arg} (w_2'(t_2))$ are all defined.

Plugging them in (2):

$$\mbox{arg} (f'(z_1(t))) = \mbox{arg} (w_1'(t)) - \mbox{arg} (z_1'(t))$$

and

$$\mbox{arg} (f'(z_2(t))) = \mbox{arg} (w_2'(t)) - \mbox{arg} (z_2'(t))$$

so $\mbox{arg} (f'(z_1(t)))$ and $\mbox{arg} (f'(z_2(t)))$ are defined. These are necessary for $f'(z_1(t))$ and $f'(z_2(t))$ to exist, but not sufficient. Since $z_1(t) = z_2(t) = p$, and $f'$ is defined as a limit, we can't have the limit converge to different values, so we must have that $f'(z_1(t)) = f'(z_2(t))$.

This is why we need the hypothesis that

$$\mbox{arg} (z_1'(t_1)) - \mbox{arg} (z_2'(t_2)) = \mbox{arg} (w_1'(t_1)) - \mbox{arg} (w_2'(t_2))$$

Because that implies $f'(z_1(t)) = f'(z_2(t))$ and that they exist. They must be non-zero because $\mbox{arg} (f'(z_1(t)))$ is defined. Since this holds for any $p = z_1(t) \in A$, we conclude $f$ is holomorphic and $f'(p) = 0$.

</proof>

## Examples

One of the simplest conformal mapping is the function $f(z) = z + w$ for a constant $w \in \mathbb{C}$. If we interpret it in the complex plane, the function corresponds to a translation of each point on its domain. It's not hard to believe that the angles are preserved on translation. In fact, we have $f'(z) = 1$, so it's never zero and it's conformal everywhere.

Another example is the function $f(z) = kz$, for a constant $k \in \mathbb{R}$. In the complex plane we can interpret it as a *homothety*. It's not quite the same as scaling because there's also displacement involved, depending on how far points are from the origin. *Figure 3* makes it more intuitive:

<figure class="center_children">
  <img src="{{resources_path}}/homothety.png" alt="See caption." />
  <figcaption>Figure 3: The letter "T" transformed by multiplying every point in it by a constant. The effect is that the object scales but also moves further away from the origin. Source <a href="https://commons.wikimedia.org/wiki/File:Zentr-streck-T-e.svg">Wikipedia</a></figcaption>
</figure>

Another interesting example is the function $f(z) = z^2$. It has the derivative $f'(z) = 2z$, so it's a conformal map except at $z = 0$. It's hard to visualize what it looks like but we can consider the domain as being horizontal and vertical lines as *Figure 4* (left). The corresponding curves after applying $f(z)$ are the "horizontal parabolas" shown in *Figure 4* (right).

<figure class="center_children">
  <img src="{{resources_path}}/conformal-map-grid.png" alt="See caption." />
  <figcaption>Figure 4: (a) Grid composed of horizontal (red) and vertical (blue) lines. (b) The result of applying $f(z) = z^2$ to them. Source <a href="https://observablehq.com/d/4da21abcd4e93220">Observable</a>.</figcaption>
</figure>

We can see that at the intersection points on the domain, the blue and red lines intersect at 90 degrees. On the transformed curves, if we consider the tangents at the intersection points, they also have 90 degrees between them, except at $(0, 0)$ where they're now at 180 degrees. But $f'(0)$ is zero, so $f$ is not conformal there.

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2](https://en.wikipedia.org/wiki/Conformal_map Wikipedia)] Conformal Map
* [[3](https://www-users.cse.umn.edu/~olver/ln_/cml.pdf)] Complex Analysis and Conformal Mapping, Peter J. Olver.
