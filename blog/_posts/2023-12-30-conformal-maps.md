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

Let's try to gain some intuition by considering an example in 2D. Let $\gamma_1$ and $\gamma_2$ be two arbitrary curves that pass through a point $z$. Consider the tangents of the curves at $z$. Since the curves are directed, we can use that to direct their tangents and find the angle between, which we'll call $\theta$. *Figure 1* (left) depicts an example.

<figure class="center_children">
  <img src="{{resources_path}}/conformal-example.png" alt="See caption." />
  <figcaption>Figure 1: (left) Two curves intersecting at point $p$ with their tangents having an angle $\theta$. (right) The same curves after applying some function $f$. Since their tangents still have an angle at $p' = f(p)$, $f$ is conformal at $p$. Source: hand-made</figcaption>
</figure>

We can apply a function $f$ to $\gamma_1$ and $\gamma_2$. Let's denoted the resulting curves by $\xi_1$ and $\xi_2$ respectively. Since $z \in \gamma_1$ and $z \in \gamma_2$, we have that $w = f(z)$ belongs to $\xi_1$ and $\xi_2$. Now consider the tangents of $\xi_1$ and $\xi_2$ at $w$. If the angle between them is also $\theta$, then this function is conformal at $z$. *Figure 1* (right) shows an example.

### Parametric curves

A continuous curve in the complex plane can be thought as a function $\gamma(t): [a, b] \rightarrow \mathbb{C}$ where $t$ is from a real-number $a \le t \le b$. One intuitive way to see this is $\gamma$ being the path taken by a point from timestamp $a$ to $b$. This is called a **parametric curve**.

### Tangent

Now that $\gamma$ is a function, it's possible to calculate precisely the direction of the tangent at a given point $z_0 = \gamma(t_0)$, as we state in *Lemma 1*.

**Lemma 1** Let $\gamma(t): [a, b] \rightarrow \mathbb{C}$ be a parametric curve. The direction of the tangent at point $z_0 = \gamma(t_0)$ is given by the *argument* of the complex number $z'$:

$$z' = \frac{d\gamma}{dt}(t_0)$$

<proof>

Let $t_0$ be such that $z_0 = \gamma(t_0)$. The idea is to consider a $t_1$ close enough to $t_0$ so the direction of vector formed by $\gamma(t_1) - \gamma(t_0)$ matches that of the tangent.
<br /><br />
We need to choose $t_1$ as close as possible to $t_0$, so we account for any possible tiny local curvature around $t_0$, meaning that at that neighborhood the path from $\gamma(t_0)$ to $ \gamma(t_1)$ looks like a straight line. We can thus compute:

$$\lim_{t_1 \rightarrow t_0} \frac{\gamma(t_1) - \gamma(t_0)}{t_1 - t_0}$$

which is the definition of $\frac{d\gamma}{dt}(t_0)$. As we see in the Figure above, this complex number represents a vector with the same direction as the tangent. We can compute the angle a complex number $z$ forms with positive x-axis via $\arg{(z)}$.

</proof>

### Curve Mapping

What happens when we apply a function $f$ over $\gamma$? We'll obtain another curve $\xi$ which we can also see as a function of $t$ or as a composition of $f$ and $\gamma$:

$$\xi(t) = (f \circ \gamma)(t)$$

Let $w_0 = f(z_0)$. What can we say about the tangent $w'$ of $\xi$ at $w_0$? Again suppose $z'$ is the tangent of $\gamma$ at $z_0$ and its angle with the origin is $\theta$, then *Lemma 2* states that the angle of the tangent $w'$ with the origin is $\theta + \arg{(f'(z_0))}$.

**Lemma 2** Let $\gamma(t): [a, b] \rightarrow \mathbb{C}$ be a parametric curve with tangent $z'$ at point $z_0$. Consider a function $f: \mathbb{C} \rightarrow \mathbb{C}$ which is holomorphic and with a non-zero derivative at $z_0$ and apply it over $\gamma$, obtaining the parametric curve $\xi(t) = (f \circ \gamma)(t)$.

Let $w_0 = f(z_0)$ and $w'$ be the tangent of $\xi$ at point $w_0$. The argument of $w'$ is given by

$$\arg{(w')} = \arg{(z')} + \arg{\left(\frac{df}{dz} (z_0) \right)}$$

<proof>

Since $\xi$ is also a parametric curve of $t, we can use <i>Lemma 1</i> to compute $w'$:

$$w' = \frac{d\xi}{dt}(t_0)$$

Since $\xi$ is a composite of $f$ and $\gamma$, we can use the chain rule to get:

$$\frac{d\xi}{dt}(t_0) = \frac{df}{dz}(\gamma(t_0)) \cdot \frac{d\gamma}{dt}(t_0)$$

The second term on the right hand side is, from <i>Lemma 1</i>, the tangent of $\gamma$ at $z_0 = \gamma(t_0)$. The first term is the complex derivative of $f$ at $z_0$ and it's a complex number. So we have a product of two complex numbers.

The argument of $w'$ is thus:

$$\arg{(w')} = \arg{\left(\frac{df}{dz}(\gamma(t_0)) \cdot \frac{d\gamma}{dt}(t_0)\right)}$$

Here's where we need the hypothesis of $f'(z_0) \ne 0$, because otherwise $\arg (0)$ isn't defined.

Since the argument of the product is the sum of the arguments:

$$\arg{(w')} = \arg{\left(\frac{df}{dz}(\gamma(t_0)) \right)} + \arg{\left(\frac{d\gamma}{dt}(t_0)\right)} = \arg{(z')} + \arg{\left(\frac{df}{dz} (z_0) \right)}$$

</proof>


### Holomorphic Functions

One way to interpret this *Lemma 2* is that applying a function $f$ over a curve, cause its tangent at a point $z_0$ to rotate by some amount $f'(z_0)$ that only depends on the point $z_0$ but not on the curve itself.

This means that if two curves $\gamma_1$ and $\gamma_2$ intersect at $z_0$ at an angle $\theta$, applying the same function over them to obtain $\xi_1$ and $\xi_2$, both their tangents have been "rotated" by the same amount, so the angle between them is still the same.

This idea is formalized in *Theorem 3*.

<theorem>
<b>Theorem 3.</b> If a function $f$ is holomorphic in $\Omega$ and its derivative is non-zero anywhere in $\Omega$ then it is conformal.
</theorem>

<proof>

Let two curves $\gamma_1$ and $\gamma_2$ intersect at $z_0$ at an angle $\theta$. By <i>Lemma 1</i>, their tangents are complex numbers $z'_1$ and $z'_2$ and the angle between them is given by:

$$\theta = \arg{(z'_1)} - \arg{(z'_2)}$$

Now we apply the function $f$ over them to obtain $\xi_1$ and $\xi_2$. Let $w'_1$ and $w'_2$ be the respective tangents at $w_0 = f(z_0)$. The angle $\theta'$ between them is:

$$\theta' = \arg{(w'_1)} - \arg{(w'_2)}$$

Since $f$ is holomorphic and with a non-zero derivative at $z_0$, we can apply <i>Lemma 2</i>:

$$\theta' = (\arg{(f'(z_0))} + \arg{(z_1')}) - (\arg{f'(z_0)} + \arg{(z_2')})$$

We conclude that $\theta' = \theta$ and that $f$ is a conformal map.

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
