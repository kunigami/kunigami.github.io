---
layout: post
title: "The Winding Number"
tags: [analysis]
vanity: "2024-05-09-the-winding-number"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/thumbnail.svg" alt="A winding curve" style="width: 100px;" />
</figure>

This is our fourth post in the series with my notes on complex integration, corresponding to *Chapter 4* in Ahlfors' Complex Analysis.

In this one, we'll explore the concept of the winding number of a curve with respect to a point, which can be interpreted how many times a curve winds around that point. It has an interesting relationship with the Cauchy integral theorem that we learned about in a prior post.
<br />


<!--more-->

The previous posts from the series:

1. [Complex Integration]({{blog}}/2024/04/05/complex-integration.html)
1. [Path-Independent Line Integrals]({{blog}}/2024/04/13/path-independent-line-integrals.html)
1. [Cauchy Integral Theorem]({{blog}}/2024/04/26/cachy-integral-theorem.html)

## Recap

In the previous post, [Cauchy Integral Theorem]({{blog}}/2024/04/26/cachy-integral-theorem.html) [4], we concluded with *Theorem 4* saying that:

$$\int_{\gamma} f(z)dz = 0$$

if $\gamma$ is contained in a disk $\Delta$ and that $f(z)$ is holomorphic in $\Delta$. And this holds even if $f(z)$ is not holomorphic in a finite set of points in $\Delta$, as long as each of these points $\xi$ satisfy:

$$(1) \quad \lim_{z \rightarrow \xi} (z - \xi) f(z) = 0$$

Now let's look at a specific function, $f(z) = 1/(z - a)$. Assuming the point $a$ is in the disk $\Delta$, then $f(z)$ is not holomorphic at $a$, but maybe it satisfies $(1)$. We can find that out by replacing $a$ in $(1)$:

$$\lim_{z \rightarrow a} (z - a) \frac{1}{z - a} = 1$$

So no, we can't use *Theorem 4* from [4] to conclude

$$\int_{\gamma} \frac{1}{z - a} dz$$

equals 0. Still, it's worth exploring this integral further. What does its value represent? This is what we'll focus on this post.

## Definition

Let $\gamma$ be a piecewise differentiable closed curve and $a$ a point not on the curve. We define **the winding number** of $a$ with respect to $\gamma$ as:

$$(2) \quad n(\gamma, a) = \frac{1}{2\pi i} \int_{\gamma} \frac{dz}{z - a}$$

The geometric interpretation of this value is how many times the curve $\gamma$ winds (i.e. completes a revolution) counter-clockwise around $a$. Wikipedia provides several examples:

<figure class="center_children">
  <img src="{{resources_path}}/winding.svg" alt="See caption."  style="width: 500px;" />
  <figcaption>Figure 1: Different winding numbers. Source: <a href="https://commons.wikimedia.org/wiki/File:Winding_Number_-2.svg">By Jim.belk - Own work, Public Domain</a></figcaption>
</figure>

Note that when the curve winds clockwise the value is negative.

Let's build a geometric intuition on why the formula $(2)$ corresponds to the number of revolutions. Recall that the set of points in the circumference of a circle of radius $\rho$ and center $a$ can be written in polar form as:

$$z(\theta) = a + \rho e^{i\theta}$$

For $0 \le \theta \lt 2 \pi$. We can generalize this idea for any closed curve $\gamma$ by picking any point $a$ not in it. The major difference is that the "radius" (i.e. the distance between a point in $\gamma$ and $a$) $\rho$ would not be fixed, but a function of $\theta$. We can parameterize both by some $0 \le t \le 1$ as:

$$z(t) = a + \rho(t) e^{i\theta(t)}$$

Noting that $a$ doesn't have to be inside the curve. We won't prove it, but it's possible to show that if $\gamma$ is differentiable so is $\rho(t)$ and $\theta(t)$. Now if we imagine $t$ is time and we place an observer at point $a$ rotating to follow the point $z(t)$ as we travel from $t = 0$ to $t = 1$, the amount of "angle displacement" this observer will perform can be found by adding up the delta angle between adjacent timestamps, $\Delta \theta = \theta(t_i) - \theta(t_{i-1})$.

Because it's a closed curve, the observer must finish facing at the same direction they started, meaning they completed an integer number of revolutions and thus the total angle displacement should be a multiple of $2 \pi$.

If we allow $\theta(t)$ to go beyond $2\pi$ (when multiple revolutions occur), then the total angle displacement is: $\theta(1) - \theta(0)$ and the number of revolutions (and hence the winding number) is

$$(3) \quad n(\gamma, a) = \frac{\theta(1) - \theta(0)}{2\pi}$$

With this intution in mind, *Lemma 1* formalizes the correspondence between $(2)$ and the number of revolutions:

**Lemma 1.** Let $\gamma$ be a piecewise differentiable closed curve and a point $a$ not in $\gamma$. The curve can parametrized with respect to $a$:

$$z(t) = a + \rho(t) e^{i\theta(t)}$$

for $0 \le t \le 1$ and differentiable functions $\rho(t)$ and $\theta(t)$. Then:

$$(4) \quad \int_{\gamma} \frac{dz}{z - a} = i(\theta(1) - \theta(0))$$

<proof>

First we want to replace $dz$ with $dt$ in $(2)$. We can find it out by differentiating $z$ with respect to $t$, using the product rule:

$$\frac{dz}{dt} = \frac{\rho(t)}{dt} e^{i\theta(t)} + i \rho(t) \frac{\theta(t)}{dt} e^{i\theta(t)}$$

So:

$$dz = (\rho'(t) e^{i\theta(t)} + i \rho(t) \theta'(t) e^{i\theta(t)}) dt$$

We then replacing $z(t)$ and $dz$ in $(2)$:

$$\int_{\gamma} \frac{dz}{z - a} = \int_{0}^{1} \frac{(\rho'(t) e^{i\theta(t)} + i \rho(t) \theta'(t) e^{i\theta(t)}) dt}{\rho(t) e^{i\theta(t)}}$$

Cancelling the common terms:

$$= \int_{0}^{1} \frac{(\rho'(t) + i \rho(t) \theta'(t)) dt}{\rho(t)}$$

We can split into two integrals, so that we can cancel more terms in the second integral:

$$= \int_{0}^{1} \frac{\rho'(t) dt}{\rho(t)} + i \int_{0}^{1} \theta'(t)$$

We can use the logarithm derivative trick $(\ln f(x))/dx = f'(x)/f(x)$ [2] to obtain:

$$= \int_{0}^{1} \frac{\ln \rho(t)}{dt} + i \int_{0}^{1} \theta'(t)$$

We can now use the fundamental theorem of calculus to evaluate the integrals:

$$= \ln \rho(1) - \ln \rho(0) + i(\theta(1) - \theta(0))$$

Since it's a closed curve, the point at $t = 0$ and $t = 1$ are the same, so their "radius" cancel out, leaving us with:

$$\int_{\gamma} \frac{dz}{z - a} = i(\theta(1) - \theta(0))$$

<i>QED</i>
</proof>

We can replace $(3)$ in $(4)$:

$$\int_{\gamma} \frac{dz}{z - a} = (2 \pi i) n(\gamma, a)$$

Which gives us $(2)$.

## Properties

### Inverse direction

The first property is that if we flip the direction of a curve $\gamma$, we negate the winding number, i.e.:

$$n(\gamma, a) = -n(-\gamma, a)$$

This follows from the application of $(2)$ and using that

$$\int_\gamma f(z)dz = -\int_{-\gamma} f(z)dz$$

### Point outside

Intuitively, if you're an observer away from the curve, you don't need to "turn around yourself" to follow a point along it, so the winding number of an external point is 0, as in the third example of *Figure 1*. How about the example from *Figure 2* (left): is it inside or outside?

<figure class="center_children">
  <img src="{{resources_path}}/mummy.svg" alt="See caption."  style="width: 500px;" />
  <figcaption>Figure 2: Point $a$ is enclosed by the curve but it's not technically inside it. Its windinging number is 0. The blue segment on the right figure make the observer turn clockwise, completing a full revolution. However, on the red segment it "unwinds" and by the time it finished following the curve it's back at the angle it started.</figcaption>
</figure>

We need a more precise way to define "outsideness". There's a topological formal definition, but we won't go over it here. Instead, we can get an intuition by imagining the curve is a rubberband on the surface of a table. You then put your finger at point $a$. If you can remove the rubberband without lifting your finger, then point $a$ is "outside".

In the case of *Figure 2*, the observer ends up going around thelselves, but they reverse back before finish, so in the end their winding number is still 0 (left image). It's possible to show that the winding number is 0 if and only if a point is outside the curve, so we could use this alternative definition for "outsideness".

We won't prove this equivalence here, but rather a weaker result, in which the observer is "away" from the curve, i.e. not surrounded by it:

**Lemma 2.** Let $\gamma$ be a closed curve, $C$ be a circle enclosing it and $a$ a point outside the circle. Then $n(\gamma, a) = 0$.

<proof>
The example in [3] comments that

$$f(z) = \frac{1}{z - a}$$

is holomorphic if $z \ne a$. Since $a$ is outside the circle $C$, we have that $C$ is a region where $f$ is holomorphic and hence can utilize <i>Theorem 3</i> from [4] to conclude that:

$$\int_\gamma \frac{1}{z - a} dz = 0$$

For any curve $\gamma$ in $C$. Using this in $(2)$ gives us $n(\gamma, a) = 0$.

</proof>

We can always enclose any bounded curve with a circle if we choose a sufficiently large radius and still have $\infty$ outside it, which leads us to the corollary:

**Corollary 3.** Let $\gamma$ be a closed curve. Then $n(\gamma, \infty) = 0$

### Regions

Generalizing the idea of inside/outside, we can consider the regions determined by a curve $\gamma$. The curve $\gamma$ is a bounded and closed set, so its complement is unbounded and open. The complement can be partitioned into [connected components]({{docs}}/math/topology.html#connectedness), exactly one of which is unbounded (the one containing infinity).

*Lemma 4* shows that any two points in the same region have the same winding number.

**Lema 4.** Let $\gamma$ be a closed curve and let $\curly{R_i}$ be the set of components determined by it. Let $a, b$ be points in the same region $R_i$. Then $n(\gamma, a) = n(\gamma, b)$.

<proof>

Since they're in the same connected component, there's a path between $a$ and $b$ using line segments, none of which crosses $\gamma$. Let $\overline{uv}$ be a line segment contained within a region. If we prove that $n(\gamma, u) = n(\gamma, v)$, then we can prove the same for $a$ and $b$ by transitivity.
<br /><br />
Let's define $\Omega$ as the complement of $\overline{uv}$. We claim that for $z \in \Omega$ the function:

$$(4.1) \quad f(z) = \frac{z - u}{z - v}$$

returns either a positive real value or a imaginary number, that is, never a negative real value. The key reason is that since $z$ doesn't belong to $\overline{uv}$. With this in mind, let us write $(z - u)$ and $(z - v)$ in polar form, say $r_u e^{i\theta_u}$ and $r_v e^{i\theta_v}$, so

$$f(z) = \frac{r_u}{r_v} e^{i(\theta_u - \theta_v)}$$

We can interpret $(z - u)$ and $(z - v)$ geometrically as directed vectors, from $\overrightarrow{uz}$ and $\overrightarrow{vz}$ respectively. We have two cases: either $z$ lies on the line defined by $u$ and $v$ (but not between them!) or not. In the first case, the angle $\theta_u = \theta_v$, since the vectors point in the same direction, so $f(z) = r_u/r_v$, that is, a positive real.
<br /><br />
In case 2, $z$ is not in the line defined by $u, v$, so it forms a triangle as depicted in <i>Figure (4.1)</i>:

<figure class="center_children">
  <img src="{{resources_path}}/angles.svg" alt="See caption."  style="width: 500px;" />
  <figcaption>Figure 4.1: Triangle formed by $u, v$ and $z$ with the relevant angles depicted</figcaption>
</figure>

We can use elementary geometry to determine $\theta_u - \theta_v$. We have the following relations:

$$
\begin{alignat*}{3}
(4.C) & \quad \pi &= \alpha_1 + \alpha_2 + \alpha_3 \\
(4.D) & \quad \theta_u &= \alpha_1 + \beta \\
(4.E) & \quad \theta_v &= \pi - \alpha_3 + \beta \\
\end{alignat*}
$$

Subtracting $(4.E)$ from $(4.D)$:

$$
\theta_u - \theta_v = \alpha_1 + \alpha_3 - \pi
$$

Replacing $(4.C)$:

$$
= \alpha_1 + \alpha_3 - (\alpha_1 + \alpha_2 + \alpha_3) = -\alpha_2
$$

So $\abs{\theta_u - \theta_v} = \alpha_2$. Since $z$ is not on the line defined by $u$ and $v$, $0 \lt \alpha_2 \lt \pi$. The imaginary part of $f(z)$ is given by $\sin (\theta_u - \theta_v)$. The only way for it to be 0 is for the angle to be a multiple of $\pi$ which we showed it isn't, so we conclude, in this case, that $f(z)$ has an imaginary component.
<br /><br />
This property is important because we wish to compute $\log(f(z))$. The function $\log(z)$ is not holomorphic on the entire complex plane but is for $\mathbb{C} - \mathbb{R}_{\le 0}$ (i.e. complex numbers excluding the non-positive reals). So basically we're saying that for any $z \in \Omega$, the log of $\log(f(z))$ has a derivative, which can be show to be:

$$(4.2) \quad g(z) = \frac{d\log(f(z))}{dz} = \frac{1}{z - u} - \frac{1}{z - v}$$

Since $g(z)$ is the derivative of a holomorphic function $\log(f(z))$, in $\Omega$, we can use <i>Corollary 1</i> in [4] to conclude:

$$\int_{\gamma'} \paren{\frac{1}{z - u} - \frac{1}{z - v}}dz = 0$$

For any closed curve $\gamma'$ in $\Omega$. Since $\overline{uv}$ is a line segment contained within a region, it doesn't cross $\gamma$ and hence $\gamma$ is contained in $\Omega$ and we can substitude $\gamma$ in $\gamma'$ and conclude:

$$\int_{\gamma} \frac{1}{z - u}dz = \int_{\gamma} \frac{1}{z - v}dz$$

Which is the definition of the winding number, and we prove that $n(\gamma, u) = n(\gamma, v)$.

</proof>

A corollary from *Lemma 4*, *Corollary 3* and that the unbounded region determined by $\gamma$ contains $\infty$:

**Corollary 5.** Let $\gamma$ be a closed curve and $R_0$ be the unbounded region determined by $\gamma$. Then if $a \in R_0$, $n(\gamma, a) = 0$.

This provides a stronger result than *Lemma 3*. We don't need $a$ to be outside a circle enclosing $\gamma$. As long as there's a path from $\infty$ to $a$ not crossing $\gamma$, the winding number of $a$ is 0, for example the one in *Figure 3*.

<figure class="center_children">
  <img src="{{resources_path}}/labirynth.svg" alt="See caption."  style="width: 250px;" />
  <figcaption>Figure 3: $a$ is enclosed deeply into $\gamma$'s bowels, but there's a way out, so its winding number is 0.</figcaption>
</figure>

It's still not a necessary condition for the winding number to be 0 though, since in *Figure 2* there still no way out but the winding number is still 0.

So far we've been consider conditions that lead to $n(\gamma, a) = 0$. We now consider conditions that lead to  $n(\gamma, a) = 1$.

### Single revolution

To simplify calculations, we'll assume $a$ is at the origin. If we take the geometric interpretation of winding number, we can see it's invariant with translation and in the equation $(2)$, the expression $z - a$ is essentially doing this normalization.

We also assume $a$ is surrounded by the curve $\gamma$, otherwise we already know its winding number is 0. Then, visualizing this on the complex plane, $\gamma$ has to exist in all four quadrants since it surrounds the origin, such as the curve in *Figure 4*.

<figure class="center_children">
  <img src="{{resources_path}}/winding-1.svg" alt="See caption."  style="width: 300px;" />
  <figcaption>Figure 4: Closed curve around the origin, counter-clockwise. $z_1$ is a point on the positive imaginary and $z_2$ on the negative one. The path from $z_1$ to $z_2$ is depicted in blue and the one from $z_2$ to $z_1$ in red.</figcaption>
</figure>

We can now state a sufficient condition for $n(\gamma, a) = 1$:

**Lemma 6.** Let $\gamma$ be a curve around the origin. We can pick points $z_1$ and $z_2$ such that $z_1$ has a positive imaginary component and $z_2$ a negative one. Denote the part of the curve from $z_1$ to $z_2$ as $\gamma_1$ and from $z_2$ to $z_1$ as $\gamma_2$.

If $\gamma_1$ doesn't cross the positive real axis and $\gamma_2$ doesn't cross the negative real axis, then $n(\gamma, a) = 1$, where $a$ is the origin.

<proof>
We draw auxiliary structures in our curve from <i>Figure 4</i>. First consider a circle $C$ centered in the origin and contained in $\gamma$. Let $\xi_1$ and $\xi_2$ the projection of $z_1$ and $z_2$ onto the circumference of $C$. Let $C_1$ be the arc from $\xi_1$ to $\xi_2$ and $C_2$ the arc from $\xi_2$ to $\xi_1$. Let $\delta_1$ be the directed segment from $z_1$ to $\xi_1$ and $\delta_2$ from $z_2$ to $\xi_2$. These are show in <i>Figure 6.1</i> (left).

<figure class="center_children">
  <img src="{{resources_path}}/winding-1-proof.svg" alt="See caption."  style="width: 600px;" />
  <figcaption>Figure 6.1: <i>Figure 4</i> with auxiliary structures and markers.</figcaption>
</figure>

We note that $\xi_1$ is still on the positive $y$-axis and $\xi_2$ on the negative. It's clearer to see that $C_1$ doesn't cross the positive $x$-axis. The angle $\theta_1$ of $\xi_1$ with the origin is between $(0, \pi)$ and $\theta_2$ from $\xi_2$ is between $(\pi, 2\pi)$. Since the arc $C_1$ is counter-clockwise, it goes continuously from $\theta_1$ to $\theta_2$ without the angle reaching $2\pi$ and hence avoids the positive $x$-axis. Similar reasoning shows that $C_2$ doesn't cross the negative $x$-axis.
<br /><br />
We now build two closed curves: $\sigma_1 = \gamma_1 + \delta_2 - C_1 - \delta_1$ and $\sigma_2 = \gamma_2 + \delta_1 - C_2 - \delta_2$. $\sigma_2$ is shown in <i>Figure 6.1</i> (right). Because $a$ lies outside of $\sigma_1$ and $\sigma_2$ by construction, we have

$$(6.1) \quad n(\sigma_1, a) = n(\sigma_2, a) = 0$$


Also when we do $\sigma_1 + \sigma_1$, terms cancel and we obtain:

$$\sigma_1 + \sigma_1 = \gamma_1 + \gamma_2 - (C_1 + C_2) = \gamma - C$$

That is,

$$\gamma = \sigma_1 + \sigma_2 + C$$

Since

$$\int_\gamma f(z)dz = \int_{\sigma_1} f(z)dz + \int_{\sigma_2} f(z)dz + \int_{C} f(z)dz$$

We have

$$n(\gamma, a) = n(\sigma_1, a) + n(\sigma_2, a) + n(C, a)$$

By $(6.1)$:

$$n(\gamma, a) = n(C, a)$$

And we know that $n(C, a) = 1$, so the lemma is proved.
</proof>

In the example of *Figure 4*, *Lemma 6* says that because the blue path doesn't cross the positive $x$-axis and the red path doesn't cross the negative one, then $n(\gamma, a) = 1$.

## Conclusion

To recap, we started by analyzing an example function for which we can't use *Theorem 4* in [4] and showed that it has a nice geometric interpretation, the number of revolutions around a point.

We considered some properties such as those sufficient for $n(\gamma, a) = 0$ and $n(\gamma, a) = 1$. The winding number is not just a geometric curiosity though, it will be needed as we progress in our study of complex integration.

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2]({{docs}}/math/derivative.html)] NP-Incompleteness: Derivatives Cheat Sheet
* [[3]({{blog}}/2024/04/13/path-independent-line-integrals.html)] NP-Incompleteness: Path-Independent Line Integrals
* [[4]({{blog}}/2024/04/26/cachy-integral-theorem.html)] NP-Incompleteness: Cauchy Integral Theorem
