---
layout: post
title: "Bipolar Coordinates and Möbius Transformations"
tags: [analysis, geometry]
excerpt_separator: <!--more-->
vanity: "2024-02-10-bipolar-coordinates"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/thumbnail.png" alt="a bunch of lines and circles abstract art" />
</figure>

In this post we'll discuss the bipolar coordinate system and how it can be used to simplify Möbius transformations. It puts together a bunch of concepts we studied in prior posts, so being familiar with the series is helpful.

This is the last post of the series based on holomorphic functions, which correspond to Chapter 3 (*Analytic Functions as Mappings*) in Ahlfors' Complex Analysis [1].

<!--more-->

1. [Holomorphic Functions]({{blog}}/2023/12/21/holomorphic-functions.html)
2. [Conformal Maps]({{blog}}/2023/12/30/conformal-maps.html)
3. [Möbius Transformation]({{blog}}/2024/01/08/mobius-transformation.html)
4. [Cross-Ratio]({{blog}}/2024/01/13/cross-ratio.html)
5. [Circles of Apollonius]({{blog}}/2024/01/20/circles-of-apollonius.html)
6. [Symmetry Points of a Circle]({{blog}}/2024/02/03/circles-symmetry.html)

In particular, we'll need the circles of Apollonius, symmetry points of a circle and, of course, the Möbius transformation.



## Circles of Apollonius

Recall from [2] that, given two points $a$ and $b$ and a ratio $q$, the circle of Apollonius is the set of points $z$ satisfying:

$$\frac{\abs{z - a}}{\abs{z - b}} = q$$

We call $a$ and $b$ the *foci* and each $q$ defines a different circle.

The collection of circle of Apollonius (for all possible $q$) and all circles through $a$ and $b$ form the **bipolar Coordinate** system induced by $a$ and $b$. Each point on the complex plane except $a$ and $b$ is the intersection of exactly one circle of Apollonius and one circle through $a$ and $b$ (see *Figure 1*, left).

That means that points can be uniquely identified by their corresponding Apollonius and one circle through $a$ and $b$, and so this forms a coordinate system, known as the **bipolar coordinate**.

Why bother with such a convoluted way to identify the position of points? In the same way that polar coordinates can be make some problems simpler, we can use the bipolar coordinates to simplify things. For this post in particular we'll focus on its application for Möbius transformations.

<figure class="center_children">
  <img src="{{resources_path}}/coordinates.png" alt="See caption." />
  <figcaption>Figure 1: (left) Bipolar coordinate system: the Apolonius circles are colored in blue, circles through $a$ and $b$ are colored in red. (right) Polar coordinate system: Apolonius circles are mapped to circles at the origin, circles through $a$ and $b$ are mapped to lines through the origin. Source: <a href="https://observablehq.com/d/bcb552da7cd8a6cf">Observable</a></figcaption>
</figure>

## Symmetry Points of a Circle

First we connect the bipolar system, in particular the circle of Apollonius, with symmetry points of a circle:

**Theorem 1.** Let $C$ be the circle of Apollonius corresponding to foci $a$ and $b$ and ratio $q$. Points $a$ and $b$ form a pair of symmetric points with respect to $C$.

<proof>

We know that the center of $C$ lies on the line formed by $a$ and $b$ and that if we assume, without loss of generality, that $q \gt 1$, then $b$ is inside $C$. This means that the line $ab$ bisects $C$ and the corresponding intersection points, say $u$ and $v$ form a diameter of $C$. Since $a$ and $b$ are on opposite sides of $C$, one of $u$ and $v$ belongs to the segmeng $ab$. Let it be $u$. Also let $p$ be any other point in $C$.
<br /><br />
We want to show that the identity of cross-ratios $(a, f, u, v) = \overline{(b, f, u, v)}$ holds, and then <i>Theorem 2</i> in [2] would show that $a$ and $b$ are symmetric with respect to $C$.
<br /><br />
For ease of reasoning, we can assume that $a$ and $b$ lie on the real axis (and hence also $u$ and $v$). This should not be an issue since we can apply a Möbius transformation to achieve this result and cross-ratios are invariant with Möbius transformations.

We have that:

$$(1.1) \quad (a, p, u, v) = \frac{a - u}{a - v} \cdot \frac{p - u}{p - v}$$

and

$$\overline{(b, p, u, v)} = \frac{\overline{b - u}}{\overline{b - v}} \cdot \frac{\overline{p - u}}{\overline{p - v}}$$

Since $b, u$ and $v$ are on the real line, they're equal to their conjugates, so

$$ = \frac{b - u}{b - v} \cdot \frac{\overline{p} - u}{\overline{p} - v}$$

We first show that:

$$(1.2) \quad \frac{a - u}{a - v} = - \frac{b - u}{b - v}$$

Since $u$ and $v$ are points on the Apollonius circle, they satisfy $\abs{a - u} = q \abs{b - u}$ and $\abs{a - v} = q \abs{b - v}$. And since $a, b, u$ and $v$ are collinear, we have $(a - u) = -q(b - u)$ and $(a - v) = q(b - v)$. Note that since $u$ is in between $a$ and $b$, $(a - u)$ and $(b - u)$ point in opposite directions, so we need the negative $q$. So

$$
\frac{a - u}{a - v} = \frac{-q(b - u)}{q(b - v)} = - \frac{b - u}{b - v}
$$

Now we show that:

$$(1.3) \quad \frac{p - u}{p - v} = - \overline{\left( \frac{p - u}{p - v} \right)} = - \frac{\overline{p} - u}{\overline{p} - v}$$

We consider $(p - u)$ in polar form as $R_u e^{i\theta_u}$ and $(p - v)$ as $R_v e^{i\theta_v}$. We first claim that $\theta_u - \theta_v = \pi/2$. This can be proven from the <i>Thales Theorem</i> which says that if $uv$ is the diameter of a circle $C$, then the angle $u \hat p v$ is $90^{\circ}$. Consider <i>Figure 1.1</i>:

<figure class="center_children">
  <img src="{{resources_path}}/thales-theorem.png" alt="See caption." />
  <figcaption>Figure 1.1: Thales Theorem shows that the angle $u \hat p v$ is $90^{\circ}$. Here $\theta_u$ and $\theta_v$ are the angles of $p - u$ and $p - v$, respectively, with the positive x-axis.</figcaption>
</figure>
<br /><br />
We have that $\theta_u + 90^{\circ} = (180^{\circ} - \theta_v) = 180^{\circ}$ so $\theta_u - \theta_v = 90^{\circ} = \pi/2$. So:

$$\frac{p - u}{p - v} = \frac{R_u}{R_v} e^{i (\theta_u - \theta_v)} = \frac{R_u}{R_v} e^{i \pi/2}$$

If we recall that if $z = R e^{i\theta}$ then $\overline{z} = R e^{-i\theta}$, the conjugate of the above is:

$$\overline{\left( \frac{p - u}{p - v} \right)} = \frac{R_u}{R_v} e^{-i \pi/2}$$

We have $-\pi/2 = 2\pi - \pi/2 = \pi + \pi/2$ and that $e^{i\pi} = -1$ (Euler's identity!), so

$$\frac{R_u}{R_v} e^{-i \pi/2} = \frac{R_u}{R_v} e^{i \pi} e^{i \pi/2} = -\frac{R_u}{R_v} e^{i \pi/2}$$

Which proves $(1.3)$. Multiplying $(1.2)$ and $(1.3)$ together gives us:

$$\frac{a - u}{a - v} \cdot \frac{p - u}{p - v}  = \frac{b - u}{b - v}  \cdot \frac{\overline{p} - u}{\overline{p} - v}$$

Showing that $(a, p, u, v) = \overline{(b, p, u, v)}$. Which proves that $a$ and $b$ are symmetric with respect to $C$.

</proof>

*Theorem 1* combined with *Theorem 9* in [2], tell us that any circles through $a$ and $b$ must then intersect $C$ orthogonally.

**Corollary 2.** Circles through $a$ and $b$ intersect Apollonious circles induced by $a$ and $b$ orthogonally.

## Möbius Transformation

### Fixed points

In our post Möbius Transformations [3] we discussed the concept of fixed points. To recap, given a transformation $T$, a fixed point is such that $\gamma = T(\gamma)$. In the general case a Möbius transformation has two fixed points. It can have a single one, but we'll ignore that case in this post.

Now let $a$ and $b$ be the fixed points of a Möbius transformation $T$ and consider the bipolar coordinates induced by these $a$ and $b$. What happens if we transform this system via:

$$(1) \quad z = U(w) = \frac{w - a}{w - b}$$

It will send point $a$ to 0 and $b$ to $\infty$. What does it do to a Apollonius circle? *Theorem 3* answers that:

**Theorem 3**. Transformation $U(w)$ maps Apollonius circles to circles at the origin.

<proof>
For a given Apollonius circle, the set of points in it satisfy:

$$\frac{\abs{w - a}}{\abs{w - b}} = q$$

If we transform them via $U(w)$:

$$
z = \frac{w - a}{w - b}
$$

Let's compute the inverse of the Möbius transformation $U^{-1}(z)$. We have

$$z(w - b) = w - a$$

Distributing:

$$zw - zb = w - a$$

Isolating $w$:

$$w = \frac{zb - a}{z - 1}$$

Let's determine $(w - a)$ by replacing with the definition of $w$ just found:

$$
w - a = \frac{zb - a}{z - 1} - a = \frac{zb - a - az + a}{z - 1} = \frac{z(b - a)}{z - 1}
$$

Similarly for $(w - b)$:

$$
w - b = \frac{zb - a}{z - 1} - b = \frac{zb - a - bz + b}{z - 1} = \frac{(b - a)}{z - 1}
$$

Dividing one by the other:

$$
\frac{w - a}{w - b} = \frac{z(b - a)/(z - 1)}{(b - a) / (z - 1)} = z
$$

Since:

$$
\abs{z} = \left \lvert \frac{w - a}{w - b} \right \rvert = \frac{\abs{w - a}}{\abs{w - b}} = q
$$

We conclude that the set of points $z = U(w)$ for points $w$ in the Apollonius circle with ration $q$ satisfy $\abs{z} = q$, that is, they belong to the circle at the origin of radius $q$.

</proof>

How about circles through $a$ and $b$? *Theorem 4* has the answer:

**Theorem 4**. Transformation $U(w)$ maps circles through $a$ and $b$ to lines through the origin (*radial lines*).

<proof>
Consider a circle $C$ through $a$ and $b$. Since $a$ and $b$ belong to $C$ and $U(a) = 0$ and $U(b) = \infty$ the corresponding transformed curve must contain the origin and the point at infinity  in the extended complex plane. By <i>Corollary 9</i> in [3], this curve must be either a circle or a line. Since we have the point at infinity we must have a line (or equivalently a circle of infinite radius) through the origin.
<br /><br />
The angle of this line will be determined by any other point on the circle $C$.
</proof>

Taken together, we see that transformation $U(w)$ maps a bipolar coordinate system into a polar one! A fixed Apollonius circle in the transformed plane denotes a fixed radius $r$ and a fixed circle through $a$ and $b$ in the transformed plane denotes a angle $\theta$. *Figure 1* illustrates this.

Another way to prove *Corollary 2* is by observing that the lines through the origin intersect circles through the origin orthogonally. Since Möbius transformations are conformal maps (*Theorem 3* in [3]), the corresponding curves in the bipolar coordinates must also intersect orthogonally.

### Normal Form

Recall from our post Möbius Transformations [3] that a given Möbius Transformation $T$ with fixed points $a$ and $b$ has the following normal form:

$$
\frac{f(z) - a}{f(z) - b} = r e^{i\theta} \frac{z - a}{z - b}
$$

Taking $U(w) = (w - a)/(w - b)$, we can rewrite it as:

$$
w' = U^{-1} ( r e^{i\theta} U(w) )
$$

The bipolar coordinate system gives us a more intuitive interpretation of this equation. First we map a point $w$ from the coordinate system into the polar one via $U(w)$.

In there, we multiply it by a factor of $r$ (moving it to another circle through the origin) and a rotation by $\theta$ (moving it to another radial line).

Finally we transform the resulting point back to the bipolar coordinate system by applying $U^{-1}(z)$ to the transformed point. *Figure 2* illustrates this process:


<figure class="center_children">
  <img src="{{resources_path}}/transform.png" alt="See caption." />
  <figcaption>Figure 2: Möbius transformations via a change of coordinates. Point $A$ is at the intersection of an Apollonius circle and a circle through $a$ and $b$. Applying the transform $U(A)$ takes us to $B$ on the polar coordinate plane. A rotation (along the blue arrow) and scaling (allow the red arrow) maps is to $C$. Applying the inverse transform $U^{-1}(C)$ takes us to $D$, which is the result of applying the Möbius transformation over $A$.</figcaption>
</figure>

## Conclusion

In [1] Ahlfors calls the bipolar coordinate system a *circular net* or *Steiner circles* but I didn't find this used on the web, except for [6].

After reading the corresponding chapter on this in [1], I didn't fully grasp how this bipolar coordinate is useful in the context of Möbius transformations. Only after reading [5] and seeing diagrams (on which *Figure 2* is based) that I got the idea and it's really elegant, so I'm glad I took the time to complement my studies.

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2](({{blog}}/2024/02/03/circles-symmetry.html))] NP-Incompleteness - Symmetry Points of a Circle
* [[3](({{blog}}/2024/01/08/mobius-transformation.html))] NP-Incompleteness - Möbius Transformation
* [[4](https://math.libretexts.org/Bookshelves/Geometry/Geometry_with_an_Introduction_to_Cosmic_Topology_(Hitchman)/03%3A_Transformations/3.05%3A_Mobius_Transformations%3A_A_Closer_Look)] Geometry with an Introduction to Cosmic Topology - 3.5: Möbius Transformations: A Closer Look.
* [[5](https://quantum.lvc.edu/lyons/algeom_text/algeom_text_files/section-11.html)] Introduction to Groups and Geometries - David W. Lyons
