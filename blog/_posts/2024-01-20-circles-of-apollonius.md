---
layout: post
title: "Circles of Apollonius"
tags: [analysis, geometry]
excerpt_separator: <!--more-->
vanity: "2024-01-20-circle-of-apollonius"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/apollonian.png" alt="a bunch of lines and circles abstract art" />
</figure>

In this post we study the *circles of Apollonius* or *Apollonian circles* from a geometric perspective. Later we relate it with concepts from complex analysis.

<!--more-->

No prior context is needed for the geometric part. For the *Complex Plane* section, it's worth becoming familiar with the series, in particular the Cross-Ratio:

1. [Holomorphic Functions]({{blog}}/2023/12/21/holomorphic-functions.html)
2. [Conformal Maps]({{blog}}/2023/12/30/conformal-maps.html)
3. [Möbius Transformation]({{blog}}/2024/01/08/mobius-transformation.html)
4. [Cross-Ratio]({{blog}}/2024/01/13/cross-ratio.html)

## Definition


Let $a$ and $b$ be two points on the plane. The **circle of Apollonius** induced by $a$ and $b$, is the set of points $p$ such that the distance to $a$ and $b$ has a constant ratio $r$.

More formally, let $d(p, q)$ represent the Euclidean distance between points $p$ and $q$. The circle of Apollonius is the set of $p$ satisfying:

$$
(1) \quad \frac{d(P, A)}{d(P, B)} = r
$$

The points $a$ and $b$ are called the **foci**.

## Circle?

Why is the set of points satisfying $(1)$ called a circle? To begin with we consider the case where $r = 1$. In this case the set $(1)$ is actually a line, perpendicular to the segment $\overline{AB}$ and bisecting it. This is a circle if we assume that a line is a circle with infinite radius.

Henceforth we'll assume $r \ne 1$. In this case it's less apparent what geometric object the set $(1)$ defines. *Corollary 6* answers that question.

Before that though, we need to build on top of some lemmas and theorems. We start with the angle bisector theorem:

**Theorem 1.** (*Angle bisector theorem*) Let $\triangle ABC$ be a triangle and let $D$ be a point on the side ${BC}$ (*Figure 1*). The segment ${AD}$ bisects (i.e. divides into equal parts) the angle $B\hat{A}C$ if and only if the following holds:

$$\frac{\abs{BD}}{\abs{CD}} = \frac{\abs{AB}}{\abs{AC}}$$

<figure class="center_children">
  <img src="{{resources_path}}/bisect.png" alt="See caption." />
  <figcaption>Figure 1: Triangle ABC with a point D within the segment BC and the dotted segment AD. α is the angle between AB and AD, and β between AD and AC.</figcaption>
</figure>
<span></span>

<proof>
We first draw a height line of length $h$, perpendicular to $BC$ as in <i>Figure 1.1</i>

<figure class="center_children">
  <img src="{{resources_path}}/bisect-height.png" alt="See caption." />
  <figcaption>Figure 1.1: Same as Figure 1 but with the height line, the segment ending in A and perpendicular to BC, with length $h$.</figcaption>
</figure>


Let's compute the area of the triangles $\triangle ABD$ and  $\triangle ACD$ in two ways. First via the base vs. height divided by 2, which gives us:

$$(1.1) \quad A(\triangle ABD) = \frac{\abs{BD}h}{2} , \qquad A(\triangle ACD) = \frac{\abs{CD}h}{2}$$

Secondly, we can compute the area from a triangle given 2 of its sides and the angle betweem them, so:

$$(1.2) \quad A(\triangle ABD) = \frac{\abs{AB}\abs{AD} \sin(\alpha)}{2}, \qquad A(\triangle ACD) = \frac{\abs{AC}\abs{AD} \sin(\beta)}{2}$$

Combining $(1.1)$ and $(1.2)$ gives us:

$$
 \frac{\abs{BD}h}{2} = \frac{\abs{AB}\abs{AD} \sin(\alpha)}{2}, \qquad \frac{\abs{CD}h}{2} = \frac{\abs{AC}\abs{AD} \sin(\beta)}{2}
$$

Dividing the equality on the left by the equality on the right and cancelling terms:

$$\frac{\abs{BD}}{\abs{CD}} = \frac{\abs{AB}\sin(\alpha)}{\abs{AC} \sin(\beta)}$$

If  ${AD}$ bisects the angle $B\hat{A}C$, then $\alpha = \beta$, so we get:

$$\frac{\abs{BD}}{\abs{CD}} = \frac{\abs{AB}}{\abs{AC}}$$

Conversely, if the above is true, we find that $\sin(\alpha) = \sin(\beta)$. Since these are internal angles of a triangle, $\alpha, \beta < \pi$, so we can assume $\alpha = \beta$.

</proof>

A related theorem is the external angle bisector theorem:

**Theorem 2.** (*External angle bisector theorem*) Let $\triangle ABC$ be a triangle and let $D$ be a point on the extended line formed by the points $B$ and $C$, and let $E$ be any point on he extended line formed by the points $A$ and $B$ (*Figure 2*). The segment ${AD}$ bisects (i.e. divides into equal parts) the external angle of $A$ (i.e. $C\hat{A}E$) if and only if the following holds:

$$\frac{\abs{BD}}{\abs{CD}} = \frac{\abs{AB}}{\abs{AC}}$$

<figure class="center_children">
  <img src="{{resources_path}}/external-bisect.png" alt="See caption." />
  <figcaption>Figure 2: Triangle ABC with a point D lying outside the triangle, on the line formed by points B and C. Point E lies outside the triangle, on the line formed by points A and B. The segment AD is dotted. α is the angle between AE and AD, and β between AD and AC.</figcaption>
</figure>
<span></span>
<proof>
We draw a line segment parallel to AD starting at C and intersecting AC at E as in <i>Figure 2.1</i>.

<figure class="center_children">
  <img src="{{resources_path}}/external-bisect-parallel.png" alt="See caption." />
  <figcaption>Figure 2.1: Same as Figure 2 but with a parallel line segment to AD.</figcaption>
</figure>

We can infer that the angle $A\hat{C}F$ is $\beta$ and that the angle $A\hat{F}C$ is $\alpha$.
<br /><br />
We notice that the triangles $\triangle{BCF}$ and $\triangle{ABD}$ are similar except for a scaling factor. In particular:

$$(2.1) \quad \frac{\abs{AB}}{\abs{BF}} = \frac{\abs{BD}}{\abs{BC}} = k$$

So $\abs{AF} = \abs{AB} - \abs{BF} = \abs{BF}(k - 1)$ and $\abs{CD} = \abs{BD} - \abs{BC} = \abs{BC}(k - 1)$. Dividing them:

$$(2.2) \quad \frac{\abs{AF}}{\abs{CD}} = \frac{\abs{BF}(k - 1)}{\abs{BC}(k - 1)} = \frac{\abs{BF}}{\abs{BC}}$$

We can rewrite $(2.1)$ as:

$$\frac{\abs{AB}}{\abs{BD}} = \frac{\abs{BF}}{\abs{BC}}$$

From $(2.2)$ we have $\abs{BF}/\abs{BC} = \abs{AF}/\abs{CD}$, so we get:

$$\frac{\abs{AB}}{\abs{BD}} = \frac{\abs{AF}}{\abs{CD}}$$

Or

$$(2.3) \quad \frac{\abs{BD}}{\abs{CD}} = \frac{\abs{AB}}{\abs{AF}}$$

Which is almost what we want, except that we have $\abs{AF}$ instead of $\abs{AC}$, but we haven't used our hypothesis yet.
<br /><br />
Now assume that ${AD}$ bisects $C\hat{A}E$, which means $\alpha = \beta$. This implies the triangle $\triangle ACF$ is isosceles with the sides $AF$ and $AC$ being of the same length. So we get

$$\frac{\abs{BD}}{\abs{CD}} = \frac{\abs{AB}}{\abs{AF}} = \frac{\abs{AB}}{\abs{AC}}$$

Conversely, suppose that we start with

$$\frac{\abs{BD}}{\abs{CD}} = \frac{\abs{AB}}{\abs{AC}}$$

Which together with $(2.3)$ imploes that $\abs{AC} = \abs{AF}$, which by definition means that the triangle $\triangle ACF$ is isosceles and the angles $\alpha = \beta$. QED.

</proof>

**Theorem 3.** (*Thales Theorem - partial*). If $\triangle ABC$ is a rectangle triangle (with $A\hat{B}C = 90^{\circ}$) then point $B$ lies on the circle with the diameter $AC$.

<proof>

The idea is that we can turn the rectangle triangle into a rectangle by creating a copy rotated by $180^{\circ}$ and connecting them by their hypothenuse. See <i>Figure 3.1</i>

<figure class="center_children">
  <img src="{{resources_path}}/rectangle-triangle.png" alt="See caption." />
  <figcaption>Figure 3.1: (left) Rectangle triangle. (right) Turned into a rectangle. Note that $\alpha + \beta = 90^{\circ}$.</figcaption>
</figure>

For a rectangle, the diagonals $AC$ and $BD$ have equal length and meet at their midpoints $M$. Thus points $A$, $B$, $C$ and $D$ are equidistant to $M$ and lie on a circle centered at it. See <i>Figure 3.2</i>.

<figure class="center_children">
  <img src="{{resources_path}}/rectangle-circle.png" alt="See caption." />
  <figcaption>Figure 3.2: Circle centered in $M$ and diameter $AC$.</figcaption>
</figure>

Since there's a line on which $A$, $M$ and $C$ lie, then $AC$ is the diameter of the circle.

</proof>

The original *Thales Theorem* also claims the converse, that if in a triangle $\triangle ABC$, $B$ lies on the circle defined by the diameter $AC$, then $A\hat{B}C = 90^{\circ}$. We don't need this result here, so we'll skip the proof.

In order to facilitate calculations in the next Lemma and Theorem, we will make some assumption without loss of generality. First, we assume that $r > 1$. If that's not the case we swap $A$ and $B$ in $(1)$:

$$\frac{d(P, B)}{d(P, A)} = \frac{1}{r}$$

Second, we can perform a translation to the space such that point $A$ is $(1)$ concides with the origin. Third, we can perform a rotation with respect to the origin such that $B$ lies on the positive $x$-axis.

With *Lemma 4* we find that the set $(1)$ contains points that allows us to use *Theorems 1, 2* and *3*:

**Lemma 4** The set of points satisfying $(1)$ contain points $C$ within the segment $\overline{AB}$ and $D$ external to that segment, as in *Figure 3*. With

$$(2) \quad \frac{\abs{AC}}{\abs{BC}} = \frac{\abs{AD}}{\abs{BD}} = r$$

<figure class="center_children">
  <img src="{{resources_path}}/segment.png" alt="See caption." />
  <figcaption>Figure 3: Points $A$, $C$, $B$ and $D$ in a line, in this order.</figcaption>
</figure>
<span></span>

<proof>

Recall that we're assuming $A = (0, 0)$ and $B = (x_b, 0)$. For some $P$ on the $x$-axis we have $P = (x, 0)$. Replacing in $(1)$ we get:

$$\frac{x^2}{(x - x_b)^2} = r$$

Re-arranging terms:

$$x^2 = r (x^2 - 2 x x_b + x_b^2)$$

Making it as a polynomial of $x$:

$$(1 - r) x^2 + (2 x_b r) x - x_b^2 r = 0$$

We can now use the quadratic formula to find $x$, with $a = 1 - r$, $b = 2 x_b r$ and $c = -x_b^2 r$:

$$x = \frac{-(2x_b r) \pm \sqrt{4x_b^2r^2 - 4(1 - r)(-x_b^2 r))}}{2(1 - r)}$$

Distributing terms:

$$x = \frac{-(2x_b r) \pm \sqrt{4x_b^2r^2 + 4x_b^2 r - 4x_b^2 r^2}}{2(1 - r)}$$

Cancelling terms:

$$x = \frac{-(2x_b r) \pm \sqrt{4x_b^2r}}{2(1 - r)}$$

Extracting from the square root:

$$x = \frac{-(2x_b r) \pm 2x_b \sqrt{r}}{2(1 - r)}$$

Factoring and cancelling:

$$x = \frac{x_b(-r \pm \sqrt{r})}{1 - r}$$

Assuming $r > 1$ and noticing that $r > \sqrt{r}$ we see that:

$$x = x_b \frac{r \pm \sqrt{r}}{r - 1}$$

is always positive. Considering the two roots:

$$
\begin{align}
x_1 &= x_b \frac{r + \sqrt{r}}{r - 1} > x_b \frac{r + \sqrt{r}}{r} > x_b \\
x_2 &= x_b \frac{r - \sqrt{r}}{r - 1} < x_b \frac{r -1 }{r - 1} < x_b \\
\end{align}
$$

So we have $C = (x_2, 0)$ within $\overline{AB}$ and $D = (x_1, 0)$ external to it. The relation $(2)$ follows from the fact that $C$ and $D$ satisfy $(1)$ by hypothesis.

</proof>

*Lemma 4* enables us to draw a useful picture to help with *Lemma 5*. For any point $P$ not in $x$-axis satisfying $(1)$ it will define the triangles and line segments from *Figure 4*.

<figure class="center_children">
  <img src="{{resources_path}}/lemma.png" alt="See caption." />
  <figcaption>Figure 4: Point $P$ defines triangles and line segments with the other points.</figcaption>
</figure>
<span></span>


**Lemma 5.** In *Figure 4*, $\alpha_2 + \alpha_3 = 90^{\circ}$.

<proof>

Since $P$ satisfies $(1)$, we have:

$$\frac{\abs{AP}}{\abs{BP}} = \frac{\abs{AC}}{\abs{BC}} = r$$

These are the exact conditions for <i>Theorem 1</i> to apply (with some re-labeling of points). So we can conclude that $\alpha_1 = \alpha_2 = \alpha$. We also have:

$$\frac{\abs{AP}}{\abs{BP}} = \frac{\abs{AD}}{\abs{BD}} = r$$

Which are the conditions for <i>Theorem 2</i> to apply (again, with some re-labeling), which leads us to $\alpha_3 = \alpha_2 = \beta$.

Since these four angles add up to a straight line, we have $2\alpha + 2\beta = 180^{\circ}$, so $\alpha_2 + \alpha_3 = 90^{\circ}$.

</proof>

*Lemma 5* lets us apply *Theorem 3* to conlude that $P$ lies on the circle with diameter $\overline{CD}$. Since we didn't specify $P$ except that it's not $C$ or $D$, this leads us to:

**Corollary 6.** The set of points satisfying $(1)$ defines a circle.

Since $\overline{CD}$ is the diameter, we can find the center $O$ and radius $r$ of the circle:

$$O = \frac{D - C}{2}, \qquad r = \frac{\abs{CD}}{2}$$

We can also conclude from $\overline{CD}$ being the diameter that:

**Corollary 7.** The center of the Apollonious circle induced by $A$ and $B$ is collinear with them.


## Complex Plane

The equation $(1)$ can be expressed in terms of complex numbers too. Let $a$ and $b$ be complex numbers and $r$ a real one. Then the set formed by $z$ satisfying:

$$
\frac{\abs{z - a}}{\abs{z - b}} = r
$$

defines a circle in the complex plane. We now connect the *circle of Apollonius* with concepts from Complex Analysis we studied so far.

### Cross Ratio

Recall the [cross-ratio]({{blog}}/2024/01/13/cross-ratio.html) for complex points $z_1, z_2, z_3, z_4$ is defined as:

$$(z_1, z_2, z_3, z_4) = \frac{(z_1 - z_3)}{(z_1 - z_4)} \cdot \frac{(z_2 - z_4)}{(z_2 - z_3)}$$

If we set $z_1 = A$, $z_2 = B$, $z_3 = P_1$ and $z_4 = P_2$ where $P_1$ and $P_2$ satisfy $(1)$, what do we get?

**Theorem 8** The cross-ratio $(A, B, P_1, P_2)$ where

$$\frac{\abs{P_1 - A}}{\abs{P_1 - B}} = \frac{\abs{P_2 - A}}{\abs{P_2 - B}} = r$$

Belongs to the unit circle.

<proof>

We want to compute

$$(8.1) \quad (A, B, P_1, P_2) = \frac{(A - P_1)}{(A - P_2)} \cdot \frac{(B - P_2)}{(B - P_1)}$$

We can write $A - P_1$ as a complex number in polar coordinates $\abs{A - P_1}e^{i\alpha_1}$ and also $B - P_1$ as $\abs{B - P_1}e^{i\beta_1}$. Similarly we have $A - P_2 = \abs{A - P_2}e^{i\alpha_2}$ and $B - P_2 = \abs{B - P_2}e^{i\beta_2}$

Replacing these in $(8.1)$:

$$(A, B, P_1, P_2) = \frac{\abs{A - P_1}e^{i\alpha_1}}{\abs{A - P_2}e^{i\alpha_2}} \cdot \frac{\abs{B - P_2}e^{i\beta_2}}{\abs{B - P_1}e^{i\beta_1}}$$

Using that $\abs{A - P_1}/\abs{B - P_1} = r$ and that $\abs{A - P_2}/\abs{B - P_2} = r$:

$$= \frac{re^{i(\alpha_1 - \beta_1)}}{re^{i(\alpha_2 - \beta_2)}}$$

Cancelling the $r$:

$$= e^{i(\alpha_1 - \beta_1 - \alpha_2 + \beta_2)}$$

Which has fixed radius 1 and the angles can vary, so this defines a unit circle.
</proof>

## References

* [[1](https://en.wikipedia.org/wiki/Apollonian_circles)] Wikipedia - Apollonian circles
