---
layout: post
title: "Cauchy Integral Theorem"
tags: [analysis]
vanity: "2024-04-26-cachy-integral-theorem"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/cauchy.png" alt="portrait of Cauchy from Wikipedia" style="width: 100px;" />
</figure>
<br />
This is our third post in the series with my notes on complex integration, corresponding to *Chapter 4* in Ahlfors' Complex Analysis.

The Cauchy integral theorem provides conditions under which the integral over a closed curve is zero.


<!--more-->

The previous posts from the series:

1. [Complex Integration]({{blog}}/2024/04/05/complex-integration.html)
1. [Path-Independent Line Integrals]({{blog}}/2024/04/13/path-independent-line-integrals.html)


## Recap

In the previous post [3] we ended with the following *Corollary 2* stating that:

> The complex line integral $\int_\gamma f(z)dz$, defined in $\Omega$, depends only on the endpoints of $\gamma$ if and only if $f$ is the derivative of some holomorphic function in $\Omega$.

Another corollary is the following:

**Corollary 1.** Let $f(z)$ be a function defined in $\Omega$. Then

$$\int_\gamma f(z)dz = 0$$

If and only if $f$ is the derivative of some holomorphic function $F$ in $\Omega$.

The general idea of Cauchy's theorem that we'll cover in this post is that we only need $f$ itself to be holomorphic in $\Omega$, for special types of the region $\Omega$.

A result we haven't proved yet says that the derivative of a holomorphic function is itself holomorphic, but not all holomorphic functions are derivatives of of a holomorphic function. So  Cauchy's theorem is a stronger result.

## In a rectangle

We first consider the case where $\Omega$ is a rectangle $R$ defined by

$$a \le x \le b, c \le y \le d$$

The curve we'll use is the border of $R$, denoted by $\partial R$, with a clock-wise orientation as depicted in *Figure 1*. In other words, it's the segments $(a, c) \rightarrow (b, c)$, $(b, c) \rightarrow (b, d)$, $(b, d) \rightarrow (a, d)$ and $(a, d) \rightarrow (a, c)$.

<figure class="center_children">
  <img src="{{resources_path}}/rectangle.svg" alt="See caption."  style="width: 500px;" />
  <figcaption>Figure 1: Reference rectangle with clockwise orientation</figcaption>
</figure>

**Theorem 1.** If the function $f(z)$ is holomorphic in $R$, then

$$\int_{\partial R} f(z) dz = 0$$

<proof>

In [1], the author defines the notation

$$\eta(R) = \int_{\partial R} f(z) dz$$

for any given $R$ and when a rectangle is partitioned into four coungruent rectangles (Figure 1.1), we can refer to them as $R^{(1)}, R^{(2)}, R^{(3)}, R^{(4)}$.

<figure class="center_children">
  <img src="{{resources_path}}/subrectangle.svg" alt="See caption."  style="width: 500px;" />
  <figcaption>Figure 1.1: Partition into congruent subrectangles, recursively.</figcaption>
</figure>


We then have that

$$(1.1) \quad \eta(R) = \eta(R^{(1)}) + \eta(R^{(2)}) + \eta(R^{(3)}) + \eta(R^{(4)})$$

The idea is that for a segment shared between two sub-recangles they have opposite directions, so their integral cancels out. The only segments left uncancelled are those belonging to the original rectangle.

By the triangle inequality, we thus have:

$$(1.2) \quad \abs{\eta(R)} \le \abs{\eta(R^{(1)})} + \abs{\eta(R^{(2)})} + \abs{\eta(R^{(3)})} + \abs{\eta(R^{(4)})}$$

We choose the subrectangle with the largest modulus and call it $R_1$. For $k = 1, 2, 3, 4$ we have:

$$\abs{\eta(R_1)} \ge \abs{\eta(R^{(k)})}$$

So that

$$4 \abs{\eta(R_1)} \ge \abs{\eta(R^{(1)})} + \abs{\eta(R^{(2)})} + \abs{\eta(R^{(3)})} + \abs{\eta(R^{(4)})} \ge \abs{\eta(R)}$$

Or

$$\abs{\eta(R_1)} \ge \frac{1}{4} \abs{\eta(R)}$$

If we repeat this process recursively for $R_n$ and obtain $R_{n+1}$, we'll get more generally that:

$$\abs{\eta(R_n)} \ge \frac{1}{4} \abs{\eta(R_{n-1})}$$

Which leads us to

$$(1.3) \quad \abs{\eta(R_n)} \ge 4^{-n} \abs{\eta(R)}$$

As $n$ grows, the subrectangle $R_n$ converges to a point which we'll call $z^*$ (shown in <i>Figure 1.1</i>). Since $f(z)$ is holomorphic in $R$ by hypothesis, the limit

$$f'(z^*) = \lim_{z \rightarrow z^*} \frac{f(z) - f(z^*)}{z - z^*}$$

exists, that is, for every $\epsilon$, there exists $\delta > 0$ such that $\abs{z - z^*} \lt \delta$ such that:

$$\abs{f'(z^*) - \frac{f(z) - f(z^*)}{z - z^*}} \lt \epsilon$$

Or

$$(1.4) \quad \abs{f(z) - f(z^*) + f'(z*) (z - z*)} \lt \epsilon \abs{z - z*}$$

Now consider the open circle $\abs{z - z^*} \lt \delta$ (i.e. the set of points for which $(1.4)$ holds). We can choose $n$ sufficiently large we can guarantee that
$R_n$ is inside that open circle and that all $z$ in $\partial R_n$ satisfy $(1.4)$.
<br /><br />
So far we have $\abs{z - z^*} \lt \delta$, but we can find a tigher upper bound. The maximum distance between two points in a rectangle is its diagonal. Let $\Delta$ be the diagonal of $R$. Every time we pick a subrectangle the diagonal is halved, so the diagonal of $R_n$ is $2^{-n} \Delta$. Thus if $z$ and $z^*$ are in $R_n$, $\abs{a - z^*} \le 2^{-n} \Delta$. We have then:

$$(1.5) \quad \abs{f(z) - f(z^*) + f'(z*) (z - z*)} \lt \epsilon \abs{z - z*} \le \epsilon 2^{-n} \Delta$$

In [2] we saw in the last example that for any curve $\gamma$,

$$\int_\gamma (z - a)^{n}dz = 0$$

For $n \gt 0$. This let's us conclude the following:

$$(1.6) \quad \int_{\partial R_n} (z - z^*) dz = 0$$

By replacing $a$ with $z^*$, $\gamma$ with $\partial R$, and setting $n = 1$. Similarly, if we do it for $n = 0$:

$$(1.7) \quad \int_{\partial R_n} dz = 0$$

Since $-f(z*)$ exists, we can multiply it by $(1.7)$ and still get a 0:

$$(1.8) \quad -f(z*) \int_{\partial R_n} dz = 0$$

Similarly, $f'(z*)$ exists and we can multiple it by $(1.6)$ and still get a 0:

$$(1.9) \quad f'(z*) \int_{\partial R_n} (z - z^*) dz = 0$$

Since $(1.8)$ and $(1.9)$ are zero we can add them to $\eta(R_n)$ and obtain [4]:

$$\eta(R_n) = \int_{\partial R_n} f(z)dz = \int_{\partial R_n} f(z)dz - f(z*) \int_{\partial R_n} dz + \int_f'(z*) {\partial R_n} (z - z^*) dz$$

Moving them under one integral (we can also move $f(z*)$ and $f'(z*)$ inside since they're constants with respect to $z \in R_n$):

$$\eta(R_n) = \int_{\partial R_n} f(z) - f(z*) + f'(z*) (z - z^*) dz$$

All this trickery so that we get to the form of the inequality $(1.5)$. However, that inequality is with respect to the modulus, so we can use <i>Theorem 2</i> from [2] which states:

$$\abs{\int_\gamma{f(z)dz}} \le \int_\gamma \abs{f(z)} \abs{dz}$$

In our case:

$$\abs{\eta(R_n)} \le \int_{\partial R_n} \abs{f(z) - f(z*) + f'(z*) (z - z^*)} \abs{dz}$$

Now we can leverage the inequality $(1.5)$ and obtain:

$$\abs{\eta(R_n)} \lt \int_{\partial R_n} \epsilon 2^{-n} \Delta \abs{dz} = \epsilon 2^{-n} \Delta \int_{\partial R} \abs{dz}$$

Where $\int_{\partial R} \abs{dz}$ is the perimeter of $R_n$. Let $P$ be the perimeter of $R$. Every time we pick a subrectangle the perimeter halves, so the perimeter of $R_n$ is $2^{-n} R$, and we have

$$\abs{\eta(R_n)} \lt 4^{-n} \epsilon \Delta P$$

Replacing this in $(1.3)$ gives us,

$$4^{-n} \abs{\eta(R)} \le \abs{\eta(R_n)} \lt 4^{-n} \epsilon \Delta P$$

So finally

$$\abs{\eta(R)} \lt \epsilon \Delta P$$

$\Delta P$ is fixed for any given $R$ and we're free to choose $\epsilon \gt 0$, which leads to $\abs{\eta(R)} = 0$, implying

$$\int_{\partial R} f(z) dz = 0$$

</proof>

The proof is very clever but I don't have a good intuition on why it works. Anyway, we can generalize the theorem a bit by allowing points in $R$ for which $f(z)$ isn't holomorphic:

**Theorem 2.** Let $f(z)$ be holomorphic in $R'$, obtained from the rectangle $R$ by removing a finite set of points $\xi_j$. Then if

$$\lim_{z \rightarrow \xi_j} (z - \xi_j) f(z) = 0$$

for all $j$, then

$$(1) \quad \int_{\partial R} f(z) dz = 0$$

<proof>

First, we use again the fact that if we subdivide a rectangle into subrectangles and $(1)$ holds for each them, it also holds for the original rectangle, which was used to show $(1.1)$ in <i>Theorem 1</i>. We can then partition the rectangle in such a way that subrectangle has at most one $\xi_j$.
<br /><br />
This lets us reduce the theorem to the case where we remove exactly one point $\xi$ from $R$ (the case with zero points is <i>Theorem 1</i>). So we just need to prove $(1)$ holds for such rectangle which we'll denote $R'$.
<br /><br />
Using the same argument, we can subdivide a rectangle $R'$ containing $\xi$ such that the subrectangle containing $\xi$, denoted by $R_0$, is:

<ul>
<li>A square of size $L$</li>
<li>$\xi$ lies on its center</li>
<li>L is infinitesimally small</li>
<li>Satisfying:

$$(2.1) \quad \int_{\partial R'} f(z)dz = \int_{\partial R_0} f(z)dz$$

</li>
</ul>

See <i>Figure 2.1</i> for an illustration.

<figure class="center_children">
  <img src="{{resources_path}}/square.svg" alt="See caption."  style="width: 500px;" />
  <figcaption>Figure 2.1: Partition of a rectangle such that the inner rectangle $R_0$ is a square and $\xi$ lies in the center.</figcaption>
</figure>


For any given $\epsilon \gt 0$ and $z \in \partial R_0$ we can make:

$$(2.2) \quad \frac{\epsilon}{\abs{z - \xi}}$$

arbitrarily large. That's because $\abs{z - \xi}$ is bounded by the diagonal of $R_0$ and $\epsilon$ is constant, so shrinking $R_0$ grows $(2.2)$. This lets us use this value as an upper bound for $f(z)$:

$$(2.3) \quad \abs{f(z)} \lt \frac{\epsilon}{\abs{z - \xi}}$$

Applying <i>Theorem 2</i> from [2] to $(2.1)$, we have:

$$\abs{\int_{\partial R'} f(z)dz} = \abs{\int_{\partial R_0} f(z)dz} = \int_{\partial R_0} \abs{f(z)}\abs{dz}$$

Using $(2.3)$:

$$(2.4) \quad \abs{\int_{\partial R'} f(z)dz} \lt \epsilon \int_{\partial R_0} \frac{\abs{dz}}{\abs{z - \xi}}$$

Since we chose $R_0$ to be a square of size $L$ with $\xi$ at is center, we have the lower bound $\abs{z - \xi} \ge L/2$, or that

$$\frac{1}{z - \xi} \le \frac{2}{L}$$

Using these in $(2.4)$:

$$\abs{\int_{\partial R'} f(z)dz} \lt \frac{2 \epsilon}{L} \int_{\partial R_0} \abs{dz}$$

We have that $\int_{\partial R_0} \abs{dz}$ is the perimeter of $R_0$, that is, $4L$, so:

$$\abs{\int_{\partial R'} f(z)dz} \lt 8\epsilon$$

Since this is true for any $\epsilon \gt 0$, it must be that:

$$\abs{\int_{\partial R'} f(z)dz} = 0$$

</proof>

So we're saying that if $f(z)$ is not holomorphic at specific points in $R$ but it tends to 0 there, integrating over its boundary is still yields 0.

## In a disk

We now consider the case where $\Omega$ is the open circle $\abs{z - z_0} \lt \rho$, which we'll denote by $\Delta$. We have the following result:

**Theorem 3.** If $f(z)$ is holomorphic in $\Delta$, then

$$\int_{\gamma} f(z) dz = 0$$

for any closed curve $\gamma$ in $\Delta$.

<proof>
Let $(x_0, y_0)$ be the center of $\Delta$ and $(x', y')$ some point in $\Delta$. Let $\sigma$ be the curve formed by the segments $(x_0, y_0) \rightarrow (x', y_0)$ and then $(x', y_0) \rightarrow (x', y')$ as shown in <i>Figure 3.1</i>:

<figure class="center_children">
  <img src="{{resources_path}}/circle.svg" alt="See caption."  style="width: 350px;" />
  <figcaption>Figure 3.1: Circle with points $(x_0, y_0)$ and $(x, y)$. The blue path corresponds to $\sigma$, the red path to $\overline{\sigma}$.</figcaption>
</figure>

<br />
We'll keep $(x_0, y_0)$ fixed and assume $(x', y')$ is variable. We can then define a function of $x'$ and $y'$:

$$F(x', y') = \int_{\sigma} f(z) dz$$

Note that $\sigma$ is implicitly a function of $x'$ and $y'$ since its endpoint is $(x', y')$. And as discussed in [3] (section <i>Complex line integral</i>), we can also write as:

$$F(x', y') = \int_{\sigma} f(z) dx + i \int_{\sigma} f(z) dy$$

We can split each integral into the horizontal and the vertical segments. For the first integral, on the second segment $x$ doesn't vary, so the integral is 0. For the second integral, on the first segment $y$ doesn't vary, so the integral is 0. We can simplify to:

$$(3.1) \quad F(x', y') = \int_{x_0}^{x'} f(x, y_0) dx + i \int_{y_0}^{y'} f(x', y) dy$$

Suppose we want to compute $\frac{\partial F}{\partial y}(x', y')$. By definition we have:

$$(3.2) \quad  \frac{\partial F}{\partial y}(x', y') = \lim_{h \rightarrow 0} \frac{F(x', y' + h) - F(x', y')}{h}$$

Replacing by the definition of $F$:

$$= \lim_{h \rightarrow 0} \frac{\paren{\int_{x_0}^{x'} f(x, y_0) dx + i \int_{y_0}^{y' + h} f(x', y) dy} - \paren{\int_{x_0}^{x'} f(x, y_0) dx + i \int_{y_0}^{y'} f(x', y) dy}}{h}$$

The integral on $x$ doesn't depend on $y$, so it cancels out, leaving us with:

$$= i \lim_{h \rightarrow 0} \frac{\int_{y_0}^{y' + h} f(x', y) dy - \int_{y_0}^{y'} f(x', y) dy}{h}$$

If we define $H(y')$ as $\int_{y_0}^{y'} f(x', y) dy$, then we have:

$$= i \lim_{h \rightarrow 0} \frac{H(y' + h) - H(y')}{h}$$

The limit is the definition of $\frac{dH}{dy}(y')$, and we have that $\frac{dH}{dy}(y') = f(x', y')$ from the fundamental theorem of calculus. Thus

$$(3.3) \quad \frac{\partial F}{\partial y}(x', y') = i f(x', y')$$

Now consider the curve $\overline{\sigma}$ composed of the segments $(x', y') \rightarrow (x_0, y')$ and $(x_0, y') \rightarrow (x_0, y_0)$ (the red path in <i>Figure 3.1</i>). We have that $\sigma + \overline{\sigma}$ form a oriented rectangle, so we have, from <i>Theorem 1</i>:

$$\int_{\sigma} f(z) dz + \int_{\overline{\sigma}} f(z) dz = 0$$

Or that

$$\int_{\sigma} f(z) dz = \int_{-\overline{\sigma}} f(z) dz = F(x', y')$$

Where $-\overline{\sigma}$ is the reverse of $\overline{\sigma}$, meaning the segments $(x_0, y_0) \rightarrow (x_0, y')$ and $(x_0, y') \rightarrow (x', y')$. Using a very similar argument we used for $\sigma$, we'll conclude that:

$$(3.4) \quad \frac{\partial F}{\partial x}(x', y') = f(x', y')$$

Equations $(3.3)$ and $(3.4)$ tells us that $F$ satisfy the Cauchy-Riemman equations and is hence holomorphic in $\Delta$ (there's a corner case to be considered, see <i>Corner case</i> at the end) and that $f$ is the derivative of $F$. We can use <i>Corollary 1</i> to conclude:

$$\int_{\gamma} f(z) dz = 0$$

for any closed curve $\gamma$ in $\Delta$.
<br /><br />
<b>Corner case.</b> What if the point $(x, y)$ lies on the same axis as $(x_0, y_0)$, say $(x, y_0)$? In this case, our curve degenerates into the segment $(x_0, y_0) \rightarrow (x, y_0)$. But let's pretend we still have the vertical segment, but that the endpoints coincide with $(x, y_0)$. How does $\frac{\partial F}{\partial y}(x', y')$ look like now? We can still follow the same reasoning and conclude that

$$\frac{\partial F}{\partial y}(x', y_0) = i \lim_{h \rightarrow 0} \frac{H(y_0 + h) - H(y_0)}{h}$$

with $\frac{dH}{dy}(y_0) = f(x', y_0)$ even though $H(y_0) = 0$. So

$$\frac{\partial F}{\partial y}(x', y_0) = i f(x', y_0)$$

Note that we can't form a rectangle anymore with $\overline{\sigma}$, but it's ok: for computing $\frac{\partial F}{\partial x}(x', y')$ nothing changed, since it only depends on the horizontal segment which is $\sigma$ now. Generalizing, for the case where $(x', y') = (x_0, y_0)$ we can still pretend we have 2 degenerate segments.
<br /><br />

One question that came to mind when trying to deal with these corner cases: can't we simply choose a different starting point for these cases? I believe the answer to be no. Then we wouldn't be able to treat $x_0$ and $y_0$ as constants and they would be a function of $x$ and $y$.
<br /><br />
Note that it's fine for the curve $\sigma$ to be a function of $x$ and $y$ (as it is in fact) since we don't make any assumption about its constancy in $(3.1)$.

</proof>

In the same way we generalized *Theorem 1.* to allow for points in the rectangle where we allow $f$ to be non-holomorphic, we can generalize *Theorem 3.*

**Theorem 4.** Let $f(z)$ be holomorphic in the region $\Delta'$ obtained by omitting a finite number of points $\xi_j$ from the open disk $\Delta$. If $f(z)$ is such that

$$\lim_{z \rightarrow \xi_j} (z - \xi_j) f(z) = 0$$

then

$$\int_{\gamma} f(z) dz = 0$$

for any closed curve $\gamma$ in $\Delta'$.

<proof>
As in the proof of <i>Theorem 3</i>, we can use two segments connecting the center $(x_0, y_0)$ to a point $(x', y')$ as long as the segments don't contain any of the $\xi_j$. In this case we use the exact same arguments, except that the rectangle might contains $\xi_j$ inside it and we have to use <i>Theorem 2</i> instead of <i>Theorem 1</i> to conclude:

$$\int_{\sigma} f(z) dz + \int_{\overline{\sigma}} f(z) dz = 0$$

However, if any of the segments do go through one, such as in <i>Figure 4.1</i>, we can use 3 segments to avoid the missing point.

<figure class="center_children">
  <img src="{{resources_path}}/holes.svg" alt="See caption."  style="width: 300px;" />
  <figcaption>Figure 4.1: Curve to avoid going over a missing point $\xi$.</figcaption>
</figure>

We can find $x_1$ in between $x_0$ and $x'$ such that the line $x = x_1$ contains no $\xi_j$, and similarly for $y_1$ between $y_0$ and $y$. For $\sigma$ we use the segments: $(x_0, y_0) \rightarrow (x_0, y_1)$, $(x_0, y_1) \rightarrow (x', y_1)$, and $(x', y_1) \rightarrow (x', y')$. For $\overline{\sigma}$ we use $(x', y') \rightarrow (x_1, y')$, $(x_1, y') \rightarrow (x_1, y_0)$ and $(x_1, y_0) \rightarrow (x_0, y_0)$, depicted in <i>Figure 4.1</i>.
<br /><br />

The first difference is that equation $(3.1)$ will now look like:

$$F(x', y') = \int_{x_0}^{x'} f(x, y_1) dx + i \paren{\int_{y_0}^{y_1} f(x_0, y) dy + \int_{y_1}^{y'} f(x', y) dy}$$

To determine $\frac{\partial F}{\partial y}(x', y')$, we'll need to compute $F(x', y' + h)$, with $h \rightarrow 0$. We can use the exact same curve as we did for $F(x', y')$ except that the last segment will now go to $y' + h$, that is:

$$F(x', y') = \int_{x_0}^{x'} f(x, y_1) dx + i \paren{\int_{y_0}^{y_1} f(x_0, y) dy + \int_{y_1}^{y' + h} f(x', y) dy}$$

By considering the limit $(3.2)$:

$$\frac{\partial F}{\partial y}(x', y') = \lim_{h \rightarrow 0} \frac{F(x', y' + h) - F(x', y')}{h}$$

We'll arrive at the same conclusion that

$$\frac{\partial F}{\partial y}(x', y') = i f(x', y')$$

If we call the blue segments $\sigma$ and the red ones $\overline{\sigma}$, we'll still conclude that

$$(4.1) \quad F(x', y') = \int_{\sigma} f(z) dz = \int_{-\overline{\sigma}} f(z) dz$$

Because the 2 rectangles cancel out.

Computing $\partial F/\partial x (x', y')$ is trickier however. That's because when we consider the point $(x' \pm h, y')$, the curve utilized is the 2-segment one because the path to $(x' \pm h, y')$ doesn't contains a $\xi_j$. This is illustrated in <i>Figure 4.2</i> (note that we inverted the directions due to $(4.1)$).

<figure class="center_children">
  <img src="{{resources_path}}/holes2.svg" alt="See caption."  style="width: 300px;" />
  <figcaption>Figure 4.2: When we increment $x'$ by a small amount, the curve changes drastically.</figcaption>
</figure>

<br />

So when we consider the difference $F(x' + h, y') - F(x', y')$ we need to be careful. The trick is to use rectangles to reduce the differences. Taking the example for <i>Figure 4.2</i>, we label the relevant segments.


<figure class="center_children">
  <img src="{{resources_path}}/holes3.svg" alt="See caption."  style="width: 300px;" />
  <figcaption>Figure 4.3: Labeling segments from <i>Figure 4.2</i>.</figcaption>
</figure>

Let's define:

$$S(\sigma) = \int_{s} f(z) dz$$

For each of the segments $s = A, B, \cdots$. We have that $F(x', y') = S(F) + S(E) + S(C)$ and $F(x' + h, y') = S(1) + S(B) + S(C) + S(D)$. We also know that $A, B, -E, -F$ form a directed rectangle and from <i>Theorem 2</i> we have

$$(4.2) \quad S(1) + S(B) - S(E) - S(F) = 0$$

Now, if we want to compute $F(x' + h, y') - F(x', y')$ we have:

$$= S(1) + S(B) + S(C) + S(D) - (S(F) + S(E) + S(C)) \\ = S(1) + S(B) + S(D) - S(F) - S(E)$$

From $(4.2)$ we know $S(1) + S(B) = S(E) + S(F)$, so we can cancel some terms out and obtain:

$$= S(D)$$

$D$ is a horizontal segment from $x'$ to $x' + h$, so we have:

$$F(x' + h, y') - F(x', y') = \int_{x'}^{x' + h} f(x, y') dx$$

If we take $\lim_{h \rightarrow 0}$, we'll conclude, using arguments similar to those for $\partial F/\partial y$, that:

$$\frac{\partial F}{\partial x} (x', y') = f(x', y')$$

The conclusion from this exercise is that even when infinitesimal changes cause a dramatic change in the chosen curve, we're still able to obtain a difference that is an infinitesimal segment.
</proof>

## Conclusion

It took me a long time to figure out the proof of *Theorem 4*. In [1], Ahlfors provides almost no details besides *Figure 4.1* include in the proof of *Theorem 4*.

On my first read of the book I thought I had understood the proof but once I tried to plug it into the definition of derivative as a limit, I realized I didn't understand it properly.

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2]({{blog}}/2024/04/05/complex-integration.html)] NP-Incompleteness: Complex Integration
* [[3]({{blog}}/2024/04/13/path-independent-line-integrals.html)] NP-Incompleteness: Path-Independent Line Integrals
* [4] Basic Complex Analysis - J. Marsden and M. Hoffman
* [[5]({{blog}}/2023/12/21/holomorphic-functions.html)] NP-Incompleteness: Holomorphic Functions
