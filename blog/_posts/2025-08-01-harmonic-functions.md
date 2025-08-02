---
layout: post
title: "Harmonic Functions"
tags: [analysis]
vanity: "2025-08-01-harmonic-functions"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/harmonic.svg" alt="3D plot of a harmonic function" />
</figure>

The concept of harmony in mathematics has been used since 6th century BC. Pythagoras is credited with this early association [6]:

> Pythagorean philosophers advanced the unshakable belief that the essence of all things are numbers and that the universe was sustained by harmony.

He also associated harmony with music. Centuries passed and further associations between music and trigonometric functions (sine waves) occured, leading to terminology such as harmonic series, harmonic analysis and harmonic functions. In this post we'll study harmonic functions.

<!--more-->

I couldn't find a clear definitive answer on how harmonic functions got their name, but [7] suggests it was Henry Poincaré who coined the term and definition as any function that satisfies the Laplace equation.

Vaguely the association seems to be that, to compute properties of wave equations in two dimensions, it is required to solve a partial differential equation which in the simplest case is the Laplace equation.

## Definition

We first defined harmonic functions when introducing [holomorphic functions](https://www.kuniga.me/blog/2023/12/21/holomorphic-functions.html). To recap, in the context of complex analysis, a harmonic function $f$ is a function $\mathbb C \rightarrow \mathbb{R}$ that satisfies the Laplace equation:

$$
(1) \quad \Delta f = \frac{\partial^2 f}{\partial x^2} + \frac{\partial^2 f}{\partial y^2} = 0
$$

Here $x$ and $y$ represent the real and imaginary axis of the complex plane, and we can think of $f$ as a function of two real variables $(x, y)$, $f: \mathbb{R}^2 \rightarrow \mathbb{R}$.

The reason harmonic functions appear with holomorphic functions is that they're related. Let $f(z)$ be a $\mathbb{C} \rightarrow \mathbb{C}$ function with real and imaginary parts, $u$ and $v$.

Then, from *Theorem 3* in [2], $f(z)$ is holomorphic if and only if $u$ and $v$ are **conjugate harmonic**.

Recalling that a pair of harmonic functions are conjugate if they satisfy the *Cauchy-Riemann* equations:

$$
(2) \quad \frac{\partial u}{\partial x} = \frac{\partial v}{\partial y}, \quad \frac{\partial u}{\partial y} = - \frac{\partial v}{\partial x}
$$

We typically denote harmonic functions as $u$ and $v$ as opposed to $f$, because they are usually discussed in pair. Let $u$ be a harmonic function. We then claim that $\partial u / \partial x$ and $-\partial u / \partial x$ are conjugate harmonic.

<proof>

First, we show that $\partial u / \partial x$ and $-\partial u / \partial x$ are harmonic. Since $u$ is harmonic it must satisfy $(1)$. Now consider the Laplace operator applied to $\partial u / \partial x$:

$$
\frac{\partial^2 u}{\partial x^2} \frac{\partial u}{\partial x} + \frac{\partial^2 u}{\partial y^2} \frac{\partial u}{\partial x}
$$

Reordering terms:

$$
\frac{\partial u}{\partial x} \left(\frac{\partial^2 u}{\partial x^2}  + \frac{\partial^2 u}{\partial y^2} \right)
$$

The term inside parenthesis is $\Delta u$ which since $u$ is assumed harmonic, is the constant 0, so the partial differential wrt $x$ is also 0, and we showed $\partial u / \partial x$ is harmonic. A similar process shows the same for $-\partial u / \partial y$.

<br /><br />

It remains to show they satisfy the Cauchy-Riemann equations. Let $U = \partial u / \partial x$ and $V = -\partial u / \partial y$. We have:

$$
\frac{\partial U}{\partial x} = \frac{\partial^2 u}{\partial x^2} = -\frac{\partial^2 u}{\partial y^2} = \frac{\partial V}{\partial y}
$$

which uses $(1)$, and

$$
\frac{\partial U}{\partial y} = \frac{\partial^2 u}{\partial x \partial y} = - \frac{\partial V}{\partial x}
$$

</proof>

Which then means that

$$
(3) \quad f(z) = \frac{\partial u}{\partial x} -i \frac{\partial u}{\partial y}
$$

is holomorphic.

## Differential Form

Consider the differential form $f dz$. If we take $f = U + iV$ and $dz = dx + idy$, then we have:

$$
\quad f dz = (U + iV) (dx + idy)
$$

Replacing $U = \partial u / \partial x$ and $V = -\partial u / \partial y$ as in $(3)$, we get:

$$
f dz = \left(\frac{\partial u}{\partial x} dx + \frac{\partial u}{\partial y} dy \right) + i \left(- \frac{\partial u}{\partial y} dx + \frac{\partial u}{\partial x} \right)
$$

The real part is equivalent to the differential $du$, defined as (see [3]):

$$
du = \frac{\partial u}{\partial x} dx + \frac{\partial u}{\partial y} dy
$$

Suppose $u$ has a conjugate harmonic $v$, which satisfy $(2)$. Then we first have by definition:

$$
dv = \frac{\partial v}{\partial x} dx + \frac{\partial v}{\partial y} dy
$$

Replacing by $(2)$ we obtain:

$$
(4) \quad dv = - \frac{\partial u}{\partial y} dx + \frac{\partial u}{\partial x}
$$

which corresponds to the imaginary part of $f dz$. In general domains however, there might not be a single function $v$ that is harmonic conjugate with $u$. This is only true locally.

However, since $dv$ ends up not depending on $v$ itself (see $(4)$), we can denote it in relation to $u$, namely $^*du$, so we have:

$$
(5) \quad {}^*\!du = - \frac{\partial u}{\partial y} dx + \frac{\partial u}{\partial x}
$$

and we define this as the **conjugate differential of $du$**. We can then rewrite $fdz$ as:

$$
(6) \quad f dz = du + i {}^*\!du
$$

Similar to an exact differential, we have that:

**Lemma 1.** Let $^* du_1$ be the exact differential of $du$ and $\gamma$ a cycle homologous to 0. Then:

$$
(7) \quad \int_\gamma {}^*\!du = 0
$$

<proof>

If we integrate $(6)$ over a cycle $\gamma$ homologous to 0, we get:

$$
\int_\gamma f dz = \int_\gamma du + i \int_\gamma {}^*\!du
$$

Since $f$ is holomorphic, we know by Cauchy's Integral Theorem (<i>Theorem 3</i> in [4]) that the integral on the lefthand size is 0.

By construction, $du$ is the exact differential of $u$, and the integral of an exact differential is also 0 over cycles homologous to 0 (<i>Theorem 5</i>). Resulting in:

$$
\int_\gamma {}^*\!du = \int_\gamma - \frac{\partial u}{\partial y} dx + \frac{\partial u}{\partial x} = 0
$$

QED.

</proof>

## Pair of Harmonic Functions

We can extend $(7)$ to a pair of harmonic function (that are not necessarily conjugate). Then we have:

**Theorem 2.** Let $u_1$ and $u_2$ be harmonic functions. Then:

$$
\int_\gamma u_1 {}^*\!du_2 - u_2 {}^*\!du_1 = 0
$$

for every cycle homologous to 0 in $\Omega$.

<proof>

First we reduce to the problem where $\gamma$ is the boundary of a rectangle $R$ contained in $\Omega$. The proof of <i>Theorem 2</i> in [4] shows how we can go from the $\partial R$ to a simply connected region. If we can prove this for any simply connected region, then we can generalize for a multiply connected region because we're working with <i>conjugate differential</i> and not assuming $u_1$ and $u_2$ have a unique harmonic conjugate in $\Omega$.
<br />
<br />

So let $\gamma = \partial R$. Here we can assume both harmonic functions have conjugates so we can write:

$$
(2.1) \quad u_1 {}^*\!du_2 - u_2 {}^*\!du_1 = u_1 dv_2 - u_2 dv_1
$$

The differential of a product of functions can be obtained via the product rule. So $d(u_2 v_1)$ is:

$$
d(u_2 v_1) = u_2 dv_1 + v_1 du_2
$$

or

$$
- u_2 dv_1 = v_1 du_2 - d(u_2 v_1)
$$

So $(2.1)$ becomes:

$$
= u_1 dv_2 + v_1 du_2 - d(u_2 v_1)
$$

Now, since $(u_1, v_1)$ and $(u_2, v_2)$ form a pair of harmonic conjugates, there exist holomorphic functions $f_1$ and $f_2$ such that:

$$
\begin{align}
f_1 &= u_1 + i v_1 \\
f_2 &= u_2 + i v_2 \\
\end{align}
$$

By $(6)$, and taking ${}^*\!du_2 = dv_2$, we have $f_2 dz = du_2 + idv_2$. And so

$$(2.2) \quad f_1 f_2 dz = (u_1 + i v_1)(du_2 + i dv_2) = (u_1 du_2 - v_1 dv_2) + i(v_1 du_2 + u_1 dv_2)$$

This means that $u_1 dv_2 + v_1 du_2$ is the imaginary part of $f_1 f_2 dz$. If we integrate this over $\partial R$, by Cauchy's Integral Theorem we have:

$$
\int_{\partial R} f_1 f_2 dz = 0
$$

Breaking down into parts via $(2.2)$:

$$
\int_{\partial R} (u_1 du_2 - v_1 dv_2) + i \int_{\partial R} (v_1 du_2 + u_1 dv_2) = 0
$$

Which means both real and imaginary parts must be equal to 0. In particular we have:

$$
\int_{\partial R} v_1 du_2 + u_1 dv_2 = 0
$$

Also, since the differential $d(u_2 v_1)$ is the exact differential of the function $u_2 v_1$ and the integral over a closed curve is 0 [4]:

$$
\int_{\partial R} d(u_2 v_1) = 0
$$

Adding them together leads to:

$$
\int_{\partial R} v_1 du_2 + u_1 dv_2 - d(u_2 v_1) = 0
$$

which proves that:

$$
\int_{\partial R} u_1 {}^*\!du_2 - u_2 {}^*\!du_1 = 0
$$

</proof>

## The Mean-Value Property

We can use *Theorem 2* to prove that the arithmetic mean of a harmonic function over a circle of radius $r$ is a linear function of $\log r$:

**Theorem 3.** Let $u$ be a harmonic function. Then:

$$
(8) \quad \frac{1}{2\pi} \int_{\abs{z} = r} u d\theta = \alpha \log r + \beta
$$

Where constants $\alpha$ and $\beta$ depend only on $u$, not on $r$. If $u$ is harmonic for $z = 0$, then $\alpha = 0$.

<proof>
We set $u_1 = \log r$ and $u_2 = u$, assuming $u$ is harmonic in the disk $\abs{z} \lt \rho$. The region $\Omega$ is the punctured disk $0 \lt \abs{z} \lt \rho$. For $\gamma$ we use $C_1 - C_2$ with $C_i$ a circle $\abs{z} = r_i$ with $r_i \lt \rho$, positively oriented (counterclockwise).
<br /><br />
We need to verify that $C_1 - C_2$ is homologous to 0 in $\Omega$. The complement of $\Omega$ is 0 plus $\abs{z} \ge \rho$. We know that for $\abs{z} \ge \rho$, the winding number $n(C_i, z) = 0$ since it's "outside" the circles. For 0 it's not true, but we have that $n(C_1, z) = n(C_2, z)$ and hence $n(C_1 - C_2, z) = 0$. Which by definition means $C_1 - C_2$ is homologous to 0.
<br /><br />
By <i>Lemma 1</i>, we have $^*du_2 = r (\partial u_2 / \partial r) d\theta = r (\partial u / \partial r) d\theta$ and $^*du_1 = r (\partial u_1 / \partial r) d\theta$. Since $u_1 = \log r$, $\partial u_1 / \partial r = 1 / r$ which simplifies to $^*du_1 = d\theta$. Now we can plug into <i>Theorem 2</i>:

$$
\int_{C_1 - C_2} \log r \cdot r \frac{\partial u}{\partial r} d\theta - u d\theta = 0
$$

Splitting it into simpler integrals:

$$
\int_{C_1} \log r \cdot r \frac{\partial u}{\partial r} d\theta - \int_{C_1} u d\theta = \int_{C_2} \log r \cdot r \frac{\partial u}{\partial r} d\theta - \int_{C_2} u d\theta
$$

The conclusion is that the expression

$$
\int_{\abs{z} = r} \log r \cdot r \frac{\partial u}{\partial r} d\theta - \int_{\abs{z} = r} u d\theta
$$

does not depend on any specific choice of $r$, so we can assume it to be a constant, which we can denote as $\beta$. Now consider:

$$
\int_{C_1 - C_2} r \frac{\partial u}{\partial r} d\theta
$$

Since the integrand is $^* du_1$, by $(7)$ we have that the integral equals to 0 and then:

$$
\int_{C_1} r \frac{\partial u}{\partial r} d\theta = \int_{C_2} r \frac{\partial u}{\partial r} d\theta
$$

Which similarly tells us that

$$
\int_{\abs{z} = r} r \frac{\partial u}{\partial r} d\theta
$$

is independent of our choice of $r$. Let's call this expression $\alpha$. Putting things together, we can write $\beta$ as:


$$
\beta = \alpha \log r  - \int_{\abs{z} = r} u d\theta
$$

or:

$$
\int_{\abs{z} = r} u d\theta = \alpha \log r - \beta
$$

We can modify $\alpha$ and $\beta$ so that the expression above becomes $(8)$.
<br /><br />
Finally, if $u$ is harmonic for $z = 0$, we can choose one of the circles to be $\abs{z} = 0$ for which we get $\alpha = 0$.

</proof>

If $u$ is harmonic in $z = 0$, then we have $\alpha = 0$ and taking $\lim r \rightarrow 0$ gives us:

$$
\lim{r \rightarrow 0} \int_{\abs{z} = r} u d\theta = u(0) 2 \pi
$$

Thus $\beta = u(0)$. Assuming a different origin $z_0$ for the circles, we can obtain the expression:

$$
(9) \quad u(z_0) = \frac{1}{2 \pi} \int_{0}^{2 \pi} u(z_0 + re^{i\theta}) d\theta
$$

## The Maximum/Minimum Principle

One way to interpret $(9)$ is that we can compute the value of a harmonic function at a point as its average over a circle around it. This interpretation allows us to conclude the following:

**Theorem 4.** A non-constant harmonic function has neither a maximum nor a minimum in its region of definition. The maximum and minimum on a bounded closed set are attained at the boundary.

<proof>
Suppose $u$ is a harmonic function in $\Omega$ and that it attains its maximum value at a point $z^*$ in $\Omega$. Since it's open, there is a neighborhood around $z^*$, in particular a circle centered in it.
<br /><br />
Now according to $(9)$, $u(z^*)$ is the average of points over that circle, so unless all values are the same, there must be some values that are smaller and others that are greater than $u(z^*)$, which contradicts the fact that $u(z^*)$ is maximum.
<br /><br />
This applies to the minimum case too. But we can alternatively use the fact that $-u$ is also harmonic.
<br /><br />
If $\Omega$ is closed and bounded and $u$ is harmonic inside $\Omega$, then by the extreme value theorem, $u$ has its maximum/minimum in $\Omega$. Since we know it's not inside it, it must be on the boundary.
</proof>

One thing to note is that when we covered the *Maximum Priciple* for holomorphic functions [5], we did not mention the minimum value. That's because holomorphic functions are complex-valued so when we say maximum it is with respect of its modulus and a holomorphic function with a zero attains its minimum there.

On the other hand, harmonic functions are real-valued, so when we say maxmimum and minimum we're referring to its actual value.

## Bounded Regions

We can generalize the idea that a harmonic function at a point $z_0$ is a function of its values on a circle to it being a function of its values on a general bounded region.

Suppose we have a bounded closed set $E$ and let $u_1$ and $u_2$ be harmonic functions inside $E$ and continuous on its boundary. Suppose also they agree on the boundary of $E$ (i.e. they have the same value for points in the boundary).

Then we know that $u = u_1 - u_2$ is also harmonic and that it equals to 0 for points on the boundary. By the maximum/minimum principle theorem it means that $u$ attains its maximum/minimum value at the border, 0, which implies $u$ is equal to 0 everywhere.

This imples in turn that $u_1 = u_2$. Which leads to the conclusion that inside a bounded region, harmonic functions are uniquely determined by the points on its boundary.

## Poisson Kernel

The discussion in the previous session tells us a harmonic function $u$ can be computed from its values on the boundary but not exactly how.

For the special case where $z_0$ is the center of a circle and we know how to compute $u$ in the circumference, we can use $(9)$ to compute $u(z_0)$. We can generalize it a bit further by computing any point inside the circle given we know $u$ in the circumference.

**Theorem 5.** Let $u$ be a harmonic function in $\abs{z} \lt R$ and continuous for $\abs{z} \le R$. Then:

$$
u(a) = \frac{1}{2\pi} \int_{\abs{z} = R} \frac{R^2 - \abs{a}^2}{\abs{z - a}^2} u(z) d\theta
$$

<proof>
To start, suppose $u$ is harmonic in the disk $\abs{z} \le R$. Note that this is a strictier requirement than stated in the theorem, but we'll relax it later. We'll use the Möbius transform:

$$
z = S(w) = R \frac{Rw + a}{R + \overline{a} w}
$$

The transform:

$$
S'(w) = \frac{Rw + a}{R + \overline{a} w}
$$

is called a auto-morphism, because it maps the unit disk onto itself, however it "scrambles" the points around. Crucially, it sends the origin $w = 0$ to $a/R$. Another crucial property is that it preserves points on the boundary, by guaranteeing that $\abs{w} = 1$ iff $\abs{S(w)} = 1$.
<br /><br />
The mean-value property showed that we can compute the value of $u$ at the origin by evaluating $u$ for points on the circumference of the unit circle. After the transformation, the origin now is in $a / R$, but the set of points on the circumference of the unit circle is the same, so we can compute $u$ at $a / R$ from those points still!
<br /><br />
Then we have $S(w) = R S'(w)$ which scales that circle to one of radius $R$ and move $a/R$ to $a$. The points on the circumference of the unit circle are now the points on the boundary of the circle of radius $R$, so $u(s)$ can be obtained from $u$ evaluated at the points $\abs{z} = R$. The remaining of the proof is to find the explicit formula.
<br /><br />
Let $v(w) = u(S(w))$. Since $u$ is harmonic in $\abs{z} \le R$, then $v$ is harmonic in the transformed domain, $\abs{w} \le 1$. Then, according to $(9)$ we have:

$$
v(0) = \frac{1}{2 \pi} \int_{\abs{z} = 1} v(w) d \arg(w)
$$

Here we use $d \arg(w)$ instead of $d\theta$ to make it clearer this is with respect to $w$ not $z$. Replacing $u$ back:

$$
u(S(0)) = \frac{1}{2 \pi} \int_{\abs{z} = 1} u(S(w)) d \arg(w)
$$

We have that $S(0) = 0$ and $S(w) = z$:

$$
u(a) = \frac{1}{2 \pi} \int_{\abs{w} = 1} u(z) d \arg(w)
$$

Now we want to do a change of variable from $w$ to $z$. First we need to express $d \arg(w)$ as $d \arg(z) = d \theta$. <i>Lemma 7</i> tells us that:

$$
d \arg(w) = -i \frac{dw}{w}
$$

Since Möbius transforms are invertible, we can express $w$ as a function of $z$:

$$
(5.1) \quad w = \frac{R(z - a)}{R^2 - \overline{a}z}
$$

We can use the quotient rule to find

$$
dw = \left(\frac{R}{R^2 - \overline{a}z} + \frac{R (z - a) \overline{a}}{(R^2 - \overline{a}z)^2}\right) dz
$$

Dividing by $(5.1)$:

$$
\frac{dw}{w} = \left(\frac{1}{z - a} + \frac{\overline{a}}{R^2 - \overline{a}z}\right) dz
$$

Thus we have

$$
d \arg(w) = -i \left(\frac{1}{z - a} + \frac{\overline{a}}{R^2 - \overline{a}z}\right) dz
$$

We have from <i>Lemma 7</i> that $dz = i z d\theta$, so:

$$
d \arg(w) = \left(\frac{z}{z - a} + \frac{z\overline{a}}{R^2 - \overline{a}z}\right) d\theta
$$

Using the fact that $R^2 = z \overline{z}$, so $R^2 - \overline{a}z = z(\overline{z} - \overline{a}) = z (\overline{z - a})$. We have:

$$
d \arg(w) = \left(\frac{z}{z - a} + \frac{\overline{a}}{\overline{z - a}}\right) d\theta
$$

Combining the fraction under the same common denominator and using the property: $x \overline{x} = \abs{x}^2$:

$$
 = \frac{z(\overline{z - a}) + \overline{a}(z - a)}{\abs{z - a}^2} d\theta = \frac{z\overline{z} - z\overline{a} + \overline{a}z - \overline{a}a}{\abs{z - a}^2} d\theta
$$

Cancelling terms and using $R^2 = z \overline{z}$:

$$
d \arg(w) = \frac{R^2 - \abs{a}^2}{\abs{z - a}^2} d\theta
$$

So we get:

$$
(5.2) \quad u(a) = \frac{1}{2 \pi} \int_{\abs{z} = R} \frac{R^2 - \abs{a}^2}{\abs{z - a}^2} u(z) d\theta
$$

Now let's relax the assumption that $u$ is harmonic in the whole disk. Instead assume $u$ is harmonic for $\abs{z} \lt R$ and continuous for $\abs{z} \le R$. Then the function $v(a) = u(ar)$ is harmonic for $0 \lt r \le R$. So we can apply $(5.2)$ for $v$, and replacing $u$ back gives:

$$
u(ar) = \frac{1}{2 \pi} \int_{\abs{z} = R} \frac{R^2 - \abs{a}^2}{\abs{z - a}^2} u(zr) d\theta
$$

If we take the limit $r \rightarrow 1$, then $u(rz)$ tends uniformily to $u(z)$ in $\abs{z} = R$.

</proof>

This identity is known as the **Poisson Kernel**.

## Conclusion

It was really interesting to learn about the property of harmonic functions as ones that can be computed as averages from its values on the boundary (Poission kernel).

I had trouble mapping harmonic functions to physics. I learned that a bunch of models can be solve using the Laplace equations but I don't know enough about them (or forgot) to make it intuitive.

## Appendix

**Lemma 6.** Let $u$ be a harmonic function and  $^*du$ the conjugate differential of $du$. On a circumference, we can express it in polar coordinates as:

$$
{}^*\!du = r \frac{\partial u}{\partial r} d\theta
$$

<proof>
We start from $(5)$:

$$
{}^*\!du = - \frac{\partial u}{\partial y} dx + \frac{\partial u}{\partial x}
$$

The coordinate $x$ and $y$ can be expressed in polar coordinates as: $r \cos \theta$ and $y = r \sin \theta$. If we assume now $x$ is a function of two variables $r$ and $\theta$, by definition we have:

$$
dx = \frac{\partial x}{\partial r} dr + \frac{\partial y}{\partial \theta} d\theta
$$

which gives us:

$$
dx = \cos \theta dr - r \sin \theta d\theta
$$

similarly for $dy$:

$$
dy = \sin \theta dr + r \cos \theta d\theta
$$

Replacing these in $(5)$ and grouping:

$$
{}^*\!du = \left(- \frac{\partial u}{\partial y} \cos \theta + \frac{\partial u}{\partial x} \sin \theta \right) dr \\
 + r \left( \frac{\partial u}{\partial y} \sin \theta + \frac{\partial u}{\partial x} \cos \theta \right) d\theta
$$

Since we're assume we're in a circle, the radius does change and so $dr = 0$:

$$
(6.1) \quad {}^*\!du = r \left( \frac{\partial u}{\partial y} \sin \theta + \frac{\partial u}{\partial x} \cos \theta \right) d\theta
$$

Now consider $\partial u / \partial r$. We can use the chain rule to obtain:

$$
\frac{\partial u}{\partial r} = \frac{\partial u}{\partial x} \frac{\partial x}{\partial r} + \frac{\partial u}{\partial y} \frac{\partial y}{\partial r}
$$

With $\partial x / \partial r = \cos \theta$ and $\partial y / \partial r = \sin \theta$:

$$
\frac{\partial u}{\partial r} = \frac{\partial u}{\partial x} \cos \theta + \frac{\partial u}{\partial y} \sin \theta
$$

which is exactly the term in parenthesis in $(6.1)$, so we can replace:

$$
{}^*\!du = r \frac{\partial u}{\partial r} d\theta
$$

QED.

</proof>

**Lemma 7.** Let $\abs{w} = R$. Then $d \arg(w) = -i dw / w$.

<proof>

We have $w = R e^{i \theta}$ where $\theta = arg(w)$. Assuming $w$ to be a function of variables $r$ and $\theta$ (polar coordinates):

$$
dw = e^{i \theta} dr + i R e^{i \theta} d\theta
$$

But since we're on a circumference, $r$ is constant and thus $dr = 0$, giving us:

$$
\frac{dw}{w} = i R d\theta
$$

Dividing by $w = R e^{i \theta}$:

$$
\frac{dw}{w} = i d\theta
$$

Multiplying by $-i$ and replacing back $\theta = arg(w)$:

$$
d \arg(w) = -i \frac{dw}{w}
$$

</proof>


## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2](https://www.kuniga.me/blog/2023/12/21/holomorphic-functions.html)] NP-Incompleteness - Holomorphic functions
* [[3](https://www.kuniga.me/docs/math/derivative.html)] NP-Incompleteness - Derivative
* [[4](https://www.kuniga.me/blog/2025/03/15/general-cauchy.html)] NP-Incompleteness - The General Form of Cauchy's Theorem
* [[5](https://www.kuniga.me/blog/2025/01/18/max-principle.html)] NP-Incompleteness - The Maximum Principle
* [[6](https://en.wikipedia.org/wiki/Pythagoreanism)] Wikipedia - Pythagoreanism
* [[7](https://math.stackexchange.com/questions/123620/why-are-harmonic-functions-called-harmonic-functions)] Mathematics - Why are harmonic functions called harmonic functions?
