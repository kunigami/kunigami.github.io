---
layout: post
title: "Path-Independent Line Integrals"
tags: [analysis]
vanity: "2024-04-13-path-independent-line-integrals"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/thumbnail.svg" alt="a bunch of lines and circles abstract art" style="width: 100px;" />
</figure>

This is our second post in the series with my notes on complex integration, corresponding to *Chapter 4* in Ahlfors' Complex Analysis.

In this installment, we'll cover line integrals that are path-independent, in other words, that depend only on the first and last point of the paths, but not the specific path or curve chosen. We'll focus on the conditions these integrals have to satisfy for this to hold.

We'll start with line integrals for the Euclidean plane and then follow with complex line integrals.

<!--more-->

The previous posts from the series:

1. [Complex Integration]({{blog}}/2024/04/05/complex-integration.html)


## Line integral of the second kind

Let $\gamma$ be a path in $\mathbb{R}^2$ and $f: \mathbb{R}^2 \rightarrow \mathbb{R}^2$. We can "split" $f(x, y)$ into two functions, one for each of the dimensions on  its domain, i.e., $f(x, y) = (p(x, y), q(x, y))$.

We can define the line integral of the second kind (or second type) as:

$$(1) \quad \int_{\gamma} pdx + qdy$$

Where $dx$ and $dy$ are the differentials along the $x$-axis and $y$-axis dimensions, respectively. We can also say that the integrand $pdx + qdy$ is in *differential form*.

Note that the image of this integral is $\mathbb{R}$.

### Intuition

To get an intuition about this notation, we can look into physics. A function $\mathbb{R}^n \rightarrow \mathbb{R}^{n}$ can be interpreted as a vector field: it associates a vector with a point in space. One classic example is wind speed (Figure 1), where at each point in the plane, we can have a vector denoting the direction and intensity of the wind.

<figure class="center_children">
  <img src="{{resources_path}}/vector-field.svg" alt="See caption."  style="width: 500px;" />
  <figcaption>Figure 1: Visualization of a vector field. Source: <a href="hthttps://commons.wikimedia.org/w/index.php?curid=8008790">By Jim.belk - Own work, Public Domain</a></figcaption>
</figure>

Another example is the force field (e.g. gravitational or magnetic), denoted by $\overrightarrow{F}$. Work on the other hand is scalar ($\mathbb{R}$) and is defined as the dot product between the force vector and displacement, so if we wish to compute work performed on a curve $\gamma$ and it's commonly expressed as:

$$\int_\gamma \overrightarrow{F} \cdot \overrightarrow{dr}$$

Here, $\cdot$ denotes the dot product and $\overrightarrow{dr}$ an infinitesimal displacement along $\gamma$, so if we breakdown $\overrightarrow{F}$ and $\overrightarrow{dr}$ into its components, we have:

$$\int_\gamma (F_x, F_y) \cdot (dx, dy)$$

Applying the definition of dot product:

$$\int_\gamma F_x dx + F_ydy$$

Gives us the form $(1)$. We can interpret $F_x(x, y)$ as the length of the $x$ component of the vector $F$ at point $(x, y)$, and similarly for $F_y(x, y)$.

### Parametric curve

We can also write $(1)$ in terms of a single parameter $t$ if consider the parametric form of $\gamma$ as a function of a scalar $t \in [a, b]$. We then have functions describing the $x$ and $y$ values, $x(t)$ and $y(t)$ respectively.

With a change of variable we obtain:

$$(2) \quad \int_{\gamma} pdx + qdy = \int_{a}^{b} (p x'(t) + q y'(t))dt$$

Where in the second form, $p$ and $q$ are shorthands for $p(x(t), y(t))$ and $q(x(t), y(t))$ and, to be super clear, $x'(t) = dx(t)/dt, y'(t) = dy(t)/dt$.

## Path-independent integrals

As the name suggests a path-independent integral is an integral that only depends on its endpoints, not on the specific path over which it's integrated.

*Theorem 1* provides a sufficient and necessary condition for an integral to be path-independent.

**Theorem 1.** The line integral $\int_\gamma pdx + qdy$, defined in $\Omega$, depends only on the endpoints of $\gamma$ if and only if there exists $U(x, y): \Omega \rightarrow \mathbb{R}$ such that $\partial U / \partial x = p, \partial U / \partial y = q$.

<proof>

First, assume $\partial U / \partial x = p, \partial U / \partial y = q$ is true. We can replace it in $(2)$ and obtain:

$$\int_a^{b} \paren{\frac{\partial U}{\partial x} x'(t) + \frac{\partial U}{\partial y} y'(t)}dt$$

From the <a href="https://www.kuniga.me/docs/math/derivative.html">chain rule</a>, we have

$$= \int_a^b \frac{dU(x(t), y(t))}{dt} dt = U(x(b), y(b)) - U(x(a), y(a))$$

And we can see it only depends on the values of $U$ evaluated at $a$ and $b$.
<br /><br />
Now we consider the other direction, assuming $\int_\gamma pdx + qdy$ only depends on the endpoints a curve $\gamma$ in $\Omega$, which means that for any two endpoints we are free to choose whatever curve we fancy as long as it lies in $\Omega$. We fix the starting point at $(x_0, y_0)$ and let the other endpoint by any point $(x, y) \in \Omega$.
<br /><br />
We can define a curve $\gamma$ from $(x_0, y_0)$ to $(x, y)$ composed of segments parallel to either $x$-axis or $y$-axis (example in Figure 1.1) and define $U(x, y)$ as:

$$(1.1) \quad U(x, y) = \int_{\gamma} pdx + qdy$$

Now, we pick a point $(x_1, y) \in \Omega$ with fixed $x_1$ and have $\gamma$ go through it, so the last segment is horizontal, i.e. connecting $(x_1, y)$ and $(x, y)$. We can split the curve $\gamma$ in two, one from $(x_0, y_0)$ to $(x_1, y)$, $\gamma_1$, and another from $(x_1, y)$ to $(x, y)$, $\gamma_2$.

<figure class="center_children">
  <img src="{{resources_path}}/curve.svg" alt="See caption."  style="width: 400px;" />
  <figcaption>Figure 1.1: The curvy boundary represents $\Omega$ and the ortogonal path is the chosen curve.</figcaption>
</figure>

<br /><br />
Now $(1.1)$ can be written as:

$$U(x, y) = \int_{\gamma_1} pdx + qdy + \int_{\gamma_1} pdx + qdy$$

The value of first integral only depends on $y$, not on $x$ because we fixed $x_1$, so we can call it $c(y)$. In the second we have a horizontal segment so $y$ is constant and thus $dy = 0$, so we have:

$$U(x, y) = c(y) + \int_{x_1}^{x} pdx$$

If we define $P(x, y)$ such that $p = \partial P/\partial  x$, then we have:

$$U(x, y) = c(y) + P(x, y) - P(x_1, y)$$

Now if we take the partial derivative of $U(x, y)$ with respect to $x$, the terms $c(y)$ and $P(x_1, y)$ go to 0 since they're not functions of $x$, and $P(x, y)$ becomes $p$, so:

$$\frac{\partial U}{\partial x} = p$$

We can use an analogous argument by choosing another curve such that the last segment is vertical and show that

$$\frac{\partial U}{\partial y} = q$$

as well. <i>QED</i>.
</proof>

### Exact Differential

Let $U(x, y)$ be a function $U: \mathbb{R}^2 \rightarrow \mathbb{R}^2$. Then we can use the [chain rule](https://www.kuniga.me/docs/math/derivative.html) to obtain:

$$\frac{dU}{dt} = \frac{\partial U}{\partial x} \frac{dx}{dt} + \frac{\partial U}{\partial y} \frac{dy}{dt}$$

In particular, we have the differential $dU$ as:

$$dU = \frac{\partial U}{\partial x} dx + \frac{\partial U}{\partial y} dy$$

If $U$ satisfies *Theorem 1*, then we have:

$$(3) \quad dU = p dx + q dy$$

Recall that the integrand $p dx + q dy$ is said to be in differential form. If *Theorem 1* is satisfied, it's exactly the same as the differential of $U$, so we can call it an **exact differential** form.

## Complex line integral

So far we've been working with $\mathbb{R}^2$. Let's now return to the complex world. Recall the contour integral (see *Contour Integral* in [2]) over a curve $\gamma$:

$$\int_\gamma f(z)dz$$

Since $dz$ is a complex number, we can consider its real and imaginary part $dz = dx + i dy$. Replacing in the above gives us:

$$= \int_\gamma f(z)dx + if(z)dy$$

As usual, we can think of $f(z)$ as a function of two real variables and also define $p(x, y) = f(z)$ and $q(x, y) = if(x, y)$ and we get a differential form of $(1)$. The major difference is that the domain of these functions is not $\mathbb{R}$ but $\mathbb{C}$.

We can still use *Theorem 1* since it doesn't depend on the type of the image of the integral. We'll denote $U(x, y)$ from the theorem as $F(z)$, so if

$$(4) \quad \frac{\partial F}{\partial x} = p = f(z)$$

and

$$(5) \quad \frac{\partial F}{\partial y} = q = if(z)$$

we can multiply $(5)$ by $i$ and add it to $(4)$:

$$\frac{\partial F}{\partial x} = - i \frac{\partial F}{\partial y}$$

which is another form of the Cauchy-Riemman equations [3]! From *Theorem 1* in [3], we conlude that $F$ is a holomorphic function. Further we have from $(3)$:

$$dF = f dx + i f dy = f dz$$

or

$$\frac{dF}{dz} = f$$

that is, $f$ is the derivative of $F$. We can thus re-state *Theorem 1* as a corollary:

**Corollary 2.** The complex line integral $\int_\gamma f(z)dz$, defined in $\Omega$, depends only on the endpoints of $\gamma$ if and only if $f$ is the derivative of some holomorphic function in $\Omega$.

### Example

A result that will be useful later is the integral

$$(6) \quad \int_\gamma (z - a)^{n} dz$$

For a closed curve $\gamma$, $n \in \mathbb{Z}$ and a constant $a \in \mathbb{C}$. We have that $(z - a)^{n}$ is the derivative of

$$(7) \quad (z - a)^{n+1}/(n + 1)$$

If $n \ge 0$, then $(7)$ is holomorphic everywhere (or entire [3]), thus we can use *Corollary 2* to claim $(6)$ only depends on its endpoints and since it's a closed curve, they coincide and thus $(6)$ is 0.

If $n \lt 1$, then  $(7)$ is holomorphic only if $z \ne a$, so as long as $\gamma$ doesn't go through $a$, $(6)$ is still 0. For $n = -1$, we can't claim $(6)$ is always 0. An example provided in [1] is

$$(8) \quad \int_C (z - a)^{-1} dz$$

Where $C$ is a circle of radius $\rho$ centered in $a$. We can thus write $z = a + \rho e^{it}$, $0 \le t \le 2\pi$, so $dz = i \rho e^{it} dt$. Replacing in $(8)$:

$$= \int_0^{2\pi} \frac{i \rho e^{it}}{a + \rho e^{it} - a} dt = \int_0^{2\pi} idt = 2\pi i$$


## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2]({{blog}}/2024/04/05/complex-integration.html)] NP-Incompleteness: Complex Integration
* [[3]({{blog}}/2023/12/21/holomorphic-functions.html)] NP-Incompleteness: Holomorphic Functions
