---
layout: post
title: "Complex Integration"
tags: [analysis]
vanity: "2024-04-05-complex-integration"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}


<figure class="image_float_left">
  <img src="{{resources_path}}/thumbnail.jpeg" alt="a violin" />
</figure>

This post inaugurates a series with my notes on complex integration, corresponding to *Chapter 4* in Ahlfors' Complex Analysis.

In this post we'll cover different definitions of complex integrals - integrals of functions whose domain and image are complex numbers - and some basic properties.

<!--more-->

## Definitions

To recap, in [2] we defined *complex derivatives* of a function $f: \Omega \rightarrow \mathbb{C}$, for $\Omega \in \mathbb{C}$, denoted by $f'(z)$, as:

$$f'(z) =  \lim_{h \rightarrow 0} \frac{f(z + h) - f(z)}{h}$$

It looks very similar to a real derivative except that we're dealing with complex numbers, which makes the concept $h \rightarrow 0$ more nuanced.

How about the complex integral? Consider first the real-integral such as

$$(1) \qquad \int_a^b f(x) dx$$

We can think of it as the sum of $f(x)$ for infinitesimal sub-intervals $dx$ over the a segment of the real-line. For complex numbers we can generalize it by letting it be any curve on the complex plane, in particular *parametric curves* as defined in [3] as we see next.

### Parametric Curve Integral

Consider the function $f(t): [a, b] \rightarrow \mathbb{C}$ where $t$ is from a real-number $a \le t \le b$. The line integral is defined as:

$$(2) \qquad \int_{a}^{b} f(t) dt$$

This is very similar to the real integral $(1)$ with the exception that the image of $f(t)$ is complex (but not its domain). If we decompose it into its real and imaginary part, say, $f(t) = u(t) + iv(t)$ we can use the linearity of real integrals to obtain:

$$(3) \qquad \int_{a}^{b} f(t) dt = \int_{a}^{b} u(t) dt + i \int_{a}^{b} v(t) dt$$

In other words we can define a line integral through the original real integral $(1)$. Using the linearity principle again, we can show that the complex line integral is also linear:

$$(4) \qquad \int_{a}^{b} cf(t) dt = c \int_{a}^{b} f(t) dt$$

For a complex constant $c = \alpha + i \beta$.

Recall that the triangle inequality which states that $\abs{u + v} \le \abs{u} + \abs{v}$ for complex $u$ and $v$. Using (4) we can generalize this property to the complex line integral, as stated in *Theorem 1*.

**Theorem 1.**

$$\abs{\int_{a}^{b} f(t) dt} \le \int_{a}^{b} \abs{f(t)} dt$$

<proof>
Let $c = e^{-i\theta}$ where $\theta = \arg{\int_{a}^{b} f(t) dt}$. If we define $w = \int_{a}^{b} f(t) dt$, then $w = \abs{w}e^{i\theta}$ and thus $cw = \abs{w}$. Since $cw$ is real, we have $cw = \Re{(cw)}$, expanding we get:

$$(1.1) \quad \Re{\paren{e^{-i\theta} \int_{a}^{b} f(t) dt}} = \abs{\int_{a}^{b} f(t) dt}$$

From $(3)$ we have that:

$$\Re{\paren{\int_{a}^{b} f(t) dt}} = \int_{a}^{b} u(t) dt = \int_{a}^{b} \Re{\paren{f(t)}} dt$$

Since $u(t) = \Re{\paren{f(t)}}$ by definition. Let $g(t) = c f(t)$ for a complext constant $c$.

$$\Re{\paren{\int_{a}^{b} c f(t) dt}} = \int_{a}^{b} \Re{\paren{c f(t)}} dt$$

Combining with $(4)$:

$$(1.2) \quad \Re{\paren{c \int_{a}^{b} f(t) dt}} = \int_{a}^{b} \Re{\paren{c f(t)}} dt$$

For any complex $z$, we have that $\Re{(z)} \le \abs{z}$. Since in this case $\abs{c} = 1$, $\Re{\paren{c f(t)}} \le \abs{f(t)}$. Using this in $(1.2)$ we get:

$$\Re{\paren{c \int_{a}^{b} f(t) dt}} \le \int_{a}^{b} \abs{f(t)} dt$$

Together with $(1.1)$, this proves the theorem.

</proof>

### Contour Integral

Instead of defining over a real interval, we can define it over a curve or arc $\gamma$, as long as it is piecewise differentiable and that $f(z)$ is continuous over $z \in \gamma$:

$$(5) \quad \int_{\gamma} f(z) dz = \int_{a}^{b} f(g(t)) g'(t) dt$$

Here we're using variable substition $z = g(t)$ to define the line integral over a curve in terms of $(1.1)$.

We can also subdivide $\gamma$ into sub-curves $\gamma = \gamma_1 + \gamma_2 + \cdots + \gamma_n$ in which case the integral over it can be expressed as the sum of the integrals of its parts:

$$\int_{\gamma} f(z) dz = \int_{\gamma_1} f(z) dz + \int_{\gamma_2} f(z) dz + \cdots + \int_{\gamma_n} f(z) dz$$

### Conjugate Integral

We can introduce the following notation:

$$(6) \quad \int_{\gamma} f(z) \overline{dz} = \overline{\int_{\gamma} \overline{f(z)} dz}$$

Recall that we can think of a function of $C$ as one taking two real values, the real and imaginary part of $z$, so $f(z) = f(x, y)$. We can integrate over only $x$ and obtain a function of $y$. That is,

$$(7) \quad g(y) = \int_{\gamma} f(x, y) dx$$

Analogously, we have:

$$(8) \quad h(x) = \int_{\gamma} f(x, y) dy$$

With this notation, we can express $(7)$ and $(8)$ via $(5)$ and $(6)$:

$$(9) \quad \int_{\gamma} f(x, y) dx = \frac{1}{2}\paren{\int_{\gamma} f(z) dz + \int_{\gamma} f(z) \overline{dz}}
$$

and

$$(10) \quad \int_{\gamma} f(x, y) dy = \frac{1}{2i}\paren{\int_{\gamma} f(z) dz - \int_{\gamma} f(z) \overline{dz}}
$$

which follows from the identities: $x = (z + \overline{z})/2$ and $y = (z - \overline{z})/(2i)$. If we multiply $(10)$ by $i$ and add with $(9)$, we can express $\int_{\gamma} f(z) dz$ as a function of its "partial integrals":

$$\int_{\gamma} f(z) dz = \int_{\gamma} f(x, y) dx + i \int_{\gamma} f(x, y) dy$$

If we split the real and imaginary parts of the result of $f$, i.e. $f(z) = u(z) + iv(z)$, we can then obtain:

$$\int_{\gamma} f(z) dz = \int_{\gamma} (u(x, y) + iv(x, y))dx + i \int_{\gamma} (u(x, y) + iv(x, y))dy$$

Grouping terms into real and imaginary parts:

$$\int_{\gamma} f(z) dz = \int_{\gamma} (u(x, y)dx - v(x, y)dy) + i \int_{\gamma} (u(x, y)dy + v(x, y)dx)$$

Or using a more succinct notation (ommitting the parameters that can be inferred from context):

$$\int_{\gamma} f dz = \int_{\gamma} (udx - vdy) + i \int_{\gamma} (udy + vdx)$$

### Arc-length Integral

We introduce yet another definition and notation, where we use the length of the differential and is defined as follows:

$$(11) \quad \int_{\gamma} f(z)\abs{dz} = \int_{a}^{b} f(g(t)) \abs{g'(t)} dt$$

Which is similar to $(6)$ except that we multiply by the real $\abs{g'(t)}$ instead of the complex $g'(t)$. An analogous result of *Theorem 1* for arc-length integral is *Theorem 2*:

**Theorem 2.**

$$\abs{\int_{\gamma} f(z) dz} \le \int_{\gamma} \abs{f(z)} \abs{dz}$$

<proof>

By definition we have:

$$\abs{\int_{\gamma} f(z) dz} = \abs{\int_{a}^{b} f(g(t)) g'(t) dt}$$

From <i>Theorem 1</i> we know that:

$$\abs{\int_{a}^{b} f(g(t)) g'(t) dt} \le \int_{a}^{b} \abs{f(g(t)) g'(t)} dt$$

and that $\abs{f(g(t)) g'(t)} = \abs{f(g(t))} \abs{g'(t)}$. On the other side we have, by definition,

$$\int_{\gamma} \abs{f(z)} \abs{dz} = \int_{a}^{b} \abs{f(g(t))} \abs{g'(t)} dt$$

So we conclude that

$$\abs{\int_{a}^{b} f(g(t)) g'(t) dt} \le \int_{a}^{b} \abs{f(g(t))} \abs{g'(t)} dt$$

and thus

$$\abs{\int_{\gamma} f(z) dz} \le \int_{\gamma} \abs{f(z)} \abs{dz}$$

</proof>

Which is nice because we can work with the contour integral notation instead of the parametric curve one.

**Curve length.** If $f(z) = 1$, then $(11)$ reduces to

$$\int_{\gamma} \abs{dz} = \int_{a}^{b} \abs{g'(t)} dt$$

and it corresponds to the length of $\gamma$. As an example, if $\gamma$ is a circle of radius $\rho$ centered at the origin, we can define the parametric curve $g(t) = \rho e^{it}$ for $0 \le t < 2 \pi$.

We have $g'(t) = g(t) = \rho e^{it}$ and that $\abs(\rho e^{it}) = \rho$ and that $f(g(t)) = 1$. Plugging this into $(11)$ gives us:

$$\int_{\gamma} \abs{dz} = \int_{a}^{b} \rho dt = 2\pi \rho$$

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2]({{blog}}/2023/12/21/holomorphic-functions.html)] NP-Incompleteness: Holomorphic Functions
* [[3]({{blog}}/2023/12/30/conformal-maps.html)] NP-Incompleteness: Conformal Maps
