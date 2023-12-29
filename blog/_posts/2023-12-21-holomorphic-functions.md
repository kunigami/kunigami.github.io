---
layout: post
title: "Holomorphic Functions"
tags: [analysis]
excerpt_separator: <!--more-->
vanity: "2023-12-21-holomorphic-functions"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/holomorphic.png" alt="Mandelbrot fractal. This is a small scale version (different color scheme) of Figure 3." />
</figure>

In this short post we'll study holomorphic functions. We'll cover its definition and an interesting identity between them and the differential equations known as the *Cauchy-Riemann* equations.

<!--more-->

## Definition

A high-level definition of *holomorphic* functions is that they're complex functions that are differentiable everywhere in its domain. Before we formalize this definition, we need to define what differentiable means for a complex function.

Even before that though, let's simplify things a bit: a complex function can be one from one or more complex variables into the complex space (i.e. $f: \mathbb{C}^n \rightarrow \mathbb{C}^m$), but for simplicity, let's consider only the functions of one complex number to another.

In particular, let $\Omega$ be an [open subset](https://www.kuniga.me/docs/math/topology.html) of $\mathbb{C}$ and a function $f: \Omega \rightarrow \mathbb{C}$. We define the **complex derivative** of $f(z)$, denoted by $f'(z)$ as:

$$(1) \quad f'(z) =  lim_{h \rightarrow 0} \frac{f(z + h) - f(z)}{h}$$

Where $z \in \Omega$ and $z + h \in \Omega$. For the derivative to exist at a point $z$, that limit must exist. In other words, from the definition of limits, for every $\epsilon \gt 0$ there should exist $\delta \gt 0$ such that for $0 \lt \abs{h} \lt \delta$, we have $\abs{f'(z) - \frac{f(z + h) - f(z)}{h}} \lt \epsilon$.

Note that $h$ is a complex number, so $\abs{h}$ corresponds to its magnitude. If we consider $h$ in the complex plane, the above should hold, no matter how $h$ approaches the point $(0, 0)$. In [2], Campuzano provides a nice interactive visualization of this idea. We say $f: \Omega \rightarrow \mathbb{C}$ is **holomorphic** in $\Omega$ if it has a complex derivative at every $z \in \Omega$.

### Complex vs. "Normal" Derivative

A question I asked myself while learning this: why does this have anything to do with complex numbers? Can't we just define this for $\mathbb{R}^2$? One important factor is that we have a division between two complex numbers inside the limit. Recall that division is defined based on complex multiplication and complex multiplication is not the same as vector multiplication [3].

### Holomorphic vs. Analytic

In [1], Ahlfors calls holomorphic functions analytic. A analytic function is defined as an infinitely differentiable function and locally equal to its own Taylor series. So while it's possible to show they're equivalent, they have different definitions. So for clarity we'll adopt the more common terminology and call them holomorphic here.

### Holomorphic vs. Entire

[Entire functions](https://en.wikipedia.org/wiki/Entire_function) are a special case of holomorphic functions in which the domain is the **entire** complex plane, that is, $\Omega = \mathbb{C}$. This also gives us the origin for the name holomorphic, which comes from the Greek *hólos* (whole) and *morphḗ* (form), alluding to the fact that holomorphic functions look like (i.e. have the "form") of entire functions when restricted to the domain $\Omega$.

### Holomorphic vs. Meromorphic

Meromorphic functions are functions that are holomorphic on a $\Omega$ *except* for a set of isolated points which are poles ($z$ such that $1/f(z) = 0$). Mero comes from the Greek *meros* (part).

## Cauchy-Riemann Equations

As we know, a given complex number $z$ can be explicitly represented by its real and imaginary parts. Let $z$ be a complex number. We can then write it as $z = x + iy$ for $x, y \in \mathbb{R}$.

Analogously, for a function $f: \mathbb{C} \rightarrow \mathbb{C}$, we can define it as a function of two variables (the real and the complex parts) and we can also split $f$ into functions $u, v: \mathbb{R}^2 \rightarrow \mathbb{R}$ corresponding to their real and imaginary parts. For example:

$$f(z) = f(x + i y)$$

Can be written as $f: \mathbb{R}^2 \rightarrow \mathbb{C}$:

$$f(x, y)$$

And decomposed as two functions:

$$f(z) = u(x, y) + i v(x, y)$$

For example, if $f(z) = z + w$ for some constant $w = a + ib$, then we can write $f(x, y) = (x + iy) + (a + ib) = (x + a) + i(y + b)$ and define $u(x, y) = x + a$ and $v(x, y) = y + b$.

The **Cauchy-Riemann equations** are equalities involving the partial derivatives of $u$ and $v$ with respect to $x$ and $y$:

$$(2) \quad \frac{\partial u}{\partial x} = \frac{\partial v}{\partial y}, \qquad
\frac{\partial u}{\partial y} = -\frac{\partial v}{\partial x}$$

Note that these are defined in the real domain, so the (partial) derivatives here is the usual "real" ones in learn in calculus.

*Theorem 1.* claims that any function that satisfies the Cauchy-Riemann equations is holomorphic and vice-versa!

<theorem>
<p>
<b>Theorem 1.</b> A function is holomorphic if and only if it satisfies the Cauchy-Riemann equations.
</p>
</theorem>

<proof>

We'll only prove one direction, that is, that if a function is holomorphic then it satisfies the Cauchy-Riemann equations.

Let $f(z)$ be a holomorphic function. We want to compute $f'(z_0)$, so we can use $(1)$:

$$f'(z_0) =  lim_{h \rightarrow 0} \frac{f(z_0 + h) - f(z_0)}{h}$$

We mentioned that if $f$ is holomorphic, then the limit exists no matter how $h$ approaches $0$. Let's first consider the special case where $h$ approaches $0$ along a horizontal line in the complex plane, in other words, $h$ is real. Let's decompose $f$ into its real and imaginary parts:

$$f'(z_0)$$

$$= f'(x_0, y_0)$$

$$= lim_{h \rightarrow 0} \frac{u(x_0 + h, y_0) - u(x_0, y_0)}{h} + i \lim_{h \rightarrow 0} \frac{v(x_0 + h, y_0) - v(x_0, y_0)}{h}$$

The first limit is the partial derivative of $u$ with respect to the $x$ axis and the second is the partial derivative of $v$ with respect to the $x$:

$$(3) \quad f'(z_0) = \frac{\partial u(x_0, y_0)}{\partial x} + i \frac{\partial v (x_0, y_0)}{\partial x}$$

Because $f$ is holomorphic, the limit $f'(z_0)$ exists and so does the partial derivatives by $(3)$.

Now, if we consider the case where $h$ approaches $0$ along a vertical line in the complex plane, that is, $h$ is the imaginary $ik$, we have:

$$f'(z_0) = lim_{k \rightarrow 0} \frac{f(z_0 + ik) - f(z_0)}{ik}$$

Multiplying both numerator and denominator by $i$

$$ = lim_{k \rightarrow 0} \left(\frac{f(z_0 + ik) - f(z_0)}{k}\right) i$$

Decomposing $f$:

$$= lim_{h \rightarrow 0} \left(\frac{u(x_0 + h, y_0) - u(x_0, y_0)}{k} \right) i - \lim_{h \rightarrow 0} \frac{v(x_0 + h, y_0) - v(x_0, y_0)}{k}$$

Note how $u$ generates the imaginary part and $v$ the real part in this case. Some arithmetic will lead us to:

$$(4) \quad f'(z_0) = \frac{\partial v (x_0, y_0)}{\partial y} - i  \frac{\partial u(x_0, y_0)}{\partial y}$$

If we equate $(3)$ and $(4)$ we get the Cauchy-Riemann equations equations $(2)$.

It's possible to show the converse is also true: that if we have functions $u, v$ that have first partial derivatives in an open set $\Omega$ and that these partial derivatives satisfy $(2)$, then $f = u + i v$ is holomorphic [2].

</proof>

## Conclusion

In this post we learned about holomorphic functions. Despire the scary name, their definition is relatively simple: functions that are differentiable everywhere in their domain.

The similarity between complex differential with the ordinary differential hides an important difference, which has to do on how the "delta" approximates the limit. For the complex one, the limit must exist no matter how the delta $h$ tends to 0.

We also saw an equivalence between functions that satisfy the Cauchy-Riemann equations and holomorphic functions. In a way this lets us define complex differentiation in terms of ordinary partial differentiation.

## Harmonic Functions

Let $f$ be a twice differentiable function of two variables. The **Laplace operator** is defined as

$$\Delta f = \frac{\partial^2 f}{\partial x^2} + \frac{\partial^2 f}{\partial y^2}$$

The **Laplace's equation** is defined as:

$$\Delta f = 0$$

A function is called **harmonic** if it satisfies Laplace's equation. If a pair of harmonic functions $u$ and $v$ satisfy the Cauchy-Riemann equations $(2)$, then they're said to be **conjugate harmonic** of each other.

It's possible to show some equivalence between holomorphic functions and harmonic functions. More precisely:

<theorem>
<p>
<b>Theorem 2.</b> A function $f(x, y) = u(x, y) + i v(x, y)$ is holomorphic if and only if $u$ and $v$ form a pair of harmonic conjugates.
</p>
</theorem>

<proof>

We can leverage <i>Theorem 1</i> for this. If $f$ is holomorphic, then it $u$ and $v$ satisfy the Cauchy-Riemann equations $(2)$. We can first differentiate the first with respect to $x$ and obtain:

$$\frac{\partial^2 u}{\partial x^2} = \frac{\partial^2 v}{\partial y \partial x}$$

and the second by $y$:

$$\frac{\partial^2 u}{\partial y^2} = - \frac{\partial^2 v}{\partial x \partial y}$$

Assuming that $\frac{\partial^2 v}{\partial y \partial x}$ and  $\frac{\partial^2 v}{\partial x \partial y}$ are continuous, it's possible to show they're equal (<i>Schwarz's theorem</i>). So we obtain:

$$\frac{\partial^2 u}{\partial x^2} = -\frac{\partial^2 u}{\partial y^2}$$

or

$$\frac{\partial^2 u}{\partial x^2} + \frac{\partial^2 u}{\partial y^2} = 0$$

So $u$ is harmonic, and by an analogous procedure so is $v$. Since they satisfy the Cauchy-Riemann equations, they form a pair of harmonic conjugates.

Conversely, if they form a pair of harmonic conjugates, by definition they satisfy the Cauchy-Riemann equations and by <i>Theorem 1</i>, the corresponding $f(x, y) = u(x, y) + i v(x, y)$ is holomorphic.

</proof>



## Related Posts

[The Basel Problem]({{blog}}/2023/03/14/basel-problem.html). In that post we relied on the fact that $\sin x$ is an entire functions (a special type of holomorphic functions) to prove that:

$$\sum_{n=1}^{\infty} \frac{1}{n^2} = \frac{\pi^2}{6}$$

## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2](https://complex-analysis.com/content/complex_differentiation.html)]  Complex Analysis - Complex Differentiation
* [[3](https://www.kuniga.me/blog/2023/10/02/complex-geometry.html)] NP-Incompleteness - Complex Numbers and Geometry
