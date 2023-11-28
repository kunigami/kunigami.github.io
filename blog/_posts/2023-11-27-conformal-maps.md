---
layout: post
title: "Conformal Maps"
tags: [analysis]
excerpt_separator: <!--more-->
vanity: "2023-11-03-gauss-lucas-theorem"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/convex-thumb.png" alt="Mandelbrot fractal. This is a small scale version (different color scheme) of Figure 3." />
</figure>

<!--more-->


Topology 

## Holomorphic Functions

Let $A$ be an open set of $\mathbb{C}$ and a function $f: A \rightarrow \mathbb{C}$. We define the **complex derivarive** of $f(z)$, denoted by $f'(z)$ as:

$$f'(z) =  lim_{h \rightarrow 0} \frac{f(z + h) - f(z)}{h}$$

Where $z \in A$ and $z + h \in A$. For the derivative to exist at a point $z$, that limit must exist. In other words, from the definition of limits, for every $\epsilon \gt 0$ there should exist $\delta \gt 0$ such that for $0 \lt \abs{h} \lt \delta$, we have $\abs{f'(z) - \frac{f(z + h) - f(z)}{h}} \lt \epsilon$. 

Note that $h$ is a complex number, so $\abs{h}$ corresponds to its magnitude. If we consider $h$ in the complex plane, the above should hold, no matter how $h$ approaches the point $(0, 0)$. [2] has a nice interactive visualization of this idea.

We say $f: A \rightarrow \mathbb{C}$ is **complex analytic** or **holomorphic** in $A$ if it has a complex derivative at every $z \in A$.

## Cauchy-Riemann Equations

## References

* [[1](https://en.wikipedia.org/wiki/Gauss%E2%80%93Lucas_theorem)] Wikipedia: Gauss-Lucas Theorem
* [[2](https://chat.openai.com/)] ChatGPT4 


https://complex-analysis.com/content/complex_differentiation.html