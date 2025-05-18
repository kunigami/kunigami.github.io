---
layout: doc
title: "Complex Integral Cheat Sheet"
---

{% include blog_vars.html %}


## The Winding Number

Let $\gamma$ be a Jordan curve in $\Omega$, and point $a \in \Omega$ not in $\gamma$. We define the [winding number]({{blog}}/2024/05/09/the-winding-number.html) of $a$ with respect to $\gamma$ as:

$$n(\gamma, a) = \frac{1}{2\pi i} \int_\gamma \frac{dz}{z - a}$$

## Cauchy's Integral Theorem

Let $f(z)$ be holomorphic in a simply connected region $\Omega$, and a Jordan curve $\gamma$. Then the [Cauchy's integral theorem]({{blog}}/2025/03/15/general-cauchy.html) states:

$$\int_{\gamma} f(z) dz = 0$$


## Cauchy's Integral Formula

Let $f(z)$ be holomorphic in a simply connected region $\Omega$, and a Jordan curve $\gamma$ in $\Omega$ and a point $z \in \Omega$ not on $\gamma$ and such that $n(\gamma, z) = 1$ ($\gamma$ winds around $z$ exactly once, counter-clockwise). Then the [Cauchy Integral formula]({{blog}}/2024/06/06/cauchy-integral-formula.html) is:

$$f(z) = \frac{1}{2\pi i} \int_{\gamma} \frac{f(w)}{w - z} dw$$
