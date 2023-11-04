---
layout: post
title: "The Gauss-Lucas Theorem"
tags: [analysis, python]
excerpt_separator: <!--more-->
vanity: "2023-11-03-gauss-lucas-theorem"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/convex-thumb.png" alt="Mandelbrot fractal. This is a small scale version (different color scheme) of Figure 3." />
</figure>

The Gauss-Lucas theorem (named after Carl Friedrich Gauss and Félix Lucas) describes a relationship between the roots of a polynomial and the roots of its derivarive when plotted in the complex plane. In particular it says that the roots of the derivative lie within the convex hull defined by the roots from the original polynomial or in less geometric terms, that each root of the derivative is a convex combination of the roots of the original polynomial.

In this post we'll define the theorem more formally, present a proof and do some computational experimentation using Python.

<!--more-->

<img src="{{resources_path}}/convex-rec.png" alt="Several convex polygons nested within each other." />

## Theorem

Let $P$ be a polynomial of degree $n$, of the form $P(x) = \sum_{k = 0}^n a_k z^k$, where $z, a_k \in \mathbb{C}$ and $a_n \neq 0$. Let $r_i$ be the zeros (or roots) of $P(z)$, that is $P(r_i) = 0$, for $i = 0, \dots, n$.

Consider its derivative $P'(z) = \sum_{k = 0}^{n-1} (k + 1) a_{k + 1} z^{k}$ and its corresponding roots $r'_j$ for which $P'(r'_j) = 0$, $j = 0, \dots, n - 1$.

Now, if we plot $r_i$ and $r'_j$ on the complex plane, the Lucas-Gauss theorem claims that $z'_j$ lies in the [convex hull](https://en.wikipedia.org/wiki/Convex_hull) defined by $r_i$.

## Proof

First we use the fundamental theorem of algebra, which allows us writing $P(z)$ as the product:

$$P(z) = a_n \prod_{i = 0}^{n} (z - r_i)$$

Since $P'(z)$ is also a polynomial,

$$P'(z) = (n a_{n-1}) \prod_{i = 0}^{n - 1} (z - r'_i)$$

Now consider the fraction:

$$\frac{P'(z)}{P(z)}$$

If a root $r$ belongs to both $P'(z)$ and $P(z)$, then the theorem is trivially true because the points defining the convex hull are considered to lie within it. In any case, their factors $(z - r)$ will cancel out in the fraction above, so we can assume that $P'(z)$ and $P(z)$ have no common roots.

This fraction is known as the logarithmic derivative because:

$$\frac{d}{dx} \ln f(x) = \frac{1}{f(x)} \frac{d f(x)}{dx}$$

This identity is very convenient because we can use it to transform a product into a sum via the logarithm:

$$\ln P(z) = \ln a_n + \sum_{i = 0}^{n} \ln (z - r_i)$$

And its derivative can be found by finding the derivative for each term individually and adding them up. For any $r_i$, we have

$$\frac{d}{dz} \ln (z - r_i) = \frac{1}{z - r_i}$$

Thus

$$\frac{P'(z)}{P(z)} = \frac{d}{dz} \ln P(z) = \sum_{i = 0}^{n} \frac{1}{z - r_i}$$

Note that $\ln a_n$ is constant with respect to $z$, so it vanishes. Now let's see what happens when we plug one of the roots of $P'(z)$, say $r'_j$, in the equation above. Remember that we're assuming it's not a root of $P(z)$, so $P(r'_j) \neq 0$ and $P'(r'_j) = 0$ by definition. We then get


$$\sum_{i = 0}^{n} \frac{1}{r'_j - r_i} = 0$$

Since $r'_j - r_i$ is a complex number, we can multiply both the numerator and denominator by its conjugate $\bar{r}'_j - \bar{r}_i$ to obtain:


$$\sum_{i = 0}^{n} \frac{\bar{r}'_j - \bar{r}_i}{\abs{r'_j - r_i}^2} = 0$$

We can separate the terms summands with $r'_j$ and $r_i$ as numerators into different sides,

$$\sum_{i = 0}^{n} \frac{\bar{r}'_j}{\abs{r'_j - r_i}^2} = \sum_{i = 0}^{n} \frac{\bar{r}_i}{\abs{r'_j - r_i}^2}$$

Since $\bar{r}'_j$ is independent of $i$, we can factor it out from the first sum. Let's also define

$$\delta_{ij} =\frac{1}{\abs{r'_j - r_i}^2}$$

Note that since $r_j' \neq r_i$, then $\abs{r_j' - r_i}^2$ is a positive real number and so is $\delta_{ij}$. We can then simplify the expression to:

$$\bar{r}'_j \sum_{i = 0}^{n} \delta_{ij} = \sum_{i = 0}^{n}  \delta_{ij} \bar{r}_i$$

And then isolating $\bar{r}'_j$:

$$r'_j = \sum_{i = 0}^{n} \frac{\delta_{ij}}{\sum_{i = 0}^{n} \delta_{ij}} \bar{r}_i$$

Let's now define

$$\alpha_{ij} = \frac{\delta_{ij}}{\sum_{i = 0}^{n} \delta_{ij}}$$

We can think of $\alpha_{ij}$ as a normalized version of $\delta_{ij}$ such that $0 \lt \alpha_{ij} \lt 1$ and $\sum_{i=0}^n \alpha_{ij} = 1$. So we finally get

$$(1) \quad \bar{r}'_j = \sum_{i = 0}^{n} \alpha_{ij} \bar{r}_i$$

Since $0 \lt \alpha_{ij} \lt 1$ and $\sum_{i=0}^n \alpha_{ij} = 1$, we conclude that $r_j'$ is a convex combination of $\bar{r_i}$. It might seem like we're cheating to define $\alpha_{ij}$ as a coefficient when it is a function of $r'_j$ and $r_i$, but we just need to show that for a fixed set of $r_i$ and $r'_j$, the convex combination holds, so it's okay to assume they're constants.

But so far we've only showed that the conjugate of $r'_j$ is a convex combination of the conjugates of $r_i$. However, we can show that the identity holds for $r'_j$ and $r_i$ as well. To see why, we can obtain one equation for the real and imaginary parts of $r'_j$, since we're only dealing with sums:

$$\Re(\bar{r}'_j) = \sum_{i = 0}^{n} \alpha_{ij} \Re{(\bar{r}_i)}$$

and

$$\Im(\bar{r}'_j) =  \sum_{i = 0}^{n} \alpha_{ij} \Im{(\bar{r}_i)}$$

Since $\Re{(z)} = \Re{(\bar{z})}$ and $\Im{(z)} = -\Im{(\bar{z})}$, we obtain,

$$\Re(r'_j) = \sum_{i = 0}^{n} \alpha_{ij} \Re{(r_i)}$$

and

$$-\Im{(r'_j)} = \sum_{i = 0}^{n} \alpha_{ij} (-\Im{(r_i)})$$

which is

$$\Im{(r'_j)} = \sum_{i = 0}^{n} \alpha_{ij} \Im{(r_i)}$$


which should give us back:

$$r'_j = \sum_{i = 0}^{n} \alpha_{ij} r_i$$

QED.


## Experimentation

We can "visualize" this theorem by following these steps:

**Step 1. (Generate)** Generate `N` random complex numbers, $r$. Assume they're the roots of a polynomial $P(z)$.

**Step 2. (From roots)** Compute the polynomial $P(z)$ from the roots $r$. We can do so using the *fundamental theorem of algebra*:

$$(2) \quad P(z) = \prod_{i = 1}^{n} (z - r_i)$$

**Step 3. (Derive)** Compute the derivative of the polynomial, $P'(z)$, which is simply

$$P'(z) = \sum_{k = 0}^{n - 1} a_{k+1} (k + 1)  x^{k}$$

Where $a_{k}$ is the coefficient of $i$-th degree of $P(z)$.

**Step 4. (Roots)** Find the roots of $P'(z)$, $r'$. We can use one of the numeric algorithms to estimate the roots such as the [Weierstrass method](https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method). 

**Step 5. (Convex Hull)** Compute the convex hull of $r$ by assuming they're 2D points. This can be done via some of convex hull algorithm such as [Graham scan](https://en.wikipedia.org/wiki/Graham_scan) which runs in $O(n \log n)$.

**Step 6. (Plot)** Plot $r'$ and verify they lie inside the convex hull.

### Python Implementation


**Step 1. (Generate)** We can generate the complex numbers using a uniform distribution and scale them to be a 10x10 square at the origin.

{% highlight python %}
import random

N = 10
rs = []
for i in range(N):
    a = random.uniform(-5, 5)
    b = random.uniform(-5, 5) 
    rs.append(a + b * 1j)
{% endhighlight %}

Note that Python supports complex numbers as first-class citizens! We can write a complex literal as:

{% highlight python %}
c = 10 + 4j
c = 1j # imaginary only
{% endhighlight %}

It uses `j` instead of `i` and there's no syntax for constructing the imaginary from a variable. However, the arithmetic operations work as one would expect, so if we want to construct a complex number with imaginary value from a real number stored in the variable `x`, we can do `x * 1j`.

**Step 2. (From Roots)** Numpy has a `Polynomial` class which we can use to perform all the operations we need in this experiment. It can be constructed from an array of coefficients `c`, where `c[i]` represents the coefficient of $i$-th degree.

The library also contains a utils function `fromroots()` which performs the product from $(2)$ and returns the coefficients which we can then use to construct the polynomial.

{% highlight python %}
from numpy.polynomial.polynomial import Polynomial, polyfromroots
p0 = Polynomial(polyfromroots(rs))
{% endhighlight %}

**Step 3. (Derive)** The `Polynomial` class also supports computing its derivative via `derive()`:

{% highlight python %}
p1 = p0.deriv() 
{% endhighlight %}

**Step 4. (Roots)** The `Polynomial` class supports finding its roots via `roots()`:

{% highlight python %}
r1 = p1.roots() 
{% endhighlight %}

The documentation says the algorithm used for this is via enginevalues of the [companion matrix](https://en.wikipedia.org/wiki/Companion_matrix). I have no idea how it works, but could be an interesting algorithm to explore in the future.

**Step 5. (Convex Hull)** We can using Scipy's library for computing the 2D convex hull:

{% highlight python %}
from scipy.spatial import ConvexHull

pts = [[r.real, r.imag] for r in rs]
hpts = [pts[idx] for idx in ConvexHull(pts).vertices]
{% endhighlight %}

From the documentation, this library implements the [Quickhull](https://en.wikipedia.org/wiki/Quickhull) algorithm.

**Step 6. (Plot)** For this step, we can use Matplotlib. We first plot `r2` as a scatter plot:

{% highlight python %}
import matplotlib.pyplot as plt

fig, ax =  plt.subplots(figsize=(18, 15))

xs = []
ys = []
for r in r2:
    xs.append(pts[0])
    ys.append(pts[1])
ax.scatter(xs, ys, color='#1f77b4')
{% endhighlight %}

And then draw a polygon corresponding to the convex hull vertices `hpts`:

{% highlight python %}
import matplotlib.patches as patches
polygon = patches.Polygon(
    hpts, 
    closed=True, 
    edgecolor='black', 
    facecolor='none'
)
ax.add_patch(polygon)
{% endhighlight %}

The full code is available as a Jupyter notebook on [Github]({{github}}/Gauss-Lucas.ipynb).

## Results

*Figure 1* contains the result for for `N = 10`. I experimented with larger number of points but noticed the root finding algorithm degrated a lot, by noticing that $\abs{P(r)} \gt 0$ for some roots found by the `.roots()` method.

<figure class="center_children">
  <img src="{{resources_path}}/convex1.png" alt="See caption." />
  <figcaption>Figure 1: The blue dots correspond to the roots of the original polynomial and the orange dots correspond to the roots of its derivative. As the theory predicts, the orange dots should lie within the convex hull defined by the blue points. Source <a href="{{github}}/Gauss-Lucas.ipynb">Notebook</a></figcaption>
</figure>

The image from the top of the post consists in applying this theorem recursively.

## Conclusion

I learned about this theorem in the book *Complex Analysis* by Ahlfors, but the version presented there is a weaker one (in terms of half-planes) and it's just called the *Lucas Theorem*. The theorem itself is relatively simple but it lends itself to a nice visualization, so I wanted to write a post about it.

The proof from this post is based on Wikipedia [1] but I found non-trivial to understand it, so I added a lot more steps and commentary.

## References

* [[1](https://en.wikipedia.org/wiki/Gauss%E2%80%93Lucas_theorem)] Wikipedia: Gauss-Lucas Theorem
* [[2](https://chat.openai.com/)] ChatGPT4 