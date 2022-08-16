---
layout: post
title: "Buffon's Needle"
tags: [geometry, probability, puzzle]
excerpt_separator: <!--more-->
vanity: "2022-08-13-buffons-needle"
---

{% include blog_vars.html %}

In his book *How Not To Be Wrong*, Jordan Ellenberg discusses the Buffon's Needle as follows. Suppose we're given a needle of length 1 and we drop it in a hardwood floor, consisting of vertical slats of width 1. What is the chance it will cross slats?

[figure]

In this post we'll explore a solution to this problem and provide an algorithm to simulate it.

<!--more-->

## Mathematical Formulation

To formalize the problem, we'll assume the needle is a line segment of length 1 and the hardwood floor consists of an infinite plane with parallel lines 1 unit of distance apart. We want to compute the probability $p$ of the line segment intersecting any line.

An observation is that the segment can cross at most one line. In theory there's a possibility the needle falls perfectly horizontal but the probability of that happening is infinitesimal.

## Simulation

We can simulate a random segment drop as follows: we choose a point in the plane at random and let that be the center $c = (c_x, c_y)$ of a unit circle, where $0 \le c_x \le 1$ (the value of $c_y$ does not matter). We then pick a random point in the circumference $b = (b_x, b_y)$ of such circle.

The segment $\overline{cb}$ is formed by the points $c$ and $b$. Let $x = (c_x, 0)$ and $\theta$ the angle between $\overline{cb}$ and $\overline{cx}$ (note that $0 \le \theta \le 2 \pi$).

To determine whether it crosses a line we have to consider two cases: if the segment $\overline{cb}$ is "leaning forward", i.e. $b_x \gt c_x$, it crosses the line to the right if $b_x \ge 1$, if the segment is "leaning backward", then it crosses the line to the left if $b_x \ge 0$.

So the simulation only needs to compute $b_x$! Note that $b_x$ does not depend on $c_y$, so we just need to sample $c_x$ from a uniform distribution in $[0, 1]$ and some angle $\theta$ in $[0, 2\pi]$.

However, we just saw in the post [Random Points in Circumference]({{blog}}/2022/08/01/random-points-in-circumference.html) how to compute points in the circumference without explicit use of angles. Given random variables $X, Y$ uniformily distributed on $[-1, 1]$ and satisfying $X^2 + Y^2 \le 1$, a point in the circumference is given by:

$$
X' = \frac{X^2 - Y^2}{X^2 + Y^2}\\
Y' = \frac{2XY}{X^2 + Y^2}
$$

If we get $c_x$ from a random variable $C_X$ uniformily distributed on $[0, 1]$, $b_x$ is then

$$\frac{X^2 - Y^2}{X^2 + Y^2} + C_X$$

so we just need 3 random variables to simulate Buffon's needle!

[image]

### Computing the Probability

We can compute the probability of the segment crossing a line geometrically. Recall from [Random Points in Circumference]({{blog}}/2022/08/01/random-points-in-circumference.html) that we compute the propability of random variables $X, Y$ uniformily distributed on $[-1, 1]$ satisfying $X^2 + Y^2 \le 1$ as $\pi/4$ by the ratio of the areas of a unit circle over that of a square with size 2.

We can use a simlar strategy for this case, but since we're using 3 random variables, we need to do a ratio of volumes. The denominator of the ratio is the volume of the samples under consideration, more precisely $X^2 + Y^2 \le 1$ and $0 \le C_X \le 1$, which is a cilinder with volume $\pi$.

The numerator of the ratio is more complicated. Let's look at one slice of the cilinder for $C_X = z$, which is a circle. Consider a point $(x, y)$ in this circle, which can be writen in polar coordinates $(r, \theta)$. By definition $b_x = \cos \theta + z$.

Let's consider two cases: the segment $\overline{cb}$ is leaning forward, in which case $0 \le \theta \le \frac{\pi}{2}$ or $\frac{3\pi}{2} \le \Theta \le 2\pi$ or leaning backward, where $\frac{\pi}{2} \le \Theta \le \frac{3\pi}{2}$. Now we note there's symmetry on the $y$-axis so it suffices to look at angles $0 \le \Theta \le \pi$.

If it's leaning forward, let $\theta_f$ be the angle such that $\cos \theta_f + z = 1$. If we lean the segment more forward (i.e. towards a horizontal position), moving the angle $\theta_f$ towards 0, the crossing will still happen.

So the samples in which a crossing happens when leaning forwars is a sector from angles $0$ to $\theta_f = \cos^{-1} (1 - z)$. The area of a sector with angle $\alpha$ is $\frac{\alpha}{2}$, so the area of the sector is

$$\frac{\cos^{-1} (1 - z)}{2}$$

but accounting for the $y$-axis symmetry, we multiply it by 2:

$$\cos^{-1} (1 - z)$$

A similar argument gives us the area of the sector when leaning backwards:

$$\cos^{-1} (z)$$

The sector corresponding to samples "leaning forward" that crosses the line is shown in green in *Figure 2*, while the one corresponding to "leaning backward" is shown in red.

<figure class="center_children">
  <img src="{{resources_path}}/circle.png" alt="See caption." />
  <figcaption>Figure 1: Cross-section of the cilinder for a fixed z. The colored areas represent samples in which a line crossing happens.</figcaption>
</figure>


If we wish to compute the volume of the points corresponding to a crossing, we thus need to integrate the area of these sectors over $z$. We note that (A) and (B) are the same if we flip the direction of integration from $z = 0 \rightarrow 1$ to $z = 1 \rightarrow 0$, so for the purpose of volume calculation we can do:

$$2 \int_{z=0}^{1} \cos^{-1} (z) dz$$

Which is

$$2 \left( z \cos^{-1}(z) - \sqrt{1 - z^2} + C \rvert_{z = 0}^{1} \right) = 2$$

So the volume of the sectors is 2 and thus the probability of a segment crossing a line is $\frac{2}{\pi}$.

## Experiments

One sanity check we can perfom is to estimate $\pi$ by generating $b_x$ and counting how many times it crossed the line, and the ratio over the valid samples should approximate $\frac{2}{\pi}$.

A sample Python code follows:

{% highlight python %}
from random import uniform

N = 100000
cross, total = 0, 0
for i in range(N):
  x = uniform(-1, 1)
  y = uniform(-1, 1)
  if x*x + y*y >= 1:
    continue

  bx = (x*x - y*y)/(x*x + y*y) + uniform(0, 1)
  if bx > 1 or bx < 0:
    cross += 1
  total += 1

print(f'Estimate of PI: {2*total/cross}')
{% endhighlight %}

It is possible to optimize it a bit by reusing computation and avoiding divisions but we keep it this way for the sake of readability.

## Buffon's Noodle

There's a very elegant solution that doesn't require solving an explicit integral and it's attributed to Joseph-Émile Barbier. It relies on the linearity of expectation, that is $E[X_1 + X_2] = E[X_1] + E[X_2]$, which holds even if $X_1$ and $X_2$ are not independent.

Let's generalize a bit and suppose we are want to compute the expected number $E_S$ of crossings given a segment of length $\ell$, not necessarily of length 1 as above.

Suppose we split the segment into two equal parts. Since they're identical, independently they have the same probability of crossing a line and hence the same expected value, say $e'$.

We can thus write the expected value $E_S$ as $E_S = 2 e'$. If we keep dividing into ever smaller parts and take this to the limit, $E_S = \ell \epsilon$, where $\epsilon$ is the expected value of a piece of infinitesimal size.

The big leap is that this process works for any curve, not necessarily a segment and that's why this problem is called Buffon's *noodle*. One of such curves is the circle with diameter 1. Its circumference is $\pi$, so its expected number of crossing, say $E_C$, is given by $E_C = \pi \epsilon$. However, such a circle always crosses lines exactly twice so $E_C = 2$.

This allows us computing $\epsilon = \frac{2}{\pi}$. Since in our original problem $\ell = 1$, $E_S$ is also $\frac{2}{\pi}$ as we demonstated previously.

## Conclusion



Trivia: this is also the first post in which I use an AI generated image (Dall-e).
