---
layout: post
title: "[Book] Complex Analysis"
tags: [analysis]
excerpt_separator: <!--more-->
vanity: "2026-04-05-book-complex-analysis"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources}}/books/alfhors.png" alt="Book cover." />
</figure>

In this post I'll share a summary on the book *Complex Analysis* by Lars V. Ahlfors, and my journey in studying it.

I'll start with the journey because I think it's the more interesting. The second part is basically a link to all the posts I wrote from studying this book.

<br />

<!--more-->

## The Journey

I've wanted to study Complex Analysis since I read Jay Cummings' [Real Analysis: A Long-Form Mathematics Textbook](https://www.kuniga.me/blog/2023/04/21/review-real-analysis.html). The book is very approachable and I finally understood many bits I had learned from Calculus. I still recall the "wow" moment when I saw the formal definition of integral.

The opportunity came when I was visiting Taichung in Taiwan and visited a bookstore called *Mollie Used Books*. There were only a few books in English and I was baffled to find a math textbook among them, so I *had* to get it. The book was only $4 if I recall correctly.

<figure class="center_children">
    <img src="{{resources_path}}/mollie.jpg" alt="See caption"/>
    <figcaption>Mollie Used Books in Taichung, Taiwan.</figcaption>
</figure>


In retrospect, that was one of the biggest "penny wise, pound foolish" mistakes of mine. Complex analysis is a hard subject and Ahlfors is not beginner friendly.


I should have started with Tristan Needham's *Visual Complex Analysis*, but I fell prey to the sunken cost fallacy: I ended up buying Needham's book but stuck with Alfhors as the main guide. I did learn about [multi-valued functions](https://www.kuniga.me/blog/2024/12/15/multi-valued-functions.html) from *Visual Complex Analysis* because I couldn't grok Alfhors' explanations.

I had to complement my readings online with [Mathematics](https://math.stackexchange.com/) on StackExchange and scattered lecture notes. But because these notes approach subjects in different angles and order, many times it wasn't easy to make use of them to understand the book. A notable example was when studying [Zeros and Poles](https://www.kuniga.me/blog/2024/11/02/poles.html). [Terence Tao's](https://terrytao.wordpress.com/2016/10/11/math-246a-notes-4-singularities-of-holomorphic-functions/) notes was much more intuitive than the book, but he relies on a different foundation so I had to study a bunch of things such as Laurent series out of order.

As ChatGPT appeared, I started using it more and more and largely replaced other sources of complement. I was using it inefficiently though: I would still try to follow Alfhor's proofs but ask ChatGPT about a leap in argument but often times ChatGPT wouldn't be able to help because it was something very specific with the proof and my edition.

Only more recently I found that I should rely on ChatGPT for the entire proof, which might be completely different from the one in Alfhors. One major benefit of using ChatGPT is that the initial proof it provides is very high level and intuitive but skims details, which I can then dig into.

It took me roughly 2 years and a half to finish this book. I'd estimate I spend 2h on average per week working through it, so about 260 hours.

## Summary

### Chapter 1: Complex Numbers

Covers the most basic properties of complex numbers.

Posts from this chapter:

* [The Cardinality of Complex Numbers](https://www.kuniga.me/blog/2023/09/16/cardinality-of-complex.html)
* [Complex Numbers and Geometry](https://www.kuniga.me/blog/2023/10/02/complex-geometry.html)
* [The Gauss-Lucas Theorem](https://www.kuniga.me/blog/2023/11/03/gauss-lucas-theorem.html)

### Chapter 2: Complex Functions

Introduces functions of complex numbers. In particular holomorphic functions which are complex differentiable functions. Alfhors calls holomorphic functions analytic, which seems to be an outdated terminology. They have different definitions, but it can be shown that analytic functions are the same as holomorphic functions.

Posts from this chapter:

* [Holomorphic Functions](https://www.kuniga.me/blog/2023/12/21/holomorphic-functions.html)


### Chapter 3: Analytic Functions as Mappings

This chapter reviews point set topology, defines conformal maps (angle-preserving transformations) and Möbius transformations which he calls linear transformations.

Posts from this chapter:

* [Conformal Maps](https://www.kuniga.me/blog/2023/12/30/conformal-maps.html)
* [Möbius Transformation](https://www.kuniga.me/blog/2024/01/08/mobius-transformation.html)
* [Cross Ratio](https://www.kuniga.me/blog/2024/01/13/cross-ratio.html)
* [Circles of Apollonius](https://www.kuniga.me/blog/2024/01/20/circles-of-apollonius.html)
* [Symmetry Points of a Circle](https://www.kuniga.me/blog/2024/02/03/circles-symmetry.html)
* [Bipolar Coordinates and Möbius Transformations](https://www.kuniga.me/blog/2024/02/10/bipolar-coordinates.html)

### Chapter 4: Complex Integration

As the name suggests, complex integration is defined here. Most of the times we're interested in integration over a 1d-curve embedded in the 2d-complex plane (line or contour integrals). It seems common to denote contour integrals as $\oint$ but I used Alfhors notation of the simple integral symbol $\int$ with the contour being implied by the subscript path, e.g. $\int_\gamma$.

Here's where Cauchy's name start popping up everywhere. This seems the longest and most information dense chapter of the book.

Posts from this chapter:

* [Complex Integration](https://www.kuniga.me/blog/2024/04/05/complex-integration.html)
* [Path-Independent Line Integrals](https://www.kuniga.me/blog/2024/04/13/path-independent-line-integrals.html)
* [Cauchy Integral Theorem](https://www.kuniga.me/blog/2024/04/26/cachy-integral-theorem.html)
* [The Winding Number](https://www.kuniga.me/blog/2024/05/09/the-winding-number.html)
* [Cauchy's Integral Formula](https://www.kuniga.me/blog/2024/06/06/cauchy-integral-formula.html)
* [Removable Singularities](https://www.kuniga.me/blog/2024/08/31/removable-singularities.html)
* [Zeros and Poles](https://www.kuniga.me/blog/2024/11/02/poles.html)
* [The Open Mapping Theorem](https://www.kuniga.me/blog/2024/12/24/open-map.html)
* [The Maximum Principle](https://www.kuniga.me/blog/2025/01/18/max-principle.html)
* [The General Form of Cauchy's Theorem](https://www.kuniga.me/blog/2025/03/15/general-cauchy.html)
* [The Residue Theorem](https://www.kuniga.me/blog/2025/04/16/residue-theorem.html)
* [Harmonic Functions](https://www.kuniga.me/blog/2025/08/01/harmonic-functions.html)

### Chapter 5: Series and Product Development

This covers infinite series and products. Here's where a bunch of surprising connections star to appear, especially with *Weierstrass Factorization Theorem* and *The Riemann Zeta Function*. This was my favorite chapter.

Posts from this chapter:

* [Runge's Theorem](https://www.kuniga.me/blog/2025/05/31/runge-theorem.html)
* [Mittag-Leffler's Theorem](https://www.kuniga.me/blog/2025/06/17/mittag-leffler-theorem.html)
* [Holomorphic Functions are Analytic](https://www.kuniga.me/blog/2024/07/02/holomorphic-functions-are-analytic.html)
* [Weierstrass Factorization Theorem](https://www.kuniga.me/blog/2025/07/02/weierstrass-factorization-theorem.html)
* [The Gamma Function](https://www.kuniga.me/blog/2025/07/19/gamma-function.html)
* [Hadamard Factorization Theorem](https://www.kuniga.me/blog/2025/08/30/hadamard-theorem.html)
* [The Riemann Zeta Function](https://www.kuniga.me/blog/2025/10/25/riemann-zeta-function.html)


### Chapter 6: Conformal Mapping. Dirichlet's Problem

Here the book discusses the Dirichlet's Problem: how to evaluate a holomorphic function if we only know its values at the boundary of a closed curve. The conformal mapping part helps reducing the problem to simpler curves but I found it a bit tedius. Perron's method to solve a special case of Dirichlet's Problem was very interesting.

This was my least favorite chapter. Posts from this chapter:

* [Subharmonic Functions](https://www.kuniga.me/blog/2025/12/14/subharmonic-functions.html)
* [The Perron Method](https://www.kuniga.me/blog/2025/12/31/perron-method.html)

### Chapter 7: Elliptic Functions

This chapter felt a bit more digestable than the previous one, back to some elementary ideas such as periodic functions. I got a glimpse of the connection with number theory and it tempted me to study number theory in more depth.

Posts from this chapter:

* [Elliptic Functions](https://www.kuniga.me/blog/2026/01/30/elliptic-functions.html)
* [The Weierstrass ℘ Function](https://www.kuniga.me/blog/2026/04/04/the-weiertrass-p-function.html)


### Chapter 8: Global Analytic Functions

I skipped this chapter entirely. The gist seems to be that for all the results we studied it's assumed the functions are (locally) single-valued, so we often have to restrict the domain to specific branches which makes it imposible to have a single function that works for the whole domain.

This chapter discusses things like germs and sheaves that help us lift the single-valued constraint and allow defining a single function for the entire domain.

I skipped because this feels like an entire new beast with new terminology and ideas, and because I'm tired. I believe this is very important in fields like algebraic geometry, so if I ever muster the courage to study it, I'm hoping I'll learn about this properly.

## Final Thoughts

Despite the time sunk and the frustration of feeling dumb for not being able to understand the book many times, I am very appreciative of what I have learned. I think this is hard stuff and I'm proud at myself for sticking with it for so long.

It feels good being able to experience this knowledge that only few people (which I presume most are being major in math) had. It's like going on a very long hike and getting very far, seeing many beautiful things along the way, while most people only explore the beginning and miss most of them.

Would any of this be useful? I have no idea. But I do hope I now have a stronger foundation to understand more practical fields such as AI and p  hysics.
