---
layout: post
title: "Review: Real Analysis"
tags: [review, analysis, topology]
excerpt_separator: <!--more-->
vanity: "2023-04-21-review-real-analysis"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/real_analysis.jpg" alt="Effective C++ book cover" />
</figure>


In this post I'd like to provide my review and notes of *Real Analysis: A Long-Form Mathematics Textbook* (2nd edition) by Jay Cummings. This book covers many topics of real analysis, providing proofs for most of the results and being very generous with the steps, making it accessible. The author also provides a lot of motivation and intuition for the discussed topics.

We'll provide a list of each chapter with a summary with my notes. All definitions and theorems are nicely numbered in the book so I'll mention them in my notes for my own reference.

<!--more-->

1. TOC
{:toc}

## The Reals

This chapter introduces the set of real numbers and some basic results around them.

It introduces the concept of **field** (*Definition 1.5*) as a set satisfying some axioms, noting that the naturals nor the integers are fields but the rationals and reals are. It adds an axiom to turn a field into an **ordered field** (*Definition 1.7*) which allows concepts such as comparison, absolute value, distance function and the triangle inequality (*Theorem 1.13*), which is used a lot in proofs.

Finally it defines the completeness axiom, which if satisfied by an ordered field is called a **complete ordered field** (*Definition 1.19*). *Theorem 1.20* proves that the only such set satisfying these axioms is the set of real numbers. Naturals are complete but not a field, rationals are an ordered field but not complete.

The *Archimedean principle* (*Lemma 1.26*) states that if $a$ and $b$ are real numbers with $a > 0$, then there exists a natural number $n$ such that $na > b$. One of the most useful form of this is having $a = \epsilon$ and $b = 1$ which means there exists  $n \in \mathbb{N}$ such that $\frac{1}{b} < \epsilon$. This result is used a lot in later proofs. The proof provided for this lemma is quite nice.

## Cardinality

This chapter touches on the cardinality of sets. Two sets have same cardinality if there's a bijection between their elements (*Principle 2.1*), which is handy for comparing sets of infinite size.

When dealing with the infinite, things can get counter-intuitive, for example we can show that the naturals have the same cardinality as integers (*Exercise 2.7*) and rationals (*Theorem 2.8*). The book also provides, in the footnote, an *explicit* bijection beween the naturals and positive rationals, which I haven't seen before. It doesn't go over much detail so I'll spend some space in here because it's pretty interesting.

The function below maps natural numbers to rational ones:

$$h(n) = \prod_{i = 1}^k p_i^{(-1)^a_i \lfloor \frac{a_i + 1}{2} \rfloor}$$

Where $n \in \mathbb{N}$ and has unique prime factorization $n = p_1^{a_1} \cdots p_k^{a_k}$. For example, $12 = 2^2 \cdot 3$, so

$$h(n) = 2^{(-1)^2 \lfloor \frac{2 + 1}{2} \rfloor} \cdot 3^{(-1) \lfloor \frac{1 + 1}{2} \rfloor} = 2 \cdot 3^{-1} = \frac{2}{3}$$

We can see that the prime factor with even exponents will end up in the numerator and the ones with odd exponents in the denominator. We also note that the numerator and denominator have no prime factors in common. Since prime factorization is unique, it's possible to show $h$ is injective.

How about the other direction? Given $\frac{x}{y}$, how to get the corresponding $n$?

Consider the prime factorization of $x = q_1^{b_1} \cdots q_l^{b_l}$. We have that $b_j$ was obtained from some $\lfloor \frac{a_i + 1}{2} \rfloor$ and that $a_i$ is even (since it's part of $x$ in the numerator). Thus $a_i = 2 b_1$ satisfies these conditions. Similarly, for the prime factorization of $y = r_1^{c_1} \cdots q_m^{c_m}$ we can see that $c_j$ was obtained from $a_i = 2c_j -1$.

This lets us reconstruct $n$ from $x$ and $y$ via:

$$h^{-1}\left(\frac{x}{y}\right) = \left(\prod_{i = 1}^l q_i^{2 b_i}\right) \left(\prod_{i = 1}^m r_i^{2 c_i - 1}\right)$$

For $x = 2$ and $y = 3$ we'd have $2^2 \cdot 3 = 12$. Assuming $x$ and $y$ are co-primes, it's possible to show $h$ is surjective. We conclude $h$ is bijective. *QED.*

The most interesting discussion of this chapter is the **continuum hypothesis** (*Unprovable 2.12*), which states that there doesn't exist a set $S$ such that

$$\abs{\mathbb{N}} < \abs{S} < \abs{\mathbb{R}}$$

The book talks about the ZFC axioms, the most common ones used as foundation of mathematics. The continuum hypothesis cannot be proven using ZFC. This means that regardless whether we assume the continuum hypothesis is true or false, it doesn't create a contradition using ZFC axioms.

There's an analogy with Euclid's axioms, where the 5-th axiom known as the [parallel postulate](https://en.wikipedia.org/wiki/Parallel_postulate) is also unprovable using the other 4 [axioms](https://en.wikipedia.org/wiki/Euclidean_geometry). Euclidean geometry assumes it's true and includes as an axiom, but we might as well assume it's false, leading to [hyperbolic geometry](https://en.wikipedia.org/wiki/Hyperbolic_geometry).

This section clarified a lot of things for me!

## Sequences

This chapter focuses on the sequence of real numbers. One very neat way to define a sequence is as a function from $\mathbb{N} \rightarrow \mathbb{R}$ (*Definition 3.1*), where the natural numbers are the indices of the sequence. For example, the sequence $1, 4, 9, 25, \dots$ is the function $f(n) = n^2$.

*Definition 3.7* provides a formal definition of **convergent sequence**: A sequence $(a_n)$ converges to $a \in \mathbb{R}$ if for all $\epsilon > 0$, there exists $N$ such that $\abs{a_n} < \epsilon$ for $n > N$.

If that happens, then $a$ is called the **limit** of $(a_n)$ and is also denoted as $a = \lim_{n \rightarrow \infty} a_n$.

Conversely, **divergent sequences** as those that don't converge (*Definition 3.15*). There are 3 possible cases: their limit is $\infty$, $-\infty$ or it doesn't exist.

It also defines a **bounded sequence**, a sequence $a_n$ such that there exist $L$ and $U$ where $L \le a_n \le U$ for all $n$. It shows that all convergent sequences are bounded and that the limit of a convergent sequence is unique. The converse is not true, but *Theorem 3.37* provides a scenario in which it holds.

*Theorem 3.37* is the *Bolzano-Weierstrass theorem* which claims that every bounded sequence has a convergent subsequence. It's not obvious why, but the proof involves showing we can always find a monotonically increasing/decreasing subsequence and because the sequence is bounded, it must eventually converge.

Finally, it defines **Cauchy sequences** as those such that for all $\epsilon > 0$ there exists $N$ such that $\abs{a_m - a_n} < \epsilon$ for all $n, m > N$. It goes on to prove the Cauchy criterion for convergence (*Theorem 3.42*): A sequence converges iff it's Cauchy.

## Series

This chapter discusses the sums of terms of a sequence which is called *series*. A **series** is the sum of a sequence. For example, given a sequence $a_n$, the corresponding series is:

$$S = \sum_{i = 1}^{\infty} a_i$$

A **partial sum** (*Definition 4.1*) is when the upper bound is fixed at a finite value, say $n$:

$$s_n = \sum_{i = 1}^{n} a_i$$

Partial sums form a sequence themselves: $s_1, s_2, \dots$ and a lot of the definitions and proofs work with the partial sums instead of directly with the series. As an example, a series **converges** to a finite $L \in \mathbb{R}$ if $s_n \rightarrow L$. It diverges if $(s_n)$ diverges.

This chapter provides a few criteria for convergence. An interesting one is for the special series of the form

$$\sum_{i = 1}^{\infty} \frac{1}{i^p}$$

*Proposition 4.16*, known as the $p$-test, proves that this series converges iff $p > 1$. When $p = 1$ this series is known as the *Harmonic series*. When $p = 2$, we get the [Basel Problem](https://en.wikipedia.org/wiki/Basel_problem), which we discussed in detail in a [recent post]({{blog}}/2023/03/14/basel-problem.html).

A series **converges absolutely** (*Definition 4.20*) if the series of its absolute terms converges:

$$S = \sum_{i = 1}^{\infty} \abs{a_i}$$

If a series converges but not absolutely, we say it **converges conditionally**.

A **rearrangement of a series** $S$ (*Definition 4.21*) is another series whose terms are a permutation of the terms in $S$. More formally a series $\sum_{i = 1}^{\infty} b_i$ is a rearrangement of $\sum_{i = 1}^{\infty} a_i$ if there's a bijection $f: \mathbb{N} \rightarrow \mathbb{N}$ such that $b_{f(i)} = a_i$ for all $i$.

*Theorem 4.23*, called the *rearrangement theorem*, states that if $S$ is a series that converges conditionally, then for any $L \in \mathbb{R}$ there's a rearrangement of $S$ that converges to $L$.

This is a very surprising result to me!

## The topology of R

This touches on topology but only for when the topological space is $\mathbb{R}$. It's a relatively short chapter.

It provides several definitions such as open and closed sets, limit points, open covers and compact sets. I'm skipping details here since they're described in my [notes on Topology]({{site.url}}/docs/math/topology.html).

The *Heine-Borel theorem* (*Theorem 5.19*) is the main result in this chapter and states that a set $S \subseteq \mathbb{R}$ is compact iff $S$ is closed and bounded.

## Continuity

This chapter topic is functional continuity. It starts by defining functional limit and builds on this definition. It also talks about the intermediate value theorem.

**Functional limit** (*Definition 6.8*) can be defined as: Let $f : A \rightarrow \mathbb{R}$ and $c$ a limit point of $A$. Then

 $$\lim_{x \rightarrow c} f(x) = L$$

 if for all $\epsilon > 0$ there is $\delta > 0$ such that for all $x$ satisfying $0 < \abs{x - c} < \delta$ we have

 $$\abs{f(x) - L} < \epsilon$$

Continuity (*Definition 6.16*) can be defined in a similar fashion: a function $f : A \rightarrow \mathbb{R}$ is **continuous at a point** $c \in A$ if for all $\epsilon > 0$ there is $\delta > 0$ such that for all $x$ satisfying $0 < \abs{x - c} < \delta$ we have

$$\abs{f(x) - f(c)} < \epsilon$$

Note how we basically replaced $L$ with $f(c)$. Function $f$ is **continuous** if it is continuous at all points in its domain.

This chapter also provides examples where a function that is continuous nowhere (*Dirichlet function*), a function that is continuous at exactly one point and a function that is continuous at irrational numbers but not continuous at rational numbers (*Thomae's function*). The graph of Thomae's function is on the cover of the book.

Let's call the definition of continuity presented above the *analytical definition of continuity*. A *topological definition of continuity* is as follows: Let $f: X \rightarrow \mathbb{R}$ be a function, for every open set $B$, if $f^{-1}(B) = A \cap X$ for some set $A$, then $f$ is continuous (also in [my Topology notes]({{site.url}}/docs/math/topology.html)).

*Theorem 6.29* shows the equivalence between the analytical and the topological definition of continuous functions.

*Theorem 6.38* is the *Intermediate Value Theorem*, which says that if $f$ is continuous in $[a, b]$ then for any $\alpha$ in $[f(a), f(b)]$, there is some $c$ in $[a, b]$ such that $f(c) = \alpha$.

Finally *Definition 6.29* introduces uniform continuity: Let $f: A \rightarrow \mathbb{R}$. We say $f$ is **uniformly continuous** if for all $\epsilon > 0$ there is $\delta > 0$ such that, for all $x, y \in A$ such that $\abs{x - y} < \delta$, then $\abs{f(x) - f(y)} \le \epsilon$.

Uniform continuity is a stronger condition than continuity but if $A$ is compact, it can be shown (*Proposition 6.40*) that continuity implies uniform continuity.

## Differentiation

This chapter starts off by providing an intuition of derivative as the slope of the tangent line to a curve at a given point. It then defines derivative formally on top of this and proves well known properties such as the product and chain rules. It also connects differentiation with continuity.

*Definition 7.2* is a formal definition of differentiation: Let $I$ be an interval, $f: I \rightarrow \mathbb{R}$ and $c \in I$. Then $f$ is **differentiable at** $c$ if the limit:

$$\lim_{x \rightarrow c} \frac{f(x) - f(c)}{x - c}$$

exists, that is, it converges to a real number. This function is called the derivative of $f$ and denoted by $f': C \rightarrow \mathbb{R}$ or $\frac{d}{dx}f(x)$. The domain $C$ is the set of points in $I$ for which $f$ is differentiable.

It then shows that differentiability implies continuity (*Theorem 7.6*). It derives (no pun intended) the *Product Rule* (*Theorem 7.11*): Let $I$ be an interval and $f, g: I \rightarrow \mathbb{R}$ be differentiable at $c \in I$. Then

$$(fg)'(c) = f'(c)g(c) + f(c)g'(c)$$

Here $fg(x) = f(x) g(x)$, that is, it's the product of two functions, not to be confused with function composition. This notation confused me at first but I don't think there's a clearer way to express it. We can't say $(f(c) g(c))'$ since $f(c) g(c)$ is a real number.

*Digression*: As a programmer I find it useful to write things in code to check my understanding, specially since mathematical notation is sometimes ambiguous. When we say $f(x)$ without specifying $x$ we're actually talking about a function definition, for example in Python:

{% highlight python %}
def f(x):
    return x + 1
{% endhighlight %}

When we specify $x$ to be some constant, say $c$, then $f(c)$ is the evaluation of that function:

{% highlight python %}
c = 1.2
f(c) # 2.2
{% endhighlight %}

Suppose we have a higher-order function, `derive()` that computes the derivative of $f$. We can't compute $(fg)'(c)$ as:

{% highlight python %}
fg1 = derive(f(x) * g(x))
fg1(c)
{% endhighlight %}

Since `x` is not defined, so it has to be a lambda:

{% highlight python %}
fg1 = derive(lambda x: f(x) * g(x))
fg1(c)
{% endhighlight %}

If we overloaded the operator `*` in Python such that `f * g` returns another function `lambda x: f(x) * g(x)`, then we could compute $(fg)'(c)$ as:

{% highlight python %}
fg1 = derive(f * g)
fg1(c)
{% endhighlight %}

This "overloaded" multiplication is exactly the special notation $fg(x) = (f \cdot g)(x)$. *End of digression*.

It also derives the *Chain Rule* (*Theorem 7.13*): Let $I$ be an interval and $f, g: I \rightarrow \mathbb{R}$ be differentiable at $c \in I$. Then

$$(f \circ g)'(x) = f'(g(c)) \cdot g'(c)$$

Here $(f \circ g)$ is function composition and $\cdot$ is the multiplication of scalars, not of functions, so no surprises.

It defines local maximum and minimum and *Proposition 7.19* shows that it happens for $f(x)$ at the point $c$ where $f'(c) = 0$.

*Theorem 6.38* implies that every continuous function satisfies the intermediate value theorem (IVT), thus *Theorem 7.6* implies differentiable functions satisfy IVT. If $f$ is differentiable, we cannot say that $f'$ is either differentiable nor continuous but we can say it satisfies IVT (*Theorem 7.20, Darboux Theorem*).

*Theorem 7.21* is known as *Rolle's theorem* and states that if $f$ is continuous in $[a, b]$ and differentiable in $(a, b)$ and $f(a) = f(b)$ then there exists $c \in (a, b)$ such that $f'(c) = 0$.

Note one subtlety in the conditions: since $f$ is differentiable in $(a, b)$, it's also continuous in $(a, b)$ but not necessarily at $a$ and $b$. That's why there's an extra condition that $f$ is continuous in the closed interval $[a, b]$.

An intuition behind *Rolle's theorem* is to assume $f$ is a the position of an object as a function of time. The derivative is the speed. If the position is the same at different points in time $f(t_1) = t(t_2)$, then either the object remained stationary (so the speed was always 0) or the object moved in one direction and then returned (or vice-versa, possibly multiple times) and thus it had to stop when changing directions.

*Theorem 7.22* generalizes *Rolle's theorem* by saying that if $f$ is continuous in $[a, b]$ and differentiable in $(a, b)$ then there exists $c \in (a, b)$ such that

$$f'(c) = \frac{f(b) - f(a)}{b - a}$$

If we assume $f(b) = f(a)$ then we get *Theorem 7.21*. The proof of *Theorem 7.22* actually reduces it to *Theorem 7.21*.

This is a very interesting result. Going back to the physics analogy, this basically states that if one travels from point $a$ to point $b$ with an average speed $v$, at some point in time we'll attain that speed $v$.

*Theorem 7.26* is the *Cauchy mean value theorem* and generalizes *Theorem 7.22*, by showing that if $f, g$ are continuous in $[a, b]$ and differentiable in $(a, b)$ there exists $c \in (a, b)$ such that

$$(f(b) - f(a)) g'(c) = (g(b) - g(a)) f'(c)$$

If we have $g(x) = x$, then $g'(x) = 1$ and we get *Theorem 7.22*. This theorem is used to prove L'Hôpital's theorem (*Theorem 7.27*): Suppose $I$ is an interval containing point $a$. If $f, g: I \rightarrow \mathbb{R}$ are differentiable in $I$ (except possibly at $a$) and $g'(x) = 0$, then if either conditions holds:

* $\lim_{x \rightarrow a} f(x) = 0$ and $\lim_{x \rightarrow a} g(x) = 0$
* $\lim_{x \rightarrow a} f(x) = \infty$ and $\lim_{x \rightarrow a} g(x) = \infty$

Then

$$\lim_{x \rightarrow a} \frac{f(x)}{g(x)} = \lim_{x \rightarrow a} \frac{f'(x)}{g'(x)}$$

assuming $\lim_{x \rightarrow a} \frac{f'(x)}{g'(x)}$ exists. An interesting bit of history is that this theorem was proved by Johann Bernoulli but Guillaume de l'Hôpital bough the rights of some of Bernoulli's theorems and got the fame!

## Integration

This chapter starts with the problem of calculating the area of the circle by approximating it with polygons with ever more sides. I liked the account of how Archimedes used this idea in his proof by contradiction by showing that the area of a circle of radius $r$ cannot be larger nor smaller than $\pi r^2$.

Then it proceeds to the problem of calculating the area under a curve an approximating it as a sum of rectangles, both via upper bounds where the top of each rectangle is strictly above the curve and lower bounds where the top of each rectangle is strictly below the curve.

It defines this method as the **Darboux Integral** even though it's often mistakenly called *Riemman Integral* and that there are tons of different definitions of integral.

To formalize things, *Definition 8.2* describes a **partition** of an interval $[a, b]$ as a set of strictly increasing scalars $P = \curly{x_0, \dots, x_n}$ with $x_0 = a$ and $x_n = b$.

This partition can be used to induce the rectangles of the Darboux integral. For a given $x_i$ for $1 \le i \le n$, we have the sub-interval $[x_{i-1}, x_i]$ forming the base of the rectangle. The height depends on whether we're computing via upper or lower bounds. For upper bound the height is denoted by (*Notation 8.3*):

$$M_i = \sup \curly{f(x) : x \in [x_{i-1}, x_i]}$$

For lower bound it is

$$m_i = \inf \curly{f(x) : x \in [x_{i-1}, x_i]}$$

Where $f(x)$ is the underlying function of the curve.

For a given partition $P$ we let $U(f, P)$ be the sum of the upper bound rectangles and $L(f, P)$ the sum of the lower bound rectangles (*Definition 8.4*). Now let $\cal{P}$ be the set of all possible partitions. Then the upper integral of $f$ is defined as (*Definition 8.8*):

$$U(f) = \inf \curly{U(f, P): P \in \cal{P}}$$

And the lower integral of $f$:

$$L(f) = \sup \curly{L(f, P): P \in \cal{P}}$$

The lower bound sum is never greater than the upper bound sum (*Lemma 8.9*) so intuitively $U(f)$ and $L(f)$ form the tighest possible approximation, which is the idea behind *Definition 8.10*: A bounded function $f: [a, b] \rightarrow \mathbb{R}$ is **integrable** if $U(f) = L(f)$. In this case we can say:

$$\int_{a}^{b} f(x) dx = L(f) = U(f)$$

This is a very precise definition of when a function is integrable and is one of my favorite definitions on the book! This reminds me a lot of the criteria for optimality for combinatorial optimization problems when we're working with the primal and dual.

Note that nothing is requiring that the partitions for which we attain $U(f)$ and $L(f)$ are the same. It could be that $U(f) = U(f, P_1)$ and $L(f) = L(f, P_2)$ with $P_1 \ne P_2$.

*Theorem 8.14* provides an analytical definition for integrability and is stronger in the sense that it assumes the same partition can used for the upper and lower bounds and allows for a arbitrary $\epsilon$ instead of exact equality: A bounded function $f: [a, b] \rightarrow \mathbb{R}$ is *integrable* if and only if for all $\epsilon > 0$ there exists a partition $P_{\epsilon}$ such that:

$$U(f, P_{\epsilon}) - L(f, P_{\epsilon}) < \epsilon$$

*Theorem 8.16* then shows that continuity implies integrability.

The chapter touches on measure theory to define a more general criteria for integrability. First *Definition 8.23* introduces measure zero: A set $A$ has **measure zero** if for all $\epsilon > 0$ there exists a countable collection $I_1, I_2, \dots$ of intervals such that

$$A \subseteq \bigcup_{k = 1}^{\infty} I_k$$

and

$$\sum_{k=1}^{\infty} \cal{L}(I_k) \le \epsilon$$

Where $\cal{L}(I_k)$ denotes the length of that interval. The *Theorem 8.24* can be now stated: Let $f: [a, b] \rightarrow \mathbb{R}$ be a bounded function with $D$ the set of points at which $f$ is discontinuous. Then $f$ is *integrable* if and only if $D$ has measure zero.

*Theorem 8.32* is the *Fundamental Theorem of Calculus* which essentially says that derivative is the reverse operation of integration.

The author makes the point that this is a surprising connection because at first sight determining the slope of the tangent line to a curve and the area under a curve seem unrelated.

He also mentions that Isaac Newton's PhD advisor was one of the two people credited for the proof of this theorem. I found it surprising that he didn't explicitly mention the names of Isaac Barrow and Evangelista Torricelli, but perhaps is to avoid the precedent of naming names of less known people for every single theorem.

In formal terms the theorem consists of two parts:

(i) If $f: [a, b] \rightarrow \mathbb{R}$ is integrable, and $F: [a, b] \rightarrow \mathbb{R}$ satisfies $F'(x) = f(x)$ for all $x \in [a, b]$ (i.e. $f$ is the derivative of $F$) then

$$\int_{a}^{b} \frac{d}{dx} F(x)dx = F(b) - F(a)$$

(ii) If $g: [a, b] \rightarrow \mathbb{R}$ is integrable, let $G: [a, b] \rightarrow \mathbb{R}$ be

$$G(x) = \int_{a}^{b} g(t) dt$$

Then $G$ is continuous. Moreover, if $g$ is continuous, then $G$ is differentiable and $G'(x) = g(x)$ that is,

$$\frac{d}{dx} \int_{a}^{b} g(t)dt = g(x)$$

Part (i) says more or less that if we derive and then integrate a function $F$ we'll obtain the original function. Part (ii) says that if we integrate and then derive a function $g$ we'll obtain the original function.

*Corollary 8.34* is the *Integration by parts* and states that

$$\int_{a}^{b} f g' (t) dt  = f(b) g(b) - f(a) g(a) - \int_{a}^{b} f' g (t) dt$$

Recall that $f g' (t)$ is the product between two function as in *Theorem 7.11*. In fact this corollary can be proved by applying the FTC to *Theorem 7.11*.

*Corollary 8.35* is known as the *w-substitution* and is the analogous of the *Chain rule* (*Theorem 7.13*) for integration: let $g$ whose derivative $g'$ is continuous in $[a, b]$ and $f$ is a continuous function in $g([a, b])$. Then

$$\int_{a}^{b} (f \circ g)(x) g'(x) dx = \int_{g(a)}^{g(b)} f(w) dw$$

## Sequences and Series of Functions

This chapter discusses sequences of functions and revisits many of the concepts such as convergence, continuity, differentiation and integration but applied to sequences of functions. Finally it covers series of functions and a special case known as power series.

A **sequence of functions** $f_1, f_2, f_3 \dots$ where each function is $f_k: A \rightarrow \mathbb{R}$ for $k \in \mathbb{N}$ and denoted by $(f_k)$. Note that all functions have the same domain.

A sequence of function $(f_k)$ **converges pointwise** (*Definition 9.3*) to a function $f: A \rightarrow \mathbb{R}$ if for every $x \in A$,

$$\lim_{k \rightarrow \infty} f_k(x) = f(x)$$

The author shows that the sequence of functions of the form $f(x) = x^{k}$ for $x \in [0, 1]$ converges to the function

$$\begin{equation}
f(x)=\left\{
\begin{array}{@{}ll@{}}
    0, & \text{if}\ x \in [0 , 1) \\
    1, & \text{if}\ x = 1
\end{array}\right.
\end{equation}$$

This is also an example of a sequence of functions where each function is continuous but the function they converge to, pointwise, isn't. In other words, pointwise convergence does not necessarily preserve continuity.

A stronger type of convergence is uniform convergence: A sequence of function $(f_k)$ **converges uniformily** (*Definition 9.7*) to a function $f: A \rightarrow \mathbb{R}$ if for every $x \in A$ and every $\epsilon > 0$, there's $N$ such that for $k \ge N$,

$$\abs{f_k(x) - f(x)} < \epsilon$$

*Proposition 9.8* shows that for uniform convergence continuity is preserved.

It goes on to analyze whether other properties such as boundedness/unboundedness, uniform continuity, differentiability and integrability are preserved under pointwise convergence and uniform convergence.

*Definition 9.20* defines series of functions as $$\sum_{k = 1}^{\infty} f_k$$ and the $k$-th partial sums as $s_n = \sum_{k = 1}^{n} f_k$. A series converges pointwise / uniformily if the *sequence* $(s_n)$ does so, respectively.

**Power series** (*Definition 9.24*) is a special type of series of functions, when the functions are of the form $f_k(x) = a_k x^k$:

$$P(x) = \sum_{k = 0}^{\infty} a_k x^k$$

*Theorem 9.27* shows that the set of points for which a power series converges is either:

* $\mathbb{R}$ or
* $(-R, R)$, $[-R, R)$, $(-R, R]$ or $[-R, R]$ for some $R \in \mathbb{R}$

The book mentions the set $\curly{0}$ as a possibility explicitly, but I think this is include in the case $(-R, R)$ for $R = 0$. We could intuitively think of $\mathbb{R} = (-\infty, \infty)$, so since these are all intervals, this set  is called **interval of convergence**. $R$ is called the **radius of convergence**. If the set of convergence is $\mathbb{R}$, we assume $R = \infty$.

For in the interval of convergence, the sequence of partial sums $(s_k)$ of a power series $P(x)$ converges pointwise to $f(x)$ where

$$f(x_0) = \lim_{n \rightarrow \infty} s_n(x) = \sum_{k = 0}^{n} a_k x^k$$

So "convergence" here means implicitly *pointwise convergence*. For uniform convergence **Theorem 9.30** states that if $P(x)$ converges absolutely (*Definition 4.20*) for some $x_0 \in convergence$ then it converges uniformly on the interval $[-\abs{x_0}, \abs{x_0}]$.

If we plot $P(x)$ over $x$, we'll see that the curve is symmetric with respect of the $y$-axis, i.e. $x = 0$, so we say $P(x)$ are centered at 0. *Definition 9.34* generalizes power series to be centered at an arbitrary point $c$:

$$\sum_{k = 0}^{\infty} a_k (x - c)^k$$

This general form is needed to describe a special type of power series, the *Taylor Series* (*Definition 9.36*):

$$\sum_{k = 0}^{\infty} \frac{f^{(k)}(c)}{k!} (x - c)^k$$

Which assumes the $k$-th derivative exists. The $n$-th partial sum for a Taylor series of a function $f$ can be denoted by

$$T^{n}_{c}(f) = \sum_{k = 0}^{n} \frac{f^{(k)}(c)}{k!} (x - c)^k$$

The Taylor Series are an approximation of $f$ and the error function for the $n$-th approximation is given by

$$E_n(f) = f(x) - T^{n}_{c}(f)$$

Intuitively, if $\lim_{n \rightarrow \infty} E_n(f) = 0$, then $f$ converges to its Taylor series.

*Theorem 9.40* proves that if $f$ is infinitely differentiable, there exists $\alpha_n$ between $x$ and $c$ such that for all $n$:

$$f(x) = \left( \sum_{k = 0}^{n - 1} \frac{f^{(k)}(c)}{k!}  (x - c)^k \right) + \frac{f^{(n)}(\alpha_n)}{n!}  (x - c)^n$$

The first sum is $T^{n - 1}\_{c}(f)$. Since $E_{n-1}(f) = f(x) - T^{n - 1}_{c}(f)$, we have

$$E_{n-1}(f) = \frac{f^{(n)}(\alpha_n)}{n!}  (x - c)^n$$

This form is useful because if we can show that $f^{(n)}(\alpha_n)$ grows slower than $n!$, then $E_n \rightarrow 0$ and then $f$ converges to its Taylor series.

In the final section, the author mentions that $\sin x$, $\cos x$ and $e^{ix}$ converge to their Taylor series, and this can be used to demonstrate *Euler*'s identity:

$$e^{i\pi} + 1 = 0$$

## Related posts
{:.no_toc}

[The Basel Problem]({{blog}}/2023/03/14/basel-problem.html). I learned about the Basel problem from this book. It turns out it involves results from complex analysis not covered in this book, but it's closely related to Taylor series.


## Conclusion
{:.no_toc}

I ran into this book because one of [my side goals this year]({{blog}}/2023/01/01/2022-in-review.html) is to learn about Measure Theory. When searching for [recommendation of books](https://mathoverflow.net/questions/11591/suggestions-for-a-good-measure-theory-book) about it, many of the suggestions are for real analysis books. Since I knew nothing about real analysis, I figured I'd pick a introductory one.

Unfortunately this book doesn't really cover measure theory, except a brief mention on *Definition 8.23*, but I found it a pretty solid and gentle introduction to real analysis and feel more comfortable looking into more advanced books.

On *Chapter 7* (page 243) the author provides a footnote which I'd like to mention because it resonated with me!

> Thomas Jefferson revered Isaac Newton and once wrote to John Adams: "I have given up newspapers in exchange for Tacitus and Thucydides, for Newton and Euclid; and I find myself much happier."

I haven't paid much attention to the news for a long time (well, except Hacker news) and in the past year or so found I enjoy reading history and math/physics a lot, so I have been focusing on those. Quoting from my [2021 in Review]({{blog}}/2022/01/01/2021-in-review.html):

> I read really good books this year, especially in history and physics. I’m thinking of perhaps focusing on those two genres at the expense of other non-fictions for the next year.

And I did happen to read Thucydides' *History of the Peloponnesian Wars* [last year]({{blog}}/2023/01/01/2022-in-review.html), despite not liking that it's incomplete.
