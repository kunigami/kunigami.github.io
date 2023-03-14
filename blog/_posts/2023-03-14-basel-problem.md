---
layout: post
title: "The Basel Problem"
tags: [analysis]
vanity: "2023-03-14-basel-problem"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
    <a href="https://en.wikipedia.org/wiki/Pietro_Mengoli#/media/File:Pietro_Mengoli.gif">
        <img src="{{resources_path}}/mengoli.png" alt="Pietro Mengoli thumbnail" />
    </a>
</figure>

Pietro Mengoli was an Italian mathematician and clergyman from Bologna, teaching at the University of Bologna [1]. He worked on infinite series and limits of geometric figures.

In 1650 he posed the following problem: What does the following summation converge to?

$$\sum_{n=1}^{\infty} \frac{1}{n^2}$$

It took almost 100 years for it to be solved. Euler provided a solution in 1735 which was correct but his proof lacked some formalism.

In this post we'll discuss Euler's proof to this problem and understand what parts it was missing. It's my first post in celebration of $\pi$-day.

<!--more-->

## Euler's Proof

The general idea of Euler's proof is to show that $\sin(x)$ equals to its Taylor series at 0, that is:

$$\sin(x) = x - \frac{x^3}{3!} + \frac{x^5}{5!} - \frac{x^7}{7!} \cdots$$

And that it's an *entire* function (we'll define it below) and hence we can apply the *Weierstrass Factorization theorem* to show that:

$$\frac{\sin(x)}{x} = \left( 1 - \left(\frac{x}{\pi}\right)^2 \right)\left( 1 - \left(\frac{x}{2\pi}\right)^2 \right)\left( 1 - \left(\frac{x}{3\pi}\right)^2 \right) \cdots$$

Then it shows that when we evaluate this product we'll obtain a polinomial and the coefficient of $x^2$ is given by:

$$-\left(\frac{1}{\pi^2} + \frac{1}{4\pi^2} + \frac{1}{9\pi^2} + \cdots \right) = - \frac{1}{\pi^2} \sum_{n = 1}^{\infty} \frac{1}{n^2}$$

Since the polinomials obtained from the product and the one given by Taylor series are the same, they must have the same coefficient for all variables. In particular for $x^2$:

$$\frac{1}{3!} = \frac{1}{\pi^2} \sum_{n = 1}^{\infty} \frac{1}{n^2}$$

Thus we obtain:

$$\sum_{n = 1}^{\infty} \frac{1}{n^2} = \frac{\pi^2}{6}$$

Euler's proof used the result of *Weierstrass Factorization theorem* without proving it. It would be another 100 years until Karl Weierstrass did it [2].

We now go over each of these steps in more details.

### Taylor Series

Taylor Series can be used to approximate a function $f$ at a point $a$ via the series:

$$f(x) \sim \sum_{n = 0}^{\infty} \frac{f^{(n)}(a)}{n!}(x - a)^n$$

An intuition behind this is provided in 3Blue1Brown's [Youtube video](https://www.youtube.com/watch?v=3d6DsjIBzJ4) [3]. For some functions $f$ the Taylor series converges to it at all points.

Let's compute Taylor series for $\sin x$ at point $a = 0$. We know that if $f(a) = \sin a$, then $f^{(1)}(a) = \cos(a)$ and that $f^{(2)}(a) = -\sin(a)$ and finally $f^{(3)}(a) = -\cos(a)$ and it cycles from there.

$$f(x) \sim \sin(a) + \frac{\cos(a)}{1!} (x - a) - \frac{\sin(a)}{2!} (x - a)^2 - \frac{\cos(a)}{3!} (x - a)^3 \\ + \frac{\sin(a)}{4!} (x - a)^4 + \frac{\cos(a)}{5!} (x - a)^5 \cdots$$

Evaluating at $a = 0$ we have:

$$f(x) \sim  x - \frac{x^3}{3!} + \frac{x^5}{5!} \cdots$$

It's not always true that the Taylor series at any point converges to $f$ at all points, but it's possible to show that's the case for $\sin(x)$!

### Entire Functions

Let $f$ be a function with complex domain and image. Let's simplify and assume it's a single-variable function, so $f: \mathbb{C} \rightarrow \mathbb{C}$. We can define the **complex derivarive** at a (complex) point $z_0$ the same way we do for real valued functions:

{% assign my_counter = 1 %}


$$(1) \quad f'(z_0) = \lim_{z \rightarrow z_0} \frac{f(z) - f(z_0)}{z - z_0}$$

Except that the values involved are complex numbers. A function $f$ is **complex differentiable** at $z_0$ if the limit (1) exists.

Let $U$ be an [open set]({{docs}}/math/topology.html). We say $f$ is **holomorphic** if it's complex differentiable at every point in $U$. If $U$ is the whole complex space, i.e. $U = \mathbb{C}$ in our case, then $f$ is called an **entire funcion**.

It's possible to show that a power series:

$$\sum_{n = 0}^{\infty} a_n z^n$$

that satisfies:

$$(2) \quad \lim_{n \rightarrow \infty} \abs{a_n}^{\frac{1}{n}} = 0$$

Is an entire function. The coefficients from $\sin(x)$'s Taylor series are either 0, or $\frac{1}{n!}$. *Lemma 1* (see Appendix) shows that $\lim_{n \rightarrow \infty} \abs{\frac{1}{n!}}^{\frac{1}{n}} = 0$ so (2) holds for all the coefficients as $n \rightarrow \infty$ which proves $\sin(x)$ is an entire function.

### Order of an Entire Function

Entire functions can always be expressed by their Taylor series expansion, which are power series, which we can think of as polynomials of infinite degree. This suggests we can define properties for entire functions analogous to polynomials, such as how fast they grow.

For polynomials their growth depend largely on their degree, so for example $a + bx + cx^2$ will be dominated by the $x^2$ factor. This is analogous to the big-O notation we use in computer science: if we determine our algorithm executed $a + bx + cx^2$ operations, we'd say it has $O(x^2)$ complexity.

We can define a growth scale for entire funtions as well. The problem with entire functions is that their domain is complex numbers, and complex numbers cannot be compared like reals can (e.g. which one is bigger: $i$ or $1$?), so we need to transform them into reals.

The natural way to do this is to work with their norm which is real-valued. So instead of working with a specific complex number $z$, we work with some real $r$ and consider all complex numbers whose norm is $r$, i.e. the set $C_r = \curly{c \in \mathbb{C}, \abs{c} = r}$.

So if we want to find an upperbound on how fast $f$ grows for a given $r \in \mathbb{R}$, we can take the largest value of $f$ for any $z \in C_r$, that is $\max_{\abs{z} = r} \abs{f(z)}$. This is defined as **maximum modulus function** and denoted as $M_f(r)$. Note that this is a real-valued function $M_f(r) : \mathbb{R} \rightarrow \mathbb{R}$.

Now we wish to find an upperbound for $M_f(r)$. Since $f$ is a power series of potentially infinite degree, we can't expect to say $M_f(r)$, bounded  $r^{d}$ for some degree $d$. What if we used a much bigger number, such as $e^{r^d}$, in hopes we can find a smaller $d$, possibly finite? We say that a function $f$ has **finite order** if:

$$(3) \quad M_f(r) < e^{r^d}$$

For all $r > R_0$, for some $R_0$. The **order** of a function is the smallest non-negative $d$ such that (3) holds. More formally, define $\rho(r)$ such that $M_f(r) = e^{r^{\rho(r)}}$ and also the set $P_{r > R_0} = \curly{\rho(r) \ge 0}$.

We wish to find $\rho$ such that every element in $P$ is smaller or equal to it. However $P$ is infinite and might not have a maximum element, so we need to use infimum: $\rho = \inf P$.

*Lemma 2* (see Appendix) shows that the order of a function can be also defined via:

$$(4) \quad \rho = \limsup_{r \rightarrow \infty} \frac{\ln \ln M_f(r)}{\ln r}$$

Where $\limsup_{r \rightarrow \infty}$ is called the *limit superior* and defined as:

$$\limsup_{r \rightarrow \infty} x_r = \lim_{r \rightarrow \infty} \left( \sup_{m \ge r} x_m \right)$$

We may ask, why not just $\sup \curly {x_r, r \in \mathbb{N}}$? It maybe be that first terms are big but then the terms get smaller as $r \rightarrow \infty$ so $\sup$ would be dominated by the initial terms, but we want the behavior of terms as $r \rightarrow \infty$.

We may then ask, why not just $\lim_{r \rightarrow \infty} x_r$? The series might not converge to a single value (e.g. it could oscilate). By taking the *supremum* we obtain a single number for the upper-bound.

With these definitions in place, let's compute the order for $\sin x$. *Lemma 3* (see Appendix) shows that:

$$\lim_{r \rightarrow \infty} M_{\sin}(r) = \frac{e^r}{2}$$

replacing that in (4),

$$\rho = \lim_{r \rightarrow \infty} \frac{\ln \ln \frac{e^r}{2}}{\ln r} = \lim_{r \rightarrow \infty} \frac{\ln (r  - \ln 2)}{\ln r}$$

We can apply L'Hôpital's rule to find this limit noting that $\frac{d(\ln (r  - \ln 2))}{dr} = \frac{1}{r - \ln 2}$:

$$\rho = \lim_{r \rightarrow \infty} \frac{r}{r - \ln 2} = 1$$

Thus *$\sin(x)$ has order 1*. *QED*

We can also show that a finite polynomial has order 0. Let $P(x) = \sum_{k = 0}^{n} a_i x^k$. If $\alpha = \sum_{k = 0}^{n} a_i$ then $P(x) \le \alpha x^n$ (for large enough $x$). Since $x$ is real, $M_f(r) = P(r) \le \alpha r^n$.

If we plug $r^n$ in (4), we get

$$\rho = \lim_{r \rightarrow \infty} \frac{\ln \ln \alpha r^n}{\ln r} = \lim_{r \rightarrow \infty} \frac{\ln(n \ln r + \ln \alpha)}{\ln r}$$

And using L'Hôpital's rule again we'll see this converges to:

$$\rho = \lim_{r \rightarrow \infty} \frac{\alpha}{\alpha \ln r + \ln b}$$

Since $\ln r$ is monotonically increasing and the other terms are constants, this converges to $\rho = 0$.

### Weierstrass Factorization Theorem

Going back to the analogy with polynomials, we can write a polynomial as a product of terms involving its roots. For example, for $x^2 - 5x + 4$. Its roots are $1$ and $4$, so it can be written as $(x - 1)(x - 4)$. This is known as the [fundamental theorem of algebra](https://en.wikipedia.org/wiki/Fundamental_theorem_of_algebra).

Sometimes roots have the same value, for example, $x^4 - 5x^3 + 4x^2$ has roots $1$, $4$ and twice $0$, so it can be written as $(x - 0)(x - 0)(x - 1)(x - 4)$ or $x^2 (x - 1)(x - 4)$. In this case we say root $0$ has **multiplicity** 2.

How can we build an analogous factorization for infinite power series? Let $z_n$ be the sequence of all roots of $f$. We could perphaps represent them as an infinite product of their roots:

$$f(x) \stackrel{?}{=} x^m \prod_{n = 1}^{\infty} (x - z_n)$$

Where $m$ is the multiplicity of root 0.

However the exponential function $\exp(x)$, an entire function, doesn't have zeroes, so a product only involving its roots doesn't suffice. In fact, it's possible to show $\exp(g(x))$ for some entire function $g(x)$ is also an entire function without zeros, so at minimum we need to include these factors in the product if we wish to represent exponentials:

$$f(x) \stackrel{?}{=} x^m  e^{g(x)} \prod_{n = 1}^{\infty} (x - z_n)$$

The major problem is that we need this infinite product to converge to a specific value $f(x)$ when given some $x$. Intuitively, when we multiply infinitely many small factors they will tend to 0, but if they're too big they'll not converge to a finite value.

The idea is to multiply the terms by some "attenuating" factor, encoded in what's called **Weierstrass elementary factors**, which are defined as:

$$\begin{equation}
  E_n(x) =\left\{
  \begin{array}{@{}ll@{}}
    (1 - x), & \text{if}\ n = 0 \\
    (1 - x) \exp(\sum_{i=1}^{n} \frac{x^i}{i}), & \text{otherwise}
  \end{array}\right.
\end{equation}$$

The term $\exp(\sum_{i=1}^{n} \frac{x^i}{i})$ is the $n$-th order Taylor series expansion for $-\log(1 - x)$. Note that $E_n(x): \mathbb{C} \rightarrow \mathbb{C}$. It's possible to show that if $\abs{x} \le \abs{z_n}$, then [7]:

$$(5) \quad \left\lvert E_{n}(\frac{x}{z_n}) \right\rvert \le 1 + \left(\frac{\abs{x}}{\abs{z_n}}\right)^{n + 1}$$

We then use that $\prod (1 + a_n)$ converges iff $\sum a_n$ converges, where $a_n \ge 0$, so if

$$(6) \quad \sum_{n = 1}^{\infty} \left(\frac{\abs{x}}{\abs{z_n}}\right)^{p_n + 1}$$

converges for some choice of exponents $p_n$, then

$$\prod_{n = 1}^{\infty} 1 + \left(\frac{\abs{x}}{\abs{z_n}}\right)^{p_n + 1}$$

converges and by (5):

$$(7) \quad \prod_{n = 1}^{\infty} \left\lvert E_{p_n}\left(\frac{x}{z_n}\right) \right\rvert$$

also converges. Note that this is only true if $\abs{x} \le \abs{z_n}$. However, since we have infinitely many roots $z_n$ and $\lim \abs{z_n} \rightarrow \infty$, it's possible to choose a large enought $n'$ such that $\abs{x} \le \abs{z_n}$ for $n \ge n'$, so

$$\prod_{n = n'}^{\infty} \left\lvert E_{p_n} \left( \frac{x}{z_n} \right) \right\rvert$$

coverges. For $n = 1, \dots, n'-1$ the product has finite terms so it converges, so we see (7) converges for all $n$. This is just to provide an intuition behind the *Weierstrass elementary factors*, but the **Weierstrass factoring theorem** states that there exists $p_n$, satisfying (6), and $g(x)$ we can actually represent $f(x)$ via:

$$f(x) = x^{m} e^{g(x)} \prod_{n = 1}^{\infty} E_{p_n} \left( \frac{x}{z_n} \right)$$

This theorem is not constructive: it doesn't show how to find $g(x)$ nor $p_n$, but just states that they exist. However, there's a more specific version of this theorem known as the *Hadamard factorization theorem*. It states that if $f$ has an integer order $\rho$ (it's a bit more general than this, but let's narrow it down for our purposes), we can replace $p_n$ by $\rho$ and that $g(x)$ is a polynomial of degree at most $\rho$:

$$f(x) = x^{m} e^{g(x)} \prod_{n = 1}^{\infty} E_{\rho} \left( \frac{x}{z_n} \right)$$

As an exercise, let's try this for a polynomial $P(x)$ of degree $n$, which is an entire function of order 0 as shown above. Then $g(x) = a$ for some constant $a$ and $E_0(x) = (1 - x)$, which gives us:

$$P(x) = x^{m} e^{a} \prod_{k = 1}^{n - m} \left(1 - \frac{x}{z_k}\right)$$

This is not quite the same form as the polynomial factorization, but if we allow $e^{a} = \pm \prod_{k=1}^{n - m} z_k$, then

$$P(x) = x^{m} \left(\pm \prod_{k=1}^{n - m} z_k \right) \prod_{k = 1}^{n - m} \left(\frac{z_k - x}{z_k}\right) = x^{m} \prod_{k = 1}^{n - m} (x - z_k)$$

So the Hadamard factorization theorem is a general version of the polynomial factorization. The Hadamard factorization theorem will also be used to prove:

**Theorem 1**

$$\frac{\sin(x)}{x} = \left( 1 - \left(\frac{x}{\pi}\right)^2 \right)\left( 1 - \left(\frac{x}{2\pi}\right)^2 \right)\left( 1 - \left(\frac{x}{3\pi}\right)^2 \right) \cdots$$

*Proof*: We start noting that $\sin (0) = 0$, and it's the only 0 root, so $m = 1$. Then, for $\sin(x)$, the zeros are all of the form $\pi k$ for $k \in \mathbb{Z}$. Also from above we know that $\sin(x)$ has order $\rho = 1$, so $g(x)$ is a polynomial of the form $ax + b$ and $E_1(x) = (1 - x) \exp(x)$, so we have:

$$\sin(x) = x e^{a + bx} \prod_{n \ne 0} \left(1 - \frac{x}{n \pi}\right) \exp\left(\frac{x}{n \pi}\right)$$

Note that index range $n$ is symmetric around the origin, so that for every $n$ there exist a corresponding $-n$. If we group these pairs together we obtain:

$$\sin(x) = x e^{a + bx} \prod_{n = 1}^{\infty} \left(1 - \frac{x}{n \pi}\right) \exp\left(\frac{x}{n \pi}\right) \left(1 - \frac{x}{-n \pi}\right) \exp\left(\frac{x}{-n \pi}\right) $$

The terms $\exp\left(\frac{x}{n \pi}\right)$ and $\exp\left(\frac{x}{-n \pi}\right)$ cancel each other out and if we use that $(1 - x)(1 + x) = 1 - x^2$, we can simplify to:

$$\sin(x) = x e^{a + bx} \prod_{n = 1}^{\infty} \left(1 - \left( \frac{x}{n \pi}\right) ^2\right)$$


We can find the coefficients $a$ and $b$ by using some identities of $f(x)$ [5]. First we use the fact that $\sin(x) = -\sin(-x)$. The terms $\left( \frac{x}{n \pi}\right) ^2$ are the same for $x$ and $-x$ so we can eliminate them, leaving us with:

$$x e^{bx} = -(-x e^{-bx})$$

That is,

$$e^{bx} = e^{-bx}$$

Taking the logarithm on both sides:

$$bx = -bx$$

Since this has to hold for all $x$, this implies $b = 0$. We have now that:

$$(8) \quad \sin(x) = x e^{a} \prod_{n = 1}^{\infty} \left(1 - \left( \frac{x}{n \pi}\right) ^2\right)$$

Now consider the derivative of $f(x)$ at 0, i.e. $f'(0)$. We know that if $f(x) = \sin x$, then $f'(x) = \cos x$ and $f'(0) = 1$. On the other side we can use the generalized product rule and evaluate it at 0. *Lemma 4* proves it's equal to $e^a$, so

$$\frac{d\sin}{dx}(0) = \cos(0) = 1 = e^{a}$$

So (8) becomes:

$$\sin(x) = x \prod_{n = 1}^{\infty} \left(1 - \left( \frac{x}{n \pi}\right) ^2\right)$$

Dividing by $x$ and expanding the terms in the product we get:

$$(9) \quad \frac{\sin(x)}{x} = \left( 1 - \left(\frac{x}{\pi}\right)^2 \right)\left( 1 - \left(\frac{x}{2\pi}\right)^2 \right)\left( 1 - \left(\frac{x}{3\pi}\right)^2 \right) \cdots$$

*QED.*

### Coefficient of $x^2$

To obtain the coefficient for $x^2$ we observe our product is of the form $(1 + b_1 x^2)(1 + b_2 x^2), \dots$ for constants $b_i$. If this was a finite product with $n$ terms, we could obtain a polynomial by multiplying these terms together. For example, if $n = 3$, $(1 + b_1 x^2)(1 + b_2 x^2)(1 + b_3 x^2)$, we'd have 8 terms:

$$1 + b_3 x^2 + b_2 x^2 + b_2 b_3 x^4 + b_1 x^2 + b_1 b_3 x^4 + b_1 b_2 x^4 + b_1 b_2 b_3 x^6$$

Grouping by factors:

$$1 + (b_3 + b_2 + b_1) x^2 + (b_2 b_3 + b_1 b_3 + b_1 b_2) x^4 + b_1 b_2 b_3 x^6$$

We can observe the terms with $x^2$ are those where we have exactly one coefficient $b_i$, so in general, the resulting coefficient for $x^2$ given a product in this form, is the sum of all $b$'s. In (9), $b_i = -(\frac{1}{i\pi})^2$, so the coefficient for $x^2$ is:

$$-\left(\frac{1}{\pi^2} + \frac{1}{4\pi^2} + \frac{1}{9\pi^2} + \cdots \right) = - \frac{1}{\pi^2} \sum_{n = 1}^{\infty} \frac{1}{n^2}$$

We can now refer to the high-level steps from Euler’s Proof to conclude that:

$$\sum_{n = 1}^{\infty} \frac{1}{n^2} = \frac{\pi^2}{6}$$


## Conclusion

It's mind boggling how a problem so simple to state have such a difficult solution, which not even Euler could prove to a satisfactory formal degree. This reminds me of other problems like [Fermat's conjecture](https://en.wikipedia.org/wiki/Fermat%27s_Last_Theorem).

In this post we understood the difficulty of the problem and got some intuition on entire functions and the Weierstrass factorization theorem, but didn't prove them either. Kruse's thesis [6] dives into the proof but also omits the proof of some Lemmas.

Sheydvasser's answer on Quora [5] was the most helpful to understand the application of Weierstrass factorization to $\sin(x)$.

I just finished the excelent book *Real Analysis: A Long-Form Mathematics Textbook* by Jay Cummings and after writing this post I'm very interested in learning about complex analysis.

If I get better understanding of the Weierstrass factorization theorem after studying complex analysis, I'll come back and edit this post.

## Related Posts

Complex power series is central to signal processing, so not surprisingly a lot of posts from [this topic]({{blog}}/tags/#signal%20processing) have related parts with this post.

In [Z-Transform]({{blog}}/2021/09/10/z-transform.html) we touched on the convergence of complex power-series, in particular that if $\sum_{n = 0}^{\infty} a_n$ converges, then:

$$\lim_{n \rightarrow \infty} \abs{a_n} = 0$$

Compare that with the property power series representing entire function have (2):

$$\lim_{n \rightarrow \infty} \abs{a_n}^{\frac{1}{n}} = 0$$

In [Cepstrum]({{blog}}/2021/10/23/cepstrum.html) we also used the Taylor series expansion for $\log(1 - x)$.

## Recap

* *What is the definition of entire functions?* A function that is differentiable at every point of the complex space.
* *What is the definition of the maximum modulus function?* It's the real-valued function $M_f(r)$ corresponding to the maximum norm of a complex-valued function $f(z)$ among all $\abs{z} = r$.
* *Why do we need maximum modulus functions?* So we can work with reals instead of complex numbers.
* *What is the definition of order for entire functions?* $\rho$ such that $f(r) < e^{r^\rho}$ for all $r > R_0$.
* *What is the Weierstrass Factorization theorem for polynomials?* It's the fundamental theorem of algebra.

## Appendix

**Lemma 1.**

$$\lim_{n \rightarrow \infty} \abs{\frac{1}{n!}}^{\frac{1}{n}} = 0$$

*Proof*. We first show that $n! \ge (\sqrt{n})^n$. Suppose $n$ is odd, then let $n = 2k + 1$ for $k \ge 1$. We can multiple the terms of the product in pairs, pairing the last elements with the first elements, except the term at the middle:

$$(2k)! = (2k \cdot 1)((2k-1) \cdot 2) \cdots ((k + 1) \cdot (k - 1) k$$

And

$$(\sqrt{2k})^n = (\sqrt{2k} \cdot \sqrt{2k})(\sqrt{2k} \cdot \sqrt{2k}) \cdots (\sqrt{2k} \cdot \sqrt{2k}) \sqrt{2k}$$

The $i$-th pair of the factorial is $(2k + 1 - i)i$, for $i = 1, \cdots, k$. Since $1 \le i \le k$, we have that $(1 - i) \le 0$, so multipling $i < k$ by it gives us

$$i(1 - i) < k(1 - i) \ge 2k(1 - i)$$

Expanding and moving terms around we get

$$i - i^2 + 2ki = (2k + 1 - i)i \ge 2k$$

This proves that each term in the paired product of $(2k)!$ is greater or equal to those in the paired product of $(\sqrt{2k})^n$ which are all $2k$. The unpaired term $k$ is greater or equal to $\sqrt{k}$ for $k \ge 1$. Thus we showed $n! \ge \sqrt{n}^n$ for odd $n$ (it's true for even $n$ too) so:

$$\frac{1}{n!} \le \frac{1}{(\sqrt{n})^n} $$

Then

$$\left(\frac{1}{n!}\right)^{1/n} \le \left(\frac{1}{(\sqrt{n})^n}\right)^{1/n} = \frac{1}{\sqrt{n}}$$

We know that $\lim_{n \rightarrow \infty} \frac{1}{\sqrt{n}} = 0$, so

$$\lim_{n \rightarrow \infty} \abs{\frac{1}{n!}}^{\frac{1}{n}} = 0$$

*QED.*

**Lemma 2** The order of a function can be defined as

$$\rho = \limsup_{r \rightarrow \infty} \frac{\ln \ln M_f(r)}{\ln r}$$

*Proof.* If $\rho$ is the order of $f(x)$, then by definition there exists $r \ge R_0$ such that:

$$M_f(r) = e^{r^\rho}$$

We can always assume $R_0$ is positive and thus for some $\epsilon > 0$, we have $e^{r^\rho} < e^{r^(\rho + \epsilon)}$.

Also since $\rho$ is a infimum for (3), for any $R_0$ there must be some $r > R_0$ such that

$$M_f(r) > e^{r^{(\rho - \epsilon)}}$$

if not the case we'd find a smaller infimum. So

$$e^{r^{(\rho - \epsilon)}} < M_f(r) < e^{r^{(\rho + \epsilon)}}$$

Taking the logarithm twice:

$$\ln r {(\rho - \epsilon)} < \ln \ln M_f(r) < \ln r {(\rho + \epsilon)}$$

Dividing by $\ln r$ and subtracting $\rho$:

$$-\epsilon < \frac{\ln \ln M_f(r)}{\ln r} - \rho < \epsilon$$

Which is equivalent to:

$$\left\lvert \frac{\ln \ln M_f(r)}{\ln r} - \rho \right\rvert < \epsilon$$

For any $\epsilon$ we can find *some* large enough $r$ such that the above holds. This does not necessary hold for all $r > R_0$ because $M_f(r)$ might not converge to a single value.

However, since $\rho$ is the infimum for $r > R_0$, we know that:

$$\left\lvert \left(\sup_{r > R_0} \frac{\ln \ln M_f(r)}{\ln r}\right) - \rho \right\rvert < \epsilon$$

So now we can say that is true for all $r > R_0$ and this is the definition of limit:

$$\lim_{R_0 \rightarrow \infty} \left(\sup_{r > R_0} \frac{\ln \ln M_f(r)}{\ln r}\right) = \rho$$

And this is the definition of limit superior:

$$\limsup_{r \rightarrow \infty} \frac{\ln \ln M_f(r)}{\ln r} = \rho$$

*QED.*


**Lemma 3**

$$\lim_{r \rightarrow \infty} M_{\sin}(r) = \frac{e^r}{2}$$

*Proof.*

$$\sin(x + iy) = \sin (x) \cos (iy) + \cos (x) \sin (iy)$$

Using the hyperbolic functions:

$$\sin(x + iy) = \sin (x) \cosh (y) + i \cos (x) \sinh (y)$$

The absolute for $z = x + iy$ is then

$$\abs{\sin z} = \sqrt{(\sin (x) \cosh (y))^2 + (\cos (x) \sinh (y))^2}$$

Using the identity $\cosh(x)^2 - \sinh(x)^2 = 1$:

$$= \sqrt{\sin (x)^2 (1 + \sinh (y))^2 + \cos (x)^2 \sinh (y)^2}$$

Re-arranging terms:

$$= \sqrt{\sin (x)^2 + \sinh (y)^2 (\sin (x)^2 + \cos (x)^2)}$$

Using the identity $\sin(x)^2 + \cos(x)^2 = 1$:

$$\abs{\sin z} = \sqrt{\sin (x)^2 + \sinh (y)^2}$$

Since $x \in \mathbb{R}$, $0 \ge \sin (x)^2 \ge 1$, so $\abs{\sin z} \ge \sqrt{\sinh (y)^2} = \abs{\sinh (y)}$ and $\abs{\sin z} \le \sqrt{1 + \sinh (y)^2} = \cosh(y)$.

For numbers $\abs{z} = r$ for $r > 0$, we know that $\abs{y} \le r$. Since $\cosh(x)$ is monotonic for reals $x \ge 0$, $\cosh(y) \le \cosh(r)$, thus

$$\max_{z = r} \abs{\sin z} \le \cosh(r)$$

Using the exponential definition for $\cosh(r)$:

$$\max_{z = r} \abs{\sin z} \le \frac{e^r + e^{-r}}{2}$$

On the other side, since $\abs{(0 + ir)} = r$, then

$$\max_{z = r} \abs{\sin z} \ge \abs{\sin (0 + ir)} \ge \abs{\sinh (r)}$$

Using the exponential definition for $\sinh(r)$ we arrive at:

$$\max_{z = r} \abs{\sin z} \ge \frac{e^r - e^{-r}}{2}$$

Summarizing

$$\frac{e^r - e^{-r}}{2} < \max_{z = r} \abs{\sin z} < \frac{e^r + e^{-r}}{2}$$

For $r \rightarrow \infty$, $e^{-r}$ tends to 0, so both sides converge to $\frac{e^r}{2}$ thus,

$$\lim_{r \rightarrow \infty} \max_{z = r} \abs{\sin z} = \frac{e^r}{2}$$

Replacing the definition of $M_{\sin}(r)$:

$$\lim_{r \rightarrow \infty} M_{\sin}(r) = \frac{e^r}{2}$$

*QED.*

**Lemma 4**. The derivative of

$$x e^{a} \prod_{n = 1}^{\infty} \left(1 - \left( \frac{x}{n \pi}\right) ^2\right)$$

at 0 is equal to $e^{a}$.

*Proof*: We can use the product rule. First we derive the first factor, $\frac{dx}{dx}$, which is 1 and keep the other terms:

$$e^{a} \prod_{n = 1}^{\infty} \left(1 - \left( \frac{x}{n \pi}\right) ^2\right)$$

Evaluating this at $x = 0$ yields $e^{a}$. When we derive the term $\left(1 - \left( \frac{x}{n \pi}\right) ^2\right)$ for some $n$ and keep the rest, we'll have the factor $x$, so the product will evaluate to 0.

*QED.*


## References

* [[1](https://mathshistory.st-andrews.ac.uk/Biographies/Mengoli/)] MacTutor - Pietro Mengoli
* [[2](https://en.wikipedia.org/wiki/Basel_problem)] Wikipedia - Basel problem
* [[3](https://www.youtube.com/watch?v=3d6DsjIBzJ4)] Youtube - 3Blue1Brown: Taylor series \| Chapter 11, Essence of calculus
* [[4](https://en.wikipedia.org/wiki/Weierstrass_factorization_theorem)] Weierstrass factorization theorem
* [[5](https://www.quora.com/What-is-the-Weierstrass-factorization-theorem-and-how-do-we-apply-it)] What is the Weierstrass factorization theorem and how do we apply it?
* [[6](https://scholarworks.umt.edu/cgi/viewcontent.cgi?article=9103&context=etd)] Growth of entire functions - Kruse, Richard Dean.
* [[7](https://math.stackexchange.com/questions/2098928/elementary-factors-in-the-weierstrass-factorization-theorem)] Elementary factors in the Weierstrass Factorization Theorem
