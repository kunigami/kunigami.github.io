---
layout: post
title: "The Weierstrass ℘ Function"
tags: [analysis]
vanity: "2026-04-04-the-weiertrass-p-function"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/wp.jpeg" alt="CC BY-SA 3.0, https://commons.wikimedia.org/w/index.php?curid=1196795" />
</figure>


In our post on Elliptic functions [2] we started with the simply periodic functions such as $\sin z$. We noted that $e^{2\pi i z} / w$ is the simplest of the periodic functions and that every single simply periodic function $f(z)$ of period $w$ can be written as a function of it: $f(z) = g(e^{2\pi i z} / w)$.

Then we introduced doubly periodic functions, also known as elliptic functions. One may ask if there is, analogously, the simplest elliptic function and whether it's possible to write all elliptic functions as a function of it. The answer is yes! And this function is known as the Weierstrass ℘ function which we'll study in this post.

<!--more-->

## Fun Trivia

The symbol ℘ denoting the Weierstrass elliptic function seems to be the only unicode character that represents a very specific mathematical object.

Even symbols you might associate with specific meanings, such as π, are just Greek letters, so they're not exclusively used to represent a constant. Even glyphs like ℝ, often associated with the set of real numbers, are just conventions.

Physics has its counterpart, ℏ, denoting Planck's constant.

## The Simplest Function

In our post on elliptic functions [2], we showed that an elliptic function without poles is a constant and that an elliptic function cannot have a single simple pole. Thus the simplest interesting function is one that has order 2. In this case it either has 2 distinct poles or it has a single pole with order 2, which is also known as a double pole. We'll show that the Weierstrass $\wp$ function is an elliptic function with a double pole.

Let $f(z)$ be an elliptic function with period module $M$. We defined the Weierstrass $\wp$ function as

$$
(1) \quad \wp(z) = \frac{1}{z^2} + \sum_{w \in M, w \ne 0} \left( \frac{1}{(z - w)^2} - \frac{1}{w^2} \right)
$$

We start by showing (*Lemma 1*) that the series converges, and thus that $\wp$ exists.

**Lemma 1.** The series in $(1)$ converges uniformly on every compact set $K \subset \mathbb{C} \setminus M$.

<proof>

Since $K$ is compact, then there is a finite

$$
R = \max_{z \in K} \abs{z}
$$

Assume for now that $\abs{w} \in M \gt 2R$. We can write

$$
\frac{1}{(z - w)^2} - \frac{1}{w^2} = \frac{2wz - z^2}{w^2(z - w)^2}
$$

From $\abs{w} \gt 2R$ we have that $\abs{z} \le \abs{w} / 2$. So $\abs{z - w} = \abs{w - z} \ge \abs{w} - \abs{z} \ge \abs{w} - \abs{w} / 2 = \abs{w}/2$. We have:

$$
\abs{\frac{2wz - z^2}{w^2(z - w)^2}} = \frac{\abs{2wz - z^2}}{\abs{w}^2 \abs{(z - w)}^2}
$$

using $\abs{w - z}^2 \ge (\abs{w}/2)^2$:

$$
\le \frac{\abs{2wz - z^2}}{4 \abs{w}^4} \le \frac{\abs{2wz} + \abs{z}^2}{4 \abs{w}^4}
$$

since $\abs{z} \lt R$:

$$
\lt \frac{\abs{2wR} + \abs{R}^2}{4 \abs{w}^4} \le \frac{\abs{Cw}}{4 \abs{w}^4} = \frac{C'}{\abs{w}^3}
$$

for some constant $C'$, so we just need to show that

$$
(1.1) \quad \sum_{w \in M, w \ne 0} \frac{1}{w^3}
$$

converges. The key observation is that the number of $w \in M$ in a disk of radius $r$ grows at an $O(r^2)$ rate because they're vertices of a lattice. That is, there exists a constant $\alpha$ such that $\# \curly{w \in M : \abs{w} \le r} \le \alpha r^2$.
<br /><br />
Now consider the annulus $n \le \abs{w} \lt n + 1$. The number of points in this region, denoted by $a_n$ can be estimated as:

$$
a_n = \# \curly{w \in M : \abs{w} \lt n + 1} - \# \curly{w \in M : \abs{w} \lt n} \le \alpha (n + 1)^2 - n^2 = 2n + 1 \le \beta n
$$

So the number of $w$ in the annulus is $O(n)$. We can now tie $\abs{w}$ to $w$ count by rewriting as a sum of "rings" of integer radii:

$$
\sum_{w \in M, w \ne 0} \frac{1}{\abs{w^3}} = \sum_{n = 1}^\infty \sum_{\substack{w \in M \\ n \le w \lt n + 1}} \frac{1}{\abs{w^3}}
$$

since $n \le \abs{w} \lt n + 1$, $1 / \abs{w} \le 1 / n$:

$$
\le \sum_{n = 1}^\infty \sum_{\substack{w \in M \\ n \le w \lt n + 1}} \frac{1}{n^3}
$$

which is a convergent series, so $(1.1)$ converges absolutely and the series in $(1)$ converges uniformly by the Weierstrass M-test.

Note that we assumed $\abs{w} \gt R$. However since there are finitely many $\abs{w} \le R$, the convergence results hold.

</proof>

Now we need to show that $\wp$ is an elliptic function with a double pole. We note that the only poles of

$$
(2) \quad \frac{1}{(z - w)^2} - \frac{1}{w^2}
$$

are when $z = w$, thus the periods in $M$. So in a compact set that avoids the lattice points induced by $M$ we get that $(2)$ is holomorphic. The Weierstrass convergence theorem states that if $f_n(z)$ is holomorphic, and the series $\sum_{n} f_n(z)$ converges uniformly, then this series is also holomorphic.

So in some carefully chosen region $(2)$ is holomorphic and thus the series in $(1)$ is a meromorphic function with a double pole at $z = 0$.

**Corollary 2.** The function $\wp$ has a double pole at 0.

With that information we just need to show:

**Lemma 3.** $\wp(z) = \wp(z + \lambda)$ for any  $\lambda \in M$

<proof>

We wish to prove $\wp(z) = \wp(z + \lambda)$ for any  $\lambda \in M$. We start by replacing in the definition:

$$
\wp(z + \lambda) = \frac{1}{(z + \lambda)^2} + \sum_{w \in M, w \ne 0} \left( \frac{1}{(z + \lambda - w)^2} - \frac{1}{w^2} \right)
$$

First we split the series into two:

$$ = \frac{1}{(z + \lambda)^2} + \sum_{w \in M, w \ne 0} \frac{1}{(z - w)^2} - \sum_{w \in M, w \ne 0} \frac{1}{w^2}
$$

We can reindex the first series via a change of variable $w' = w + \lambda$ (note that by construction $w' \in M$ too):

$$ = \frac{1}{(z + \lambda)^2} + \sum_{w' \in M, w' \ne -\lambda} \frac{1}{(z - w')^2}  - \sum_{w \in M, w \ne 0} \frac{1}{w^2}
$$

Consider the first series. It covers almost the exact same elements as the one from before the change of variable, except that the new one includes $w = 0$ and excludes $w = -\lambda$, so we can equate the series via:

$$
\sum_{w' \in M, w' \ne -\lambda} \frac{1}{(z - w')^2} = \sum_{w \in M, w \ne 0} \left( \frac{1}{(z - w)^2} \right) - \frac{1}{(z + \lambda)^2} + \frac{1}{z^2}
$$

Let $P(z)$ be the series from $\wp(z)$, so that we have:

$$
\wp(z) = \frac{1}{z^2} + P(z)
$$

and we've shown that

$$
\wp(z + \lambda) = \frac{1}{(z + \lambda)^2} + P(z) - \frac{1}{(z + \lambda)^2} + \frac{1}{z^2}
$$

cancelling factors:

$$
\wp(z + \lambda) = P(z) + \frac{1}{z^2} = \wp(z)
$$

So $\wp$ is a function with period module $M$.
</proof>

to show $\wp$ is elliptic.

So we've defined the Weierstrass function and have shown it's elliptic with a double pole at 0. Now we consider some other properties.

## Properties

Because $\wp$ has only one double pole at 0, we can write its [Laurent Series](https://www.kuniga.me/blog/2024/11/02/poles.html) expansion as:

$$
(3) \quad \wp(z) = c_{-2} z^{-2} + \sum_{n = 0}^\infty c_nz^n
$$

We can then use *Lemma 4* to show that $\wp$ is even:

**Lemma 4.** Let $f(z)$ be a function with the Laurent expansion as in $(3)$. Then $f(z) = f(-z)$.

<proof>
Let $g(z) = f(z) - f(-z)$. Since $z^{k} = (-z)^{k}$ for even $k$ the corresponding terms in the Laurent series expansion cancel out leaving us with only the odd terms:

$$
g(z) = 2 c_1 z + 2 c_3 z^3 + 2 c_5 z^5 \dots
$$

This means $g(z)$ has no poles and is thus holomorphic around 0, in fact $g(0) = 0$ by replacing it above. $g(z)$ also has the same period $w \in M$ as $f(z)$ because $g(z + w) = f(z + w) - f(-z - w) = f(z) - f(-z)$. The only other poles $g(z)$ might have are for $w \in M$ because these are the poles for $f(z)$ (since $1/f(0) = 1/f(w) = 0$), but since $g(w) = g(0)$ we conclude $g$ is an elliptic function without poles which means $g$ is constant and thus 0. This means that $f(z) = f(-z)$ everywhere.

</proof>

Another conclusion from the Laurent series is that $\wp$ has residue 0 [4], because by definition it's the coefficient $c_{-1}$ of the Laurent series.

**Corollary 5.** The function $\wp$ has residue 0.

We now explore the anti-derivative of the Weierstrass function.

## Weierstrass Zeta Function

Recall that the anti-derivative $g$ of a function $f$ is one such that $g' = f$. For the Weierstrass function, its anti-derivative is known as the *Weierstrass Zeta Function* and denoted by $\zeta$, but conventionally we work with the negative of the anti-derivative, so $\zeta'(z) = -\wp(z)$.

We can compute it from $(1)$ by integrating over the terms of the series, as shown in *Lemma 6.*

**Lemma 6.** The Weierstrass Zeta Function can be written as

$$
(4) \quad \zeta(z) = \frac{1}{z} + \sum_{w \in M, w \ne 0} \left(\frac{1}{z-w} + \frac{1}{w} + \frac{z}{w^2}\right)
$$

<proof>
We integrate each term of $(1)$ individually. The integral of $1/z^2$ is $-1/z$ (plus a constant $C_1$). The integral of $-1/{(z-w)}^2$ is $1/(z - w)$ and of $-1/w^2$ is $-z/w^2$ (plus a constant $C_2$). So if we negate the equation, we have

$$
\frac{1}{z} + \sum_{w \in M, w \ne 0} \left(\frac{1}{z-w} + \frac{z}{w^2} + C_2 \right) + C_1
$$

We can normalize $C_1$ to 0, but we can't do that for $C_2$ because in that case the series would not converge. To see why, consider the Taylor expansion of $1/(z-w)$ around 0 (which is holomorphic since $w \ne 0$):

$$
\frac{1}{z - w} = -\frac{1}{w} - \frac{z}{w^2} - \frac{z^2}{w^3} - \cdots
$$

If we set $C_2 = 0$, then the term inside the series becomes

$$
\frac{1}{z - w} + \frac{z}{w^2} = - \frac{1}{w} - \frac{z^2}{w^3} - \cdots
$$

which is dominated by $1/w$ and the series becomes the non-convergent harmonic series. If we set $C_2 = 1/w$, then the term inside the series becomes

$$
\frac{1}{z - w} + \frac{1}{w} + \frac{z}{w^2} = - \frac{z^2}{w^3} - \cdots = O(1/w^3)
$$

and the corresponding series now converges. This proves the result we are interested in.

</proof>


The Laurent series of $\zeta$ around 0 has the principal part $1/z$ and the rest, $h(z)$, is a holomorphic function:

$$
\zeta(z) = \frac{1}{z} + h(z)
$$

this means this function has residue 1 around 0. *Lemma 7.* generalizes this for all other periods in $M$.

**Lemma 7.** The function $\zeta$ has residue 1 around $w \in M$.

<proof>
We can rewrite $(4)$ as:

$$
\zeta(z) = \frac{1}{z} + \left(\frac{1}{z-\lambda} + \frac{1}{\lambda} + \frac{z}{\lambda^2}\right) + \sum_{w \in M, w \ne 0, w \ne \lambda} \left(\frac{1}{z-w} + \frac{1}{w} + \frac{z}{w^2}\right)
$$

so around $\lambda$ the only pole is $1/(z - \lambda)$. This gives us the residue 1 around any $\lambda \in M$.
</proof>

From *Lemma 40* in [2], which claims that the sum of residues of an elliptic function is 0, this implies that $\zeta$ is not a periodic function. It is however, as *Lemma 8* shows, a *quasi-periodic* function. A **quasi-periodic** function $f$ is one for which $f(z) = f(z + w) + \eta_w$ for some constant $\eta_w \in \mathbb{C}$ dependent on $w$.

Note that a quasi-periodic function is a weaker version of periodic functions, since the latter are quasi-periodic functions with $\eta_w = 0$.

**Lemma 8.** The function $\zeta$ is quasi-periodic.

<proof>

Let

$$
g_\lambda(z) = \zeta(z + \lambda) - \zeta(z)
$$

for some $\lambda \in M$. Differentiating:

$$
g_\lambda'(z) = -\wp(z + \lambda) + \wp(z)
$$

Since $\wp$ is periodic on $\lambda$, $g_\lambda'(z) = 0$. For $z \not \in M$, we have that $\zeta(z)$ and $\zeta(z + \lambda)$ have no poles and thus $g_\lambda(z)$ is holomorphic, and then $g_\lambda'(z) = 0$ implies $g_\lambda(z)$ is a constant.
<br /><br />
We wish to show that $g_\lambda$ is holomorphic even around $z \in M$. We've seen in the proof of <i>Lemma 7</i> that for any $a \in M$ we have:

$$
(8.1) \quad \zeta(z) = \frac{1}{z-a} + h(z)
$$

for some holomorphic function $h(z)$. Since $a + \lambda$ is also in $M$, we can also write:

$$
\zeta(z) = \frac{1}{z-(a + \lambda)} + k(z)
$$

for another holomorphic function $k(z)$. Doing this one for $z + \lambda$:

$$
(8.2) \quad  \zeta(z + \lambda) = \frac{1}{(z + \lambda) - (a + \lambda)} + k(z + \lambda) = \frac{1}{z - a} + k(z + \lambda)
$$

so both $(8.1)$ and $(8.2)$ have a single pole when $z$ is around $a$. If we subtract one from another:

$$
g_\lambda(z) = \zeta(z + \lambda) - \zeta(z) = k(z + \lambda) - h(z)
$$

we are left with a holomorphic function, even around points in $M$. Thus $g_\lambda(z)$ is entire and thus must be a constant. We then have

$$
\zeta(z + \lambda) = \zeta(z) + \eta_\lambda
$$

which is the definition of a quasi-periodic function.
</proof>

There's a nice identity between the base of the period module and the corresponding quasi-period constants for the zeta function, known as **Legendre's Relation**. *Lemma 9* has more details:

**Lemma 9.** Let $(w_1, w_2)$ be a base for the period module $M$. Let $\eta_1, \eta_2$ be the constants in $\zeta(z + w_1) = \zeta(z) + \eta_1$ and $\zeta(z + w_2) = \zeta(z) + \eta_2$. Then:

$$
w_1 \eta_2 + w_2 \eta_1 = 2 \pi i
$$

<proof>

Let $\delta P$ be the boundary of the parallelogram of the lattice induced by $(w_1, w_2)$ at the origin, that is with vertices $0, w_1, w_1 + w_2, w_2$. Let $\gamma$ be $\delta P$ shifted by a small amount $\epsilon \lt 0$, so that $0$ is inside this closed curve, that is, $\epsilon, w_1 + \epsilon, w_1 + w_2 + \epsilon, w_2 + \epsilon$.
<br /><br />
To prove the lemma we'll compute

$$\int_{\gamma} \zeta(z) dz$$

with two different methods to obtain an equality. First we compute using residues. Since we know the residue of $\zeta(z)$ is 1 with a pole at 0, by Cauchy's residue theorem we have:

$$
\frac{1}{2 \pi i} \int_{\gamma} \zeta(z) dz = \sum_{j=1}^{n-1} \eta(\gamma, a_j) \mbox{Res}_{z=a_j} \zeta(z) = 1
$$

so our first method gives:

$$
\int_{\gamma} \zeta(z) dz = 2 \pi i
$$

The other method is by integrating it via paths. The boundary $\gamma$ can be decomposed into 4 segments: $\epsilon$ to $w_1 + \epsilon$, $w_1 + \epsilon$ to $w_1 + w_2 + \epsilon$, and so forth. Let's consider the first segment, $\epsilon \rightarrow w_1 + \epsilon$:

$$
I_1 = \int_{\epsilon}^{w_1 + \epsilon} \zeta(z) dz
$$

now we do the second $w_1 + \epsilon \rightarrow w_1 + w_2 + \epsilon$:

$$
I_2 = \int_{w_1 + \epsilon}^{w_1 + w_2 + \epsilon} \zeta(z) dz
$$

We do a change of variable $z = w + w_1$ to get:

$$
I_2 = \int_{\epsilon}^{w_2 + \epsilon} \zeta(w + w_1) dw
$$

Using the quasi-periodicity: $\zeta(w + w_1) = \zeta(w) + \eta_1$ so

$$
I_2 = \int_{\epsilon}^{w_2 + \epsilon} \zeta(w) dw + w_2 \eta_1
$$

the third segment gives us:

$$
I_3 = \int_{w_1 + w_2 + \epsilon}^{w_2 + \epsilon} \zeta(z) dz
$$

Now we use $z = w + w_2$ to get:

$$
I_3 = \int_{w_1 + \epsilon}^{\epsilon} \zeta(z) dz + w_1 \eta_2
$$

finally the fourth segment is

$$
I_4 = \int_{w_1 + \epsilon}^{\epsilon} \zeta(z) dz
$$

Adding together, the remaining integral of $I_2$ and $I_3$ cancel with $I_4$ and $I_1$, and what remains is $w_1 \eta_2 + w_2 \eta_1$ which proves the lemma.

</proof>

**Lemma 10.** The function $\zeta(z)$ is odd.

<proof>
This is similar to the proof to <i>Lemma 4</i>. We define $g(z) = \zeta(z) + \zeta(-z)$, then around 0 the principal part $1/z$ will cancel out making $g(z)$ a holomorphic function, so for $z \rightarrow 0$, $g(z) = 0$.
<br /><br />
We now use the fact that $\wp$ is even, and we have:

$$
(10.1) \quad \zeta'(-z) = -\wp(-z) = -\wp(z) = \zeta'(z)
$$

Differentiate $g(z)$:

$$
g'(z) = \zeta'(z) - \zeta'(-z)
$$

from $(10.1)$ we get $g'(z) = 0$ and hence $g(z)$ is constant. Since $g(z) = 0$ for $z \rightarrow 0$, $g(z) = 0$ everywhere and thus $\zeta(z) = -\zeta(-z)$.

</proof>


## Weierstrass Sigma Function

Another important function related to $\wp$ is *Weierstrass sigma function*, denoted by $\sigma$ and defined as a function of which $\zeta(z)$ is the log derivative of. More precisely, we have that:

$$
\frac{\sigma'(z)}{\sigma(z)} = \frac{d \ln \sigma(z)}{dz} = \zeta(z)
$$

This function has a specific product form, as shown in *Lemma 11*.

**Lemma 11.** The Weierstrass sigma function can be expressed as

$$
(5) \quad \sigma(z) = z \prod_{w \in M, w \ne 0} \left(1 - \frac{z}{w}\right) \exp \left(\frac{z}{w} + \frac{z^2}{2w^2}\right)
$$

<proof>
We just need to take the log of $(5)$ and differentiate, and see if we obtain $(4)$. For now we assume $z \not \in M$, so we can take the log to obtain:

$$
(11.1) \quad \ln \sigma(z) = \ln z + \sum_{w \in M, w \ne 0} \left( \ln \left(1 - \frac{z}{w} \right) + \frac{z}{w} +  \frac{z^2}{2w^2} \right)
$$

both $\ln z$ and $\ln (1 - z/w)$ exist since $z \not \in M$ and we can choose a suitable branch to make it a single-valued function. We can show the series converges in $(11.1)$ in which case we can differentiate term by term, to obtain:

$$
\frac{d\ln \sigma(z)}{dz} = \frac{1}{z} + \sum_{w \in M, w \ne 0} \left( \frac{d}{dz} \ln \left(1 - \frac{z}{w} \right) + \frac{1}{w} +  \frac{z}{w^2} \right)
$$

with

$$
\frac{d}{dz} \ln \left(1 - \frac{z}{w} \right) = - \frac{1/w}{1 - z/w} = \frac{1}{z - w}
$$

which gives us:

$$
\frac{d\ln \sigma(z)}{dz} = \frac{1}{z} + \sum_{w \in M, w \ne 0} \left(  \frac{1}{z - w} + \frac{1}{w} +  \frac{z}{w^2} \right)
$$

which is exactly $\zeta(z)$. It remains to show this holds for some $\lambda \in M$. Since $\lambda$ is a zero of $\sigma(z)$,  near $\lambda$, we have

$$
\sigma(z) = (z - \lambda) g(z)
$$

where $g(z)$ is non-zero and holomorphic. Taking the derivative:

$$
\sigma'(z) = g(z) + (z - \lambda) g'(z)
$$

dividing:

$$
\frac{\sigma'(z)}{\sigma(z)} = \frac{1}{z - \lambda} + \frac{g'(z)}{g(z)}
$$

compare that with the shape of $\zeta$ near $\lambda$

$$
\zeta(z) = \frac{1}{z - \lambda} + h(z)
$$

for a holomorphic function $h(z)$. If we take the difference between $\zeta(z)$ and $\frac{\sigma'(z)}{\sigma(z)}$, say $\delta(z)$, the poles cancel and we end up with a holomorphic function

$$
\delta(z) = h(z) + \frac{g'(z)}{g(z)}
$$

even for $z \in M$. We have $\delta(z) = 0$ for $z \not \in M$, since $(\lambda  - z) h(z)$, this is a removable singularity and we can extend $h(z) = 0$ to it, which gives us $\delta(z) = \sigma'(z) / \sigma(z)$ everywhere.

</proof>

*Quasi-periodicity* doesn't have to be additive, it can also be multiplicative. In the sigma function case it's possible to show

$$\sigma(z + w) = -\exp(\eta_w (z + w) / 2) \sigma(z)$$

## The derivative of $\wp(z)$

The derivative $\wp'(z)$ can be expressed as:

**Lemma 12.**

$$
\wp'(z) = -2 \sum_{w \in M} \frac{1}{(z - w)^3}
$$

<proof>
We differentiate $(1)$ term-by-term to obtain:

$$
\wp'(z) = -2 \frac{1}{z^3} + \sum_{w \in M, w \ne 0} \left(-2 \frac{1}{(z - w)^3}\right)
$$

The term $1/w^2$ disappears because it's constant wrt $z$. Now we don't have to handle $w = 0$ separately and can move it to the sum:

$$
= -2 \sum_{w \in M} \frac{1}{(z - w)^3}
$$

</proof>

From this expression we can see the poles of $\wp'(z)$ are the same as $\wp(z)$, i.e. the periods in $M$. Also, by plugging $z$ and $-z$ in this formula, we can conclude that

**Corollary 13.** The function $\wp'(z)$ is odd.

By *Lemma 3* in [2] which says that elliptic functions are closed under derivation, we conclude that:

**Corollary 14.**  The function $\wp'(z)$ is elliptic with the same period module $M$.

We now characterize the zeros of $\wp'(z)$, via *Lemma 15*:

**Lemma 15.** Let $a \not \in M$. Then $\wp'(a) = 0$ if and only if $2a \in M$.

<proof>
First we assume $\wp'(a) = 0$ and we want to show $2a \in M$. We define the function $g(z) = \wp(z) - \wp(a)$. Since we're assuming $a \not \in M$, $\wp(a)$ is defined and thus $g(a) = 0$. Differentiating $g(z)$ gives us $g'(z) = \wp'(z)$. Since $\wp'(a) = 0$ by hypothesis, $g'(a) = 0$. We conclude that $a$ is a zero of $g(z)$ with multiplicity at least 2.
<br /><br />
Since $\wp(a)$ is constant, $g(z)$ has the same poles as $\wp(z)$ which is the double pole at $0$ in the fundamental parallelogram, which by <i>Lemma 41</i> in [2], means there are 2 zeros in this region. This means all zeros are the double zeroes from above.
<br /><br />
We have that $g(z)$ is even:
$$
g(-z) = \wp(-z) - \wp(a) =  \wp(z) - \wp(a) = g(z)
$$
Thus if $a$ is a 0 of $g(z)$, so is $-a$. We claim then that $a$ and $-a$ are the same point, modulo some parallelogram from the lattice. Otherwise some translation of $-a$ would correspond to a distinct point in the parallelogram of $a$ and that would give us at least 3 zeros, so $a \equiv -a \pmod M$ which implies $2a \in M$.
<br /><br />
Now suppose $2a \in M$. We wish to prove $\wp'(a) = 0$. The hypothesis gives us $a \equiv -a \pmod M$. Since $\wp'$ has period module $M$, then $\wp'(-a) = \wp'(a)$. Since $\wp'(z)$ is odd, $-\wp'(a) = \wp'(a)$ adding these up $0 = 2\wp'(a)$ so $\wp'(a) = 0$.
</proof>

## Elliptic Functions

We're now ready to show that every elliptical function can be written from $\wp(z)$ and $\wp'(z)$. We handle even functions and odd functions separately, which suffices since any function can be decomposed into even and odd functions [6].

**Lemma 16.** Let $f(z)$ be an even elliptic function. Then there exists a rational function $R$, such that:

$$
f(z) = R(\wp(z))
$$

<proof>

The key idea is to, for each pole $a$ of $f(z)$, come up with an expression $Q_a(\wp(z))$ involving $\wp(z)$ to "cancel out" the principal parts of $f(z)$. We sum all such expressions into one $Q(\wp(z))$. Then we subtract $f(z)$ from each $Q_a$ which will leave us with an elliptical function without poles, which must be a constant $C$ [2], so we can write

$$
f(z) = Q(\wp(z)) + C
$$

In the remainder of the proof we'll show what $Q_a$ looks like. We'll have to handle 3 types of poles separately:

<ol>
<li>$a \in M$</li>
<li>$a \not \in M$, $2a \in M$</li>
<li>$a \not \in M$, $2a \not \in M$</li>
</ol>

<br /><br />


We first handle the case where $a \in M$. Since $0 \equiv a \pmod M$, that is $f(a) = f(0)$, it suffices to analyze the case where $a = 0$. Consider the Laurent expansion around $0$:

$$
f(z) = \sum_{n = -m}^{\infty} c_n z^n
$$

For $-z$ we have:

$$
f(-z) = \sum_{n = -m}^{\infty} (-1)^n c_n z^n
$$

Since $f(z) = f(-z)$ the odd coefficients $k$ will match as $(-1)^k c_k z^k = c_k z^k$, which implies $c_k = 0$, leaving us with:

$$
f(z) = \sum_{n = -m \\ n \mbox{ is even}}^{\infty} c_n z^n
$$

Thus the principal part of $f(z)$ around 0 is:

$$
\frac{c_{-2}}{z^2} + \frac{c_{-4}}{z^4} + \cdots + \frac{c_{-m}}{z^m}
$$

We recall that $\wp(z)$ around 0 has the principal part $z^{-2}$. When we exponentiate it, $\wp(z)^k$ we get principal parts containing $z^{-2k}$. Notice however that we also get powers like $z^{-2k-2}$, $z^{-2k-4}$, etc.
<br /><br />
We want to construct a polynomial in $\wp(z)$ to match the principal part of $f(z)$ is: first we add the term $c_{-m} \wp(z)^{m/2}$. Next we add the term $\beta_{m-1} \wp(z)^{(m-2)/2}$ where $\beta_{m-2}$ needs to add up to $c_{m-2}$ + any factor $c_{-m} \wp(z)^{m/2}$ added to $z^{(m-2)/2}$. So the polynomial:

$$
Q_0(z) = \beta_{2} \wp(z) + \beta_{4} \wp(z)^2 + \cdots + \beta_{m} \wp(z)^{m/2}
$$

matches exactly the principal part of $f(z)$ around $0$. So if we do $f(z) - Q_0(z)$ the resulting function is holomorphic at $0$.

<br /><br />

Now consider the case in which $a \not \in M$, $2a \in M$ or that $a \equiv -a \pmod M$. Define $g(z) = \wp(z) - \wp(a)$. Since $a$ is not a pole of $\wp(a)$, $g(a) = 0$. We've also shown that $\wp'(a) = 0$, so $g'(a) = \wp'(a) = 0$. This means $g(z)$ has a double zero at $a$ and thus

$$
\frac{1}{\wp(z) - \wp(a)}
$$

has a double pole at $a$. For $a \not \in M$, $\wp(z)$ is holomorphic, so consider the Taylor expansion around $a$:

$$
\wp(z) = \wp(a) + \wp'(a)(z - a) + \frac{\wp''}{2} (z - a)^2 + \cdots
$$

because $a$ is a double zero of $\wp(z)$, $\wp'(a) = 0$ and $\wp''(a) \ne 0$:

$$
\wp(z) - \wp(a) = \frac{\wp''(a)}{2} (z - a)^2 + \cdots
$$

Finally because $a \equiv -a \pmod M$ (i.e. they're the same point relative to their parallelogram) and $\wp(z)$ is an even function its Taylor expansion around $a$ can only have coefficients of even index, so:

$$
\wp(z) - \wp(a) = \frac{\wp''(a)}{2} (z - a)^2 + \frac{\wp^{(4)}(a)}{4!} (z - a)^4 + \cdots
$$

So $\wp(z) - \wp(a)$ is a polynomial on $(z - a)$ with even powers. We can use <i>Lemma 19</i> to show that near $a$, we have:

$$
\frac{1}{\wp(z) - \wp(a)} = \frac{c'_1}{(z - a)^2} + c'_2 + c'_3 (z - a)^2 + c'_4 (z - a)^4 + \cdots
$$

So we have a series where the only principal part is $(z - a)^2$, so we can use the same scheme we did for the case where $a = 0$ to "cancel out" the principal parts of $f(z)$ near $a$.
<br /><br />
Finally consider the case $a \not \in M$ and $2a \not \in M$. This is the most complicated case because now $a$ and $-a$ are not the same point so we need to consider the "shape" of $f(z)$ at both $a$ and $-a$. The Laurent expansion of $f(z)$ around $a$:

$$
f(z) = \sum_{n = -m}^{\infty} c_n (z - a)^n
$$

Now use the fact that $f(z)$ is even. Consider some $z$ around $-a$, say $z = -a + \epsilon$. Then $f(-a + \epsilon) = f(a - \epsilon)$. Since $a - \epsilon$ is a point around $a$ we have:

$$
f(-a + \epsilon) = f(a - \epsilon) = \sum_{n = -m}^{\infty} c_n (-1)^n \epsilon^n
$$

Replacing $\epsilon = z + a$ back, gives us:

$$
f(-a + \epsilon) = \sum_{n = -m}^{\infty} c_n (-1)^n (z + a)^n
$$

So we have, around $-a$:

$$
f(z) = \sum_{n = -m}^{\infty} c_n (-1)^n (z + a)^n
$$

Which tells us that while the functions for $a$ and $-a$ when they're distinct points are not exactly the same as in the second case, there's this constraint that their coefficients on the Laurent series match up to a negative sign. In particular their principal part have the same coefficients. This is important because we now need to come up with a single function $Q_a(\wp(z))$ that cancels out the principal parts at both $a$ and $-a$.
<br /><br />
It turns out this function is still a linear combination of $1/(\wp(z) \pm \wp(a))$ as in the second case, but we need to show why. Since $2a \not \in M$, we have from <i>Lemma 15</i> that $\wp'(a) \ne 0$. Since $a$ is not a pole of $\wp$, consider its Taylor expansion around $a$:

$$
\wp(z) = \wp(a) + \wp'(a)(z - a) + \frac{\wp''(a)}{2!}(z - a)^2 + \cdots
$$

so

$$
\wp(z) - \wp(a) = \wp'(a)(z - a) + \frac{\wp'(a)}{2!}(z - a)^2 + \cdots
$$

From <i>Lemma 20</i> we have that the inverse around $a$ has the series:

$$
\frac{1}{\wp(z) - \wp(a)} = \frac{c'_1}{z - a} + c'_2 + c'_3 (z - a) + c'_4 (z - a)^2 + \cdots
$$

with $c'_1 = 1 / \wp'(a)$. We only care about the principal part of this series, so we hide the rest behind a holomorphic function:

$$
\frac{1}{\wp(z) - \wp(a)} = \frac{c'_1}{z + a} + h_a(z + a)
$$

Now consider the Taylor expansion of $\wp(z)$ around $-a$:

$$
\wp(z) = \wp(-a) + \wp'(a)(z + a) + \frac{\wp''(a)}{2!}(z + a)^2 + \cdots
$$

Using that $\wp(a) = \wp(-a)$ and $\wp''(a) = -\wp''(-a)$ we obtain:

$$
\wp(z) - \wp(a) = (-\wp'(a))(z + a) + \frac{\wp''(-a)}{2!}(z + a)^2 + \cdots
$$

Inverting and using <i>Lemma 20</i> gives us:

$$
\frac{1}{\wp(z) - \wp(a)} = \frac{c''_1}{z + a} + c''_2 + c''_3 (z + a) + c''_4 (z + a)^2 + \cdots
$$

with $c''_1 = -1 / \wp''(a)$. So this gives us $c'_1 = -c''_1$. Hiding the non-principal part behind a holomorphic function:

$$
\frac{1}{\wp(z) - \wp(a)} = -\frac{c'_1}{z + a} + h_{-a}(z + a)
$$

This lets us see things more clearly. If we define

$$
P_a(z) = \frac{1}{\wp(z) - \wp(a)}
$$

then we can use the construct from the first case to eliminate the poles of $f(z)$ both at $a$ and $-a$ with the same linear combination $Q_a(z)$. The beauty of it is that because the coefficients of the principal part of $f(a)$ and $f(-a)$ are the same but opposite for odd powers, exponentiating $P_a(z)$ for odd parts will also yield opposite signs for factor $1/(z - a)$.

</proof>

Recall that a rational function is of the form $R(z) = P(z)/Q(z)$ where $P$ and $Q$ are polynomials of $z$ and $Q(z) \ne 0$. Now for odd functions:

**Lemma 17.** Let $f(z)$ be an odd function. Then there exists a rational function $R(z)$ such that $f(z) = \wp'(z) R(\wp(z))$.

<proof>
We define:

$$
h(z) = \frac{f(z)}{\wp'(z)}
$$

We have that $h(z)$ is an even function because $h(-z)$ is:

$$
h(-z) = \frac{f(-z)}{\wp'(-z)} = \frac{-f(z)}{-\wp'(z)} =  \frac{f(z)}{\wp'(z)} = h(z)
$$

and $f(z)$ and $\wp'(z)$ are odd functions. Further since $f(z)$ and $\wp'(z)$ are elliptic, so is $h(z)$. We need to be careful about when $\wp'(z) = 0$. Since $f(z)$ and $\wp'(z)$ are meromorphic, so is $h(z)$ and it has isolated poles. Define:

$$
H(z) = h(z) - h(-z)
$$

we have $H(z) = 0$ for all $z$ that is not a pole of $h(z)$, so its Laurent series near any pole $a$ would need to have all coefficients 0. So now we reduce the odd function $f(z)$ to the even $h(z)$, and we can use <i>Lemma 16</i> to show that there exists some $Q(\wp(z))$ such that:

$$
h(z) = Q(\wp(z)) + C
$$

and finally

$$
f(z) = (Q(\wp(z)) + C) \wp'(z)
$$

</proof>

In [6] we showed that every function $f(z)$ can be decomposed into odd and even components:

$$
f(z) = f_o(z) + f_e(z)
$$

With

$$f_e(z) = \frac{f(z) + f(-z)}{2} \qquad f_o(z) = \frac{f(z) - f(-z)}{2}$$

Since elliptic functions are closed under arithmetic operations, we conclude that $f_e(z)$ and $f_o(z)$ are elliptic and can use *Lemma 16* to show that $f_o(z)$ can be written as $R_1(\wp(z))$ and *Lemma 17* that $f_e(z)$ can be written as $\wp'(z) R_2(\wp(z))$, so $f(z) = R_1(\wp(z)) + \wp'(z) R_2(\wp(z))$ leading to the following:

**Corollary 18.** Every elliptic function $f(z)$ can be written as a rational function of $\wp(z)$.

## Conclusion

In this post we introduced the Weierstrass functions which include the $\wp$, the zeta and the sigma functions. We ended up not using the latter two, but I included them because they're discussed in Ahlfors.

The proof that every elliptic function can be expressed from $\wp(z)$ was extremely difficult to understand. It has many subtle details and every time I re-read the proof I realized I had glossed over something.

This proof is also not provided in Ahlfors (in any obvious way at least) and I relied entirely on ChatGPT to understand it! Like with learning history, I'm finding using ChatGPT a lot more effective at learning math: I can ask for it to explain me things in different angles and dig into different parts.

Part of me is reluctant on this approach because I really like the idea of reading books, but going forward I might need to rethink my approach. Perhaps I'll use textbooks to get a general idea of the field but rely more on ChatGPT to really understand things.

This post also marks the end of my journey with the textbook *Complex Analysis* by Lars V. Ahlfors. I'll write another post summarizing the book and look back on this journey.

## Appendix

**Lemma 19.** Let $P(z)$ be a polynomial:

$$
P(z) = c_1 z^2 + c_2 z^4 + c_3 z^6 + \cdots
$$

then near 0, $1/P(z)$ is of the form:

$$
\frac{1}{P(z)} = \frac{c'_1}{z^2} + c'_2 + c'_3 z^2 + c'_4 z^4 + \cdots
$$

where $c'_1 = 1/c_1$.

<proof>
First we factor $z^2$:

$$
P(z) = z^2 (c_1 + c_2 z^2 + c_3 z^4 + \cdots)
$$

Invert:

$$
\frac{1}{P(z)} = \frac{1}{z^2} \cdot \frac{1}{c_1 + c_2 z^2 + c_3 z^4 + \cdots} = \frac{1}{z^2} \cdot \frac{1}{c_1(1 + \alpha_2 z^2 + \alpha_3 z^4 + \cdots)}
$$

Now define

$$
u(z) = \alpha_2 z^2 + \alpha_3 z^4 + \cdots
$$

so we have:

$$
\frac{1}{P(z)} = \frac{1}{z^2} \frac{1}{c_1(1 + u(z))}
$$


as $z \rightarrow 0$, $u(z) \rightarrow 0$ since it has no constant terms. Thus we can consider the series:

$$
\frac{1}{1 + u} = 1 - u + u^2 - u^3 + \cdots
$$

so we end up with:

$$
\frac{1}{P(z)} = \frac{1}{z^2} \frac{1}{c_1} (1 - u(z) + u(z)^2 - u(z)^3 + \cdots)
$$

because each term of $u(z)$ is an even power of $z$, the result will be some polynomial of $z$ of even powers. Thus:

$$
\frac{1}{P(z)} = \frac{c'_1}{z^2} + c'_2 + c'_3 z^2 + c'_4 z^4 + \cdots
$$

with $c'_1 = 1/c_1$.

QED.

</proof>

**Lemma 20.** Let $P(z)$ be a polynomial:

$$
P(z) = c_1 z + c_2 z^2 + c_3 z^3 + \cdots
$$

then near 0, $1/P(z)$ is of the form:

$$
\frac{1}{P(z)} = \frac{c'_1}{z} + c'_2 + c'_3 z + c'_4 z^2 + \cdots
$$

where $c'_1 = 1/c_1$.

<proof>
We reduce this to <i>Lemma 19</i> by writing $z = w^2$ so we have:

$$
P(w) = c_1 w^2 + c_2 w^4 + c_3 w^6 + \cdots
$$

</proof>


## References

* [1] Complex Analysis - Lars V. Ahlfors
* [[2](https://www.kuniga.me/blog/2024/11/02/poles.html)] NP-Incompleteness: Elliptic Functions
* [[3](https://www.kuniga.me/blog/2024/11/02/poles.html)] NP-Incompleteness: Zeros and Poles
* [[4](https://www.kuniga.me/blog/2025/04/16/residue-theorem.html)] NP-Incompleteness: The Residue Theorem
* [[5](https://www.kuniga.me/blog/2024/08/31/removable-singularities.html)] NP-Incompleteness: Removable Singularities
* [[6](https://www.kuniga.me/blog/2021/10/09/hermitian-functions.html)] NP-Incompleteness: Hermitian Functions
