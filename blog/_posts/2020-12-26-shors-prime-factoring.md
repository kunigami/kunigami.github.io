---
layout: post
title: "Shor's Prime Factoring Algorithm"
tags: [quantum computing, linear algebra, number theory]
vanity: "2020-12-26-shors-prime-factoring"
excerpt_separator: <!--more-->
description: "Shor's Prime Factoring Algorithm"
---

{% include blog_vars.html %}

<figure class="image_float_left">
    <img src="{{resources_path}}/peter-shor.png" alt="Peter Shor thumbnail" />
</figure>

Peter Shor is an American professor at MIT. He received his B.S. in Mathematics at Caltech and earned his Ph.D. in Applied Mathematics from MIT advised by Tom Leighton. While at Bell Labs, Shor developed the Shor's prime factorization quantum algorithm, which awarded him prizes including the Gödel Prize in 1999.

In this post we'll combine the parts we studied before to understand Peter Shor's prime factorization quantum algorithm, which can find a factor of a composite number exponentially faster than the best known algorithm using classic computation.

We'll need basic familiarity with quantum computing, covered in a [previous post]({{site.url}}/blog/2020/10/11/deutsch-jozsa-algorithm.html). The bulk of the post is showing how to efficiently solve the order-finding problem since we learned from [Number Factorization from Order-Finding]({{site.url}}/blog/2020/12/11/factorization-from-order.html) that it is the bottleneck step in finding a prime factor of a composite number. The remaining is putting everything together and do some analysis of the performance of the algorithm as a whole.

<!--more-->

## Quantum Order-finding

Recall the definition of order-finding from [2]:

> Given integers $x$, $N$, the problem of *order-finding* consists in finding the smallest positive number $r$ such that $x^r \equiv 1 \Mod{N}$, where $r$ is called the *order of* $x \Mod{N}$.

The basic idea is to choose a unitary matrix $U$ and show that its eigenvalue contains the order of $x \Mod{N}$.

### Choosing the Operator $U$

We choose $U$ such that $U \ket{u} = \ket{x u \Mod{N}}$, which can be shown to be a unitary matrix. Now suppose our eigenvector is

$$\ket{u_s} = \frac{1}{\sqrt{r}} \sum_{k = 0}^{r-1} \exp({\frac{-2 \pi i s k}{r}}) \ket{x^k \Mod{N}}$$

For a parameter $0 \le s \le r - 1$. If we apply the operator $U$:

$$U \ket{u_s} = \ket{x} \frac{1}{\sqrt{r}} \sum_{k = 0}^{r-1} \exp({\frac{-2 \pi i s k}{r}}) \ket{x^k \Mod{N}}$$

$$ = \frac{1}{\sqrt{r}} \sum_{k = 0}^{r-1} \exp({\frac{-2 \pi i s k}{r}}) \ket{x^{k+1} \Mod{N}}$$

We can show that (see *Lemma 1* in the *Appendix*)

$$U \ket{u_s} = \exp({\frac{2 \pi i s}{r}}) \ket{u_s}$$

 We conclude that $\exp({\frac{2 \pi i s}{r}})$ is the eigenvalue for $U$ and $\ket{u_s}$. We can then measure $\varphi \approx s/r$ via [Quantum Phase Estimation]({{site.url}}/blog/2020/12/23/quantum-phase-estimation.html).

But how do we prepare the state $\ket{u_s}$ for some $s$?

### Preparing the eigenvector

We don't know how to prepare  $\ket{u_s}$ for a specific $s$, but we can prepare a state which is a linear combination of $\ket{u_s}$, in particular:

$$\frac{1}{\sqrt{r}} \sum_{s=0}^{r-1} \ket{u_s}$$

Which can be show to be exactly $\ket{1}$ (see *Lemma 2* in the *Appendix*). That means if we use $\ket{1}$ as our initial eigenvector, we'll measure $\varphi$ corresponding to one of the eigenvalues $\exp({\frac{2 \pi i s}{r}})$, but we don't know which $s$ was used!

Let's take a detour to revisit continued fractions and learn how to leverage them to recover $r$ and $s$.

## Continued Fractions

Continued fractions is a way to represent rational numbers such that they can be iteratively approximated. For example, consider the rational $\frac{31}{13}$.

We can represent it as $2 + \frac{5}{13}$, which is the same as $2 + \frac{1}{\frac{13}{5}}$. We can repeat this process for $13/5$ to get

$$\frac{31}{13} = 2 + \frac{1}{2 + \frac{1}{\frac{3}{5}}}$$

If we continue with this, we'll end up with

$$\frac{31}{13} = 2 + \frac{1}{2 + \frac{1}{1 + \frac{1}{1 + \frac{1}{2}}}}$$

We can't keep doing this with $\frac{1}{2}$ since the denominator of $\frac{1}{(\frac{2}{1})}$ leaves no remainder.

More formally, consider a rational number greater than 1, $p/q$. We rewrite it as $a + p/q$, such that $p = aq + b$, $b < q$. Since $b/q < 1$, $1/(q/b) > 1$, and we can repeat the procedure for $q/b$. Note that because $b < q$ this algorithm will eventually end. The algorithm should return the list of $a$'s generated in the process, which provide a unique representation for $p/q$.

This idea can be implemented in a short Python code:

{% highlight python %}
def continued_fraction(p, q):
    a = []
    while q > 0 and p > q:
        a.append(p // q)
        p, q = q, p % q
    return a
{% endhighlight %}

As an example, if we run it with $p = 31, q = 13$, it returns $a = [2, 2, 1, 1, 2]$.

### Recovering the Rational Number

Given a list of integers $a = [a_0, \cdots, a_n]$, it's possible to obtain recover the numerator and denominator $p$ and $q$ that has the continued fraction corresponding to $a$.

If we define $p_0 = a_0$, $q_0 = 1$, $p_1 = 1 + a_0 a_1$ and $q_1 = a_1$, and then recursively

$$
\begin{aligned}
    p_n &= a_n p_{n-1} + p_{n-2}\\
    q_n &= a_n q_{n-1} + q_{n-2}
\end{aligned}
$$

It's possible to show that $[a_0, \cdots, a_n] = p_n / q_n$ and furthermore, that $p_n$ and $q_n$ are co-primes. This provides an easy $O(n)$ algorithm to recover $p$ and $q$ given $[a_0, \cdots, a_n]$.

### Finding Nearby Rational Numbers

Suppose we are given a rational number $x$ and we want to recover co-primes $p$ and $q$, such that

$$\abs{\frac{p}{q} - x} \le \frac{1}{2q^2}$$

It's possible to show that $x$ has a continued fraction $a = [a_0, \cdots, a_n]$ and that $p / q = [a_0, \cdots, a_k]$, for $k \le n$.

This means that if we feed $x$'s continued fraction to the algorithm from the section above, we'll invariably run into $p = p_k$ and $q = q_k$.

This flexibility is important in the context of our problem because the value we'll measure, $\varphi$, is not exactly $s/r$ but an approximation.

### Recovering $r$ via Continued Fractions

We'll now see how to recover $s$ and $r$ from $\varphi$. We know that both $s$ and $r$ are integers, so $s/r$ is a rational, and we can use continued fractions to extract them.

We first compute the continued fraction of $\varphi$. Then we try to recover its numerator and denominator, and it can be shown that

$$\abs{\frac{s}{r} - \varphi} \le \frac{1}{2r^2}$$

So by the discussion in the previous section we'll invariably pass by $p_k / q_k$ such that $p_k / q_k = s / r$.

The problem is that we don't know for which $k$ that's the case, but we can determine if $r = q_k$ for each $k$ by checking whether $x^{q_k} \equiv 1 \Mod{N}$. If $r$ and $s$ are co-primes, then we'll find it.

If not, let $r_0 = q_k$ for a given iteration $k$ and assume that $x^{r_0} \not \equiv 1 \Mod{N}$. We can show that $r_0$ is a factor of $r$ (see *Lemma 3* in *Appendix*). Let $x_0 \equiv x^{r_0} \Mod{N}$. The order of $x_0$ is $r_{r_0} = r/r_0$ since $x_0^{r/r_0} = x^{r}$. We'll obtain $r_1$, which if happens to be the order of $x_0$ allows us to get $r$ via $r = r_1 r_0$. Otherwise we repeat for $x_0' \equiv x^{r_1}$. Since $r_0$ is a proper factor of $r$, $r_0 \le r/2$, so on each iteraction we at least halve the order, which means we only need $O(\log r)$ iterations. If we reach the point where $r_{n} = 1$, it means that $q_k$ is not valid and hence $p_k / q_k \ne s / r$.

It's also possible that the true value of $s$ is $0$, in which case we won't be able to find $r$. This can happen with probability $p(s=0) \le r$, in which case we repeat the whole algorithm.

**Note.** In [1], it suggests we can find $s'/r' = s/r$ where $s'$ and $r'$ are co-prime from $\varphi$ using continued fractions alone, but I don't understand how from studying the proof of *Theorem A 4.16*. In other words, we know $k$ for which $p_k / q_k = s / r$, which in turn will lead to a more efficient algorithm.

## Shor's Prime Factoring

We now have all the pieces to solve the prime factoring. We first use $U \ket{u} = \ket{x u \Mod{N}}$ and $\ket{u} = \ket{1}$. We'll measure $s / r$ for $s \in \curly{0, \cdots, r-1}$ with high-probability. We can recover $r$, the order of $x \Mod{N}$, by using the method outlined in the previous section.

Finally, once we know $r$ we can obtain a prime factor with high-probability as described in [Number Factorization from Order-Finding]({{site.url}}/blog/2020-12-11-factorization-from-order.html).

## Performance

### Runtime Complexity

Let's first consider the number of gates needed for performing the steps above. Let $L$ represent the number of bits of $x$ for which we want to compute the order modulo $N$.

**Measuring $\phi$.** We assume $t$ is roughly the size of $L$. From the circuit depicted in *Figure 1* in [Quantum Phase Estimation]({{site.url}}/blog/2020/12/23/quantum-phase-estimation.html), we need $O(L)$ Hadamard gates and $O(L)$ of the $U^{2^k}$ gates. We can use an [efficient modular exponentiation algorithm](https://en.wikipedia.org/wiki/Modular_exponentiation) which can compute $a^b$ in $O(\log(b) \log^2(a))$ where $\log^2(a)$ is due to the multiplications, so $U^{2^k}$ can be implemented using $O(L^3)$ gates. Summarizing, we can measure $\phi \approx s/r$ using $O(L^4)$ gates.

**Recovering $r$ via Continued Fractions.** It's possible to show that we can compute the continued fraction of $\phi$ in $O(L^3)$, and that the output has size $O(L)$. We then need $O(L)$ iterations to recover its numerator and denominator, but at each step $k$ we need to know whether the numerator $q_k$ is $r$ by computing $x^r \Mod{N}$, which can be done in $O(L^3)$ using fast modular exponentiation.

However, if we didn't find $r$, we need to repeat the process up to $O(\log r) = O(L)$ times, each time re-computing $x^r \Mod{N}$, for a total of $O(L^4)$ operations each time we need to test a candidate $q_k$. This amounts up to $O(L^5)$.

**Number Factorization from Order-Finding.** Finally, as we discussed in [3], the complexity of obtaining a prime factor is $O(L^3)$ excluding the order finding step.

Recovering $r$ from $\phi$ dominates the overall complexity, leading to a $O(L^5)$ probabilistic quantum algorithm that can find a prime factor of a composite number.

For comparison, if we use a linear algorithm to find the order as discussed in [3], we would end up with a $O(2^L)$ one.

### Precision

We have a few sources of uncertainty in the algorithm, which we recap now:

* The possibility of not measuring the real $s / r$, which is less than 60% (see *Measuring $\phi$* in [2]) and can further reduced by using more gates, or simply repeating the phase estimation algorithm since each run is independent.
* The possibility that $s = 0$ (see *Recovering $r$ via Continued Fractions* above), which has low probability ($1 / r$) and can be further reduced by repeating the phase estimation algorithm.
* The possibility that the randomly chosen $x$ in the prime factoring algorithm (see *Prime Factoring Algorithm* in [3]) yields a "bad" $r$, which is less than 25% and can be further reduced by repeatedly choosing a new random $x$.

## Conclusion

This is the final post that led to the Shor's prime factoring. Regardless of the practical applicability of this method, I found it fascinating how much theory it relies on. I learned a lot about quantum computing in the process and while I might not have grasped every single detail, I think I have a good overall idea on how everything comes together.

I found that:

$$\frac{1}{\sqrt{r}} \sum_{s=0}^{r-1} \ket{u_s} = \ket{1}$$

Is mind-blowing. It's as if a bunch of eigenvectors are "hiding" inside $\ket{1}$ and only "come out" when measured.

## Related Posts

* [Pell Equation]({{blog}}/2012/02/12/pell-equation.html) - in finding solutions to Pell's equations we also leverage continued fractions.
* [Consistent Hashing]({{site.url}}/blog/2019/04/12/consistent-hashing.html) - this is a trivia rather than real relatedness, but I found out that [Tom Leighton](https://en.wikipedia.org/wiki/F._Thomson_Leighton) was the advisor of both [Daniel Lewin](https://en.wikipedia.org/wiki/Daniel_Lewin) (founder of Akamai, featured in that post) and Peter Shor (featured in this post).

## Appendix

**Lemma 1.** $U \ket{u_s} = \exp({\frac{2 \pi i s}{r}}) \ket{u_s}$

*Proof.* To simplify the notation, assume $\alpha = \frac{-2 \pi i s}{r}$ and $y^k = x^k \Mod{N}$. We can write $\ket{u_s}$ as:

$$\ket{u_s} = \frac{1}{\sqrt{r}} (e^{\alpha 0} \ket{y^0} + e^{\alpha 1} \ket{y^1} + \cdots + e^{\alpha (r-1)} \ket{y^{r-1}})$$

We can write $U \ket{u_s}$ as:

$$U \ket{u_s} = \frac{1}{\sqrt{r}} (e^{\alpha 0} \ket{y^1} + e^{\alpha 1} \ket{y^2} + \cdots + e^{\alpha (r-1)} \ket{y^{r}})$$

We note that $\ket{y^{r}} = \ket{y^{0}}$ since by definition $x^r \equiv x^0 \equiv 1 \Mod{N}$, so we can rearrange:

$$U \ket{u_s} = \frac{1}{\sqrt{r}} (e^{\alpha (r-1)} \ket{y^{0}} + e^{\alpha 0} \ket{y^1} + \cdots + e^{\alpha (r-2)} \ket{y^{r - 1}})$$

This looks almost like $\ket{u_s}$ except the exponents are shifted by 1. We can fix this by pulling a factor of $e^{\alpha}$:

$$U \ket{u_s} = \frac{1}{e^{\alpha} \sqrt{r}} (e^{\alpha r} \ket{y^{0}} + e^{\alpha 1} \ket{y^1} + \cdots + e^{\alpha (r-1)} \ket{y^{r - 1}})$$

We have $e^{\alpha r} = \exp(\frac{2 \pi i s r}{r}) = \exp(2 \pi i s)$ and since $s$ is integer, by Euler's formula we conclude that $e^{\alpha r} = 1 = e^{\alpha 0}$, so we can do

$$U \ket{u_s} = \frac{1}{e^{\alpha} \sqrt{r}} (e^{\alpha 0} \ket{y^{0}} + e^{\alpha 1} \ket{y^1} + \cdots + e^{\alpha (r-1)} \ket{y^{r - 1}})$$

Now it's easy to see that $U \ket{u_s} = \frac{1}{e^{\alpha}} \ket{u_s} = e^{-\alpha} \ket{u_s} = \exp({\frac{2 \pi i s}{r}}) \ket{u_s}$. *QED*

**Lemma 2.**

$$\frac{1}{\sqrt{r}} \sum_{s=0}^{r-1} \ket{u_s} = \ket{1}$$

*Proof:*

Let

$$S = \frac{1}{\sqrt{r}} \sum_{s=0}^{r-1} \ket{u_s}$$

Replace the definition of $\ket{u_s}$:

$$S = \frac{1}{\sqrt{r}} \sum_{s=0}^{r-1} (\frac{1}{\sqrt{r}} \sum_{k = 0}^{r-1} e^{-2 \pi i s k /r} \ket{x^k \Mod{N}})$$

Moving scalars around and changing the order of the sums yields:

$$\frac{1}{r} \sum_{k = 0}^{r-1} \ket{x^k \Mod{N}} (\sum_{s=0}^{r-1}  e^{-2 \pi i s k /r} )$$

For $k = 0$ the terms of the inner sum are equal to 1, so it adds up to $r$. Otherwise, the inner sum is a geometric sum on $s$, which has a closed form (see *Interlude: Classic Inverse Fourier Transform* [2] for more details):

$$\sum_{s=0}^{r-1}  e^{-2 \pi i s k /r}  = \frac{1 - e^{-2 \pi i s k}}{1 - e^{-2 \pi i s k / r}}$$

Since $k$ is a positive integer, $e^{-2 \pi i s k} = 1$. Since $k < r$, $k / r < 1$ and $e^{-2 \pi i s k / r} \neq 1$, which means the inner sum is 0 for $k > 0$.

Thus,

$$S = \frac{1}{r} r \ket{x^0 \Mod{N}} = \ket{1}$$

*QED*

**Lemma 3.** Let $r, s, r', s'$ be positive integers. If $s' / r' = s / r$ and $s'$ and $r'$ are co-primes, then $r'$ divides $r$. Furthermore, if $s$ and $r$ are not co-prime then $r' < r$.

*Proof* We have $s' r = s r'$, and because prime factorization is unique, both sides have the same prime factors. All prime factors contributed by $r'$ must be matched by $r$ on the other side since $r'$ and $s'$ do not share any prime factors. This means that $r$ can be divided by $r'$.

If $r$ and $s$ are not co-prime then $r \neq r'$ because otherwise $s' = s$ but $r'$ and $s'$ are co-prime. Also, since $r$ can be divided by $r'$, $r \ge r'$, so it must be $r > r'$. *QED*

## References

* [[1](https://www.amazon.com/Quantum-Computation-Information-10th-Anniversary/dp/1107002176)] Quantum Computation and Quantum Information - Nielsen, M. and Chuang, I.
* [[2]({{site.url}}/blog/2020/12/23/quantum-phase-estimation.html)] Quantum Phase Estimation
* [[3](({{site.url}}/blog/2020-12-11-factorization-from-order.html))] Number Factorization from Order-Finding
