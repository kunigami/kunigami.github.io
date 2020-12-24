---
layout: post
title: "Number Factorization from Order-Finding"
tags: [number theory]
excerpt_separator: <!--more-->
---

Given integers $x$, $N$, the problem of *order-finding* consists in finding the smallest positive number $r$ such that $x^r \equiv 1 \Mod{N}$, where $r$ is called the *order of* $x \Mod{N}$.

In this post we'll show that if we know how to solve the order of $x \Mod{N}$, we can use it to get a probabilistic algorithm for finding a non-trivial factor of a number $N$.

The motivation is that this is a crucial step in Shor's quantum factorization, but only relies on classic number theory.

<!--more-->

## Definitions

In this section we define a bunch of terminology, most of which the reader might already be familiar with. Feel free to skip ahead and refer to this when seeing them later.

**Prime factorization.** Given a positive number $N$, the prime factorization of $N$ is a set of distinct prime factors $p_1, p_2, \cdots, p_m$ and positive exponents $\alpha_1, \alpha_2, \cdots, \alpha_m$ such that $N = p_1^{\alpha_1} p_2^{\alpha_2} \cdots p_m^{\alpha_m}$. For convenience we assume $p_1 < p_2 < \cdots < p_m$. It's possible to show that any integer larger than 1 can be uniquely represented by its prime factorization. Example: $600$ can be uniquely represented by $2^3 3^1 5^2$.

**Divisibility.** We say a positive integer $x$ divides $y$, denoted as $x \mid y$ if there's a positive integer $k$ such that $y = kx$. Otherwise, we denote it as $x \nmid y$ and there's a positive integer $c < x$ such that $y = kx + c$.

**Greatest Common Divisor.** The greatest common divisor of two integers $x$ and $y$ is the largest integer that divides both $x$ and $y$ and is denoted by $\gcd(x, y)$. It can be computed in $O(\min(\log(x), \log(y)))$.

**Co-primality.** Given two positive integers $x$ and $y$, we say $x$ and $y$ are co-prime if they don't share any prime factors, or $\gcd(x, y) = 1$. For example, 9 and 10 are co-prime, but 10 and 12 are not since they share the prime factor 2.

**Set of co-primes.** Given an integer $N$, we define $Z_N = \curly{1, \cdots, N}$ and $Z_N^{\*}$ as the elements in $Z_N$ that are co-prime with $N$. For example, if $N = 10$, $Z^{\*}_N = \curly{1, 3, 7, 9}$.

**Euler $\varphi$ function.** is defined as the number of co-primes of $N$ less than $N$ and denoted as $\varphi(N)$. Note that $\abs{Z_N^{\*}} = \varphi(N)$.

## Theory

We'll now state a few Theorems from which we'll build the prime factoring algorithm. Their proofs are described in the *Appendix*.

**Theorem 1.** Given co-primes $x$, $N$ and $r$ the order of $x \Mod{N}$, then $r \le N$.

**Theorem 2.** Let $N$ be a non-prime number and $1 \le x \le N$ a non-trivial solution to $x^2 \equiv 1 \Mod{N}$ (by non-trivial we mean $x \not \equiv \pm 1 \Mod{N}$), then at least one of $\gcd (x - 1, N)$ or $\gcd (x + 1, N)$ is a non-trivial factor of $N$.

**Theorem 3.** Let $N$ be a odd non-prime positive integer $N$ with prime factors $N = p_1^{\alpha_1} p_2^{\alpha_2} \cdots p_m^{\alpha_m}$. Let $x$ be an element chosen at random from $Z_N^{\*}$ and $r$ the order of $x \Mod{N}$. Then

$$p(r \mbox{ is even and } x^{r/2} \not \equiv -1 \Mod{N}) \ge 1 - \frac{1}{2^m}$$

## Prime Factoring Algorithm

*Theorem 3* seems highly specific but combined with *Theorem 2*, it allows us to find a factor of $N$. To see how, suppose $r$ is even and $x^{r/2} \not \equiv -1 \Mod{N}$, which can happen with probability at least $1 - \frac{1}{2^m}$. Let $y = x^{r/2}$, so $y \not \equiv -1 \Mod{N}$. We also have $y \not \equiv 1 \Mod{N}$, since otherwise $r/2$ would be the order of $x \Mod{N}$. This means that by Theorem B, $\gcd (y - 1, N)$ or $\gcd (y + 1, N)$ is a non-trivial factor of $N$.

We can now define the algorithm to obtain a non-trivial factor of $N$. Here's a simple Python implementation:

{% highlight python %}
from math import gcd
import random

def get_factor(N):
    if N % 2 == 0:
        return 2

    x = random.randint(1, N - 1) # inclusive

    f = gcd(x, N)
    if f > 1:
        return f

    r = order(x, N)
    if r % 2 != 0 or mod_exp(x, r//2, N) == N - 1:
        return None

    y = x ** (r // 2)
    f = gcd(y - 1, N)
    if f > 1:
        return f

    return gcd(y + 1, N)
{% endhighlight %}

`mod_exp(b, e, N)` is computes $b^n \Mod{N}$. `order(x, N)` returns $r$ such that $x^r \equiv 1 \Mod{N}$.

### Refining

If $N$'s prime factorization is $N = p_1^{\alpha_1}$ for $\alpha_1 > 1$, then $m = 1$ and the probability lower bound is only $0.5$.

We can detect when that's the case with the following algorithm: for each exponent starting from $e = 2$, we find the largest value $a$ such that $a^b <= N$ via binary search. We stop looking for exponents when $2^{b} > N$ or when our binary search returns 1.

{% highlight python %}
def bin_search(f):
    x = 1
    while f(x << 1) <= 0:
        x <<= 1

    p2 = x
    while p2 > 1:
        p2 >>= 1
        if f(x + p2) <= 0:
            x += p2

    return x

def get_single_base(N):
    e = 2
    while True:
        f = lambda b: b**e - N
        b = bin_search(f)
        if b == 1:
            break

        if f(b) == 0:
            return b

        e += 1

    return None
{% endhighlight %}

In the code above `bin_search()` finds $a$ by constructing the bits from the most to least significant bits, so its complexity is $O(log N)$. Python implements the power function using repeated squares, so `f()` is also $O(log N)$. Finally we'll stop when $2^{b} > N$, so $b \le log(N)$. This leads to a total complexity of $O(log^3 N)$.

We know how to determine $a$ when $N = a^b$, for any positive integers $a > 1$ and $b > 1$. In that case $a$ is a non-trivial factor. Otherwise we know $N$'s prime factorization has at least 2 distinct prime factors, so $m > 1$ and the probability lower bound is now $0.75$.

## Complexity

Let's analyze the complexity of `get_factor()`. The `gcd(a, b)` can be implemented as $O(\log min(a, b))$. If we check for `get_single_base()` discussed above, it will add an $O(log^3 N)$ component.

However, the dominant complexity of the function is `order(x, N)`. We know from Theorem 1 that $r \le N$. A brute approach consists in looking for all the possibilities, which leads to an $O(N)$ algorithm (or rather $O(N \log N)$ if we were to account for the arithmetic operations):

{% highlight python %}
def order(x, N):
    m = x
    for r in range(1, N + 1):
        if m % N == 1:
            return r
        m *= x
{% endhighlight %}

Being able to solve `order(x, N)` efficiently is the secret ingredient behind Shor's factorization but we need to resort to quantum computing. We'll not discuss it here, but we know have the background and motivation from perspective of classic number theory.

## Experiments

Setting the running time aside, let's not forget the algorithm we described is probabilistic. The refined version of `get_factor()` provides a lower bound of $0.75$, but how accurate is it in practice?

If we run for the first 5,000 non-prime, odd numbers, we get ~75% accuracy on average. If we exclude number of the form $N = a^b$, we get 77% accuracy, only slightly better.

One thing we can do is to repeat the algorithm $k$ times or until it finds a factor. If the probability of one run is $p$, and assuming each run is [iid](https://en.wikipedia.org/wiki/Independent_and_identically_distributed_random_variables) then the resulting probability should be $1 - (1 - p)^k$.

If we run for $k=5$ for example, the accuracy is 99.6%. It's thus possible to get pretty good accuracies with a small number of repetitions.

## Conclusion

I really like number theory and it was fun to study the reduction from the prime factoring to the order finding, so much so that I decided to post about this before actually writing on how to solve the order finding problem using quantum computation.

I'm wondering what is the best classic algorithm for solving order finding, including probabilistic ones. We have probabilistic algorithms for detecting primes that are very efficient.

We recall studying some of the modular arithmetic and its properties in college, probably in the context of criptography classes.

## Appendix

**Theorem 1.** Given co-prime integers $x$, $N$, the order $r$ of $x \Mod{N}$ is $r \le N$.

*Proof.* We know that $x^i \Mod{N} \in \curly{1, \cdots N - 1}$ for some positive integer $i$. It follows from the pigeonhole principle that there must exist $j \le N + 1$ such that $x^i \equiv x^j \Mod{N}$, for $i < j$. To see why, note we have $N$ possible outcomes for $x^i \Mod{N}$ so if we consider the first $N + 1$ values of $i$ there ought to be a repeated value.

Since $j > i$, there is some $r > 1$ such that $j = i + r$, thus

$$x^j = x^r x^i$$

and

$$x^i \equiv x^r x^i \Mod{N}$$

Since $$x^i \Mod{N} > 0$$, this implies $x^r \equiv 1 \Mod{N}$. Since $j \le N + 1$ and $i \ge 1$, $r \le N$. *QED*

**Theorem 2.** Let $N$ be a non-prime number and $1 \le x \le N$ a non-trivial solution to $x^2 \equiv 1 \Mod{N}$ (by non-trivial we mean $x \not \equiv \pm 1 \Mod{N}$), then at least one of $\gcd (x - 1, N)$ or $\gcd (x + 1, N)$ is a non-trivial factor of $N$.

*Proof.* Assuming $x^2 \equiv 1 \Mod{N}$, then $N \mid x^2 - 1 = (x + 1)(x - 1)$, so $N$ must have a common factor with at least one of $(x + 1)$ or $(x - 1)$.

Since $x \not \equiv \pm 1 \Mod{N}$, then $x \neq 1$ and  $x \neq N-1$. Which implies $0 < x - 1$ and $x + 1 < N$, hence the common factor is not $N$. Then at least one of $\gcd (x - 1, N)$ or $\gcd (x + 1, N)$ is a non-trivial factor of $N$. *QED*

The proof of theorem 3 is much more involved, so let's introduce some helpers.

**Chinese Remainder Theorem** Let $b_1, \cdots, b_n$ be a set of integers pairwise co-prime and integers $a_1, \cdots, a_n$, where $0 \le a_i < b_i$. Let $N = b_1 \cdots b_m$.

Then the is exactly one $0 \le x < N$ that safisfies $x \equiv a_i \Mod{b_i}$ for every $i$.

*Proof.* Not included here. Refer to [1], Theorem A4.16 (p629).

**Lemma 3.1** Let $N$ be a positive integer $N$ with prime factors $N = p_1^{\alpha_1} p_2^{\alpha_2} \cdots p_m^{\alpha_m}$. Let $x$ be an element chosen at random from $Z_N^{\*}$. This is equivalent to picking $x_1, x_2, \cdots, x_m$ at random from $Z_{p_1^{\alpha_1}}^{\*}, Z_{p_2^{\alpha_2}}^{\*}, \cdots, Z_{p_m^{\alpha_m}}^{\*}$, respectively.

*Proof.* We just need to show that there's a one-to-one mapping between $x$ and ($x_1, x_2, \cdots x_m$).

$\rightarrow$ If we define $x_i$ as the remainder of $x$ divided by $p_i^{\alpha_i}$ for every $i$, then this is a unique map from $x$ to ($x_1, x_2, \cdots x_m$), but we need to prove that if $x \in Z_N^{\*}$ then $x_i \in Z_{p_i^{\alpha_i}}^{\*}$.

Since $x$ is co-prime with $N$, then $x$ is co-prime with $p_i^{\alpha_i}$ (since it has a subset of factors of $N$). We claim that $x_i$ is also co-prime with $p_i^{\alpha_i}$. Otherwise, since $x = k p_i^{\alpha_i} + x_i$ for some integer $k$, if $x_i$ is not co-prime with $p_i^{\alpha_i}$, then they share at least one factor $p_i$, so $x_i = p_i \alpha$, thus $x = p_i (k p_i^{\alpha_i - 1} + \alpha)$, which implies $x$ is not co-prime with $p_i^{\alpha_i}$, a contradiction.

$\leftarrow$ Assume now we have $x_i \in Z_{p_i^{\alpha_i}}^{\*}$. Since $p_1^{\alpha_1}$, $p_2^{\alpha_2}$ and $p_m^{\alpha_m}$ are pairwise co-prime, we can use the *Chinese Remainder Theorem*
 to show there's exactly one solution $0 \le x < N$ to $x \equiv x_i \Mod{p_i^{\alpha_i}}$ for every $i$.

 To show $x \in Z_N^{\*}$ it remains to show $x$ and $N$ are co-prime. Suppose it's not. Then it shares a factor $p_j$ with $N$ for some $j$, but then since it holds that $x \equiv x_j \Mod{p_j^{\alpha_j}}$, then $x = p_j \alpha = k p_j^{\alpha_j} + x_j$, which means $x_j = p_j(\alpha - k p_j^{\alpha_j - 1})$, so $x_j$ is not co-prime with $p_j^{\alpha_j}$, contradicting the hypothesis that $x_j \in Z_{p_j^{\alpha_j}}^{\*}$. *QED*

**Lemma 3.2** Let $a$ and $N$ be co-primes. Then $a^{\varphi(N)} \equiv 1 \Mod{N}$

*Proof.* Not included here. Refer to [1], Theorem A4.9 (p631).

**Lemma 3.3** Let $r$ be the order of $x \Mod{N}$ for co-primes $x$ and $N$. Let $r'$ be such that $x^{r'} \equiv 1 \Mod{N}$. Then $r$ divides $r'$.

*Proof.* If $r = r'$, this is trivially true, so consider the case where $r' > r$ (by definition $r$ cannot be bigger than $r'$). Let's now assume $r \nmid r'$, so there's $k > 0$ (since $r < r'$) and $0 < \alpha < r$  such that $r' = kr + \alpha$.

Then $x^{r'} \equiv x^{kr} x^{\alpha} \equiv 1 \Mod{N}$. We know $x^{r} \equiv 1 \Mod{N}$ and so is $(x^{r})^k \equiv x^{rk} \equiv 1 \Mod{N}$. But this means $x^{\alpha} \equiv 1 \Mod{N}$ with $\alpha < r$, which is a contradiction that $r$ is minimal. *QED*

**Lemma 3.4** Let $r$ be the order of $x \Mod{N}$ for co-primes $x$ and $N$. Then $r$ divides $\varphi(N)$

*Proof.* This follows from *Lemma 3.2*, which states $x^{\varphi(N)} \equiv 1 \Mod{N}$, which allows us to use *Lemma 3.4*, with $r' = \varphi(N)$ to conclude that $r$ divides $\varphi(N)$. *QED*

**Definition 3** From now until Theorem 3, we'll assume that $x \in Z_N^{\*}$, $x_i \in Z_{p_i^{\alpha_i}}^{\*}$ and that $x \equiv x_i \Mod{p_i^{\alpha_i}}$. Furthermore, we'll assume $r$ is the order of $x \Mod{N}$, and $r_i$ the order of $x_i \Mod{p_i^{\alpha_i}}$.

**Lemma 3.5** $r_i \mid r$ for every $i$

*Proof.* We have $x \equiv x_i \Mod{p_i^{\alpha_i}}$, which holds if we raise both to a power $k$. In particular $x^{r} \equiv x_i^{r} \equiv 1 \Mod{p_i^{\alpha_i}}$. We can use *Lemma 3.3* for $x_i$, $r_i$ and $r$, to show $r_i$ divides $r$.

**Lemma 3.6** Let $d_i$ be the largest exponent such that $2^{d_i}$ divides $r_i$. If $r$ is odd or $x^{r/2} \equiv -1 \mod{N}$ then $d_i$ is the same for any $i$.

*Proof.* Let's consider the case where $r$ is odd. This implies $r_i$ must be too, and the largest power of two that divides it is $2^0 = 1$, hence $d_i = 0$ for all $i$s.

Let's consider the case where $r$ is even and $x^{r/2} \equiv -1 \mod{N}$.

This means $x^{r/2} + 1 = k N$ for some integer $k$. Since $p_i^{\alpha_i}$ is a factor of $N$ for any $i$, $x^{r/2} + 1 = k' p_i^{\alpha_i}$, where $k' = k (N/p_i^{\alpha_i})$ is an integer. Thus $x^{r/2} \equiv -1 \mod{p_i^{\alpha_i}}$. Similar to a previous argument, since $x \equiv x_i \Mod{p_i^{\alpha_i}}$, we have $x^{r/2} \equiv x_i^{r/2} \equiv - 1 \Mod{p_i^{\alpha_i}}$.

Now suppose that $r_i$ divides $r/2$, so $r/2 = k r_i$, so  $x_i^{r/2} \equiv x_i^{r_i k} \equiv - 1 \Mod{p_i^{\alpha_i}}$, but $x_i^{r_i} \equiv (x_i^{r_i})^k \equiv x_i^{r_i k} \equiv 1 \Mod{p_i^{\alpha_i}}$ which is a contradiction, so it must be $r_i \nmid r/2$.

Let $d$ be the largest exponent such that $2^{d}$ divides $r$. From *Lemma 3.5* we have $r_i \nmid r$, so $r_i \le r$, thus $2^{d_i} \le 2^{d}$. If $2^{d_i} < 2^{d}$ then $2^{d_i} \le 2^{d - 1}$ but since $2^{d - 1}$ divides $r / 2$, it must be $2^{d_i} = 2^{d}$.

We just proved, for all $i$, that $d_i = 0$ if $r$ is odd and $d_i = d$ if $x^{r/2} \equiv -1 \mod{N}$. *QED*

**Cyclic Group Theorem** A group $Z_N^{\*}$ is called *cyclic* if there's $g \in Z_N^{\*}$ such that for any element $x \in Z_N^{\*}$, $x \equiv g^k \Mod{N}$ for some $k \ge 0$. If $N = p^\alpha$ for some odd prime $p$ and positive integer $\alpha$, then $Z_{p^\alpha}^{\*}$ is cyclic.

*Proof.* Not included here. This is also not included in [1].

**Lemma 3.7.** Suppose $g$ is a generator for $Z_{p^\alpha}^{\*}$ and $r$ the order of $g$ $\Mod{p^\alpha}$. Then $r = \abs{Z_{p^\alpha}^{\*}} = \varphi(p^\alpha)$.

*Proof.* We first prove that every $x \in Z_{p^\alpha}^{\*}$ can be expressed as $x \equiv g^{i} \Mod{p^\alpha}$ for $0 \le i \le r - 1$. From the *Cyclic Group Theorem*, there is $k \ge 0$ such $x \equiv g^k \Mod{p^\alpha}$. Let $k'$ be the smallest such $k$. If $k' > r$, then there is $0 < \delta < k'$ such that $k' = r + \delta$, and $g^{k'} \equiv g^{r} g^{\delta} \Mod{p^\alpha}$, which implies $g^{k'} \equiv g^{\delta} \Mod{p^\alpha}$ which contradicts the fact $k'$ is minimal, thus $k' \le r$. We also know that $k' \neq r$ because $g^0 \equiv g^r \equiv 1 \Mod{p^\alpha}$.

What we conclude here is that there are  $\abs{Z_{p^\alpha}^{\*}} = \varphi(p^\alpha)$ distinct elements in $Z_{p^\alpha}^{\*}$ and they all can be expressed with exponents $0 \le k \le r - 1$, which gives a lower bound $r \ge \varphi(p^\alpha)$.

Now consider the set $S$ of $x \equiv g^{i} \Mod{p^\alpha}$ for $0 \le i \le r - 1$. Let $i$ and $j$ represent the exponents of two elements in $S$. We claim that if $g^{i} \equiv g^{j} \Mod{p^\alpha}$ then $i = j$. Suppose not, that there's $i < j$ such that $g^{i} \equiv g^{j} \Mod{p^\alpha}$. Then $j = i + \delta$, for $0 < \delta < r$, and since $g^i \equiv g^i g^{\delta} \Mod{N}$, which means $g^\delta \equiv 1 \Mod{p^\alpha}$ which contradicts the definition of $r$. This implies that every element of $g^{i} \Mod{p^\alpha}$ for $0 \le i \le r - 1$ is unique, so the size of $S$ is exactly $r$.

We also note that every element of $S$ is in $Z_{p^\alpha}^{\*}$, so $S$ is a subset of it and thus $r = \abs{S} \le \abs{Z_{p^\alpha}^{\*}} = \varphi(p^\alpha)$, which is an upper bound for $r$.

Combining the lower bound and upper bound of $r$, we conclude it has to be exactly $ \varphi(p^\alpha)$. *QED*

**Lemma 3.8** For a prime $p$ and integer $\alpha$, $\varphi(p^\alpha) = p^{\alpha - 1}(p - 1)$.

*Proof.* We start by noting that $\varphi(p) = p - 1$ since no number smaller than $p$ has a common prime factor with $p$. For $p^\alpha$, the only numbers smaller than it that share a prime factor with it must be multiples of $p$, that is, $p k$ for $k = 1, \cdots, p^{\alpha - 1} - 1$. So the number of co-primes of $p^\alpha$ is $p^\alpha - 1$ minus $p^{\alpha - 1} - 1$, so $\varphi(p^\alpha) = p^{\alpha - 1}(p - 1)$. *QED*

**Lemma 3.9** Let $p$ be an odd prime and $2^d$ the largest power of 2 dividing $\varphi(p^\alpha)$. Let $r'$ be the order of a randomly chosen element $x$ from  $Z_{p^\alpha}^{\*}$. Then the probability that $2^d$ divides $r$ is 1/2.

*Proof.* From Lemma 3.8 we have $\varphi(p^\alpha) = p^{\alpha - 1}(p - 1)$. Since $p$ is odd, $p-1$ and $\varphi(p^\alpha)$ are even and thus $d \ge 1$.

From the *Cyclic Group Theorem*, there is $g \in Z_{p^\alpha}^{\*}$ such that a randomly chosen element $x$ satisfies $x \equiv g^{k} \Mod{p^{\alpha}}$. Let's consider 2 cases:

Case 1: $k$ is odd. We have that $x^r \equiv g^{kr} \equiv 1 \Mod{p^{\alpha - 1}}$. Let $r_g$ be the order of $g^{k} \Mod{p^\alpha}$. From *Lemma 3.7*, $r_g = \varphi(p^\alpha)$ and then from *Lemma 3.3* we conclude that $r_g \mid kr$ and thus $\varphi(p^\alpha) \mid kr$. Since $k$ is odd, $r$ and $\varphi(p^\alpha)$ have the same number of 2 factors, hence $2^d$ divides $r$.

Case 2: $k$ is even. From *Lemma 3.2* $g^{\varphi(p^\alpha)} \equiv 1 \Mod{p^\alpha}$, and since $k/2$ is integer, $g^{\varphi(p^\alpha) k/2} \equiv 1 \Mod{p^\alpha}$, so $x^{\varphi(p^\alpha)/2} \equiv 1 \Mod{p^\alpha}$, and by *Lemma 3.3* $r \mid \varphi(p^\alpha) / 2$. It must be that $2^d \nmid r$ otherwise $2^d \mid \varphi(p^\alpha) / 2$ and $2^{d+1} \mid \varphi(p^\alpha)$ contradicting the fact that $d$ is maximum.

Summarizing $k$ is odd if and only if $2^d \mid r$. It remains to show that $k$ is odd with 1/2 probability for a random $x$ from  $Z_{p^\alpha}^{\*}$. We can refer to the proof of *Lemma 3.7.* that states every $x \in Z_{p^\alpha}^{\*}$ can be expressed as $x \equiv g^{i} \Mod{p^\alpha}$ for $0 \le i \le r - 1 = \varphi(p^\alpha) - 1$. Since $\varphi(p^\alpha)$ is even, $\varphi(p^\alpha) - 1$ is odd and if we divide the set of numbers $\curly{0, \cdots, \varphi(p^\alpha) - 1}$ into odds and evens we get two sets of the same size.

**Theorem 3.** Let $N$ be an odd non-prime positive integer $N$ with prime factors $N = p_1^{\alpha_1} p_2^{\alpha_2} \cdots p_m^{\alpha_m}$. Let $x$ be an element chosen at random from $Z_N^{\*}$ and $r$ the order of $x \Mod{N}$. Then

$$p(r \mbox{ is even and } x^{r/2} \not \equiv -1 \Mod{N}) \ge 1 - \frac{1}{2^m}$$

*Proof.* We'll prove the equivalent statement:

$$p(r \mbox{ is odd or } x^{r/2} \equiv -1 \Mod{N}) \le \frac{1}{2^m}$$

Let $x \in Z_N^{\*}$, $x_i \in Z_{p_i^{\alpha_i}}^{\*}$ such that $x \equiv x_i \Mod{p_i^{\alpha_i}}$. By *Lemma 3.1* we can assume we're picking $x_i$ instead of $x$.

Let $r_i$ be the order of $x_i \Mod{p_i^{\alpha_i}}$ as in *Definition 3*. Let $d_i$ be the largest exponent such that $2^{d_i}$ divides $r_i$. By *Lemma 3.6* if $r$ is odd or $x^{r/2} \equiv -1 \mod{N}$ then $d_i$ is the same for any $i$.

[1] claims it's enough to use *Lemma 3.9* to prove it, but it's not clear to me why. My hunch is to show that if $r \mbox{ is odd or } x^{r/2} \equiv -1 \Mod{N}$ then all of $x_1, x_2, \cdots, x_m$ will be either divisible by $2^d$ (as defined in *Lemma 3.9*) or not. Since only $\frac{1}{2^m}$ of all the possible values of $x_1, x_2, \cdots, x_m$ can satisfy that, then the condition  $r \mbox{ is odd or } x^{r/2} \equiv -1 \Mod{N}$ cannot happen with more than that probability.

I'll leave this as is for now. If I figure out I can update the post.

## References

* [[1](https://www.amazon.com/Quantum-Computation-Information-10th-Anniversary/dp/1107002176)] Quantum Computation and Quantum Information - Nielsen, M. and Chuang, I.
