---
layout: doc
title: "Probability Cheat Sheet"
---

# Index
{:.no_toc}

1. TOC
{:toc}

# Discrete

## Event

An **event** can be interpreted as a proposition that is `true` with certain probability. For example, "the outcome of a dice throw is 4", which is `true` with probability $1/6$.

Let $\Omega$ denote the set of possible events, as called **sample space**. If $\Omega$ is countable, then we're dealing with the discrete case.

## Probability Distributions

The probability distribution is a function $P: \Omega \rightarrow {0 \le x \le 1 \mid x \in \mathbb{R}}$ such that

$$\sum_{\omega \in \Omega} P(\omega) = 1$$

Let $A$ be a subset of $\Omega$. Then

$$P(A) = \sum_{\omega \in A} P(\omega)$$

Events from the same sample space are assumed mutually exclusive. For example, the outcome of a coin is either heads or tail but not both. The interpretation of $P(A)$ is the probability of *some* proposition in $A$ being true.

## Random Variable

A random variable $X$ is a function associating a value to an event, $X: \Omega \rightarrow \mathbb{R}$. If $\Omega$ is countable, we say it's a discrete random variable.

Random variables are usually denoted with a capital letter, for example $X$. The value associated with an event $\omega \in \Omega$ as $X(\omega)$. Usually we abstract away the original set of events and work directly with the image. In this (abused) notation we also assume $X$ is a set, $X = \curly{x = X(\omega) \mid \omega \in \Omega}$.

A special type of random variable is one that encodes membership of a set. More precisely, let $\Omega$ be the sample space and $A$ a subset of it. We denote the random variable $\[A\]$ as one $\[A\](\omega) = 1$ if $\omega \in A$ and $0$ otherwise.

Worth noting that the probability functions do not take in random variables but rather events. There's a special syntax for "turning" a random variable into an event. Suppose $X(\omega) = x$. Then we can denote $\omega$ as $X = x$, so that $P(\omega)$ as $P(X = x)$, which is a more common notation.

A generalization of $X = x$ is $X \in S$, where $S$ is a subset of $X$'s image. This represents the set of events $\omega$ such that $X(\omega) \in S$.

## Joint Probability

The joint probability distribution of two random variables $X$ and $Y$ is denoted by $P(X, Y)$ or $P(X \cap Y)$. The probability of $X = x$ and $Y = y$ is denoted by $P(X = x, Y = y)$.

### Law of Total Probability

The law of total probability states that:

$$P(X = x) = \sum_{y \in D_Y} P(X = x, Y = y)$$

Which holds even when $X$ and $Y$ are not independent.

## Conditional Probability

The probability of a proposition $A$ being true if proposition $B$ is true is denoted by $P(A \mid B)$.

$$P(A \mid B) = \frac{P(A, B)}{P(B)}$$

$A$ and $B$ are often assumed to be random variables, but in reality what's meant is them assuming a specific value, e.g.

$$P(X = x \mid Y = y) = \frac{P(X = x, Y = y)}{P(Y = y)}$$

## OR Probability

The probability distribution of either one of two random variables $X$ or $Y$ is denoted by $P(X \cup Y)$. It can be defined in terms of joint probability:

$$P(X \cup Y) = P(X) + P(Y) - P(X \cap Y)$$

## Expectation

Let $X$ be a random variable with possible values $x \in X$ with probability distribution $P(X = x)$. The expected value of $X$, denoted by $E[X]$ is defined as:

$$E[X] = \sum_{\omega \in \Omega} X(\omega) P(\omega)$$

Using a shorthand syntax:

$$E[X] = x \sum_{x} P(X=x)$$

### Conditional Expectation

Let $X$ be a random variable and set of events $A$. The conditional expectation is defined as:

$$E[X \mid A] = \sum_{\omega \in A} X(\omega) \frac{P(\omega)}{Pr(A)}$$

For example, the expected value of a dice throw assuming the outcome is even. In this case $A = \curly{2, 4, 6}$ and $P(A) = 1/2$, with $X$ being the identity function. This gives us:

$$E[X \mid A] = (2 + 4 + 6) \frac{1/6}{1/2} = 4$$

Note that if $A = \Omega$, since $P(\Omega) = 1$ we have $E[X \mid \Omega] = E[X]$.

If $Y$ is another random variable and we know how to compute, $E[X \mid Y = y]$, we can write:

$$E[X] = \sum_{y} E[X \mid Y = y] P(Y = y) = \sum_{y}\sum_{x} x P(X = x \mid Y = y) P(Y = y)$$

In [1], Knuth introduces the syntax $E[X \mid Y]$ where both $X$ and $Y$ are random variables. Let $\Omega'$ be the sample space of $Y$. We define another random variable $Y'$ with the same sample space and probability distribution as $Y$ but with value $Y'(\omega) = E(X \mid \omega)$, for $\omega \in \Omega'$. We then defined $E[X \mid Y] = Y'$.

Then we claim that $E[X] = E[E[X \mid Y]]$.

<proof>

We compute the expectation of $E[X \mid Y] = Y'$:

$$E[E[X \mid Y]] = E[Y'] = \sum_{\omega \in \Omega'} Y'(\omega) P(\omega)$$

Replacing by the definition of $Y'(w)$:

$$ = \sum_{\omega \in \Omega'} E[X \mid \omega] P(\omega)$$

Since $Y = y$ is $\omega$,

$$ = \sum_{y} E[X \mid Y = y] P(Y = y) = E[X]$$

Thus $E[X \mid Y] = E[X]$. QED.

</proof>


### Properties

**Additivity.**

$$E[X + Y] = E[X] + E[Y]$$

**Law of the Unconscious Statistician.**  This is useful to compute the expectation of $g(X)$ when we don't know the probability distribution of $g(X)$ but we do of $X$:

$$E[g(X)] = \sum_{i = 1}^{n} P(X=x_i) g(x_i)$$

**Markov's Inequality.** Let $X$ be a random variable and a non-negative function $f(X)$. Suppose $f(x) \ge s \gt 0$ for $x \in S$. Then:

$$P(X \in S) \le \frac{E [f(X)]}{s}$$

<proof>
We can split the set of values of $X$ into $S$ and $\overline{S} = X \setminus S$. Then

$$E[f(x)] = \sum_{x \in S} f(x) Pr(X = x) + \sum_{y \in \overline{S}} f(y) Pr(X = y)$$

In the first sum we know $f(x) \ge s$ by hypothesis and since it is non-negative $f(y) \ge 0$, so:

$$\ge \sum_{x \in S} s Pr(X = x) + \sum_{y \in \overline{S}} 0 Pr(X = y) = s \sum_{x \in S} Pr(X = x) = s Pr(X \in S)$$

Thus

$$E[f(x)] = s Pr(X \in S)$$

QED.
</proof>

## Likelihood

Let $X$ be a discrete random variable, with probability distribution depending on a parameter $\theta$ (not necessarily a scalar). For example, a biased coin could have probability distribution $p_H = \theta$ and $p_T = 1 - \theta$.

The likelihood is a function of a specific value $x$ from domain $D$ and $\theta$, denoted as $\mathcal{L}(\theta \| x)$, representing the probability of $X$ assuming the value of $x$.

$$\mathcal{L}(\theta | x) = P_{\theta}(X = x)$$

For the biased coin above, suppose $\theta = 0.6$. The $\mathcal{L}(\theta \| H) = 0.6$.

# Continuous

## Random Variable

A continuous random variable is a variable that can be a value of a continuous domain, for example, $\mathbb{R}$.

## Probability Distributions

For continuous random variable we can't assign probabilities to specific values of $X$ because it would be 0. Instead we use a continuous function, $f_X(x)$, defined as **probability density function**.

To compute the probability define the probability in terms of intervals,

$$P[a \le X \le b] = \int_{a}^{b} f_X(x) dx$$

The **cumulative distribution function** or CDF, denoted by $F_X(x)$, is the cumulative probability of $X$ being in the interval from its lowest value to $x$, and can be defined as:

$$F_X(x) = P[X \le x] = \int_{-\infty}^{x} f_X(u) du$$

### Properties

* The cumulative distribution function must add up to 1 over the entire domain of $X$:

$$F_X(\infty) = \int_{-\infty}^{\infty} f_X(u) du = 1$$

* CDF is monotonic increasing, so if $x_1 < x_2$, $F_X(x_1) \le F_X(x_2)$.

## References

* [1] The Art of Computer Programming - Volume 4B - Part 2
