---
layout: doc
title: "Probability Cheat Sheet"
---

# Discrete

## Random Variable

Random variables are usually denoted with a capital letter, for example $X$. A discrete random variable is a variable that can be a value of a countable domain $D$. For example, the outcome of a dice throw.


## Probability Distributions

The probability distribution for a discrete random variable $X$ is a value associated to each value $x$ of $X$, and denoted by $P(X = x)$. For example, for a dice throw the probability distribution is $1/6$ for each side.

## Joint Probability

The joint probability distribution of two random variables $X$ and $Y$ is denoted by $P(X, Y)$ or $P(X \cap Y)$. The probability of $X = x$ and $Y = y$ is denoted by $P(X = x, Y = y)$.

### Law of Total Probability

The law of total probability states that:

$$P(X = x) = \sum_{y \in D_Y} P(X = x, Y = y)$$

Which holds even when $X$ and $Y$ are not independent.

## Conditional Probability

The conditional probability distribution of a random variable $X$ on random variable $Y$ is denoted by $P(X \mid Y)$. It assumes the value of $Y$ is determined a priori. It can be defined as a function of joint probabilities:

$$P(X \mid Y) = \frac{P(X, Y)}{P(Y)}$$

## OR Probability

The probability distribution of either one of two random variables $X$ or $Y$ is denoted by $P(X \cup Y)$. It can be defined in terms of joint probability:

$$P(X \cup Y) = P(X) + P(Y) - P(X \cap Y)$$

## Expectation

Let $X$ be a discrete random variable with possible values $x_1, \cdots, x_n$ with probability distribution $p_1, \cdots, p_n$. The expected value of $X$, denoted by $E[X]$ is defined as:

$$E[X] = \sum_{i = 1}^{n} P(X=x_i) x_i$$

### Additivity

$$E[X + Y] = E[X] + E[Y]$$

### Law of the Unconscious Statistician

This is useful to compute the expectation of $g(X)$ when we don't know the probability distribution of $g(X)$ but we do of $X$:

$$E[g(X)] = \sum_{i = 1}^{n} P(X=x_i) g(x_i)$$

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
