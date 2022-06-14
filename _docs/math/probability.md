---
layout: doc
title: "Probability Cheat Sheet"
---

## Random Variable

Random variables are usually denoted with a capital letter, for example $X$.

### Discrete

A discrete random variable is a variable that can be a value of a countable domain $D$. For example, the outcome of a dice throw.

## Probability Distributions

### Discrete

The probability distribution for a discrete random variable $X$ is a value associated to each value of $X$. For example, for a dice throw the probability distribution is $1/6$ for each side.

## Expectation

### Discrete

Let $X$ be a discrete random variable with possible values $x_1, \cdots, x_n$ with probability distribution $p_1, \cdots, p_n$. The expected value of $X$, denoted by $E[X]$ is defined as:

$$E[X] = \sum_{i = 1}^{n} p_i x_i$$

## Likelihood

### Discrete

Let $X$ be a discrete random variable, with probability distribution depending on a parameter $\theta$ (not necessarily a scalar). For example, a biased coin could have probability distribution $p_H = \theta$ and $p_T = 1 - \theta$.

The likelihood is a function of a specific value $x$ from domain $D$ and $\theta$, denoted as $\mathcal{L}(\theta \| x)$, representing the probability of $X$ assuming the value of $x$.

$$\mathcal{L}(\theta | x) = P_{\theta}(X = x)$$

For the biased coin above, suppose $\theta = 0.6$. The $\mathcal{L}(\theta \| H) = 0.6$.
