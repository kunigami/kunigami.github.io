---
layout: post
title: "Probabilistic Graphical Model Lecture Notes - Week 2"
tags: [artificial intelligence]
vanity: "2012-03-31-pgm-lecture-notes-week-2"
excerpt_separator: <!--more-->
---
{% include blog_vars.html %}

After writing/studying the videos of the classes corresponding to the first week, I went to do the practical and theoretical exercises. I learned a little about Octave and got to know the SAMIAM tool, a graphical interface to facilitate the manipulation of Bayesian networks.

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2012-03-18-pgm-lecture-notes-week-1/course-logo.png" alt="PGM Logo" />
</figure>


# Structured CPDs

A CPD (Conditional Probability Distribution) is normally represented through a table, considering all possible combinations of values of the variables on which it depends. In the week 2 lectures other types of CPDs are presented.

## Fundamentals

An important concept that will be used in the next sections is **context-specific independence**. In some cases two variables are independent only if the observed value of another variable has a specific value.

For example, consider a Bayesian network where a random variable is the OR of two other random variables, as shown in Figure 1. Initially, `x1` and `x2` are independent. If we observe `y = 1`, they become dependent because if `x1 = 0` we can infer the value of `x2` (and vice-versa).

On the other hand, if `y =0`, the variables are independent because *independent* of the value of the other variable, we know that the value of `x1` and `x2` will be 0.

We can think of this kind of context-specific independence as a special case of conditional independence. In the latter, for there to be independence, there must be context-specific independence for *all* combinations of values of the observed variables.

## Deterministic CPDs

The example above is called a **deterministic** CPD. In such cases, the children's values can be computed deterministically from its parents and hence we don't need to include it in the combinations table.

<figure class="center_children">
    <img src="{{resources_path}}/or-bn1.png" alt="Bayesian network"/>
    <figcaption>Figure 1: Deterministic OR</figcaption>
</figure>


## CPD with Tree Structure

In cases where there is no conditional independence between two variables, but there is context-specific independence, we can use a tree representation instead of a table since the number of possible combinations are restricted, for example in Figure 2.

<figure class="center_children">
    <img src="{{resources_path}}/tree-cpd.png" alt="Bayesian network"/>
    <figcaption>Figure 2: CPD as a tree</figcaption>
</figure>

## Multiplexer CPD

Using the tree structure, we can build a graph that represents a multiplexer, as shown in Figure 3. The variable $A$ can assume values from $1$ to $k$, while $Y$ can assume the values $Z_1, Z_2, \cdots, Z_k$. $A$ serves as the multiplexer for $Y$, by indexing the value $Z_i$. In other words, suppose $a$ is the value of $A$. Then $Y$ has value $Z_a$. The corresponding CPD for this graph is: $P(Y = Z_a \mid A = a) = 1$ and $P(Y = Z_a \mid A \neq a) = 0$ for every $1 \le a \le k$.

<figure class="center_children">
    <img src="{{resources_path}}/mutiplexer.png" alt="Bayesian network"/>
    <figcaption>Figure 3: Multiplexer CPD</figcaption>
</figure>

## Noisy-OR CPD

A generalization of the deterministic CPD is the noisy CPD. This models the case where the parent variables do not include 100% the children one.

More concretely, suppose we have variable $X$ that influences $Y$, but not 100% due to some noise. If $X = 1$, then $Y = 1$ but with probability $\lambda$, or $Y = 0$ with probability $1 - \lambda$. If $X = 0$ then $Y = 0$ with 100% chance.

What if we have multiple $X$s each of which can affect the outcome of $Y$? To model this case, we add a "layer" of variables $Z_1, \cdots, Z_k$, where each $Z_i$ is binary, being 1 if and only if, the variable $X_k = 1$ would have set $Y = 1$.

We can also add a variable $Z_0$ to model the fact that $Y$ can be caused by something other than the variables $X$. In this model, $Y$ a deterministic OR of $Z_0, \cdots, Z_k$, as shown in
Figure 4.

<figure class="center_children">
    <img src="{{resources_path}}/noisy-or-cpd.png" alt="Bayesian network"/>
    <figcaption>Figure 4: Noisy OR CPD</figcaption>
</figure>


The CPDs of the variables $Z_k$ for $k > 0$ are $P(Z_k = 1 \mid X_k = 0) = 0$ and $P(Z_k = 1 \mid X_k = 1) = \lambda_k$, where $\lambda_k$ is the noise associated with $X_k$.

$Y$ itself is now deterministically dependent of the $Z$s via the deterministic OR. Thus, it is possible to write the CPD of $Y$ as a function of $X_1, \cdots, X_k$. Since for $Y = 0$ all $Z$s must be 0, we have:

$$P(Y = 0 \mid X_1, \cdots, X_k) = \prod_{i} P(Z_k = 0 \mid X_i)$$

Since $P(Z_k = 1 \mid X_k = 0) = 1$ and $P(Z_k = 0 \mid X_k = 1) = 1 - \lambda_k$ we have

$$P(Y = 0 \mid X_1, \cdots, X_k) =  (1 - \lambda_0) \prod_{i \mid X_i = 1} (1 - \lambda_i)$$

## CPDs for the Continuous Case

If a given variable $Y$ depends on a continuous variable $X$, it is impossible to enumerate all possible values of $X$. In this case, we can define $Y$ as a probability distribution function with $X$ as a parameter.

An example is to use the Gaussian distribution for $Y$, with the mean being the observed value of $X$, say $x$, and a fixed variance, i.e. $Y \sim \mathcal{N}(x, \sigma^2)$.

If $Y$ depends on multiple continuous variables, we can for example use some kind of function of the observed values to define the mean. If $Y$ also depends on a discrete variable, we can have a mixed CPD, where the discrete variables define the rows of the table and in each row there is a distribution with different parameters.
