---
layout: post
title: "Discrete Probability As Counting"
tags: [probability]
excerpt_separator: <!--more-->
vanity: "2022-06-24-discrete-probability-as-counting"
---
{% include blog_vars.html %}

In this post we'd like to revisit some probability theory for discrete cases using combinatorics to build an intuition. I find it hard to grasp probability but have an easier time with discrete mathematics.

<!--more-->

## Discrete Random Variables

A discrete random variable is a variable that can be a value of a countable domain $D$. For example, the outcome of a dice throw.

## Probability Distributions

The probability distribution for a discrete random variable $X$ is denoted by $P(X)$. The probability of $X$ being equal to a given value $x \in D$ is denoted by $P(X = x)$, with $0 \le P(X = x) \le 1$.

If we assume $P(X = x) \in \mathbb{Q}$, we can see it as the ratio of the count of events over the total number of events. Let $C_x$ correspond to the count of events where $X = x$. Then $P(X = x)$ is

$$P(X = x) = \frac{C_x}{\sum_{x' \in D} C_x'}$$

For example, what is the probability of a fair dice landing on face 4? There is *one* event corresponding to the face 4 and 6 events in total, so the ratio of "face 4" events over the total events is $1/6$, which is also the probability of observing "4" when rolling a dice.

Another example: what is the probability of observing an odd label when rolling a dice? There are three events corresponding to faces with odd labels, so the probability of observing an odd label when rolling a dice is the ratio $3/6$ or $0.5$.

### Biased Coin

Suppose we have a biased coin where the probability of landing on heads is $0.6$ and on tails $0.4$. We can assume instead a case where we have a urn with 10 balls, 6 of which are blue, 4 of which are red.

The probability of landing on heads (tails) is analogous to selecting a blue (red) ball at random from the urn, and they can be modeled by the same random variable.

As long as every value of the probability distribution is a rational number, we can always imagine a sufficiently large urn that can be modeled the same way.

### Sum of All Probabilities

One property that follows naturally from our definition is that the sum of probabilities of all values of $X$ adds up to 1:

$$\sum_{x \in D} P(X = x) = \frac{\sum_{x \in D} C_x}{\sum_{x' \in D} C_x'} = 1$$

## Joint Probability

The joint probability distribution of two random variables $X$ and $Y$ is denoted by $P(X, Y)$. The probability of $X = x$ and $Y = y$ is denoted by $P(X = x, Y = y)$.

Let $D_X$ and $D_Y$ be the domain of $X$ and $Y$, respectively. We can model a joint probability distribution as the probability distribution of a 2-dimensional random variable, say $Z$, with domain $D_{XY} =  D_X \times D_Y$.

For each $(x, y) \in D_{XY}$, we can define $C_{xy}$ as the count of events where $Z = (x, y)$. The total number of events is given by ${\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}}$, so we have

$$(1) \quad P(X = x, Y = y) = P(Z = (x, y)) = \frac{C_{xy}} {\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}}$$

### Law of Total Probability

The law of total probability states that:

$$P(X = x) = \sum_{y \in D_Y} P(X = x, Y = y)$$

Which holds even when $X$ and $Y$ are not independent.

The intuition behind this is as follows: suppose we select a random human out of the living population and want to determine the probability their height is $h$. Consider height and weight random variables $H$ and $W$ from domains $D_H$ and $D_W$, respectively.

Suppose we have a table listing all possible combinations of height $h \in D_H$ and weight $w \in D_W$, with the count of people with these attributes, denoted by $C_{hw}$.

To determine how many people have a given height $h$, we sum the $C_{hw'}$ for all $w' \in W$. The insight is that every one with height $h$ must have a corresponding weight $w \in W$ and that is accounted for in $C_{hw}$. Further, each person has exactly one weight, so they're not double counted.

The probability of a random person having height $h$ is thus that sum divided by the total population (sum of all $C$'s).

$$P(H = h) = \frac{\sum_{w' \in D_W} C_{hw'}}{\sum_{h' \in D_H} \sum_{w' \in D_W} C_{h'w'}} = \sum_{w' \in D_W} P(H = h, W = w')$$

Note that human weight and height are not independent variables.


## Conditional Probability

The conditional probability distribution of a random variable $X$ on random variable $Y$ is denoted by $P(X \mid Y)$. It assumes the value of $Y$ is determined a priori. In the event analogy the effect of this knowledge is that we can narrow down the domain of $Y$ to a single element, i.e. $D_Y = \curly{y}$.

We'll show that conditional probability can be defined as a function of joint probabilities:

$$P(X \mid Y) = \frac{P(X, Y)}{P(Y)}$$

So $P(X = x \mid Y = y)$ is the ratio of events with value $x$ and $y$ divided by the number of events having $Y = y$, so:

$$(2) \quad P(X = x \mid Y = y) = \frac{C_{xy}} {\sum_{x' \in D_X} C_{x'y}}$$

From (1) we have $C_{xy} = P(X = x, Y = y) \sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}$. Replacing in (2):

$$\quad P(X = x \mid Y = y) = \frac{P(X = x, Y = y) \sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}} {\sum_{x' \in D_X} C_{x'y}} = $$

$$(3) \quad P(X = x, Y = y) (\frac{\sum_{x' \in D_X} C_{x'y}}{\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}})^{-1}$$

We have that

$$(4) \quad \frac{C_{x'y}}{\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}} = P(X = x', Y = y)$$

Replacing (4) in (3):

$$P(X = x, Y = y) (\sum_{x' \in D_X} P(X = x', Y = y))^{-1} = \frac{P(X = x, Y = y)}{\sum_{x' \in D_X} P(X = x', Y = y)}$$

By the *Law of Total Probability* $\sum_{x' \in D_X} P(X = x', Y = y) = P(Y = y)$, so:

$$(5) \quad P(X = x \mid Y = y) = \frac{P(X = x, Y = y)}{P(Y = y)}$$

### Bayes' Theorem

The Bayes' Theorem states that:

$$P(X \mid Y) = \frac{P(Y \mid X) P(X)}{P(Y)}$$

It's easy to prove this by using the conditional probability identity. First we use (5):

$$(6) \quad P(X \mid Y) = \frac{P(X, Y)}{P(Y)}$$

We now use (5) with the arguments reversed. Not that the order or arguments in a joint probability does not matter, so:

$$P(Y \mid X) = \frac{P(X, Y)}{P(X)}$$

Isolating $P(X, Y)$:

$$P(X, Y) = P(Y \mid X) P(X)$$

Replacing in (6):

$$P(X \mid Y) = \frac{P(Y \mid X) P(X)}{P(Y)}$$

## OR Probability

The probability distribution of either one of two random variables $X$ or $Y$ is denoted by $P(X \cup Y)$. It can be defined in terms of joint probability:

$$(7) \quad P(X \cup Y) = P(X) + P(Y) - P(X \cap Y)$$

It's useful as we did in *Join Probability* to define a 2-dimensional random variable, say $Z$, with domain $D_{XY} =  D_X \times D_Y$. We want to compute the ratio of events where $Z = (x, y')$ for any $y' \in D_Y$ or $Z = (x', y)$ for any $x' \in D_X$, over the total ${\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}}$.

The insight is that events where $Z = (x, y)$ will be counted twice, so we need to subtract them. We can compute such ratio as:

$$P(X \cup Y) = \frac{ \sum_{y' \in D_Y} C_{xy'} + \sum_{x' \in D_X} C_{x'y}  - C_{xy} } {\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}}$$

We can split into three individual sums:

$$=
\frac{ \sum_{y' \in D_Y} C_{xy'} } {\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}} +
\frac{ \sum_{y' \in D_Y} C_{xy'} } {\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}} -
\frac{ C_{xy} } {\sum_{x' \in D_X} \sum_{y' \in D_Y} C_{x'y'}}
$$

The first sum is $P(X = x)$ and the second sum is $P(Y = y)$ by the law of total probability. The third sum is the joint probability $P(X = x, Y = y)$ by (1), which leads to (7).

## Probability and Relational Algebra

The idea of counting events reminds me of relational algebra. We can draw some parallels between probability and relational algebra by using SQL queries. For example suppose we have a table for all events associated with a random variable, with columns `id` and `value`.

For example, for a dice we could have have a table [dice.csv]({{github}}/dice.csv):

{% highlight text %}
| id | value |
| -- | ----- |
| 1  | 1     |
| 2  | 2     |
| 3  | 3     |
| 4  | 4     |
| 5  | 5     |
| 6  | 6     |
{% endhighlight %}

In this case `id` and `value` coincide, but if we take a urn with 6 red balls and 4 blue balls, we would have 10 rows with distinct ids but only 2 distinct values. Now, to determine the probability of the dice landing on face 4 we can do, using `sqlite3` (tested with v3.37):

{% highlight sql %}
SELECT
  1.0*COUNT(*)/(SELECT COUNT(*) FROM dice)
FROM dice
WHERE value = 4
{% endhighlight %}

We can actually run this [query]({{github}}/query1.sh) against a CSV file [3], via

{% highlight text %}
sqlite3 :memory: -cmd '.mode csv' -cmd '.import <file>.csv <table name>' '<query>`
{% endhighlight %}

For joint probability we can do a `JOIN` between two event tables. For example, for independent dice throws, let's compute the probability of getting faces 4 and 2 respectively. First we compute the joint table, `dices`, then we perform the [query]({{github}}/query2.sh):

{% highlight sql %}
WITH dices AS (
  SELECT d1.id, d2.id, d1.value as v1, d2.value as v2
  FROM dice d1, dice d2
)
SELECT
  1.0*COUNT(*)/(SELECT COUNT(*) FROM dices)
FROM dices
WHERE v1=4 and v2=2
{% endhighlight %}

Which returns $1/36$.

If the event tables are not independent we need to account for it in the `ON` clause of the join. For example, if we have a table `height` and `weight` and the event `id` identifies a specific human, to generate the joint table we would do:

{% highlight sql %}
SELECT h.id, h.value as hv, w.value as wv
FROM height h JOIN weight w ON h.id = w.id
{% endhighlight %}

For conditional probability, we can add a where clause when creating the joint table. Suppose we want to determine the probability of selecting a person with weight 80Kg given we know their height is 180cm.

Assume we have tables [height.csv]({{github}}/height.csv) and [weight.csv]({{github}}/weight.csv) with rows:

{% highlight text %}
height            weight
| id | value |    | id | value |
| -- | ----- |    | -- | ----- |
| 1  | 150   |    | 1  | 60    |
| 2  | 150   |    | 2  | 70    |
| 3  | 180   |    | 3  | 70    |
| 4  | 180   |    | 4  | 80    |
{% endhighlight %}

We can add `WHERE h.value=180` to the `human` table and then the final query is the same as a normal random variable. The complete [query]({{github}}/query3.sh) is given by:

{% highlight sql %}
WITH human AS (
  SELECT h.id, h.value as hv, w.value as wv
  FROM height h JOIN weight w ON h.id = w.id
  WHERE h.value=180
)
SELECT
  1.0*COUNT(*)/(SELECT COUNT(*) FROM human)
FROM human
WHERE wv=80
{% endhighlight %}


## Limitations

As we mentioned earlier, we assume the probability $P(X = x)$ is a rational number so we can make analogies with counting events. If the probability is given by, say $P(X = x) = \pi$, then we could in practice approximate it by a ratio of very large numbers but in theory it cannot be done.

Thus, the arguments we provided above are not rigorous proofs for the general case. There's a branch of mathematics called [measure theory](https://en.wikipedia.org/wiki/Measure_(mathematics)) of which I know nothing about, but it seems to generalize the idea of counts or probabilities associated with events to general scalars (metrics) satisfying some basic constraints.

## Conclusion

I ran into some proof requiring basic probability understanding and realized I didn't remember their definitions. I then decided to create a [sheet cheat]({{site}}/docs/math/probability.html) for definitions and identities and noticed I didn't have a good intuition behind them.

This post is my attempt to make sense of them.

## References

* [[1](https://en.wikipedia.org/wiki/Conditional_probability)] Wikipedia - Conditional probability
* [[2](https://en.wikipedia.org/wiki/Law_of_total_probability)] Wikipedia - Law of total probability
* [[3](https://til.simonwillison.net/sqlite/one-line-csv-operations)] Simon Willison's TILs - One-liner for running queries against CSV files with SQLite
