---
layout: post
title: "Pell Equation"
tags: [number theory]
---

Diophantine equations are polynomial equations that only admit whole solutions. In this post, I’ll comment on three particular cases and focus on one, known as Pell's equations.

## Pythagorean triplets

The best known type of Diophantine equations is of the form:

$$x ^ 2 + y ^ 2 = z ^ 2$$

The integer solutions to this equation are known as [Pythagorean triplets](https://en.wikipedia.org/wiki/Pythagorean_triple), from the Pythagorean theorem regarding the relationship between the lengths of the sides of a right triangle.
For the more general case, of the form $ x ^ n + y ^ n = z ^ n $ with $ n > 2 $, the famous [Fermat's last theorem](https://en.wikipedia.org/wiki/Fermat%27s_Last_Theorem) says that there are no whole solutions.

## Bézout's identity

Another known type is the linear Diophantine equations of two variables, for example, $ x $ and $ y $, of the form

$$ ax + by = c $$

Where $ a, b, c $ are integers. In the case where $ c $ is the greatest common factor of $ a, b $, we have [Bézout's identity](https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity). It is possible to find an integer $ x, y $ pair using the [extended Euclid's algorithm](https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm).

Note however that there are infinite integer solutions to this equation, since if $ x, y $ is a solution, $ x '= x + b $ and $ y' = y - a $ is also a solution.

## Pell's equation

I only recently heard about another particular case, known as Pell's equations, of the form:

$$(1) \quad x ^ 2 - ny ^ 2 = 1 $$

where $ n $ is a positive integer. If $ n $ does not have an exact root (i.e. $\sqrt(n)$ is not integer), then there are infinite integer solutions $ x, y $ (if $ n $ has an exact root you can show that the only solution is $ x = \ pm 1 $ and $ y = 0 $). We will present an algorithm to find the solutions for this particular case of $ n $.

## Continued fractions

A concept used in the algorithm is that of [continued fractions](https://en.wikipedia.org/wiki/Continued_fraction), which can be used to approximate an irrational number through fractions.

Given a number $d$, the coefficients of its continued fraction, $ a_0, a_1, \ cdots, a_n $ are positive and can be calculated using recurrences. Let $a_0 = \lfloor d \rfloor$ and $r_0 = c$. Then for $ i \ge 1 $:

$$ r_i = \frac {1}{r_{i-1} - a_{i-1}} $$

$$ a_i = \lfloor r_i \rfloor $$

We can represent a continued fraction by its coefficients, that is, $ [a_0, a_1, a_2, \ cdots] $. Incredibly, in the particular case in which the irrational number is of the form $ \sqrt{n} $, it can be proved that the coefficients of its continued fraction are periodic, that is $ \sqrt{n} = [a_0, \ overline {a_1, a_2, \ cdots, a_{r-1}, a_{r}}] $ and $ a_{r} = 2a_0$.

As an example, for $ n = 14 $ we have $ \sqrt{14} = [3, \overline {1,2,1,6}] $

It is possible to explicitly calculate the numerator $ p_i $ and the denominator $ q_i $ of the continued fraction with $ i $ terms, through recurrences:

$$
\begin{array} {lcl}
p_0 & = & a_0 \\
p_1 & = & a_1 a_0 + 1 \\
p_n & = & a_n p_{n-1} + p_{n-2}
\end{array}
$$

and

$$
\begin{array} {lcl}
q_0 & = & 1 \\
q_1 & = & a_1 \\
q_n & = & a_n q_ { n-1} + q_ {n-2}
\end{array}
$$

The fraction $ p_i / q_i $ is called the $n$-th convergent. Returning to the example for $ n = 14 $, we have:

$$
\begin{array} {llcl}
i: & p_i / q_i & & \\
0: & 3/1 & = & 3.0 \\
1: & 4 / 1 & = & 4.0 \\
2: & 11/3 & = & 3.66666666667 \\
3: & 15/4 & = & 3.75 \\
4: & 101/27 & = & 3.74074074074
\end{array}
$$

Where $ \sqrt{14} $ is approximately $ 3.74165769645 $

## A solution to the Pell equation

Consider the coefficients of the continued fraction of $ \sqrt{n} $ and $ r $ the index from which the coefficients are periodic.
If $ r $ is even, let $ x = p_{r-1} $ and $ y = q_{r-1} $. Otherwise, be $ x = p_{2r-1} $ and $ y = q_{2r-1} $. Surprisingly, it's possible to show that $ x, y $ represent the smallest positive integer solution for (1), which we'll call the *fundamental solution*.

### Finding the fundamental solution

Given these properties, an algorithm to find a solution for (1), consists of iterating over the $ p_i $ and $ q_i $ coefficients, until you find $ r $ such that $ a_r = 2a_0 $. According to Lenstra [2], $ r \in O (\sqrt{n} \log n) $, that is, it has complexity pseudo-polynomial. Thus, if $ d $ is the number of bits needed to represent $ n $, we have to $ r \in O(2^{d / 2} d) $, that is, even ignoring the complexities of the arithmetic operations in code, our algorithm is exponential in the number of bits.

### Generating more solutions

Given a fundamental solution $ (x_1, y_1) $ , Lenstra [2] states that if we sort the solutions by magnitude, then the $k$-th solution $ (x_k, y_k) $ is such that

$$(2) \quad x_k + \sqrt{n} y_k = (x_1 + \sqrt{n} y_1)^k$$

Since both $ (x_1, y_1) $ and $ (x_k, y_k) $ are solution for (1), we have:

$$ x_k^2 - ny_k^2 = (x_1^2 - ny_1^2)^k = 1 $$

Factoring we have,

$$ (x_k + \sqrt{n} y_k) (x_k - \sqrt{n} y_k) = (x_1 + \sqrt{n} y_1)^k (x_1 - \sqrt{n} y_1)^k $$

From (2) we conclude that

$$(3) \quad x_k - \sqrt{n} y_k = (x_1 - \sqrt{n} y_1)^k $$

Solving for (2) and (3), we arrive at:

$$
\begin{array} {lcl}
x_k & = & \frac {(x_1 + y_1 \sqrt{n})^k + (x_1 - y_1 \sqrt{n})^k} {2} \\
\\
y_k & = & \frac {(x_1 + y_1 \sqrt{n})^k - (x_1 - y_1 \sqrt{n})^k} {2 \sqrt {n}}
\end{array}
$$

### Implementation

Based on the theory presented above, it is simple to write an algorithm. I added a python implementation to my personal number theory library, available on [Github](https://github.com/kunigami/pynumbers/blob/master/pell.py).

I found a very interesting post about Pell and Haskell equations here [3]. To practice, I decided to implement my version before looking at the post code. I had to deal with the fact that Haskell does not convert from floating point and integer types automatically. It is necessary to do this explicitly and that is why the `fromIntegral()` function in the code of this [link](https://github.com/kunigami/Blog-Examples/blob/master/pell-equations/pell.hs).

## References

* [[1](https://mathworld.wolfram.com/PellEquation.html)] Wolfram - Pell Equation
* [[2](http://www.ams.org/notices/200202/fea-lenstra.pdf)] Solving the Pell Equation - HW Lenstra Jr.
* [[3](https://www.akalin.com/number-theory-haskell-foray)] A Foray into Number Theory with Haskell
