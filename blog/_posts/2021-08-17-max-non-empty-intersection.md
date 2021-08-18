---
layout: post
title: "Maximum Non-Empty Intersection of Constraints"
tags: [combinatorial optimization, computational complexity, integer programming, puzzle]
vanity: "2021-08-17-max_non_empty_interval_intersection"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

Here's a puzzle I thought about recently: given a set of constraints of the form $ax \le b$, where $a, b \in \mathbb{R}$, find the maximum number of constraints we can choose such there exists $x \in \mathbb{R}$ that satisfies all of them.

For example, given $x \le 2$, $3x \le 7$, $-2x \le 1$, $-2x \le -9$, we can choose $x \le 2$, $x \le 3$, $-2x \le 1$, then any $\frac{-1}{2} \le x \le 2$ satisfies all 3 of them. This also happens to be the maximum number of constraints we can choose.

In this post we'll explore a solution to this puzzle. Feel free to stop here and solve it before proceeding.

<!--more-->

## Solution

We can simplify the problem a bit by normalizing the coeficient of $x$ so that all constraints are of either $x \le a$ or $x \ge a$ form. For example, $3x \le 7$ is normalized to $x \le \frac{7}{3}$ and $-2x \le 1$ to $x \ge -\frac{1}{2}$.

The key observation is that in our solution we can't have a pair $x \ge a$ and $x \le b$ if $b < a$ since their intersection is already empty. This implies that in our solution, all the right-hand side (RHS) of the "$\ge$" constraints will be smaller or equal to the RHS of the "$\le$" constraints.

Let's sort the constraints by their RHS and break ties by having "$\ge$" show up before "$\le$" (since we could pick both $x \ge a$ and $x \le a$ for the intersection $x = a$).

We now need to find the maximum number of "$\ge$" followed by "$\le$" constraints. Another way to frame this is: given a string of $0$s (corresponding to "$\ge$") and $1$s (corresponding to "$\le$") , find a subsequence such that it starts with all $0$s and ends with all $1$s. In other words, it matches the regular expression `\0*1*\`, so this is the problem we'll be solving now.

### Induction

Suppose our input is a string $s$ of length $n$. Let $s_i$ represent the $i$-th character ($i = 1, \cdots, n$) and $s_{i,j}$ the substring $s_i, \cdots, s_j$ ($1 \le i \le j \le n$). Let's call any string satisfying the regex `\0*1*\` a **valid** string.

Let $u_i$ be the length of the largest *valid* substring of $s_{1,i}$ that ends in $1$ and $z_i$ the length of the largest *valid* substring of $s_{1,i}$ that ends in $0$. The largest *valid* substring of $s$ has to end in either $0$ or $1$, so the solution to the problem is $\max(u_n, z_n)$.

Since we cannot have a $1$ following a $0$, the only valid substring ending in $0$ is one with all $0$s, so $z_i$ is equivalent to how many $0$s there are in $s_{1,i}$.

How about $u_i$? Suppose we know how to compute $u_k$ for $k < i$. Then if $s_i = 0$, there's nothing we can do to extend $u_i$, so $u_i = u_{i-1}$. Otherwise, we either add $1$ to the substring represented by $u_{i-1}$ or we add it to the one full of zeros in $z_{i-1}$.

Why is that optimal? Suppose it's not. Then there exists a valid substring whose length is greater than $\max(u_{i-1}, z_{i-1}) + 1$. If the second to last character of that string is a $0$, we found a valid substring of $s_{1,i-1}$ greater than $z_{i-1}$, if it's $1$, we found a valid substring of $s_{1,i-1}$ greater than $u_{i-1}$, thus in both cases we get a contradiction.

### Code

We can solve this problem in $O(n)$ using constant extra memory since we only ever depend on the previous index:

{% highlight python %}
z = 0
u = 0
for c in s:
    if c == '0':
        z += 1
    else:
        u = max(u, z) + 1

return max(u, z)
{% endhighlight %}

This allows us solving the original problem in $O(n \log n)$ (dominated by the sorting step).

## Generalizations

What if instead of $1$-d constraints we had $n$ dimensional ones? More formally, given a set of constraints

$$a_{i, 1} x_1 + \cdots + a_{i, m} x_m \le b_i$$

For $i = 1, \cdots, n$, and we want the largest subset of these constraints such that there exists $\vec{x} = x_1, \cdots, x_m$ satisfying all of them.

Another way to frame it in terms of combinatorial optimization: given a Linear Program, what is the minimum number of constraints we need to remove to have a feasible solution?

Removing a constraint is akin to relaxing the problem and we want to minimize the number of relaxations, so we'll name our problem the *Least Relaxed Linear Program* or *LRLP* for short.

### NP-Completeness

We'll prove that LRLP is NP-Complete by reducing a known NP-Complete to it. Our choice is the *0-1 Integer Linear Program Feasibility* [1]. Consider an Integer Linear Program (ILP) defined by the set of constraints

$$(1) \quad A\vec{x} \le b$$

where $A$ is a $n \times m$ coefficient matrix of real values, $b$ is a vector $\mathbb{R}^n$ and $\vec{x}$ is a $m$-vector of 0 or 1, i.e. $\vec{x} \in \curly{0, 1}^m$. The problem consists of deciding whether there exists any $\vec{x}$ satisfying these constraints.

We can solve this problem by reducing to LRLP. We include the original constraints (1) but relax the 0-1 integrality constraints by having $\vec{x} \in\mathbb{R}^m$. We then add $2m$ new constraints:

$$(2) \quad x_j = 1 \quad j = 1, \cdots, m$$

$$(3) \quad x_j = 0 \quad j = 1, \cdots, m$$

Recall that an equality constraint can be implemented by 2 inequalities.

Now, if the original ILP has a feasible integer solution, then it's a candidate solution to the target LRLP, satisfying all $n$ of the (1) constraints plus exactly $m$ of the constraints between (2) and (3).

Conversely, if the target LRLP can satisfy $n + m$ constraints (note this is an upper bound), it's easy to construct a feasible integer solution to the original ILP.

Hence, the original ILP has a feasible solution if and only if LRLP satisfies $n + m$ constraints.

## Related Posts

* [Lagrangean Relaxation - Theory]({{site.url}}/blog/2012/02/05/lagrangean-relaxation-theory.html). This problem seems related to the Lagrangean Relaxation, in which we remove some of the constraints and add them to the objective function and penalize violated constraints. How good of a solution would we get if we relaxed all the constraints and tried to optimize it? In theory the multipliers would be real values, so we might end up picking fractions of constraints, but I wonder if in practice it would yield good approximations.

## References

* [[1](https://math.stackexchange.com/questions/2969290/maximizing-the-total-number-of-feasible-constraints-of-a-linear-program
)] Mathematics - Maximizing the total number of feasible constraints of a linear program
