---
layout: doc
title: "Computational Complexity Cheat Sheet"
---

{% include blog_vars.html %}

Common definitions and results in the area of Computational Complexity Theory.

## Decision vs. Optimization problems

A decision problem is one for which the answer is either "yes" or "no". An optization problem is one in which a (optimal) value is returned.

An optmization problem $P$ can be reduced to a decision one, by re-stating it as: is there a solution for $P$ with value at least $v$? And then binary search to find the optimal (max/min).

## Complexity class

Decision problems can be arranged into classes or sets. The relationship between the sets listed below is as follows:

$$\mathbf{P} \subseteq \mathbf{NP} \subseteq \mathbf{PSPACE}$$

### Class $\mathbf{P}$

Is the set of decision problems that can be solved by a deterministic Turing machine in polynomial time on the size of the input.

### Class $\mathbf{NP}$

Is the set of decision problems for which a given input can be determined yield to "yes" or "no" by a deterministic Turing machine in polynomial time.

**$\mathbf{NP}$-Complete.** A problem in $\mathbf{NP}$ is $\mathbf{NP}$-complete if there's a polynomial-time reduction from any problem in $\mathbf{NP}$ to it.

**$\mathbf{NP}$-Hard.** A problem is $\mathbf{NP}$-hard if there's a polynomial-time reduction from any problem in $\mathbf{NP}$ to it. The difference with $\mathbf{NP}$-complete is that problems in $\mathbf{NP}$-hard do not need to be in $\mathbf{NP}$ (nor be decision problems), but by definition $\mathbf{NP}\mbox{-complete} \subseteq \mathbf{NP}\mbox{-hard}$.

**Weakly $\mathbf{NP}$-Complete.** A weakly $\mathbf{NP}$-complete is a problem in $\mathbf{NP}$-complete that can be solved via a pseudo-polynomial-time algorithm, i.e. an algorithm that depends on the value of the instance, not just on its size.

A classic example is the [integer knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem). A problem in $\mathbf{NP}$-complete that is not weakly, is **strongly** $\mathbf{NP}$**-complete**.

### Class $\mathbf{PSPACE}$

$\mathbf{PSPACE}$ is the set of all decision problems that can be solved by a Turing machine using a polynomial amount of *space*.

Since it doesn't restrict how long a problem can take to be solved, all $\mathbf{NP}$ are included.
