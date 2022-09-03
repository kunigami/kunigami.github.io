---
layout: doc
title: "Topology Cheat Sheet"
---

# Metric Spaces

We need to revisit metric spaces since some definitions on topological spaces depend on it.

Let $X$ be a non-empty set and $d:X \times X \rightarrow \mathbb{R}$ a [metric function](https://en.wikipedia.org/wiki/Metric_space#Definition). Then $(X, d)$ is a *metric space*.

## Open Ball

Given a metric space $(X, d)$, a point $a \in X$ and a scalar $\delta \gt 0$, an *open ball*, denoted by $B(a, \delta)$ is a set of points $x \in X$ satisfying $d(a, x) \lt \delta$.

## Neighborhood

Given a metric space $(X, d)$, a subset $N$ of $X$ is a *neighborhood of point* $a \in X$ if there exists some $\delta \gt 0$ and $N$ contains $B(a, \delta)$.

## Open Set

Given a metric space $(X, d)$, a subset $O$ of $X$ is an *open set* if it's a neighborhood of its points. That is, for every $a \in O$, there exists some $\delta$ such that $B(a, \delta)  \subseteq O$.

Open sets satisfy the following properties:

* $O1$. $X$ is open
* $O2$. $\emptyset$ is open
* $O3$. The union of a finite or infinite number of open sets is an open set
* $O4$. The intersection of a finite number open sets is an open set

# Topological Spaces

A topological space is a generalization of a metric space. We can obtain a topological space from a metric space by discarding the metric function and instead working with the open sets which can be derived from the metric function.

Not every collection of open subsets of $X$ can be obtained via a metric function, so we can say that all metric spaces are a topological spaces but not the opposite.

## Definition

Let $X$ be a non-empty set and $\tau$ a collection of subsets of $X$. These subsets satisfy the open set properties $O1$-$O4$ which we can state as follows:

* $O1$. $X \in \tau$
* $O2$. $\emptyset \in \tau$
* $O3$. The union of finite or infinite members of $\tau$ is in $\tau$
* $O4$. The intersection of finite members of $\tau$ is in $\tau$

Then $(X, \tau)$ is a *topological space*. $\tau$ is called the *topology* on set $X$. Members of $\tau$ are called *open sets*.

## Neighborhood

Given a topological space $(X, \tau)$ and a subset $N$ of $X$, $N$ is called a *neighborhood* of a point $a \in X$ if it contains an open set that contains $a$. That is, there exists $O \in \tau$ such that $a \in O$ and $O \subseteq N$.

## Complement

Given a topological space $(X, \tau)$ and a subset $F$ of $X$, the *complement* of $F$, $C(F)$ is $X \setminus F$.

## Closed Set

Given a topological space $(X, \tau)$ and a subset $F$ of $X$, is *closed* if the complement of $F$, $C(F)$ is open.

## Hausforff Space

A topological space $(X, \tau)$ is called a *Hausforff space* if for every pair of distinct elements $a, b \in X$, there exists neighborhoods of $a$ and $b$, $N$ and $M$ respectively, such that $N \cap M = \emptyset$.

## Closure

Given a topological space $(X, \tau)$ and a subset $A$ of $X$, the *closure of* $A$, denoted by $\overline A$ is the set of elements $a$ such that for every neighborhood $N$ of $a$, $N \cap A \neq \emptyset$.

Intuitively the closure of a set $A$ is the set $A$ plus elements that are arbitrarily close to $A$. If $A$ is open these arbitrarily close points can be outside of itself. If $A$ is closed, then no, as the following *Lemma* states.

**Lemma.** $A$ is closed if and only if $A = \overline A$.

Another characterization of the closure of $A$ is the intersection of all closed sets that contain $A$, via the following *Theorem*:

**Theorem.** Let $Z$ be the set of all closed sets containing $A$. Then $\overline A = \bigcap_{F \in Z} F$

## Interior

Given a topological space $(X, \tau)$ and a subset $A$ of $X$, the *interior of* $A$, denoted by $\mbox{Int}(A)$ is the set of elements $a$ for which $A$ is a neighborhood.

Intuitively the interior of $A$ is a subset of $A$ excluding elements that are at the "border" of $A$.

Another characterization of the interior of $A$ is the union of all open sets contained in $A$, via the following *Theorem*:

**Theorem.** Let $Z$ be the set of all closed sets containing $A$. Then $\mbox{Int}(A) = \bigcup_{O \in Z} O$

## Boundary

Given a topological space $(X, \tau)$ and a subset $A$ of $X$, the *boundary of* $A$, denoted by $\mbox{Bd}(A)$ is the set of elements in the closure of $A$ and the closure of its complement, that is,

$$\mbox{Bd}(A) = \overline{A} \cap \overline{C(A)}$$

We might think that we can obtain $\mbox{Bd}(A)$ via $\overline{A} \setminus A$ but this only works if $A$ is open. If $A$ is closed however, $C(A)$ is open, so the boundary could be obtained via $\overline{C(A)} \setminus C(A)$. The definition using intersection accounts for these two cases.

A more intuitive definition is that of the difference between the closure and the interior:

$$\mbox{Bd}(A) = \overline{A} \setminus \mbox{Int}(A)$$

# Functions

A function maps one topological space $(X, \tau)$ into another $(Y, \tau')$. If $a \in X$, then $f(a) \in Y$. If $A$ is a subset of $X$, then $f(A)$ is a subset of $Y$.

We can define the inverse function for a set $B \in Y$, denoted by $f^{-1}(B)$, as the subset $A'$ of $X$ such that $f(A') = B$, that is:

$$f^{-1}(B) = \curly{a \in X : f(a) \in B}$$

Note that if distincts $a, b \in X$ could have $f(a) = f(b) \in Y$. This leads to the following *Lemma*:

**Lemma.** $A \subseteq f^{-1}(f(A))$.

## Continuity

A function $f:(X, \tau) \rightarrow (Y, \tau')$ is said to be *continuous at point* $a \in X$ if for every neighborhood of $N$ of $f(a)$, $f^{-1}(N)$ is a neighborhood of $a$. $f$ is *continuous* if it's continuous for all points in $X$.

## Homeomorphism

Topological spaces $(X, \tau)$ and $(Y, \tau')$ are called *homeomorphic* if there exist *inverse* functions $f:X \rightarrow Y$ and $g:Y \rightarrow X$ and $f$ and $g$ are continuous.

Functions $f$ and $g$ are called *homeomorphisms* and they define a *homeomorphism between* $(X, \tau)$ and $(Y, \tau')$.
