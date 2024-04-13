---
layout: doc
title: "Topology Cheat Sheet"
---

# Index
{:.no_toc}

1. TOC
{:toc}

# Metric Spaces

We need to revisit metric spaces since some definitions on topological spaces depend on it.

Let $X$ be a non-empty set and $d:X \times X \rightarrow \mathbb{R}$ a [metric function](https://en.wikipedia.org/wiki/Metric_space#Definition). Then $(X, d)$ is a *metric space*.

## Open Ball

Given a metric space $(X, d)$, a point $a \in X$ and a scalar $\delta \gt 0$, an *open ball about* $a$, denoted by $B(a, \delta)$ is a set of points $x \in X$ satisfying $d(a, x) \lt \delta$.

## Neighborhood

Given a metric space $(X, d)$, a subset $N$ of $X$ is a *neighborhood of point* $a \in X$ if there exists some $\delta \gt 0$ and $N$ contains $B(a, \delta)$.

## Limit Point

Given a metric space $(X, d)$ and a subset $A \subseteq X$, a point $x \in X$ is **limit point** if every neighborhood of $x$ contains a point of $A$ different than $x$. Note that $x$ doesn't need to be in $A$, nor that every point in $A$ is a limit point (e.g. in the set $\mathbb{N}$ no element is a limit point).

*Example.* If $A = \curly{1/x : x \in \mathbb{R}}$, then $0$ is a limit point of $A$ even though it's not in $A$, since for any ball $B(0, \delta)$ contains $\delta > 0$ and $\delta \in A$.

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

Then $(X, \tau)$ is a *topological space*. $\tau$ is called the *topology* of set $X$. Members of $\tau$ are called *open sets*.

## Neighborhood

Given a topological space $(X, \tau)$ and a subset $N$ of $X$, $N$ is called a *neighborhood* of a point $a \in X$ if it contains an open set that contains $a$. That is, there exists $O \in \tau$ such that $a \in O$ and $O \subseteq N$.

## Complement

Given a topological space $(X, \tau)$ and a subset $F$ of $X$, the *complement* of $F$, $C(F)$ is $X \setminus F$.

## Closed Set

Given a topological space $(X, \tau)$ and a subset $F$ of $X$, is *closed* if the complement of $F$, $C(F)$ is open.

Some sets are neither open or closed. For example, in $\mathbb{R}$, the semi-open interval $[a, b)$ is neither open nor close.

Some sets are both open and closed. For example, in the subspace of $\mathbb{R}$ with $X = (x \lt 0) \cup (x \gt 0)$, we have that $(x \gt 0)$ is open and since its complement in $X$ is $(x \lt 0)$ is open, $(x \gt 0)$ is also closed!

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

## Isolated Point

Given a topological space $(X, \tau)$ and a subset $S$ of $X$, $x \in S$ is a **isolated point** if there exists a neighborhood of $x$ that contains no other points of $S$.

## Limit Point

The definition is equivalent to the one for Metric Spaces (see *Metric Spaces > Limit Point*). Given a topological space $(X, \tau)$ and a subset $S$ of $X$, $x \in S$ is a **limit point** or **accumulation point** if every neighborhood of $x$ contains a point in $S$ other than $x$.

## Dense Set

Given a topological space $(X, \tau)$ and a subset $S$ of $X$, $S$ is a **dense set** for every $x \in X$, it's either in $S$ or arbitrarily close to a member of $A$.

*Example.* The set of rationals are dense in the reals, since for any $x \in \mathbb{R}$ either $x$ is rational or for any $\delta > 0$, there exists a rational $q$ such that $\abs{x - q} < \delta$.

# Functions

A function maps one topological space $(X, \tau)$ into another $(Y, \tau')$. If $a \in X$, then $f(a) \in Y$. If $A$ is a subset of $X$, then $f(A)$ is a subset of $Y$.

We can define the inverse function for a set $B \in Y$, denoted by $f^{-1}(B)$, as the subset $A'$ of $X$ such that $f(A') = B$, that is:

$$f^{-1}(B) = \curly{a \in X : f(a) \in B}$$

Note that if distincts $a, b \in X$ could have $f(a) = f(b) \in Y$. This leads to the following *Lemma*:

**Lemma.** $A \subseteq f^{-1}(f(A))$.

A function $f:X \rightarrow Y$ is *surjective* or *onto* if $Y = f(X)$, or that for every $y \in Y$ there is $x \in X$ such that $f(x) = y$. Note that the opposite, that for every $x \in X$ there is $f(x) \in Y$, is implicit in the definition of a function.

A function $f:X \rightarrow Y$ is *injective* or *one-to-one* if for every $f(a) = f(b) \in Y$, then $a = b \in X$. In other words, no two distinct elements in $X$ map to the same element in $Y$.

A function $f:X \rightarrow Y$ is *bijective* or *one-to-one correspondence* if it's surjective and injective.

## Continuity

A function $f:(X, \tau) \rightarrow (Y, \tau')$ is said to be *continuous at point* $a \in X$ if for every neighborhood of $N$ of $f(a)$, $f^{-1}(N)$ is a neighborhood of $a$. $f$ is *continuous* if it's continuous for all points in $X$.

An equivalent definition: $f:(X, \tau) \rightarrow (Y, \tau')$ is continuous if and only if for every $U$ that is an open set in $Y$, $f^{-1}(U)$ is an open set in $X$.

## Homeomorphism

Topological spaces $(X, \tau)$ and $(Y, \tau')$ are called *homeomorphic* if there exist *inverse* functions $f:X \rightarrow Y$ and $g:Y \rightarrow X$ and $f$ and $g$ are continuous.

Functions $f$ and $g$ are called *homeomorphisms* and they define a *homeomorphism between* $(X, \tau)$ and $(Y, \tau')$.

Another characterization: $(X, \tau)$ and $(Y, \tau')$ are homeomorphic if there exists a bijective function $f:X \rightarrow Y$ such that for every open set $O$ in $X$, $f(O)$ is open in $Y$.

### Topological Property

Let $X$ and $Y$ be homeomorphic topological spaces. If $X$ having a property implies $Y$ having that property and vice-versa, such property is called a *topological property*.

Examples include connectedness and path-connectedness.

# Subspaces

Let $\tau_1$ and $\tau_2$ be topologies on a given set $X$. $\tau_1$ is said to be *weaker* than $\tau_2$ if $\tau_1 \subset \tau_2$.

# Product

The product of two sets $A$ and $B$, denoted by $A \times B$ is the set of pairs corresponding to all combinations of elements from $A$ and $B$, that is $\forall a \in A, b \in B : (a, b) \in A \times B$.

The product of multiple sets $X_i$, $1 \le i le n$ can be denoted as $X = \prod_{i=1}^{n} X_i$. Every element $a \in X$ can be written as $a = (a_1, \cdots, a_n)$, where $a_i \in X_i$.

Let $(X_i, \tau_i)$, $1 \le i le n$ be topological spaces. Let $X = \prod_{i=1}^{n} X_i$. Let $O = \prod_{i=1}^{n} O_i$ where $O_i$ is some open set in $X_i$.

Let $\tau$ be the collection of subsets of $X$ that are unions of sets in the form of $O$ above. It's possible to show $\tau$ is a topology, and the topological space $(X, \tau)$ is defined as the *product* of the topological spaces $(X_i, \tau_i)$, $1 \le i le n$.

## Projection

Let $X_i$, $1 \le i \le n$ be sets and $X = \prod_{i=1}^{n} X_i$. Let $a = (a_1, \cdots, a_n) \in X$.

The function $p_i:X \rightarrow X_i$ called the $i$-th *projection* is defined as $p_i(a) = a_i$. If each $a_i$ are sets, then we can defined $p^{-1}_i(a_i) = X_1 \times \cdots \times a_i \times \cdots \times X_n$, that is, we're "fixing" the $i$-th coordinate to be $a_i$ but leaving the others unrestricted.

# Identification Topologies

Let $(X, \tau)$ and $(Y, \tau')$ topological spaces. An *identification* is a continuous function $f:X \rightarrow Y$ if for each subset $U$ of $Y$, $f^{-1}(U) \in X$ being open in $X$ implies $U$ being open in $Y$.

Continuity only implies: if $U$ open set then $f^{-1}(U)$ open set. Identification adds the converse: if $f^{-1}(U)$ open set then $U$ open set.

Let $f: X \rightarrow Y$ be a surjective function. Define $\tau'$ as the set of subsets $U \in Y$ such that $f^{-1}(U)$ is open in $X$. It's possible to show $\tau'$ is a topology of $Y$. This in turn proves that $f$ is continuous since if $U \in \tau'$ by construction it was added there because $f^{-1}(U)$ is an open set.

It's also that case that for every $U \in Y$ if $f^{-1}(U)$ is open, then by construction $U \in \tau'$, so this function is an identification. We say that $\tau'$ is an *identification topology on* $Y$ *determined by* $f$.

# Connectedness

A topological space is **connected** if it cannot be defined as the union of two disjoint non-empty open sets. If it can, then it's disconnected.

An example of a disconnected space in $\mathbb{R}$: $x \lt 0 \cup x \gt 0$, since this set is the literal union of two disjoint non-empty open sets. On the other hand, the set $x \gt 0$ is connected. We can express it as the union of the disjoint sets $0 \lt x \le a$ and $x \ge a$ but the second set is not open.

Formally, let $(X, \tau)$ be a topological space. Then it is connected if no sets $A$ and $B$ exist such that:

* $A \cup B = X$
* $A \cap B = \emptyset$ (disjoint)
* $A, B \in \tau$ (open sets)
* $A, B \neq \emptyset$

If an open set $Y \in \tau$ also satisfies the above conditions, it's called a connected open set. If $Y \neq \emptyset$ it's also known as a **region** or **domain**.

## Component

A *component* of $a \in X$, denoted by $\mbox{Cmp}(a)$, is a maximal subset of $X$ that contains $a$ and is connected.

## Local Connectedness

A topological space $X$ is *locally connected at point* $a$ if every neighborhood of $N$ of $a$ contains a neighborhood of $a$ that is connected.

A topological space $X$ is *locally connected* if it's locally connected at each of its points.

## Path Connectedness

Let $X$ be a topological space. A continuous function $f:[0, 1] \rightarrow X$ is called a *path* in $X$. $f(0)$ is called the *initial point* and $f(1)$ is called the *terminal point*. $f$ is said to *connect* points $f(0)$ and $f(1)$.

$X$ is *path-connected* if for every pair of points $a, b \in X$, there is a path that connects $a$ and $b$.

## Simple Connectedness

A *simply connected* space is a path-connected space where every path between two points can be continuously transformed into any other such path while preserving those points.

Intuitively a simply connected space is a connected space that has no holes, because if we consider two paths on different sides of the hole, they cannot be transformed into another continuously.

A *loop* is a *path* whose endpoints coincide. In a simply connected space, every loop can be contracted (transformed continuously) into a point. Again, in the presence of holes, a loop surrounding such hole cannot be turned into a point.

# Compacteness

## Covering, subcovering and open covering

Let $X$ be a topological space, $Y$ a subset of $X$ and $\curly{A_\alpha}\_{\alpha \in I}$ an indexed family of subsets of $X$ (See [Set Theory]({{site}}/docs/math/set.html)). $\curly{A_\alpha}\_{\alpha \in I}$ is called a **covering** of $Y$ if every element of $Y$ belongs to one of the $A_\alpha$, in other words: $Y \subseteq \cup_{\alpha \in I} A_\alpha$. If $I$ is finite, $\curly{A_\alpha}\_{\alpha \in I}$ is called a **finite covering** of $Y$.

Let $\curly{A_\alpha}\_{\alpha \in I}$ and $\curly{B_\beta}\_{\beta \in J}$ be coverings of $Y$. If for every $A_\alpha$ there is $\beta \in J$ such that $A_\alpha = B_\beta$, then $\curly{A_\alpha}\_{\alpha \in I}$ is a **subcovering** of $\curly{B_\beta}\_{\beta \in J}$.

Note that we can't define subcovering as $I \subseteq J$ because $A_\alpha$ and $B_\beta$ might be indexed differently (e.g. $A_\gamma \neq B_\gamma$ even if $\gamma \in I$, $\gamma \in J$).

Let $\curly{A_\alpha}\_{\alpha \in I}$ be a covering of $Y$. If every $A_\alpha$ is an open subset of $X$ then $\curly{A_\alpha}\_{\alpha \in I}$ is a **open covering** of $Y$.

## Compact Topological Space

Let $X$ be a topological space. It's said to be **compact** if for *every* open covering  $\curly{A_\alpha}\_{\alpha \in I}$ of $X$, there exists a *finite* covering subcovering $\curly{A_\beta}\_{\beta \in J}$ of $X$.
