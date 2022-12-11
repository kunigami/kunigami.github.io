---
layout: doc
title: "Set Theory Cheat Sheet"
---

{% include blog_vars.html %}

## Family of Sets

Let $I$ be a set. For each $\alpha \in I$, let $A_\alpha$ be a subset of a given set $S$.
$S$ be a set. We call $I$ the **indexing set** and the set of $A_\alpha$ the **indexed family of subsets of** $S$ and denoted by $\curly{A_\alpha}_{\alpha \in I}$.

This notation let's us express operations with an arbitrary (possibly infinite) number of terms, for example, the union of all $\curly{A_\alpha}\_{\alpha \in I}$ can be denoted as $\cup_{\alpha \in I} A_\alpha$.

## Relation

A **relation** $R$ between two sets $A$ and $B$ is a subset of $A \times B$. In other words, it's the set of pairs $(a, b) \in A \times B$ which satisfy a given predicate.

Example: the relation between $\mathbb{N} \times \mathbb{N}$ corresponding to the predicate $a < b$ is the set of integers $(0, 1), (0, 2), \cdots$. A relation can be defined for the same set, by assuming $A = B$, which is the case on the previous example.

A function $f: A \rightarrow B$ can be seen as a special case of a relation. The corresponding relation $R$ is such that $(a, b) \in R \subseteq A \times B$ iff $f(a) = b$.

A relation $R$ on set $E$ is **reflexive** if for all $a \in E$, $(a, a) \in R$. It's **symmetric** if $\forall a, b \in E$, $(a, b) \in R$ then $(b, a) \in R$. It's **transitive** if $\forall a, b, c \in E$, $(a, b) \in R$ and $(b, c) \in R$, then $(a, c) \in R$.

A relation that is reflexive, symmetric and transitive is called an **equivalence relation**. A trivial example is the relation corresponding to the predicate $a = b$ for $a, b \in E$.

Let $R$ be equivalence relation on set $E$. For each $a \in E$, the set of $x$ such that $(a, x) \in R$ is called the **equivalence class** of $a$ and denoted by $\pi(a)$. It's possible to show that equivalent classes are non-empty and disjoint.
