---
layout: doc
title: "Matrix Cheatsheet"
---

# Index
{:.no_toc}

- TOC
{:toc}

# Concepts

## Orthogonal Complement

Let $S$ be a closed subspace of $H$. Its **orthogonal complement** is defined as

$$S^{\perp} = \{\vec{y} \in H | \langle \vec{y}, \vec{x} \rangle = 0, \, \forall \vec{x} \in S \}$$

An example for some geometric intuition could be for the $\mathbb{R}^3$, with $S$ being the vectors in the xy plane, that is, those with $z = 0$. Then $S^{\perp}$ would be those perpendicular to the xy plane ($x = y = 0$).

## Orthogonal Projection

Let $S$ be a closed subspace of $H$. The **orthogonal projection** of $\vec{x}$ onto $S$ is a vector $\vec{x_S}$ that minimizes the distance from $\vec{x}$ to $S$. Note that $\vec{x}$ does not need to be in $S$.

More formally, it's

$$\vec{x_S} = \mbox{argmin}_{\vec{y} \in S} \norm{\vec{x} - \vec{y}}$$

It's possible to prove $x_S$ exists and is unique.

## The Projection Theorem

Let $S$ be a closed subspace of $H$. The theorem states that every $\vec{x} \in H$ can be written as $\vec{x} = \vec{x_S} + \vec{x}^{\perp}$, where $\vec{x_S}$ is the **orthogonal projection** of $\vec{x}$ onto $S$ and $\vec{x}^{\perp} \in S^{\perp}$.

Going back to our geometry example, we can say that every vector in $\mathbb{R}^3$ can be expressed as the sum of a vector in the xy-plane and a vector perpendicular to it, or more specifically, $\vec{x} = (x, y, z)$, $\vec{x_S} = (x, y, 0)$ and $(0, 0, z) \in S^{\perp}$.

# Hilbert Spaces

The main post discussing them is [Hilbert Spaces](https://www.kuniga.me/blog/2021/06/26/hilbert-spaces.html).

In short summary, a Hilbert Space is a vector space equipped with a inner product and that is *complete*. Here *complete* means that every Cauchy sequence converges to a point in the space.
