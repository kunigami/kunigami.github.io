---
layout: doc
title: "Determinants Cheatsheet"
---

## Definitions

The **determinant** of a square matrix $A$, denoted by $\det(A)$, is a scalar that can be recursively defined as

$$\det(A) = \sum_{i=1}^{m}  a_{ij} (-1)^{i + j} \mid A_{ij} \mid$$

For any choice of column $j$. Here $A_{ij}$ is the matrix resulting from removing row $i$ and column $j$ from $A$. The base of this recursion is $m = 1$, for which $\det(A) = a_{11}$. This definition is known as *Laplace's expansion*.

The value $\det(A_{ij})$ is also called the **minor** of $A$. In this context $(-1)^{i + j} \det(A_{ij})$ is called the **cofactor** of $a_{ij}$.

## Properties

### Transpose

The determinant is invariant with transposition:

$$\det(A^{T}) = \det(A)$$

Because of this property many of the results that are obtained for rows are applicable for columns as well.

### Linear Dependence

The determinant of $A$ is $0$ if and only if the rows $A$ are linear dependent.

More explicitly, let $\curly{v_i \mid i = 1, \cdots, n}$ be the row vectors of $A$. The rows of $A$ are linear dependent if there exist non-zero scalars $\alpha_i$ for $i = 1, \cdots, n$ such that:

$$\sum_{i = 1}^{n} v_i \alpha_i = \mathbf{0}$$

In other words, one can express one of the rows as a linear combination of the others.

Another way to state this is that the determinant of $A$ is non-zero if and only if $A$ has full rank.

Another consequence is that if one of the columns is the $0$-vector, then the determinant is $0$.

### Swapping rows

Let $A'$ be obtained from $A$ by swapping two rows. Then

$$\det(A') = -\det(A)$$

### Multiplying row by scalar

Let $A'$ be obtained from $A$ by multiplying one of its row by a scalar $\rho$. Then

$$\det(A') = \rho \det(A)$$

### Adding/Subtracting row

Let $A'$ be obtained from $A$ with a modified row $i$ by adding or subtracting row $i' \ne i$ to it. Then:

$$\det(A') = \det(A)$$
