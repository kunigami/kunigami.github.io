---
layout: doc
title: "Matrix Cheatsheet"
---

# Index
{:.no_toc}

- TOC
{:toc}

# Definitions

An $m \times n$ matrix is a matrix with $m$ rows and $n$ columns. As a mnemonic, this is also the order in which we index 2-dimensional vectors in languages like Python, e.g. `m[row_index][column_index]`. The pair $(m, n)$ defines the dimension of the matrix.

A matrix is usually denoted by uppercase letters (e.g. $M$, $A$). An element of a matrix at row $i$ and $j$ is either represented with lowercase $a_{ij}$.

An $m \times n$ matrix can be explicitly shown like:

$$
A = \begin{bmatrix}
    a_{11} & a_{12} & \cdots & a_{1n} \\
    a_{21} & a_{22} & \cdots & a_{2n} \\
    \vdots & \ddots & \ddots & \vdots \\
    a_{m1} & a_{m2} & \cdots & a_{mn} \\
    \end{bmatrix}
$$

A *row vector* is a matrix with $m = 1$. A *column vector* is a matrix with $n = 1$. A *square matrix* is one where $n = m$.

## Diagonal

The *diagonal* of a square matrix is the set of elememnts $a_{ij}$ for which $i = j$.

## Identity

The *identity*, denoted by $I$, is a square matrix for which the elements in the diagonal are 1 but 0 otherwise, that is

$$
\begin{equation}
  a_{ij}=\left\{
  \begin{array}{@{}ll@{}}
    0, & \text{if}\ i=j \\
    1, & \text{otherwise}
  \end{array}\right.
\end{equation}
$$

# Operations

## Transpose

The transpose of an $m \times n$ matrix $M$ , denoted as $M^T$, is a $n \times m$ matrix where $M_{ij}^T = M_{ji}$.


## Matrix Addition

Is only defined for matrices $A$ and $B$ with the same dimensions. $C = A + B$ is the matrix resulting from the element-wise addition of $A$ and $B$: $c_{ij} = a_{ij} + b_{ij}$.

## Matrix Subtraction

Is only defined for matrices $A$ and $B$ with the same dimensions. $C = A - B$ is the matrix resulting from the element-wise addition of $A$ and $B$: $c_{ij} = a_{ij} - b_{ij}$.

## Matrix Multiplication

Given matrix $A$ with dimensions $m \times k$ and $B$ with dimensions $k \times n$ (note the number of columns in $A$ must be equal to the number of rows in $B$), the product $AB$ is a $m \times n$ matrix defined as

$$
AB = \begin{bmatrix}
    a_{11}b_{11} + a_{12}b_{21} + \cdots + a_{1k}b_{k1} & \ddots & a_{11}b_{1n} + a_{12}b_{2n} + \cdots + a_{1k}b_{kn} \\
    \vdots & \ddots & \vdots \\
    a_{m1}b_{11} + a_{m2}b_{21} + \cdots + a_{mk}b_{k1} & \ddots & a_{m1}b_{1n} + a_{m2}b_{2n} + \cdots + a_{mk}b_{kn} \\
    \end{bmatrix}
$$

I find it easier to read this in Python:

{% highlight python %}
# Given A = matrix(m, k) and B = matrix(k, n)
prod = zeros(m, n)
for i in range(m):
    for j in range(n):
        for x in range(k):
            prod[i][j] += a[i][x]*b[x][j]
{% endhighlight %}

## Inverse

See *Matrix Multiplication*.

The inverse of a square matrix $M$, denoted by $M^{-1}$ is a matrix which when pre-multiplied by $M$ is equal to the identity: $M M^{-1} = I$

## Matrix 'Division'

See *Inverse*.

For scalars, we have the division operator $q = a/b$. This is equivalent to $qb = a$, so $q$ can be also defined as the value which multiplied by $b$ results in $a$.

Analogously for matrix, the value $Q$ for which $QB = A$ is $Q = AB^{-1}$

## Conjugate

This applies when the domain of the elements is the complex numbers. The conjugate of a matrix $M$, denoted by $\overline{M}$ is the matrix where each element is the complex conjugate of the corresponding element in $M$.

## Conjugate Transpose

This is when we conjugate and transpose (order doesn't matter). It's commonly denoted by $M^{*}$.


## Determinant

See [Determinants](https://www.kuniga.me/docs/math/determinants.html).

# Special Types

## Symmetric

A square matrix $M$ is **symmetric** if $M^T = M$. In other words $a_{ij} = a_{ji}$ for all entries.

## Hermitian

Hermitian matrices are the analogue of a symmetric metrices for the complex domain. A square matrix $M$ is **Hermitian** if $M^T = \overline{M}$.

## Positive Definite

A  $n \times n$ symmetric (for reals) or Hermitian (for complex) matrix $M$ is **positive definite** if $x^T A x \gt 0$ for all $x \in \mathbb{R}^n, x \ne 0$. The *definite* comes from the fact that it's *always* greater than 0 (definitely).

Note that $x^T A x$ is a single matrix, so effectively a scalar. As an example, consider the matrix

$$
M = \begin{bmatrix}
  a_{11} & a_{12} \\
  a_{21} & a_{22} \\
\end{bmatrix}
$$

then $x^T M x$ is the 2-variable polynomial: $a_{11}x_1^2 + (a_{12} + a_{21})x_1x_2 + a_{22}x_2^2$. On example where $M$ is positive definite is when $a_{12} = a_{21} = 0$ so that for any $(x_1, x_2)$ the result is positive.

## Hessian Matrix

The Hessian is a $n \times n$ matrix where entries correspond to the 2nd partial derivaties of a $n$-dimensional function. In particular, entry $a_{ij}$ is $\partial f(x) / \partial i \partial j$. For the 2-dimensional case:

$$
M = \begin{bmatrix}
  \frac{\partial f(r)}{\partial^2 x} & \frac{\partial f(r)}{\partial x \partial y} \\
  \frac{\partial f(r)}{\partial y \partial x} & \frac{\partial f(r)}{\partial^2 y} \\
\end{bmatrix}
$$
