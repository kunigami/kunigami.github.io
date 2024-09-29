---
layout: doc
title: "Matrix Cheatsheet"
---

## Definitions

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

### Transpose

The transpose of an $m \times n$ matrix $M$ , denoted as $M^T$, is a $n \times m$ matrix where $M_{ij}^T = M_{ji}$.

### Determinant

See [Determinants]({{site.url}}/docs/math/determinants.html).

### Diagonal

The *diagonal* of a square matrix is the set of elememnts $a_{ij}$ for which $i = j$.

### Identity

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

### Matrix Addition

Is only defined for matrices $A$ and $B$ with the same dimensions. $C = A + B$ is the matrix resulting from the element-wise addition of $A$ and $B$: $c_{ij} = a_{ij} + b_{ij}$.

### Matrix Subtraction

Is only defined for matrices $A$ and $B$ with the same dimensions. $C = A - B$ is the matrix resulting from the element-wise addition of $A$ and $B$: $c_{ij} = a_{ij} - b_{ij}$.

### Matrix Multiplication

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

### Inverse

See *Matrix Multiplication*.

The inverse of a square matrix $M$, denoted by $M^{-1}$ is a matrix which when pre-multiplied by $M$ is equal to the identity: $M M^{-1} = I$

### Matrix 'Division'

See *Inverse*.

For scalars, we have the division operator $q = a/b$. This is equivalent to $qb = a$, so $q$ can be also defined as the value which multiplied by $b$ results in $a$.

Analogously for matrix, the value $Q$ for which $QB = A$ is $Q = AB^{-1}$
