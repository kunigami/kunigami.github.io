---
layout: post
title: "Totally Unimodular Matrix Recognition"
tags: [integer programming, linear algebra]
vanity: "2013-08-13-totally-unimodular-matrix-recognition"
---

{% include blog_vars.html %}

<figure class="image_float_left">
    <a href="http://kunigami.files.wordpress.com/2111/08/seymour.png"><img src="{{resources_path}}/seymour.png" alt="" /></a>
</figure>

Paul Seymour is an english mathematician, graduated from Oxford. He is currently teaching at Princeton.

His research area concentrates on discrete mathematics, where he obtained important results including in Regular Matroids, Totally Unimodular Matrices (TU) and the Four Color Theorem, being awarded the [Fulkerson Prize](http://en.wikipedia.org/wiki/Fulkerson_Prize) four times.

In this post we'll present one of his results regarding decomposition of totally unimodular matrices, which are the key piece in deciding whether a given matrix is TU.

<!--more-->

This is the third post about Totally Unimodular (TU) matrices. We introduced them [here]({{site.url}}/blog/2012/09/02/totally-unimodular-matrices.html) and described a special case called Network matrices [here]({{site.url}}/blog/2013/06/30/network-matrices.html).

To recap, a *unimodular matrix* is a square matrix with integer entries such that its determinant is either $-1$, $0$ or $1$. A *totally unimodular matrix* is a matrix such that all its square submatrices are unimodular.

Seymour's theorem states that every TU matrix can be decomposed into network matrices or one of $(1)$ or $(2)$:

$$
(1) \quad \left[ \begin{array}{rrrrr}
1 & -1 & 0 & 0 & -1\\
-1 & 1 & -1 & 0 & 0\\
0 & -1 & 1 & -1 & 0\\
0 & 0 & -1 & 1 & -1\\
-1 & 0 & 0 & -1 & 1\\
\end{array} \right]$$

$$
(2) \quad \left[ \begin{array}{rrrrr}
1 & 1 & 1 & 1 & 1\\
1 & 1 & 1 & 0 & 0\\
1 & 0 & 1 & 1 & 0\\
1 & 0 & 0 & 1 & 1\\
1 & 1 & 0 & 0 & 1\\
\end{array} \right]$$

The theorem also provides an algorithm to determine whether a matrix is TU. The overall idea is to use the properties we saw for total unimodular matrices in [4] to reduce them into a form that can then be decomposed into smaller ones, as we'll see next.

## Recognizing Total Unimodularity

The algorithm for recognizing whether a given matrix $M$ is TU consists in finding whether it is a network matrix or one the matrix $(1)$ or $(2)$.

**Step 1.** If any of the entries of M is not in $\{-1, 0, 1\}$, then $M$ is not TU.

Justification: *Property 1* in [4] states all entries of a TU matrix must be in $\{-1, 0, 1\}$.

**Step 2.** Remove all rows and columns with one or less non-zero entries.

Justification: Since *adding* a row or column with at most one non-zero entry preserves TU (*Property 3* in [4]), we can *remove* such rows and columns from $A$ to obtain $B$. If we prove that $B$ is TU, then $A$ is TU.

**Step 3.** Remove repeated rows and columns.

Justification: Since duplicating any row or column preserves TU (*Property 7* in [4]), we can remove duplicate rows and columns from $A$ to obtain $B$. If we prove that $B$ is TU, then $A$ is TU.

**Step 4.** Test if $M$ or its transpose $M^T$ is a network matrix or $(1)$ or $(2)$, by leveraging permutation of rows or columns, multiplying rows or columns by $-1$.

Justification: Transposition (*Property 2* in [4]), permutation of rows and columns (*Property 6* in [4]) and multiplication of rows or columns by $-1$ (*Property 5* in [4]) all preserve TU.

If it passes the test, then we're done. If not, go to *Step 5*.

**Step 5.** Test whether $M$ can be decomposed as follows:

$$(3) \quad M = \left[ \begin{array}{rr}
A & B\\
C & D\\
\end{array} \right]$$

such that $$\mbox{rank}(B) + \mbox{rank}(C) \le 2$$ and for both $A$ and $D$, the number of rows plus the number of columns is greater or equal than 4.

Cunningham and Edmonds stated it's possible to find such a decomposition for a matrix $M$ or conclude no such decomposition exists in polynomial time using the matroid intersection algorithm.

If $M$ has cannot be decomposed as $(3)$, then it's not TU and we're done. If it can, go to *Step 6.*.

**Step 6.** We split it into cases depending on the values of $$\mbox{rank}(B)$$ and $$\mbox{rank}(C)$$. Given that $$\mbox{rank}(B) + \mbox{rank}(C) \le 2$$, we have the 6 possibilities:

$$\begin{array}{l\mid c\mid c}
\mbox{Case} & \mbox{rank}(B) & \mbox{rank}(C)\\
\hline
1 & 0 & 0 \\
2 & 1 & 0 \\
3 & 0 & 1 \\
4 & 1 & 1 \\
5 & 2 & 0 \\
6 & 0 & 2 \\
\end{array}$$


*Case 1:*

A matrix with rank zero is the zero matrix, so we have

$$M = \left[ \begin{array}{rr}
A & 0\\
0 & D\\
\end{array} \right]$$

By Property (vii), if $A$ and $D$ are TU, $M$ is TU. Since $A$ and $D$ are submatrices of $M$, the converse holds, so it suffices to recursively verify that $A$ and $D$ are TU.

*Case 2:*

Since $B$ has rank 1, it has all duplicate rows/columns or rows/columns with all zeroes. Thus, we can write it as $$B = fg$$, where $$f$$ is a column vector and $$g$$ is a row vector. In this form, we can write M as the result of the 3-sum operation:

$$
\left[ \begin{array}{rr}
A & f\\
\end{array} \right] \oplus_2
\left[ \begin{array}{r}
g\\
D\\
\end{array} \right] :=
\left[ \begin{array}{rr}
A & fg\\
0 & D\\
\end{array} \right]$$

From Property (viii), if $$
\left[ \begin{array}{rr}
A & f\\
\end{array} \right]$$ and $$\left[ \begin{array}{r}
g\\
D\\
\end{array} \right]$$ are TU, $M$ is TU. Also, since both are submatrices of $M$, if one of them is not TU, then $M$ can't be TU either, so we can test $$
\left[ \begin{array}{rr}
A & f\\
\end{array} \right]$$ and $$\left[ \begin{array}{r}
g\\
D\\
\end{array} \right]$$ recursively.

*Case 3:* Analogous to Case 2.

*Case 4:*

In this case, both $B$ and $C$ have all duplicate rows/columns or rows/columns with all zeroes. We can re-arrange the rows and columns in such a way that $M$ looks like this:

$$
\left[ \begin{array}{rrrr}
A_1 & A_2 & 0 & 0\\
A_3 & A_4 & 1 & 0\\
0 & 1 & D_1 & D_2\\
0 & 0 & D_3 & D_4\\
\end{array} \right]$$

where

$$A := \left[ \begin{array}{rr}
A_1 & A_2\\
A_3 & A_4\\
\end{array} \right]$$

and

$$D := \left[ \begin{array}{rr}
D_1 & D_2\\
D_3 & D_4\\
\end{array} \right]$$

We define a bipartite graph $G_A$ with one partition being the set of rows of $A$ and the other partition the set of columns of $A$. There's an edge between two vertices $R$ and $C$ if the entry $$(r,c)$$ is non-zero in $A$.

Let $R$ be the set of vertices of the row partition that intercepts $A_4$ and $K$ those of the column partition that intercepts $A_4$. If there's no path between $R$ and $K$, then we can reduce to Cases 2 and 3, so without loss of generality, consider the shortest path $\prod_A$ between $R$ and $K$. Let $\delta$ be the sum of entries of $A$ corresponding to $\prod_A$.

Since $R$ and $K$ are in different partitions, $\prod_A$ has odd length $$\delta$$.  Let

$$\xi_A = \left\{ \begin{array}{rr}
1 & \mbox{if } \delta \equiv 1\, (\mbox{mod } 4)\\
-1 & \mbox{if } \delta \equiv 3\, (\mbox{mod } 4)\\
\end{array} \right.$$

and $$\xi_D$$ defined analogously. Consider the matrices:

$$(4) \quad \left[ \begin{array}{rrr}
A_1 & A_2 & 0\\
A_3 & A_4 & 1\\
0 & 1 & \xi_D
\end{array} \right] \quad (5) \quad \left[ \begin{array}{rrr}
\xi_A & 1 & 0\\
1 & D_1 & D_2\\
0 & D_3 & D_4
\end{array} \right]$$

and

$$(6) \quad \left[ \begin{array}{rrr}
A_1 & A_2 & 0\\
A_3 & A_4 & 1\\
0 & 1 & 0
\end{array} \right] \quad (7) \quad \left[ \begin{array}{rrr}
0 & 1 & 0\\
1 & D_1 & D_2\\
0 & D_3 & D_4
\end{array} \right]$$

The algorithm says that $M$ is TU if and only if $(4)-(7)$ are TU.

*Case 5:* Can be reduced to Case 4 by pivoting (Property iv).

*Case 6:* Analogous to Case 5.

## Complexity and Implementation

In [2], Truemper presents a $O(n + m)^3$ algorithm for the total unimodularity testing and in [3] he Walter and Truemper provide an implementation in C++ of a simplified $O(n + m)^5$ version.

## Conclusion

The matroid intersection algorithm is used in one of the steps of the algorithm. I've heard about this algorithm before, but never studied it, so this will the next subject I'm researching about in the series of posts on Combinatorial Optimization.

## References

* [[1](http://www.amazon.com/Theory-Integer-Programming-Alexander-Schrijver/dp/0471982326/)]
 Theory of Linear and Integer Programming – A. Schrijver (Chapter 20)
* [2] A decomposition theory for matroids. V. Testing of matrix total unimodularity - K. Truemper
* [[3](http://www.utdallas.edu/~klaus/TUtest/index.html)]
 Implementation of a Unimodularity Test - M. Walter and K. Truemper
* [[4]]({{blog}}/2012/09/02/totally-unimodular-matrices.html) NP Incompleteness - Totally Unimodular Matrices
