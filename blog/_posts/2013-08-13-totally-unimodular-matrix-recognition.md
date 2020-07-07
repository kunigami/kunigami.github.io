---
layout: post
title: "Totally Unimodular Matrix Recognition"
tags: [integer programming, linear algebra]
---

<figure class="image_float_left">
    <a href="http://kunigami.files.wordpress.com/2111/08/seymour.png"><img src="{{site.url}}/resources/blog/2013-08-13-totally-unimodular-matrix-recognition/2111_08_seymour.png" alt="" /></a>
</figure>

Paul Seymour is an english mathematician, graduated from Oxford. He is currently teaching at Princeton.

His research area concentrates on discrete mathematics, where he obtained important results including in Regular Matroids, Totally Unimodular Matrices and the Four Color Theorem, being awarded the [Fulkerson Prize](http://en.wikipedia.org/wiki/Fulkerson_Prize) four times.

In this post we'll present one of his results regarding decomposition of totally unimodular matrices, which are the key piece in deciding whether a given matrix is TU.



---



This is the third post about Totally Unimodular (TU) matrices. We introduced them [here]({{site.url}}/blog/2012/09/02/totally-unimodular-matrices.html) and described a special case called Network matrices [here]({{site.url}}/blog/2013/06/30/network-matrices.html).

Although it's not true that all TU matrices are network matrices, Seymour's theorem basically says that all TU matrices are some kind of combination of network matrices and the following matrices:

$$
(1) \left[ \begin{array}{rrrrr}
1 & -1 & 0 & 0 & -1\\
-1 & 1 & -1 & 0 & 0\\
0 & -1 & 1 & -1 & 0\\
0 & 0 & -1 & 1 & -1\\
-1 & 0 & 0 & -1 & 1\\
\end{array} \right]$$

$$
(2) \left[ \begin{array}{rrrrr}
1 & 1 & 1 & 1 & 1\\
1 & 1 & 1 & 0 & 0\\
1 & 0 & 1 & 1 & 0\\
1 & 0 & 0 & 1 & 1\\
1 & 1 & 0 & 0 & 1\\
\end{array} \right]$$

It's possible to show that TU matrices are closed under the following operations:

(i) permuting rows or columns
(ii) taking the transpose
(iii) multiplying a row or column by -1
(iv) pivoting, that is, transforming

$$\left[ \begin{array}{cc}
\xi & c\\
b & D\\
\end{array} \right]$$

into

$$\left[ \begin{array}{cc}
-\xi & -\xi c\\
\xi b & D - \xi b c \\
\end{array} \right]$$

where $$\xi$$ is a scalar and $$c$$ and $$b$$ are a row and a column vector of the appropriate sizes, respecitvely.

(v) adding a row or column with at most one non-zero entry
(vi) repeating a row or a column

Also, given TU matrices A and B, the following operations preserve total unimodularity:

1-sum:
(vii) $$A \oplus_1 B := \left[ \begin{array}{rr}
A & 0\\
0 & B\\
\end{array} \right]$$

2-sum:
(viii) $$
\left[ \begin{array}{rr}
A & a\\
\end{array} \right] \oplus_2
\left[ \begin{array}{r}
b\\
B\\
\end{array} \right] :=
\left[ \begin{array}{rr}
A & ab\\
0 & B\\
\end{array} \right]$$

3-sum:
(ix) $$
\left[ \begin{array}{rrr}
A & a & a\\
c & 0 & 1\\
\end{array} \right]
\oplus_3
\left[ \begin{array}{rrr}
1 & 0 & b\\
d & d & B\\
\end{array} \right]
:=
\left[ \begin{array}{rr}
A & ab\\
dc & B\\
\end{array} \right]$$

We can now state Seymour's theorem:

**Theorem 1.** (Seymour's decomposition theorem for totally unimodular matrices). *A matrix $$A$$ is totally unimodular if and only if $$A$$ arises from network matrices and the matrices (1) and (2) by applying the operations (i) to (ix). Here, the operations (vii) to (ix) are only applied if for $$A$$ and $$B$$, the number of rows and columns added is at least 4.*

### Recognizing Total Unimodularity

The algorithm for recognizing whether a given matrix $$M$$ is TU consists in finding whether it is a network matrix or one the matrix (1) or (2).

1. If any of the entries of M is not in $$\{-1, 0, 1\}$$, then $$M$$ is not TU.

2. Remove all rows and columns with one or less non-zero entries (Property (v)).

3. Remove repeated rows and columns (Property (vi)).

4. Test if $$M$$ or its transpose $$M^T$$ is a network matrix or (1) or (2), possibly permuting and multiplying rows and columns by -1 (Properties (i), (ii) and (iii)). If yes, then the matrix is TU.

At this point of the algorithm, we still don't know whether our matrix is TU. We'll now check whether $$M$$ can be decomposed as follows:

(3) $$M = \left[ \begin{array}{rr}
A & B\\
C & D\\
\end{array} \right]$$

such that $$\mbox{rank}(B) + \mbox{rank}(C) \le 2$$ and for both $$A$$ and $$D$$, the number of rows plus the number of columns is greater or equal than 4.

Cunningham and Edmonds stated it's possible to find such a decomposition for a matrix $$M$$ or conclude no such decomposition exists in polynomial time using the matroid intersection algorithm.

Going back to the algorithm:

5. If $$M$$ has cannot be decomposed as (3), then it's not TU.

6. If it can, then we break into cases depending on the values of $$\mbox{rank}(B)$$ and $$\mbox{rank}(C)$$. Given that $$\mbox{rank}(B) + \mbox{rank}(C) \le 2$$, we have the 6 possible combinations:




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

By Property (vii), if $$A$$ and $$D$$ are TU, $$M$$ is TU. Since $$A$$ and $$D$$ are submatrices of $$M$$, the converse holds, so it suffices to recursively verify that $$A$$ and $$D$$ are TU.

*Case 2:*

Since $$B$$ has rank 1, it has all duplicate rows/columns or rows/columns with all zeroes. Thus, we can write it as $$B = fg$$, where $$f$$ is a column vector and $$g$$ is a row vector. In this form, we can write M as the result of the 3-sum operation:

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
\end{array} \right]$$ are TU, $$M$$ is TU. Also, since both are submatrices of $$M$$, if one of them is not TU, then $$M$$ can't be TU either, so we can test $$
\left[ \begin{array}{rr}
A & f\\
\end{array} \right]$$ and $$\left[ \begin{array}{r}
g\\
D\\
\end{array} \right]$$ recursively.

*Case 3:* Analogous to Case 2.

*Case 4:*

In this case, both $$B$$ and $$C$$ have all duplicate rows/columns or rows/columns with all zeroes. We can re-arrange the rows and columns in such a way that $$M$$ looks like this:

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

We define a bipartite graph $$G_A$$ with one partition being the set of rows of $$A$$ and the other partition the set of columns of $$A$$. There's an edge between two vertices $$r$$ and $$c$$ if the entry $$(r,c)$$ is non-zero in $$A$$.

Let $$R$$ be the set of vertices of the row partition that intercepts $$A_4$$ and $$K$$ those of the column partition that intercepts $$A_4$$. If there's no path between $$R$$ and $$K$$, then we can reduce to Cases 2 and 3, so without loss of generality, consider the shortest path $$\prod_A$$ between $$R$$ and $$K$$.

Since $$R$$ and $$K$$ are in different partitions, $$\prod_A$$ has odd length $$\delta$$.  Let

$$\xi_A = \left\{ \begin{array}{rr}
1 & \mbox{if } \delta \equiv 1\, (\mbox{mod } 4)\\
-1 & \mbox{if } \delta \equiv 3\, (\mbox{mod } 4)\\
\end{array} \right.$$

and $$\xi_D$$ defined analogously.

(4) $$\left[ \begin{array}{rrr}
A_1 & A_2 & 0\\
A_3 & A_4 & 0\\
0 & 1 & \xi_D
\end{array} \right]$$

(5) $$\left[ \begin{array}{rrr}
\xi_A & 1 & 0\\
1 & D_1 & D_2\\
0 & D_3 & D_4
\end{array} \right]$$

The algorithm says that M is TU if and only if (4) and (5) are TU.

*Case 5:* Can be reduced to Case 4 by pivoting (Property iv).

*Case 6:* Analogous to Case 5.

### Complexity and Implementation

In [2], Truemper presents a $$O(n + m)^3$$ algorithm for the total unimodularity testing and in [3] he Walter and Truemper provides an implementation in C++ of a simplified $$O(n + m)^5$$ version.

### Conclusion

The matroid intersection algorithm is used in one of the steps of the algorithm. I've heard about this algorithm before, but never studied it, so this will the next subject I'm researching about in the series of posts on Combinatorial Optimization.

### References

* [[1](http://www.amazon.com/Theory-Integer-Programming-Alexander-Schrijver/dp/0471982326/)] 
 Theory of Linear and Integer Programming – A. Schrijver (Chapter 20)
* [2] A decomposition theory for matroids. V. Testing of matrix total unimodularity - K. Truemper
* [[3](http://www.utdallas.edu/~klaus/TUtest/index.html)] 
 Implementation of a Unimodularity Test - M. Walter and K. Truemper