---
layout: post
title: "Totally Unimodular Matrices"
tags: [combinatorics, integer programming, linear algebra]
---

In this post we study totally unimodular matrices. We'll start with their definition and understand their importance by covering different equivalences between them and problems in combinatorial optimization.

We then show some properties specific to totally unimodular matrices and finish with some examples.

## Motivation

[Sometime ago]({{blog}}/2010/08/13/dijkstra-and-the-longest-path.html), we said that problems such as the minimum path, maximum flow and minimum cost max flow can be modeled using linear programming with the interesting property that the optimal solutions are always integral.

In that post, we also said that it was because the coefficient matrix of the constraints of the formulations are totally unimodular. In this post we'll prove this result.

Since linear programming algorithms work by visiting vertices of its polyhedra, this implies that integer solutions of linear programs with constraints corresponding to a totally unimodular matrix can be found in polynomial time.

In other words, if you can model a problem as a linear program satisfying the totally unimodular matrix constraint, then your problem is proven to be in $\mathbf{P}$.

## Definition

An **unimodular matrix** is a square matrix with integer entries such that its determinant is either $-1$, $0$ or $1$. A matrix is said **totally unimodular** (TU for short) if all its square submatrices are unimodular. Note that a totally unimodular matrix is not necessarily square and hence not necessarily unimodular!


## Properties

Let $A$ be a totally unimodular matrix with dimensions $n \times m$. Then we have the following properties:

**Lemma 1.** All elements in $A$ are either $0$, $1$ or $-1$.

<proof>
This follows from the definition because a $1 \times 1$ matrix is a submatrix and its determinant is equal to itself.
</proof>

**Lemma 2.** Its transpose, $A^{T}$, is TU.

<proof>
Since we're only considering square submatrices, every submatrix of $A^{T}$ is the transpose of some submatrix of $A$. Since determinants are invariant with transposition, it follows that the square submatrices of $A^{T}$ are unimodular and thus $A^{T}$ is TU.
</proof>

**Lemma 3.** Let $B$ obtained by appending a column vector ($n \times 1$) with at most 1 non-zero entry to $A$. Then $B$ is TU.

<proof>
Consider a square submatrix of $B$, say $B'$. If it's also a submatrix of $A$, then we know it's unimodular. Otherwise it must include a subset of entries from the added column vector. If all these entries of it are 0, the determinant is zero.
<br /><br />
Otherwise there's exactly one entry equal to 1, say at row $k$. We can use Laplace's expansion along the last column of $B'$, $m + 1$. Then we can claim that:

$$\det(B') = (-1)^{k + m + 1} \det(B'_{k,m+1}$$

Where $B'_{k,m+1}$ is the matrix $B'$ without the last column and the row $k$. This (square) submatrix is definetely a submatrix of $A$ so its determinant is in $\curly{-1, 0, 1}$, so we conclude $\det(B')$ is also in $\curly{-1, 0, 1}$ and thus $B'$ is unimodular.
<br /><br />
Since this applies to any square submatrix of $B$, $B$ is TU.
</proof>

A corollary of this property is that appending the identity matrix $n \times n$ to the right of $A$ preserves TU.

Another corollary is that appending *rows* with at most one non-zero entry also preserves TU by combinig *Property 3* and *Property 2* (transposition).

**Lemma 4.** Any submatrix of $A$ is TU.

<proof>
Let $A'$ be a square submatrix of $A$. The set of square submatrices of $A$ includes all square submatrices of $A'$. Since $A$ is TU, all its square submatrices are unimodular. Thus all square submatrices of $A'$ are unimodular and hence $A'$ is TU.
</proof>

**Lemma 5.** The matrix obtained by multiplying any row of $A$ by $-1$ is TU.

<proof>

Suppose we multiple row $k$ of $A$ by $-1$. Let $A'$ be a square submatrix of $A$. If $A'$ doesn't contain (part of) row $k$, then we know it's unimodular. Otherwise, the effect of multiplying row $k$ of $A$ by $-1$ is multiplying some row $k'$ of $A'$ by $-1$. Let $B'$ be the result of the latter.
<br /><br />
We can use Laplace's expansion along such row $k'$ of $B'$:

$$
\det(B') = \sum_{j = 1}^m 1^{k' + j} b'_{k', j} \det(B'_{k', j})
$$

Where $b'_{k', j}$ is the entry at row $k'$ and column $j$ of $B'$ and $B'_{k', j}$ is the matrix $B'$ without row $k'$ and column $j$. Because $B'_{k', j}$ does not contain $k'$, it's the same as $A'_{k', j}$.
<br /><br />
Also $b'_{k',j} = - a'_{k',j}$, which gives us:

$$
\det(B') = - \sum_{j = 1}^m 1^{k' + j} a'_{k', j} \det(A'_{k', j}) = -\det(A')
$$

Since $\det(A') \in \curly{-1, 0, 1}$, so is $\det(B')$.
</proof>

By combining *Property 5* and *Property 2* (transposition) we get the corollary that multiplying any column of $A$ by $-1$ also preserves TU.

**Lemma 6.** The permutation of rows or column preserves TU.

<proof>
To show this, we just need to show that swaping the order of any two adjacent rows preserves TU. That's because we can apply such swaps multiple times and obtain any ordering we want (a la bubble sort). Further, by using transposition we can obtain the same result for columns.
<br /><br />
Let $1 \le i \le n - 1$ be a row of $A$ and $B$ the result of swapping rows $i$ and $i + 1$. Now consider a square submatrix $B'$ of $B$. If $B'$ doesn't intercept row $i$ nor $i + 1$, then it's not affected by the swap and it exists as a square submatrix of $A$ and is thus unimodular.
<br /><br />
If $B'$ intercepts only $i$ then it must be its first row. Let the set of rows of $B'$ be denoted as $r_1, r_2, \cdots, r_k$ with $r_1 = i$ and columns $c_1, c_2, \cdots, c_k$. There should be a square submatrix $A'$ of $A$ with rows $i, i + 1, r_2, \dots, r_k$ and columns equal to $B'$'s except for an extra column $c'$, i.e. $c_1, c_2, \cdots, c', \cdots, c_k$. Then the submatrix obtained by removing row $i + 1$ and column $c'$ is also a square submatrix of $A$ and is equal to $B'$ which is thus unimodular. A similar argument holds if $B'$ intercepts only $i + 1$.
<br /><br />
Otherwise $B'$ intercepts both $i$ and $i+1$ and they're also adjacent in it. Let $B'$ rows be denoted by $r_1, r_2, \cdots, r_{i'}, r_{i'+1} \cdots, r_k$, where $r_{i'} = i + 1$ and $r_{i' + 1} = i$. There's a square submatrix $A'$ of $A$ which is similar to $B'$ except that rows $i$ are swapped, that is, its columns are $r_1, r_2, \cdots, r_{i' + 1}, r_{i'} \cdots, r_k$. We can define $B'$'s determinant via Laplace's expansion along its row $i'$-th row:

$$
\det(B') = \sum_{j = 1}^{k} (-1)^{i' + j} b'_{i',j} \det(B'_{i'j})
$$

We define $A'$'s determinant along its $i'+1$-th row:

$$
\det(A') = \sum_{j = 1}^{k} (-1)^{i' + 1 + j} a'_{i'+1,j}\det(A'_{i' + 1,j})
$$

Now, because $A'$'s $i'+1$-th row is $B'$'s $i'$-th row, we have $a'_{i'+1,j} = b'_{i',j}$ and removing that row leads to the same submatrix, so $A'_{i' + 1,j} = B'_{i'j}$. Also $(-1)^{i' + 1 + j} = (-1)(-1)^{i' + j}$  thus we have:

$$
\det(A') = \sum_{j = 1}^{k} (-1)(-1)^{i' + j} b'_{i',j}\det(B'_{i' + 1,j}) = -\det(B')
$$

Since $\det(A') \in \curly{-1, 0, 1}$, so is $\det(B')$ and $B'$ is unimodular. We just showed that all square submatrices of $B$ are unimodular and thus $B$ is TU.

</proof>

Combining this property with *Property 3* enables us to insert a column or row with at most one non-zero entries anywhere in the matrix and preserve TU.

**Lemma 7.** The matrix obtained by duplicating any row or column of $A$ is TU

<proof>
Let $i$ be any row of $A$ and we obtain a matrix $B$ by inserting a copy of $i$ at position $i + 1$. Now consider a square submatrix $B'$ of $B$. If $B'$ doesn't intercept $i$ or $i + 1$, it's also a submatrix of $A$ and hence unimodular. If it intercepts only one of $i$ and $i + 1$, then it also exists as a submatrix of $A$.
<br /><br />
When it intercepts both $i$ and $i + 1$, $B'$ has duplicate rows and it can be shown that its determinant must be $0$ because these are linearly depended rows. For duplicate rows in particular we can use Laplace's expansion to show that $\det(B') = -\det(B')$, which leds to the same conclusion. Anyway, $B'$ is also unimodular and hence $B$ is TU.
<br /><br />
By utilizing <i>Property 6</i> we can show that the duplicated row $i$ can be inserted anywhere, not just at position $i + 1$. Further by leveraging transposition, <i>Property 7</i>,
</proof>

### Operations on TUs

Given TU matrices $A$ and $B$, the following operations preserve TU.

**Lemma 9.** 1-sum:

$$A \oplus_1 B := \left[ \begin{array}{rr}
A & 0\\
0 & B\\
\end{array} \right]$$

<proof>
We can use the equivalence between TU $(i)$ and $(iii)$, described in the section <i>Equivalences</i>. We first observe that $A' = \left[ \begin{array}{c} A \\0 \end{array} \right]$ is TU and so is $B' = \left[ \begin{array}{c} 0 \\B \end{array} \right]$.
<br /><br />
Let $M = [A' \mid B']$ and $C$ any subset of columns of $M$. Let $C_A$ be the columns intersecting $A'$ and $C_B$ those intersecting $B'$. We know that there exists a partition of $C_A$, $C_{A_1}$ and $C_{A_2}$ such that the sum of columns in $C_{A_1}$ minus those in $C_{A_2}$ have entries in $\curly{-1, 0, 1}$ and similarly for $C_B$ into $C_{B_1}$ and $C_{B_2}$.
<br ><br />
Now we consider the partition of $C$ into $C_1 = C_{A_1} \cup C_{B_1}$ and $C_2 = C_{A_2} \cup C_{B_2}$. We claim that the sum of columns in $C_1$ minus those in $C_{2}$ have entries in $\curly{-1, 0, 1}$. Let $l$ be the sum of columns in $C_1$, $r$ be the sum of columns in $C_2$ and $v = l - r$.
<br ><br />
Consider an element in row $i$, $v_i$. If $i$ is a row intersecting $A$, then $l_i$ and $r_i$ only contains non-zero elements from $C_{A_1}$ and $C_{A_2}$, respectively and since $A'$ is TU, their difference must be in $\curly{-1, 0, 1}$. Same if $i$ intersects $B$. Thus $M$ satisfies $(iii)$ and is thus TU.
</proof>


**Lemma 10.** 2-sum:

$$
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

**Lemma 11** 3-sum:

$$
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


## Equivalences

There are many different characterizations of totally unimodular matrices. Let $A$ be a matrix with entries in $\curly{-1, 0, 1}$. Then the following are equivalent:

$(i)$ $A$ is totally unimodular

$(ii)$ If $a, b, c, d$ are integral vectors, the polytope

$$P = \{x \mid a \le Ax \le b; c \le x \le d \}$$

has integer vertices.

$(iii)$ Any subset of columns $C$ of $A$ can be split in two parts $C_1$ and $C_2$ such that the sums such that the sum of columns in $C_1$ minus those in $C_2$ yields a column vector with entries in $\curly{-1, 0, 1}$.

$(iv)$ Each non-singular submatrix of $A$ has a row with an odd number of non-zero elements

$(v)$ The sum of entries in any square submatrix with even row and colum sums is divisible by 4.

$(vi)$ No square submatrix of $A$ has determinant $+2$ or $-2$

To prove these equivalences, we just need to find a cycle involving all these items. We defer to different theorems in the *Appendix*:

* $(i) \rightarrow (ii)$ - *Corollary 16*.
* $(ii) \rightarrow (iii)$ - *Theorem 17*.
* $(iii) \rightarrow (iv)$ - *TBD*.
* $(iii) \rightarrow (v)$ - *TBD*.
* $(iv) \rightarrow (vi)$ - *TBD*.
* $(v) \rightarrow (vi)$ - *TBD*.
* $(vi) \rightarrow (i)$ - *TBD*.

One such cycle is (bold denoting unique occurrence):

$$\textbf{(i)} \rightarrow \textbf{(ii)}  \rightarrow \textbf{(iii)} \rightarrow \textbf{(iv)} \rightarrow (vi) \\ \rightarrow (i) \rightarrow (ii) \rightarrow (iii) \rightarrow \textbf{(v)} \rightarrow \textbf{(vi)} \rightarrow (i)$$

### Polytopes

**Corollary 12.** Let $A$ be a totally unimodular matrix and $P$ the polytope

$$P = \{c^Tx \mid Ax \le b; x \ge 0 \}$$

Then its dual,

$$Q = \{b^Ty \mid A^Ty \ge c; y \ge 0\}$$

also has integer vertices.

## Examples

### Bipartite Graphs

Let $$G = (V, E)$$ be an undirected graph and $$M$$ the incidence matrix of $$G$$. That is, a binary matrix where each line corresponds to a vertex $$v$$ and each column to an edge $$e$$. We have $$M_{v,e} = 1$$ if $$v$$ is an endpoint of $$e$$ or $$M_{v,e} = 0$$ otherwise. Then, we have the following result:

**Theorem 13.** The incidence matrix of a graph $$G$$ is totally unimodular if and only if, $$G$$ is bipartite.

This result can be used to derive the [König-Egerváry](http://en.wikipedia.org/wiki/K%C3%B6nig's_theorem_(graph_theory)) theorem, stating that the maximum cardinality matching and the minimum vertex cover have the same value bipartite graphs.

The maximum cardinality can be modeled as integer linear programming:

$$\max \sum_{e \in E} y_e$$

$$
\begin{array}{llclr}
 & \sum_{e = (u, v)} y_e & \le & 1 & \forall v \in V\\
 & y_e & \in & \{0, 1\} & \forall e \in E
\end{array}$$

And its dual is the minimum vertex cover:

$$\min \sum_{v \in V} x_v$$

$$
\begin{array}{llclr}
 & x_u + x_v  & \ge & 1 & \forall (u,v) \in E\\
 & x_v  & \le & \{0, 1\} & \forall v \in V
\end{array}$$

It's not hard to see that if $$M$$ is the incidence matrix of the graph, then the problems can be stated as

$$(1) \quad \max \{1y \mid My \le 1; y \mbox{ binary} \}$$

and

$$(2) \quad \min \{x1 \mid xM \ge 1; x \mbox{ binary} \}$$

If the graph is bipartite, we can use *Theorem 2* and the [strong duality](http://en.wikipedia.org/wiki/Strong_duality) for linear programs to conclude that $(1) = (2)$.

### Directed Graphs

Let $$D = (V, A)$$ a directed graph, and $$M$$ be the incidence matrix of $$D$$. That is, a matrix where each line corresponds to a vertex and each column to an arc. For each arc $$e = (u, v)$$, we have $$M_{u, e} = -1$$ and $$M_{v, e} = 1$$ and 0 otherwise. For directed graphs, we have a even stronger result:

**Theorem 14.** The incidence matrix of a directed graph $$D$$ is totally modular.

Consider a network represented by $$D$$ and with capacities represented by $$c : A \rightarrow \mathbb{R}_{+}$$. For each directed edge $$(ij) \in A$$, let $$x_{ij}$$ be the flow in this edge.

If $$M$$ is the incidence matrix of $$D$$, then $$Mx = 0$$ corresponds to

$$
(3) \quad \sum_{(iv) \in A} x_{iv} = \sum_{(vi) \in A} x_{vi} \qquad \forall v \in V$$

which are the flow conservation constraints. Since $$M$$ is totally unimodular, then if $$c$$ is integer, it's possible to show that the polytope $$\{x \mid 0 \le x \le c; Mx = 0\}$$ has integral vertices. This polytope also represents the constraints of the max circulation problem.

Now, we can use these observations to prove that the following LP formulation for the max-flow problem has an optimal integer solution:

$$\max \sum_{(si) \in A} x_{si}$$

Subject to:

$$
\begin{array}{llclr}
& \sum_{(iv) \in A} x_{iv} & = & \sum_{(vi) \in A} x_{vi} & \forall v \in V \setminus \{s, t\}\\
0 \le & x_{ij} & \le & c_{ij} & \forall (ij) \in A\\
\end{array}$$

We can see that the constraints matrix of the above formulation is a submatrix of the max circulation problem and by Property 3, it's also TU, which in turn means the corresponding polytope has integral vertices.

## Conclusion

In this post, we introduced the concept of total unimodular matrices and presented two simple examples: the incidence matrix of a bipartite graph and the incidence matrix of a directed graph.

Here's a cool chart of common polynomial time solvable problems organized by their generality [2].

<figure class="center_children">
    <a href="http://kunigami.files.wordpress.com/2222/08/graph.png"><img src="{{site.url}}/resources/blog/2012-09-02-totally-unimodular-matrices/2222_08_graph.png" alt="" /></a>
</figure>

In future posts, we'll keep exploring this subject by studying other examples and properties of TU matrices.

## Appendix

**Theorem 15.** Let $A$ be a totally unimodular matrix and $b$ an integer vector. Then the polytope

$$P = \{x \mid Ax \le b \}$$

has integer vertices.

**Corollary 16.** Let $A$ be a totally unimodular matrix and integer vectors of appropriate dimensions $a$, $b$, $c$ and $d$. Then the polytope

$$P = \{x \mid a \le Ax \le b; c \le x \le d \}$$

has integer vertices.

<proof>
We need to construct the set of inequalities: $Ax \le b$, $-Ax \le -a$, $x \le d$ and $-x \le -c$, which we obtain by multiplying

$$\left[ \begin{array}{c} A \\ -A \\ I \\ -I \end{array} \right]$$

by $x$. Since that matrix can be obtained using properties of TU matrices, we conlude it's TU and by <i>Theorem 15</i>.
</proof>

**Theorem 17.** Let $A$ be a TU matrix. Let $C$ be any subset of columns of $A$. Then there exists a partition of $C$ into two parts $C_1$ and $C_2$ such that the sum of columns in $C_1$ minus those in $C_2$ yields a column vector with entries in $\curly{-1, 0, 1}$.

<proof>
Consider the polytope:

$$P = \curly{x \mid 0 \le x \le d, \left\lfloor \frac{1}{2} A d \right\rfloor \le Ax \le \left\lceil \frac{1}{2} A d \right\rceil}$$

Where $d$ is the characteristic vector of $C$, that is $d_i = 1$ if column $i \in C$ and $d_i = 0$ otherwise. $Ad$ can be seen as the sum of the columns of $A$ belonging to $C$. Let $v = Ad$, so that the $i$-th element of this column vector can be expressed as:

$$v_i = \sum_{j \in C} a_{ij}$$

Then $\lfloor \frac{1}{2} A d \rfloor$ is the column vector where the $i$-th element is: $\lfloor \frac{1}{2} v_i \rfloor$, and analogously for $\lceil \frac{1}{2} A d \rceil$. It's clear that both of these column vectors have integer elements. And since $A$ is TU, by <i>Corollary 16</i> we know that it has integer vertices.
<br /><br />
This polytope is not empty because the vector $x = \frac{1}{2} d$ satisfies all the constraints and is thus an internal point of $P$. Due to $0 \le x \le d$, every integer point in this polytope must be a $\curly{0, 1}$-vector.
<br /><br />
Now define the vector $y = d - 2x$. Let's analyze its contents. If $d_j = 0$, then $x_j = 0$, so $y_j = 0$. Now suppose $d_j = 1$. If $x_j = 0$, then $y_j = 1$ and if $x_j = 1$, then $y_j = -1$. We can interpret $y$ as follows: if $y_j = 1$, then $j$ is a column in $C_1$ and if $y_j = -1$, then $j$ is a column in $C_2$. Thus $Ay$ represents the sum of columns in $C_1$ minus the sum of columns in $C_2$, exactly the thing the theorem wants to calculate. It remains to prove that $Ay$ only contains entries in $\curly{-1, 0, 1}$.
<br /><br />
Let's define $u = Ax$ and $w = Ay$. From the constraints we have that the $i$-th element of $u$ is subject to:

$$(17.1) \left\lfloor \frac{1}{2} v_i \right\rfloor \le u_i \le \left\lceil \frac{1}{2} v_i \right\rceil$$

We can write the $w$ as a function of $u$ and $v$, since $Ay = Ad - 2Ax$:

$$w_i = v_i - 2u_i$$

Multply $(17.1)$ by 2:

$$2 \left\lfloor \frac{1}{2} v_i \right\rfloor \le 2 u_i \le 2 \left\lceil \frac{1}{2} v_i \right\rceil$$

if $v_i$ is even, then this equation reduces to $2u_i = v_i$ and $w_i = 0$. If $v_i$ is odd, say $2k + 1$, then $2 \lfloor \frac{1}{2} v_i \rfloor = 2k = v_i - 1$ and $2 \lceil \frac{1}{2} v_i \rceil = 2k + 2 = v_i + 1$, so we get $v_i - 1 \le 2u_i \le v_i + 1$ so $-1 \le w_i \le 1$. Since all elements involved are integers, $u_i$ and $v_i$ are also integers and thus $w_i$. This then proves $Ay$ only contains entries in $\curly{-1, 0, 1}$.

QED

</proof>


## References

* [[1](http://www.amazon.com/Theory-Integer-Programming-Alexander-Schrijver/dp/0471982326/)]
 Theory of Linear and Integer Programming - A. Schrijver
* [[2](http://www.imada.sdu.dk/~marco/Teaching/Fall2009/DM204/Slides/TUM-lau.pdf)]
 Marco Chiarandini - Scheduling, Timetabling and Routing DM204 (Lecture Notes)
* [[3](http://www.lehigh.edu/~tkr2/teaching/ie418/lectures/Lecture8.pdf)]
 Ted Ralphs - Integer Programming IE418 (Lecture Notes)
