---
layout: post
title: "Totally Unimodular Matrices"
tags: [combinatorics, integer programming, linear algebra]
---

An **unimodular matrix** is a square matrix with integer entries such that its determinant is either $-1$, $0$ or $1$. A matrix is said **totally unimodular** (TU for short) if all its square submatrices are unimodular.

[Sometime ago]({{blog}}/2010/08/13/dijkstra-and-the-longest-path.html), we said that problems such as the minimum path, maximum flow and minimum cost max flow can be modeled using linear programming with the interesting property that the optimal solutions are always integer.

In that post, we also said that it was because the coefficient matrix of the constraints of the formulations are totally unimodular. More formally, we have the following theorem:

**Theorem 1.** Let $$A$$ be a totally unimodular matrix and $$b$$ be an integer vector. Then, the polyhedra $$P = \{x \mid Ax \le b\}$$ has integer vertices.

In this post we'll present some properties of TU matrices and discuss about two simple examples.

## Properties

Let $A$ be a totally unimodular matrix. Then we have the following properties:

**Property 1.** All elements in $A$ are either $0$, $1$ or $-1$.

<proof>
This follows from the definition because a 1x1 matrix is a submatrix and its determinant is equal to itself.
</proof>

**Property 2.** Its transpose, $A^{T}$, is TU.

<proof>
Since we're only considering square submatrices, every submatrix of $A^{T}$ is the transpose of some submatrix of $A$. Since determinants are invariant with transposition, it follows that the submatrices of $A^{T}$ satisfy the determinant constraint and thus $A^{T}$ is TU.
</proof>

**Property 3.** The matrix obtained by appending the identity matrix to $$A$$, that is $$[A, I]$$, is TU

**Property 4.** Any submatrix of $$A$$ is TU.

<proof>
The set of submatrices of $A$ includes all submatrices of any of its submatrices. Since $A$ is TU, all its submatrices satisfy the determinant constraints. For any submatrix $A'$ of $A$ its submatrices are a subset of those of $A$, so it follows that all its submatrices satisfy the determinant constraints and hence $A'$ is TU.
</proof>

**Property 5.** The matrix obtained by multiplying any row of $$A$$ by -1 is TU

**Property 6.** The matrix obtained by duplicating any row of $$A$$ is TU

Using this properties we can get some Corollaries from Theorem 1.

Since $$[A, -I]$$ is TU, we have the following

**Corollary 2.** The polytope $$P = \{x \mid Ax \le b; x \ge 0 \}$$ has integer vertices.

Also, since $$[A^T, -A^T, I, -I]$$ is TU,

**Corollary 3.** The dual of $$P = \{c^Tx \mid Ax \le b; x \ge 0 \}$$, namely $$Q = \{b^Ty \mid A^Ty \ge c; y \ge 0\}$$ has also integer vertices.

## Examples

### Bipartite Graphs

Let $$G = (V, E)$$ be an undirected graph and $$M$$ the incidence matrix of $$G$$. That is, a binary matrix where each line corresponds to a vertex $$v$$ and each column to an edge $$e$$. We have $$M_{v,e} = 1$$ if $$v$$ is an endpoint of $$e$$ or $$M_{v,e} = 0$$ otherwise. Then, we have the following result:

**Theorem 4.** The incidence matrix of a graph $$G$$ is totally unimodular if and only if, $$G$$ is bipartite.

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

(1) $$\max \{1y \mid My \le 1; y \mbox{ binary} \}$$ and

(2) $$\min \{x1 \mid xM \ge 1; x \mbox{ binary} \}$$

If the graph is bipartite, we can use Theorem 2 and the [strong duality](http://en.wikipedia.org/wiki/Strong_duality) for linear programs to conclude that (1) = (2).

### Directed Graphs

Let $$D = (V, A)$$ a directed graph, and $$M$$ be the incidence matrix of $$D$$. That is, a matrix where each line corresponds to a vertex and each column to an arc. For each arc $$e = (u, v)$$, we have $$M_{u, e} = -1$$ and $$M_{v, e} = 1$$ and 0 otherwise. For directed graphs, we have a even stronger result:

**Theorem 5.** The incidence matrix of a directed graph $$D$$ is totally modular.

Consider a network represented by $$D$$ and with capacities represented by $$c : A \rightarrow \mathbb{R}_{+}$$. For each directed edge $$(ij) \in A$$, let $$x_{ij}$$ be the flow in this edge.

If $$M$$ is the incidence matrix of $$D$$, then $$Mx = 0$$ corresponds to

$$
\begin{array}{llclr}
(3) & \sum_{(iv) \in A} x_{iv} & = & \sum_{(vi) \in A} x_{vi} & \forall v \in V\\
\end{array}$$

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

## References

* [[1](http://www.amazon.com/Theory-Integer-Programming-Alexander-Schrijver/dp/0471982326/)]
 Theory of Linear and Integer Programming - A. Schrijver
* [[2](http://www.imada.sdu.dk/~marco/Teaching/Fall2009/DM204/Slides/TUM-lau.pdf)]
 Marco Chiarandini - Scheduling, Timetabling and Routing DM204 (Lecture Notes)
* [[3](http://www.lehigh.edu/~tkr2/teaching/ie418/lectures/Lecture8.pdf)]
 Ted Ralphs - Integer Programming IE418 (Lecture Notes)
