---
layout: post
title: "The Hungarian Algorithm"
tags: [combinatorial optimization, graph theory, integer programming]
vanity: "2022-12-02-hungarian"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}


<figure class="image_float_left">
    <a href="https://en.wikipedia.org/wiki/File:Harold_W._Kuhn.jpg">
        <img src="{{resources_path}}/kuhn.jpg" alt="Harold W. Kuhn thumbnail" />
    </a>
</figure>

Harold William Kuhn was an American mathematician, known for the Karush–Kuhn–Tucker conditions and the Hungarian method for the assignment problem [1].

According to Wikipedia [2], Kuhn named the algorithm *Hungarian method* because it was largely based on the earlier works of Hungarian mathematicians Dénes Kőnig and Jenő Egerváry.

However in 2006, the mathematician Francois Ollivier found out that Carl Jacobi (known for [Jacobian matrices](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)) had already developed a similar algorithm in the 19th century in the context of systems of differential equations and emailed Kuhn about it [6].

One fascinating coincidence is that Jacobi is Kuhn's ancestral advisor according to the Mathematics Genealogy Project! [5]. Here's the ancestry chain (year when they got their PhD in parenthesis):

* [Harold William Kuhn](https://mathgenealogy.org/id.php?id=27174) (1950)
* [Ralph Hartzler Fox](https://mathgenealogy.org/id.php?id=15155) (1939)
* [Solomon Lefschetz](https://mathgenealogy.org/id.php?id=7461) (1911)
* [William Edward Story](https://mathgenealogy.org/id.php?id=7451) (1875)
* [Carl Gottfried Neumann](https://mathgenealogy.org/id.php?id=32858) (1856)
* [Friedrich Julius Richelot](https://mathgenealogy.org/id.php?id=57706) (1831)
* [Carl Gustav Jacob Jacobi](https://mathgenealogy.org/id.php?id=15635) (1825)

If this genealogy is to be trusted, I wonder if Kuhn was aware of this fact, given he wrote about Jacobi's life in [6].

In this post we'll explore this algorithm and provide an implementation in Python.

<!--more-->


## The Balanced Assignment Problem

Suppose we have a bipartite graph with a set of vertices $S$ and $T$ of same size $n$, with a set of edges $E$ edges between them associated with a non-negative weight. A matching $M$ is a subset of $E$ such that no two edges in $M$ are incident to the same vertex. A perfect matching is one that covers every vertex. The weight of a matching is the sum of weighs of its edges.

The balanced assignment problem asks to find the perfect matching with the maximum weight. We can reduce several variants of this problem to this specific version.

### Negative weights

We can assume all the weights are non-negative. If there are negative weights, let $W$ be the smallest weight. We add $W$ to all edges so they're now non-negative. Since the perfect matching has exactly $n$ edges, we just need to discount $nW$ from our solution.

### Incomplete graph

We can assume the bipartite graph is complete. If the original graph is not, we can always add artificial edges with weight 0 and remove them later from the solution without affecting the optimal value.

If it's incomplete *and* has negative weights, we can simply ignore those by setting those to 0. A matching of maximum weight does not include any negative edges *unless* we also require the matching to be of maximum cardinality. In this case the reduction to the problem at hand is non-trival.

### Unbalanced assignment

We can assume that both partitions have the same size. If in the original graph they aren't, we can add artificial vertices (and then make them complete as above) and remove edges incident to them later from the solution without affecting the optimal value.

## Integer Linear Programming

We can formulate the assignment problem as follows. Suppose we're given weights $w_{ij}$ for $1 \le i, j \le n$. We introduce binary variables $x_{ij}$ where $x_{ij} = 1$ indicates the edge $(i,j)$ is in the matching.

The objective function is thus:

$$\mbox{maximize} \qquad \sum_{i=1}^{n} \sum_{j=1}^{n} x_{ij} w_{ij}$$

Subject to:

$$
\begin{align}
(1) \quad & \sum_{j = 1}^{n} x_{ij} & \le 1 & \qquad \qquad 1 \le i \le n  \\
(2) \quad & \sum_{i = 1}^{n} x_{ij} & \le 1 &  \qquad \qquad 1 \le j \le n  \\
(3) \quad & x_{ij} &  \in \curly{0, 1} &  \qquad \qquad 1 \le i, j \le n \\
\end{align}
$$

Let's find the [dual](https://en.wikipedia.org/wiki/Dual_linear_program) of this ILP. Constraints (1) map to variables we'll name $u$ and constraints (2) map to variables we'll name $v$.

$$\mbox{minimize} \qquad \sum_{i=1}^{n} u_i + \sum_{j=1}^{n} v_j$$

Subject to:

$$
\begin{align}
(4) \quad & u_i + v_j \ge w_{ij}, & \qquad \qquad 1 \le i, j \le n \\
(5) \quad & u_i \ge 0, & \qquad 1 \le i \le n\\
(6) \quad & v_j \ge 0, & \qquad 1 \le j \le n\\
\end{align}
$$

It's possible to show that if there are feasible solutions $x^{\*}_{ij}$ and $u^{\*}_i$, $v^{\*}_j$ such that their objective functions are equal, that is,

$$\sum_{i=1}^{n} \sum_{j=1}^{n} x^{*}_{ij} w_{ij} = \sum_{i=1}^{n} u^{*}_i + \sum_{j=1}^{n} v^{*}_j$$

then they're optimal solutions for their respective formulations. Another characterization of optimizality is that the variables satisfy the complementarity constraints:

$$
\begin{align}
x_{ij} (u_i + v_j - w_{ij}) &= 0, & \qquad \qquad 1 \le i, j \le n\\
u_i (1 - \sum_{j = 1}^{n} x_{ij}) &= 0 & \qquad \qquad 1 \le i \le n \\
v_j (1 - \sum_{i = 1}^{n} x_{ij}) &= 0 & \qquad \qquad 1 \le j \le n \\
\end{align}
$$

Another way to have these constraints:

$$
\begin{align}
(7) \quad & x_{ij} \gt 0 & \quad \rightarrow \quad &  u_i + v_j = w_{ij} \\
(8) \quad & u_i \gt 0 & \quad \rightarrow \quad & \sum_{j = 1}^{n} x_{ij} = 1 \\
(9) \quad & v_i \gt 0 & \quad \rightarrow \quad & \sum_{i = 1}^{n} x_{ij} = 1 \\
\end{align}
$$

## The Hungarian algorithm

The Hungarian algorithm aims to solve the ILP by finding solutions that satisfy (1)-(9). Initially we set

$$
\begin{align}
u_i &= \max(w_{ij}), \qquad 1 \le i \le n\\
v_j &= 0\\
x_{ij} &=0\\
\end{align}
$$

*Figure 1* depicts an example from [5] which we'll use throughout this post. It displays the value of variables such $u$ (to the left of the $S$ partition), $v$ (to the right of the $T$ partition), $w_{ij}$ (along the corresponding edge) and the difference $u_i + v_j - w_{ij}$ (defined as *slack* later, in parenthesis).

<figure class="center_children">
  <img src="{{resources_path}}/initial.png" alt="See caption." />
  <figcaption>Figure 1: Initial choice of the dual variables.</figcaption>
</figure>

We can verify this satisfies all constraints except (8). The algorithm will then iterate on satisfying these constraints without ever violating the other ones. We'll now describe a high-level way to do this.

### Max matching in induced graph

First, consider the graph $G(u,v)$ which is the original bipartite graph but only with those edges $(i, j)$ satisfying $u_i + v_j = w_{ij}$. We call such edges **tight**, whereas those with $u_i + v_j \gt w_{ij}$ are **loose**. The amount to remove from $u_i + v_j$ of a loose edge to turn it into a tight edge is called **slack** and denoted by $\delta = u_i + v_j - w_{ij}$.

From our initial choice of $u$ every node in $S$ has at least one tight edge incident to it. To see why, let $j_i$ such that $w_{ij_i} = \max(w_{ij}) = u_i$. Since $v_{j_i} = 0$, we have $u_i + v_{j_i} = w_{ij_i} + 0 = w_{ij_i}$. Thus edge $(i,j_i)$ is in $G(u,v)$.

We then try to find a maximum cardinality matching $M_{uv}$ in $G(u,v)$. If we find a perfect match (i.e. cardinality $n$), then setting $x_{ij} = 1$ for edges in the matching will satisfy (8) and we're done.

If not, we need to add edges to $G(u,v)$ by manipulating $u$ and $v$ such that the resulting $G(u,v)$ will allow a bigger matching. We need some theory first.

*Figure 2* shows the induced graph corresponding to our initial example where the edges are tight and the red bold edge is some maximal matching in such graph.

<figure class="center_children">
  <img src="{{resources_path}}/induced.png" alt="See caption." />
  <figcaption>Figure 2: Induced graph with a maximal matching.</figcaption>
</figure>

### Berge's theorem

We note that the matching $M_{uv}$ is also a matching in the original graph. Since the original graph is complete it has to have a perfect match, so $M_{uv}$ is not maximal there. We'll now see how to increase or augment the size of $M_{uv}$.


Let $M$ be a matching in a bipartite graph. We define an **alternating path** in $M$ as a path in the graph whose edges alternate between those in the matching and those not. If no edge in the matching is incident to a vertex $v$, we say the vertex is **unmatched**. If an alternating path starts *and* ends in an unmatched vertex, we call it an **augmenting path**.

The reason it's called *augmenting* is that it's can be used to increase the size of a matching. The augmenting path necessarily has an odd number of edges, say $2k + 1$, and $k + 1$ of these edges are not in the matching, while $k$ are. We can increase the matching by 1 by simply putting all edges not in the matching into the matching and vice-versa. *Figure 3* has an example.

<figure class="center_children">
  <img src="{{resources_path}}/augmenting.png" alt="See caption." />
  <figcaption>Figure 3: Example of an augmenting path in a match of size 2. By "flipping" the edges in-out of the matching we augment it by one</figcaption>
</figure>

[Berge's theorem](https://en.wikipedia.org/wiki/Berge%27s_theorem) states that a matching $M$ in a graph is maximum if and only if there is no augmenting path in $M$. This implies that if the matching is not maximal, there must exist an augmenting path we can use to increase the size of $M$.

Starting from the fact that $M_{uv}$ is maximal in $G(u,v)$ but not in $G$, we can use Berge's theorem to claim there is an augmenting path $P$ in $G$ that doesn't exist in $G(u,v)$.

How can we find $P$? We first need to introduce an auxiliary structure.

### Forest of alternating paths

Let $r$ be an unmatched vertex from $S$. Traverse the graph from $r$ but only following alternating edges and do not visit any vertex more than once. One way to achieve this is to orient the edges: for every edge in $(i,j) \in G(u,v)$ with $i \in S, j \in T$, we orient $i \rightarrow j$ if $(i,j)$ is not in $M_{uv}$ and orient $j \rightarrow i$ otherwise. Then we can just do a BFS in this directed graph from $r$. Let $C(r)$ be the tree corresponding to such traversal.

Repeat this for every other unmatched vertex in $S$, with the care not to visit any vertex that belongs to some other tree. The result of these traversals is a collection of disjoint trees, or **forest of alternating paths** which we'll call $F$.

<figure class="center_children">
  <img src="{{resources_path}}/crown_shyness.jpg" alt="See caption." />
  <figcaption>Figure 4: <a href="https://en.wikipedia.org/wiki/Crown_shyness">Crown Shyness</a>: a phenomenon observed in some tree species, in which the crowns of trees do not touch each other. Evoking the disjointness of the trees in our data structure.</figcaption>
</figure>

Note that this forest does not necessarily contain all edges from the matching.

<figure class="center_children">
  <img src="{{resources_path}}/forest.png" alt="See caption." />
  <figcaption>Figure 5: Example of a forest (with a single tree, colored green) that does not include the edge (3, 3) which is the matching. 3 does not form a tree because it's already matched.</figcaption>
</figure>

### Adding edges to the forest

We can manipulate $u$ and $v$ in order to add an edge to $F$. Consider some edge $(x, y)$ where $x \in S$ and $x \in F$, $y \in T$ and $y \not \in F$ and defined as a **frontier edge**. Let $\delta = u_x + v_y - w_{xy}$. If we subtract $\delta$ for all $u_i$, $i \in F \cap S$ and add $\delta$ for all $v_j$, $j \in F \cap T$, it's easy to see that all edges in $F$ will continue to be tight. Since we didn't add $\delta$ to $v_y$ but subtracted from $u_x$, edge $(x, y)$ is now tight.

However, by doing this we might violate (4), i.e. $u_i + v_j \ge w_{ij}$, for some edges. Suppose there is another frontier edge $(x', y')$ with slack $\delta' = u_x' + v_y' - w_{x'y'} \lt \delta$. We have that $u_x' + v_y' - \delta' = w_{x'y'}$. If we subtract $\delta$ from $u_x'$ and leave $v_y'$ unchanged, we get $u_x' + v_y' - \delta \lt u_x' + v_y' - \delta' = w_{x'y'}$ which violates (4).

To avoid this, we choose the smallest slack $\delta$ among all frontier edges. We're guaranteed to add at least one edge to the forest $F$ without violating any new constraints. *Figure 6.* shows an example.

Now with $(x, y)$ added, we have that either $y$ is unmatched which means we just found an augmenting path, or $y$ is part of an edge $(x', y)$ in the matching that is not reachable by any of the root vertices in $F$, but we can now add it to $F$, since $(x, y) + (x', y)$ is alternating.

<figure class="center_children">
  <img src="{{resources_path}}/dual_change.png" alt="See caption." />
  <figcaption>Figure 6: Before and after the dual variable changes. The dashed edges are not in the forest on the left, but they have the smallest slack, 1. So this is what we're going to subtract from the green nodes in S and add to the green nodes in T. With these new edges a augmenting path can be found, as shown on the right as a sequence of blue and red edges, starting from 3 in S.</figcaption>
</figure>

Once we find an augmenting path $P$ and "flip" the edges to increase the matching $M_{uv}$ by one, we need to recalculate the forest of alternating paths because the first vertex of $P$ is no longer unmatched.

Every time we do a change of dual variables we either increase the matching size by one, or increase the forest by 2 edges. When we increase the matching, we recalculate the forest so we can't assume the size of the tree will remain constant, but even if tree was emptied out every time that happened, there would still be a ceiling of $O(n^2)$ change of dual variables that can happen.

This proves the algorithm finishes. Let's now prove it is correct.

### Optimality

Suppose by contradition that there are no frontier edges in $F$ and the corresponding matching $M_{uv}$ is not perfect. By Berge's theorem, there is an augmenting path $P$ for $M_{uv}$ in $G$.

Let $r$ be its first vertex. By definition, it's unmatched and w.l.o.g. assume it's in $S$ and thus root of some tree in $F$. Let $(i, j)$ be the first edge that doesn't belong to $C(r)$. There are a few scenarios to consider.

**Case 1.** Suppose $(i, j)$ exists in $G(u,v)$. This is possible because we might not add an edge $(i,j)$ to  $C(r)$ if the vertex $j$ has already been visited by some other tree, rooted at $r'$. This is the same as *Case 2.2*.

**Case 2.** Suppose $(i, j)$ does not exist in $G(u,v)$. We have that $i$ belongs to $F$ (because we're assume the edge $(i, j')$ preceding $(i, j) \in P$ exists in $F$). We have now 2 subcases:

**Case 2.1** $j \not \in F$. Then $(i, j)$ is a frontier edge but that contradicts our initial hypothesis. Hence this case can't happen.

**Case 2.2** $j \in F$. Then $j$ belongs to the tree of another root $r'$. Let $Q$ be the (alternating) path from $r'$ to $j$ and $P_j$ be the part of path $P$ starting at $j$ and $P_i$ the part of path $P$ ending in vertex $i$. Since $P_i$ belongs to the tree of $r$, it's disjoint of $Q$ and hence $Q + P_j$ forms a path, it's an augmenting one and it has at least one fewer edge not in $F$ than $P$, so this eventually reduces to *Case 2.1*.

## Python Implementation

With the ideas discussed above we are ready to implement this algorithm. There are three major routines: building the alternating path forest, augmenting an augmenting path and changing the dual variables.

Let's start with `augment()`:

{% highlight python %}
def augment(self, j):
  while j is not None:
    i = self.parent[T][j]
    self.match[T][j] = i
    self.match[S][i] = j
    j = self.parent[S][i]
{% endhighlight %}

In this method `parent[.][x]` is the parent of vertex `x` in the forest. `match[.][x]` is the index of the vertex matched with `x`. `T` is an alias to 1 and `S` to 0. I've opted to use a $2 \times n$ matrix to store information about the vertices on the bipartite graph.

The idea is simple: we start an unmatched $j \in T$ so we know that the edge $(i, j)$ with its parent $i$ is not in the match, so we can match $i$ and $j$. Note that $i$ was matched with its parent $j'$, but we don't need to unmatch because $j'$ will be eventually rematched.

Next we explore `change_duals()`:

{% highlight python %}
def change_duals(self):
  delta = min(v for v in self.slack if v > 0)

  for i in range(self.n):
    if self.visited[S][i]:
      self.u[i] -= delta

  for j in range(self.n):
    if self.visited[T][j]:
      self.v[j] += delta
    # frontier edge
    elif self.slack[j] > 0:
      self.slack[j] -= delta
      # note: self.parent[j] has been set
      # during the forest visit
      if self.slack[j] == 0:
        self.candidates.put((1, j))
{% endhighlight %}

Here `slack[j]` represents the slack for $j \in T$. If slack is positive then `(parent[T][j], j)` is either a frontier edge or it's an unreacheable vertex, in which case `slack[j] = INF` so `delta = min(...)` works.

Then we proceed to subtract `delta` from `u` and add to `v`. We also update `slack` to keep it consistent with `u` and `v`. `visited[.][x]` indicates whether the vertex belongs to the forest.

We finally have an optimization: if `slack[j]` became 0, we added `j` to the forest, so we can continue building the forest from `j` instead of doing it from scratch.

We now define `visit_forest()`:

{% highlight python %}
def visit_forest(self):
  while not self.candidates.empty():
    [side, c] = self.candidates.get()

    if self.visited[side][c]:
      continue
    self.visited[side][c] = True

    if side == S: # c in S
      self.visit_s(c)
    else: # c in T
      augmented = self.visit_t(c)
      if augmented:
        return True

  return False
{% endhighlight %}

This method basically implements a BFS using a queue. The current nodes to be visited are stored in `candidates`. We have to handle vertices in $S$ and $T$ differently.

When visiting $T$, there's a chance we find an augmenting path. If we do, we'll need to restart the construction of the forest, so we have to short-circuit.

For $i \in S$ we call `visit_s()`:

{% highlight python %}
def visit_s(self, i):
  # update pi from edges not in match
  for j in range(self.n):
    if self.match[S][i] == j:
      continue

    slack = self.u[i] + self.v[j] - self.adj[S][i][j]
    if slack < self.slack[j]:
      self.slack[j] = slack
      self.parent[T][j] = i

      # edge (i,j) is now in the forest. keep visiting
      if slack == 0:
        self.candidates.put((1, j))
{% endhighlight %}

We first observe that we skip visiting its match. That's because if $(i, j) \in M_{uv}$, then we arrived at $i$ from $j$ already.

We just need to update the slacks from its neighbors in $T$ in $G$. Note that we update the `slack` and `parent` even for edges that are not in $G(u, v)$. This is important for the `change_duals()`, since once we change `u` and `v` the `slack` on that edge might go to 0 and cause it to be added.

For $j \in T$ we call `visit_t()`:

{% highlight python %}
def visit_t(self, j):
  i = self.match[T][j]
  # found an augmenting path
  if i is None:
    self.augment(j)
    return True

  self.parent[S][i] = j
  self.candidates.put((0, i))
  return False
{% endhighlight %}

If $j$ is unmatched, we found an augmenting path so we can call `augment(j)` and stop trying to visit/construct the forest.

Otherwise we keep constructing the alternating trees by following the edge from the match (recalling that `j` was reached from a non-match edge).

We can put it together in `expand_matching()`:

{% highlight python %}
def expand_matching(self):
  self.init_candidates()
  self.init_parent()
  self.init_visited()
  self.init_slack()

  while True:
    augmented = self.visit_forest()
    if augmented:
      return

    self.change_duals()
{% endhighlight %}

We first reset all the variables so we can the recreate the forest from scratch. The only non-trivial `init_*` function is `init_candidates()` which initializes the `candidates` queue with all the unmatched vertices from $S$.

Then we do a mix of BFS exploration with dual variable changes until we are able to augment an augmenting path. Finally the main routine `solve()`:

{% highlight python %}
def solve(self):
  self.init_match()
  self.init_u()
  self.init_v()

  # for each iteration we should increase the matching side
  for t in range(self.n):
    self.expand_matching()

  return sum(self.u) + sum(self.v)
{% endhighlight %}

First we initialize the dual variables and the match which don't need to be reset during the execution of the algorithm. Recalling that `init_u()` starts with the maximum weight of edges incident to each vertex in $S$:

{% highlight python %}
def init_u(self):
  u = [0]*self.n
  for i in range(self.n):
    for j in range(self.n):
      u[i] = max(u[i], self.adj[S][i][j])

  self.u = u
{% endhighlight %}


It then calls the `expand_matching()` function `n` times. Each `expand_matching()` increases the matching size by at least one, so this is enough to find the optimal matching.

After the loop, since `u` and `v` are optimal, we can use the objective function of the dual ILP which is a bit simpler to compute than adding the weights of the edges in the matching.

The full [Python]({{github}}/hungarian.py) implementation as well as a [C++ implementation]({{github}}/hungarian.cpp) one are available on Github.

### Runtime complexity

We argue that `expand_matching()` is $O(n^2)$, the cost of doing a BFS on the graph. Even when we call `change_duals()` we don't re-do the BFS from scratch but continue from where we stopped, so we visit each edge at most once in `expand_matching()`.

We only reset the BFS once we do `augment()` but we only do it $O(n)$ times through the algorithm, so the total runtime complexity is $O(n^3)$.

## Conclusion

I was studying the Hungarian method from Lawler's *Combinatorial Optimization: Networks and Matroids* [3] and it was incredibly hard to convert the provided pseudo-code into a working implementation.

I missed several details like the fact that the graph has to be complete (if it's not, the algorithm is unable to find augmenting paths correctly) and the partitions of the same size. There's vagueness about in which order to visit vertices or terms like unscanned.

What ultimately helped me understand the algorithm in detail was Topcoder's article [4] and Wikipedia [2].

The Hungarian is another one of the algorithms I used multiple times but never understood in detail until writing about it, like the [KMP]({{blog}}/2016/03/13/tree-ring-matching-using-the-kmp-algorithm.html).

## Related Posts

[An Introduction to Matroids]({{blog}}/2013/11/11/lawler-and-an-introduction-to-matroids.html) - In that post we talk about the greedy algorithm for finding minimum/maximum spanning tree, known as the Kruskal algorithm. The idea behind the change of dual variables in the Hungarian algorithm vaguely reminds me of the Kruskal algorithm, in which we choose the edge with lowest weight to add to the existing forest.

[Lagrangian Relaxation]({{blog}}/2012/02/05/lagrangean-relaxation-theory.html) - Duality is utilized in Lagrangian Relaxation to obtain upper bounds (in case of maximization) for branch-and-bound algorithms.

[Totally Unimodular Matrices]({{blog}}/2012/09/02/totally-unimodular-matrices.html) - The incidence matrix of a bipartite graph is totally unimodular (TU). This allows deriving the König-Egerváry theorem (*the* Hungarians) which says that maximum cardinality matching and minimum vertex cover are duals in bipartite graphs.

As we described, the Hungarian algorithm uses maximum cardinality matching as a sub-routine. There's a variant which we didn't mention which uses minimum vertex cover as sub-routine [2] (see *Matrix interpretation*).

## References

* [[1](https://en.wikipedia.org/wiki/Harold_W._Kuhn)] Wikipedia: Harold W. Kuhn
* [[2](https://en.wikipedia.org/wiki/Hungarian_algorithm)] Wikipedia: Hungarian Algorithm
* [3] Combinatorial Optimization: Networks and Matroids, Eugene Lawler.
* [[4](https://www.topcoder.com/thrive/articles/Assignment%20Problem%20and%20Hungarian%20Algorithm)] Topcoder: Assignment Problem and Hungarian Algorithm
* [[5](https://mathgenealogy.org)] Mathematics Genealogy Project
* [6] A tale of three eras: The discovery and rediscovery of the Hungarian Method, Harold W. Kuhn.
