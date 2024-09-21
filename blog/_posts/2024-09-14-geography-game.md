---
layout: post
title: "The Geography Game"
tags: [computational complexity, combinatorial optimization, graph theory]
vanity: "2024-09-14-geography-game"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/globe.svg" alt="SVG of a globe" style="width: 100px;" />
</figure>

In this post we'd like to discuss the Geography game. In the variant we'll consider, an instance of the game is an undirected graph, and a token placed in one of its vertices.

Two players take turn in moving the token to an adjacent vertex with the constraint that no vertex can be visited twice. If a player cannot move the token without violating that constraint, they lose.

Assuming both players play optimally, it turns out that we can determine which player will win. We'll also present the optimal strategy each player can use and discuss other variants.

<!--more-->


## Definitions

Let's provide a more formal definition of the problem and some related terminology. Let $G = (V, E)$ be an undirected graph and $v \in V$ a vertex on which a token is placed. We denote by $G \setminus \curly{v}$ the graph with vertex $v$ removed, i.e. with vertices $V \setminus \curly{v}$ and edges $E \setminus \curly{(v, u) \in E, u \in V}$.

Player 1 is the first to go and they can only move the token using an edge $(v, u) \in E$. Then, vertex $v$ is removed from the graph and it's player 2's turn with a starting vertex $u$ and graph $G \setminus \curly{v}$. A player loses if their starting vertex is isolated (i.e. they cannot move the token).

The problem consists in determining which player will win (or equivalently which one will lose). We'll see how to solve this problem in the next section.

Before that, let's define some useful terminology. A **matching** $M$ of a graph $G = (V, E)$ is a subset of $E$ such that no two edges are incident to the same vertex. The cardinality of a matching is simply the number of edges in it and denoted by $\abs{M}$.

A **maximum matching** of $G$ is a matching of the largest cardinality, in other words, if $M^\*$ is a maximum matching, no other matching $M$ of $G$ exists such that $\abs{M} \gt \abs{M^\*}$. Note that there may be multiple maximum matchings.

A matching **saturates** a vertex $v \in V$ if it contains an edge incident to $v$.

## Solving the Problem

We're ready to state the winning condition of the geography game:

**Lemma 1.** Player 1 wins (and player 2 loses) if and only if all maximum matchings $M$ of $G$, $M$ saturates $v$.

<proof>
Suppose there is some maximum matching $M'$ which does not saturate $v$. Then every neighbor $u$ of $v$ must be saturated in $M'$, otherwise we could include edge $(u, v)$ in the matching and it wouldn't be maximum. So after player 1 moves the token to $u$, player 2 can then move the token along an edge $(u, w)$ in $M'$.
<br /><br />
No other edges incident to $w$ can be in the matching (by definition of matching), so player 1 will then be forced to move along an edge $(w, x)$ not in $M'$. We claim that $M'$ saturates $x$, that is, some edge $(x, y) \in M'$. Otherwise we could remove edge $(u, w)$ from $M'$ and add edges $(v, u)$ and $(w, x)$ to it, contradicting the fact it's maximum (<i>Figure 1.1</i>).

<figure class="center_children">
  <img src="{{resources_path}}/contradict1.svg" alt="See caption."  style="width: 400px;" />
  <figcaption>Figure 1.1. We are assuming edges $(v, u)$ and $(w, x)$ are not in $M'$. If $M'$ does not saturate $x$ then it means no edge incident to $x$ is in $M'$ (top) and we can obtain another matching one unit larger than $M'$ (bottom).</figcaption>
</figure>

<br />
So player 2 can keep using edges in $M'$ and player 1 is forced to use edges not in $M'$ until either of them cannot "extend" this path any longer. This path is then an alternating path (as defined in the theory of matching in graphs [2]). We claim that the last edge on this path is in $M'$. If not, this path would be an <i>augmenting path</i> [2], which would let us obtain a matching of size $\abs{M'} + 1$  (<i>Figure 1.2</i>), and which contradicts the fact that $M'$ is maximum. Thus, player 1 is the one unable to play and loses.

<figure class="center_children">
  <img src="{{resources_path}}/augmenting.svg" alt="See caption."  style="width: 500px;" />
  <figcaption>Figure 1.2. If player 1 was the last to play, we have an augmenting path, in which we alternate between edges in (red) and outside (blue) $M'$. The assumption is the matching saturates neither the first nor the last nodes, so we can "flip" the blue and red edges to obtain a larger matching.</figcaption>
</figure>
<br />
Conversely, if $v$ is saturated, player 1 can pick some maximum matching $M$, which will contain $v$, and move along the edge $(v, u) \in M$. So we now have an instance $G \setminus \curly{v}$, starting at $u \in V$ which has a maximum matching that does not saturate $u$. Since player 2 is the first to play on this new instance, we conclude by prior arguments that player 2 will lose.
<br /><br />
QED.
</proof>

It sounds hard to determine whether *all* maximum matchings saturate $v$. However, it's easier than it seems, we don't have to enumerate all of them. We just need to find *any* maximum matching $M$ of $G$ and *any* maximum matching $M'$ of $G \setminus \curly{v}$. If the size of $\abs{M'} = \abs{M}$, then there's at least one maximum matching in $G$ that does not saturate $v$, but if $\abs{M'} \lt \abs{M}$, we know for sure $v$ is needed in all maximum matchings of $G$!

A maximum matching can be found in polynomial time, for example via the [Blossom algorithm](https://en.wikipedia.org/wiki/Blossom_algorithm), which is $O(\abs{E}\abs{V}^2)$. This lead us to the corollary:

**Corollary 2.** Let $G = (V, E), v \in V$ be an instance of the undirected vertex Geography game. It's possible to determine which player loses first in polynomial time.

## Optimal strategy

Summarizing the proof, the optimal strategy for players depend on whether the condition of *Lemma 1* is satisfied. We consider both cases:

*Case 1.* All maximum matches saturate $v$, so player 1 wins. In this case player 1 can win by computing any maximum matching $M$ and moving along an edge in $M$. No matter which move player 2 makes, it's guaranteed that player 1 will be able to continually move along edges in $M$. Further, player 1 will be the last one to move, so player 2 loses.

*Case 2.* There's at least one maximum match $M$ that doesn't saturate $v$, so player 2 wins. Supose they move along an edge $(v, u)$. Then player 2 computes any maximum match $M'$ of $G \setminus v$. The proof shows that player 2 can keep moving along edges in $M'$ throughout the game, no matter which move player 1 makes, and they will be the last to play, so player 1 loses.

**Algorithm.** Even though it's possible to determine which case a given instance belongs to, players don't need to do this. Their optimal strategy is to compute the maximum match of the graph in their first respective turn. Then always try to move along edges in that matching. If they can't, it's a sign they can't win this instance (assuming the other player plays perfectly), so they might as well choose a random edge.

## Generalizations and Variants

A generalization of the problem is one in which we don't specify the starting vertex $v$, but let player 1 choose it.

We can extend the solution of the original problem to this one. Player 1 first computes the maximum matching $M$ of $G$. Then they iterate over each vertex $v$ until they find a matching $M'$ of $G \setminus \curly{v}$ with $\abs{M'} = \abs{M}$. Once they do, they pick that vertex which will give them the victory.

If they cannot find any such vertex, they will lose, so they can just play randomly.

The authors in [1] consider other variants of this problem: the case where the graph is directed, and the case where only the edges cannot be reused, by the vertices can. Surprisingly, all these variants are NP-Complete!

## Conclusion

The solution to the Geography game is really neat. It's non trivial to me that it boils down to computing maximum matchings!

An interesting thing is that the edge-based version of this game is NP-Complete, while the vertex one is in P. A parallel can be drawn between the Hamiltonian path problem (NP-Complete) and the Eulerian path (P). The Hamiltonian is vertex-constrained while the Eulerian is edge-constrained.

## Related Posts

[Eulerian Circuits]({{blog}}/2018/11/26/eulerian-circuits.html). In that post we discussed the Eulerian circuit which is closely related to the Eulerian path, and can also be solved in polynomial time. The "vertex version" of this problem is the [Travelling Salesman Problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem) which is NP-Complete.

## References

* [1] A. S. Fraenkel, E. R. Scheinerman and D. Ullman. Undirected edge geography. Theoretical Computer Science 112, 371–381 (1993).
* [[2](https://en.wikipedia.org/wiki/Matching_(graph_theory))] Wikipedia - Matching (graph theory)
