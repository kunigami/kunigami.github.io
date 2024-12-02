---
layout: post
title: "The Computational Complexity of Uno"
tags: [computational complexity, puzzle]
vanity: "2024-10-30-how-hard-is-uno"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/uno-logo.svg" alt="SVG of the Uno logo" />
</figure>

In this post we provide a summary of the results from the paper *UNO is hard, even for a single player* by Demaine et al [1]. To keep it shorter, basic familiarity with the [rules of UNO](https://www.unorules.com/) [2], the card game, is expected.

<!--more-->

## Model

The problem considered by [1] is a simplified model of UNO. Firstly, *action cards*, the ones that reverse turns or makes the opponent draw cards are not included, only the cards with a number and color are used. However, an arbitrary set of numbers and colors is allowed.

Another simplification is that all the cards are given out to the players at the begining (i.e. there's no drawing), players are not necessarily dealt the same number of cards and they play with their hands open (i.e. perfect information). Further, we assume that players play perfectly, so if there's a way to win the game they will play accordingly.

Like in the real Uno, a player can only play a card that matches the number and/or color of the last played card (the first played card can be any). A player who cannot play a card immediately loses⁽¹⁾. In this model, a player can only win if they play all their cards. It's possible there are no winners (e.g. if no player can play any cards they all lose).

More formally, we assume there are $n$ cards, each with a number from $1$ to $b$ and a color from $1$ to $c$. A card is represented by a pair number-color $(i, j)$, $1 \le i \le b$, $1 \le j \le c$. Each player $p$ is dealt the set of cards $C_p$. Players take turns, starting with player 1, then player 2, etc.

The authors consider a few variants of the problem:

* Single-player
* Cooperative two-players
* Uncooperative two-players


## Cooperative two-players

In this variant of the problem, we want to decide whether the players can cooperate to have Player 1 play all its cards.

This problem can be shown to be NP-complete by reducing the Hamiltonian-path problem to it in polynomial time. To recap, given an undirected graph $G = (V, E)$, the Hamiltonian-path problem consists in deciding whether there is a path in $G$ that traverses each vertex in $V$ exactly once. We assume $\abs{V} \le \abs{E}$ and that the graph is connected.

To show that, first we construct an instance of this variant from $G$, with $n = \abs{V} + \abs{E}$ and $b = c = \abs{V}$. Set $C_1$ will correspond to $V$ and $C_2$ to $E$. For each vertex $v_i \in V$, we have a card $(i, i) \in C_1$. For each edge $e = (v_i, v_j)$ in $E$ we have a card $(i, j) \in C_2$.

Now we show that there's a 1:1 mapping between solutions to the Hamiltonian-path problem and this variant of UNO. Let $P = (v_{i_1}, v_{i_2}, \cdots, v_{i_c})$ be a Hamiltonian path in $G$. Then there exists a sequence of plays $(i_1, i_1)_1, (i_1, i_2)_2, (i_2, i_2)_1, \cdots, (i_c, i_c)_1$, where the subscript in the pair identifies the player. This is a feasible solution because it causes Player 1 to win, since they use all their cards.

Conversely, a feasible solution to this UNO variant must be of the form $(i_1, i_1)_1, (i_1, i_2)_2, (i_2, i_2)_1, \cdots, (i_c, i_c)_1$, starting and ending with player 1 (because he's the first to win) and alternating between vertex and edge since we don't allow skipping. This solution corresponds to a path which visits each vertex exactly once, since player 1 has one copy of each vertex and he plays them all to win.

We conclude that the cooperative two-player variant of Uno is NP-Complete.

## Single-player

In this variant, we want to decide whether player 1 can play all its cards.

For a set of cards $C$, we define the Uno-graph $G = (V, E)$ where the vertices correspond to the cards in $C$ and there's an edge $(c_i, c_j)$ is cards $c_1$ and $c_2$ share a color and/or a number.

A **line graph** of a graph $G = (V, E)$, denoted by $L(G) = (V_L, E_L)$, is a graph whose vertices correspond to $E$ (i.e. $V_L = E$) and there's an edge $(e_i, e_j) \in E_L$ with $e_i, e_j \in E$ if the edges $e_i$ and $e_j$ share a vertex in $G$.

Now consider the bipartite graph with partitions $P_1$, corresponding to the colors $1, \cdots, c$, and $P_2$, to numbers $1, \cdots, b$. An edge exists between color $c_i$ and number $n_j$ if there's a card $(c_i, n_j) \in C$.

The Uno-graph $G = (V, E)$ is the line graph of such bipartite graph: its vertices correspond to the edges, and an edge $(c_i, c_j)$ exists in $E$ if the corresponding cards share a color and/or a number, and this edge is a vertex in the bipartite graph.

Now, for any bipartite graph $B = (U, V, E')$, its line graph can be interpreted as a Uno-graph, or as a instance to the single-player uno. Each edge $(u, v)$ in $E'$ corresponds to a card in $C$. We conclude that there's no restriction in which kind of bipartite graphs we can consider.

Finding whether the player can play all its cards is the same as finding a Hamiltonian path in the Uno-graph. There's a result mentioned in [1] that states that:

> The Hamiltonian path for line graphs of bipartite graphs is NP-Complete.

Which proves that the single player variant of Uni is also NP-Complete.

### P = NP?

The authors in [1] provide a dynamic programming (DP) algorithm which has runtime complexity $O(n^{c^2})$ where $c$ is the number of colors. We won't discuss it in here because the state and recurrence of the DP is quite involved.

But one might ask is this implies that Uno-1 is in $P$, which would then imply $P = NP$. The answer is no, at least not unless $c$ is a constant. The reason is that $c$ is related to the value of the elements of the instance but not the size of the instance itself, i.e. $n$.

More formally, the size of the instance grows logarithmically with $c$ (we just need $\log_2 c$ bits to store $c$), whereas it grows linearly with $n$. Conversely, a linear growth of $c$ would cause an exponential growth in the runtime complexity.

This algorithm is known as a pseudo-polynomial time algorithm and problems in NP-Complete that can be solved with such algorithms are also known as *Weakly NP-Complete* [2]. The most famous Weakly NP-Complete is the [integer knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem).

If we were to try solving an instance of similar dimensions to the real Uno, then we have $c = 4$, $b = 19$ and $n = 76$, for which it the DP would use roughly $10^{30}$ operations, more than the number of stars in the Universe. However, for a complete Uno set, finding a feasible solution manually is quite easy!

## Uncooperative two-players

In this variant of the problem, we want to decide which player is the first to lose, again assuming both players play perfectly. Note this is not a decision problem, but since we only two players, we can turn it into one by asking "is player 1 the first one to lose?".

The idea is to reduce this variant to a game known as "Geography", in particular the variant the authors call **undirected vertex Geography**. We describe the game here brielfly.

We're given an undirected graph $G$ and a token is placed at a initial vertex $v$. A player must move the token to an adjent vertex of their choice. When doing so, the vertex where the token was is removed together with the edges incident to it.

If the player cannot move the token anywhere, they lose the game. Turns out that this problem is in $\mathbf{P}$ [3]:

**Theorem 1.** Let $G(V, E), v \in V$ be an instance of the undirected vertex Geography game. It's possible to determine which player loses first in polynomial time.

<proof>

We first claim is that player 1 wins (and player 2 loses) if and only if in all maximum matchings $M$ of $G$, $M$ saturates $v$. A matching saturates a vertex if it contains an edge adjent to $v$.
<br /><br />
Suppose there is some maximum matching $M'$ which does not saturate $v$, then every neighbor $u$ of $v$ must be saturated in $M'$, otherwise we could include edge $(u, v)$ in the matching and it wouldn't be maximum. So after player 1 moves the token to $u$, player 2 can then move the token along an edge $(u, w)$ in $M'$.
<br /><br />
No other edges incident to $w$ can be in the matching, so Player 1 will then be forced to move along an edge $(w, x)$ not in $M'$. We claim that $M'$ saturates $x$, that is, some edge $(x, y) \in M'$. Otherwise we could remove edge $(u, w)$ from $M'$ and add edges $(v, u)$ and $(w, x)$ to it, contradicting the fact it's maximum.
<br /><br />
So player 2 can keep using edges in $M'$ and player 1 is forced to use edges not in $M'$ until either of them cannot "extend" this path any longer. This path is then an alternating path (as defined in the theory of matching in graphs [4]). We claim that the last edge on this path is in $M'$. If not, this path would be an <i>augmenting path</i> [4], which would let us obtain a matching of size $\abs{M'} + 1$, and which contradicts the fact that $M'$ is maximum. Thus, player 1 is the one unable to play and loses.
<br /><br />
Conversely, if $v$ is saturated, player 1 can pick some maximum matching $M$, which will contain $v$, and move along the edge $(v, u) \in M$. So we now have an instance $I' = G \setminus \curly{v}, u \in V$ which has a maximum matching that does not saturate $u$, with player 2 starting, which we saw causes player 2 to lose.
<br /><br />
To determine whether all matchings of a graph $G$ saturates vertex $v$, we compute the maximum matching of $G \setminus \curly{v}$. If the size of the maximum matching is equal to that of $G$, then there's a maximum matching in $G$ that does not use $v$, but if the size is less than that of $G$, we know for sure $v$ is needed in all maximum matchings of $G$.
<br /><br />
Maximum matches can be computed in polynomial time, so we can determine the first player to lose in polynomial time as well.
</proof>

The proof is very clever and is based on maximum matches. It also gives us an algorithm for each player to use.

*Case 1.* All maximum matches have an edge incident to $v$. In this case player 1 can win by computing any maximum matching $M$ and moving along an edge in $M$. No matter which move player 2 makes, it's guaranteed that player 1 will be able to continually move along edges in $M$. Further, player 1 will be the last one to move, so player 2 loses.

*Case 2.* If there's at least one maximum match $M$ that doesn't have any edge incident to $v$, then player 1 loses. Supose they move along an edge $(v, u)$. Then player 2 computes any maximum match $M'$ of $G \setminus v$. The proof shows that player 2 can keep moving along edges in $M'$ throughout the game, no matter which move player 1 makes, and they will be the last to play.

**Algorithm.** Even though it's possible to determine which case a given instance belongs to, players don't need to do this. Their optimal strategy is to compute the maximum match of the graph in their first respective turn. Then always try to move along edges in that matching. If they can't, it's a sign they can't win this instance (assuming the other player plays perfectly), so they might as well choose a random edge.

**Generalization.** We can easily generalize this strategy to a variant where player can pick the initial vertex $v$. They just need to iterate over all vertices until they find one such that all maximum matchings of $G$ contain an edge incident to it.

**Uno to Geography.** We can reduce the Uno variant to this generalized undirected vertex Geography problem. The idea is to construct a bipartite graph $B = (P_1, P_2, E)$ with vertex partitions $P_1$ and $P_2$. Vertices in $P_1$ and $P_2$ correspond to the cards in $C_1$ and $C_2$, respectively. There's an edge between $u \in P_1$ and $v \in P_2$ if their corresponding cards share a color and/or a number.

Determining the winner in the 2-player competitive variant of Uno is exactly the same as determining the winner in the generalized variant of the undirected vertex Geography game.

**Corollary 2.** The 2-player competitive variant of Uno is in $\mathbf{P}$.

One might think that if the single-player and the 2-player cooperative variants of Uno are NP-Complete, the generalization for 2-player competitive could only be harder. We've just seen however that this intuition failed us for this case.

## Conclusion

I'm interested in learning about the computational complexity of board games and landed on the Uno first.

The result that the 2-player competitive variant of Uno can be solved in polynomial time is really cool! The algorithm for solving it, based on that of undirected vertex Geography game is beautiful. It's non trivial to me that it boils down to computing maximum matchings!

⁽¹⁾: One thing that made me particularly pleased in studying the paper [1] is that I was able to find a small error in it! The issue is that we cannot allow players to skip turns as proposed in the cooperative variants. I sent an email to professor Demaine and he responded aknowledging the issue!

## References

* [[1](https://erikdemaine.org/papers/Uno_TCS/)] Erik D. Demaine, Martin L. Demaine, Ryuhei Uehara, Takeaki Uno, and Yushi Uno, "UNO is hard, even for a single player".
* [[2](https://www.kuniga.me/docs/cs/computational_complexity.html)] NP-Incompleteness - Computational Complexity Cheat Sheet
* [3] A. S. Fraenkel, E. R. Scheinerman and D. Ullman. Undirected edge geography. Theoretical Computer Science 112, 371–381 (1993).
* [[4](https://en.wikipedia.org/wiki/Matching_(graph_theory))] Wikipedia - Matching (graph theory)
