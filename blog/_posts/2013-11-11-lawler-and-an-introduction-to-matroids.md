---
layout: post
title: "An Introduction to Matroids"
tags: [combinatorial optimization, combinatorics, integer programming, linear algebra]
---

<figure class="image_float_left">
    <a href="http://sunsite.berkeley.edu/uchistory/archives_exhibits/in_memoriam/catalog/lawler_eugene.html"><img src="{{site.url}}/resources/blog/2013-11-11-lawler-and-an-introduction-to-matroids/3013_10_lawler.png" alt="lawler" /></a>
</figure>

[Eugene Lawler](http://en.wikipedia.org/wiki/Eugene_Lawler) was an American computer scientist, professor of UC Berkeley and was one of the founders of the field of Combinatorial Optimization.

Lawler made important contributions on branch and bound algorithms, dynamic programming, and was also the first one to observe that matroid intersection could be done in polynomial time.

He also proved that two of Karp's 21 NP-Complete problems, The Directed Hamiltonian Cycle and 3-Dimensional Matching were NP-Complete.

This is the first post in our series of Matroids in Combinatorial Optimization context. They will be mainly based on the *Matroids and the Greedy Algorithm* chapter from *Combinatorial Optimization - Networks and Matroids*, by Eugene Lawler.



## Introduction

Hassler Whitney developed Matroids in 1935 in the context of algebraic theory and it was further applied by Jack Edmonds in the context of combinatorial optimization.

Matroids are a structure that allows us to solve problems by always taking local optimal steps and by doing that there's the guarantee we'll reach the global optimum. These types of algorithms are known as greedy algorithms.

First we'll define Matroids and then give some examples of problems to modeled as matroids. Next, we'll introduce weighted matroids and describe a generic algorithm to solve them and how such algorithm applied to matroids in graphs is actually the famous Kruskal algorithm.

## Definition

Let $$E$$ be a set of elements and $$\cal I$$ a family of subsets of $$E$$ (family here has the meaning of a set of elements that share some properties and it's also clearer than using 'set of subsets'). Let $$M = (E, \cal I)$$ and consider the following properties:

1) $$\emptyset \in \cal I$$ and if a subset $$I \in \cal I$$, then all subsets of $$I$$ belong to $$\cal I$$ as well.

2) If $$I_p \in {\cal I}$$ with $$\mid I_p \mid = p$$ and $$I_{p+1} \in {\cal I}$$ with $$\mid I_{p+1}\mid = p+1$$, then there is an element $$e \in I_{p+1} \setminus I_{p}$$ such that $I_p + e \in {\cal I}$. (*Henceforth, by abuse of notation, when we say $$I + e$$ we mean $$I \cup \{e\}$$*).

If $$M$$ satisfies both (1) and (2), we say that $$M$$ is a matroid.

Each element in $$\cal I$$ is called an **independent set**. A subset of $E$ not in $\cal I$ is called a dependent set.

If there's no other set containing $$I$$ in $$\cal I$$ we say it's a *maximal independent set*.

We claim that all maximal independent sets have the same size. To see why, suppose there are two maximal independent sets $I, J$ of different sizes, $\abs{I} = n < m = \abs{J}$. By (1), there exist a subset $J'$ of $J$ of size $n + 1$. Now by property (2), there exists $e \in J\' \setminus I$ and that $I^{*} = I + e$ exists in ${\cal I}$, which clearly contains $I$ and contradicts the fact $I$ is maximal.

The **rank** of a set $$E$$, denoted by $$r(E)$$ is the maximal independent subset in $$E$$. A minimal dependent set is called *circuit*.

## Special types of matroids

We can model some structures as matroids and take advantage of matroids properties to find solutions to problems involving these structures. In this section we'll present a few examples of such modelings.

The modelling process consists in defining the set $$E$$, the family $$\cal I$$ (usually by defining the property that the subsets of $$E$$ must have to be in there) and then proving that $$\cal I$$ satisfies (1) and (2).

### Matric Matroid

A *matric matroid* is a matroid in the context of matrices. Given a matrix $$A$$ with the set of columns as $$E$$, if $$\cal I$$ is the family of sets containing only linear independent columns, then $$M = (E, {\cal I})$$ is a matric matroid.

*Proof.* It's easy to see that any subset of a linear independent (LI) set of columns is also LI, so (1) is straightforward. For (2), we need to prove that given subsets $$I_{p+1}$$ and $$I_{p}$$ of $$p+1$$ and  $$p$$ LI columns, respectively, then there must be a column $$c$$ from $$I_{p+1} \setminus I_{p}$$ such that $$I_{p} + c$$ is still LI. If it's not the case, we claim that every column in $I_{p+1}$ can be written as a linear combination of the $I_{p}$.

To see why, we must have either $c \in I_{p+1} \setminus I_{p}$ or $c \in I_{p+1} \cap I_{p}$. In the first case, since we're assuming $I_{p} + c$ is not LI, $c$ can be written as a linear combination of $I_{p}$. In the second case, since $c \in I_{p}$, it's a trivial linear combination of $I_{p}$.

This claim contradicts the fact that $$I_{p+1}$$ is LI.

### Graphic Matroid

A *graphic matroid* is a matroid in the context of graphs. Given a graph $$G = (V, E)$$, if $$\cal I$$ is the family of arcs that do not form a cycle, then $$M = (E, {\cal I})$$ is a graphic matroid.

*Idea of the proof.* It's possible to show that subsets of edges that are cycle free, correspond to LI columns in the incidence matrix of $$G$$, so in this case we can also view $$M$$ as a matric matroid of the incidence matrix of $$G$$.

### Matching Matroid

A *matching matroid* is a matroid in the context of matchings in graphs.

Let $$G = (V, E)$$ be an undirected graph and let $$\cal I$$ be the family of subsets of nodes $$I \subseteq V$$ such that there's a matching in $$G$$ that covers all nodes in $$I$$ (we say that an node in $$I$$ is covered if there's an edge in the matching incident to it, even if the other end of the edge is not in $$I$$). Then $$M = (V, {\cal I})$$ is a matching matroid.

*Sketch of the Proof.* It's easy to verify that $$\cal I$$ satisfies (1). The proof of why it satisfies (2) consists in getting the symmetric difference between  matchings covering $$p+1$$ and $$p$$ vertices respectively and showing that in that difference there will be at least one alternating path such that if we change the alternation will increase the number of matched vertices.

## Weighted Matroids

Let $$M = (E, {\cal I})$$ be a matroid and $$w$$ a weight function for elements in $$E$$. The problem we want to solve is to find the independent set $$I \in {\cal I}$$ that has maximum weight, where the weight of a set is the sum of the weight of its elements.

Let's assume the elements on each independent set are listed in the non-increasing order of weight, so given sets $$I_1$$ and $$I_2$$, we list them as

$$I_1 = \{a_1, a_2, \cdots, a_m\}$$ and $$I_2 = \{b_1, b_2, \cdots, b_n\}$$

such that $$w(a_1) \ge w(a_2) \ge \cdots w(a_m)$$ and $$w(b_1) \ge w(b_2) \ge \cdots w(b_n)$$

We say $$I_1$$ is *lexicographically greater* than $$I_2$$ if for the first position their components differ, say at index $$k$$, $$a_k > b_k$$ or in case $$I_2$$ listing is a prefix of $$I_1$$ listing (that is, the same way we sort strings).

A set that is not lexicographically less than any other set is said to be *lexicographically maximum*. We can show such set is also a maximum independent set, because otherwise, by property (2), we can always add more elements to it, making it lexicographically greater.

The following Theorem from Rado and Edmonds states an important property of weighted matroids:

> **Theorem 1.** Let $$M = (E, {\cal I})$$ be a matroid. Then
>
> 3) For any negative weight function on $$E$$, a lexicographically maximum set in $$\cal I$$ is also the set with the maximum weight.
>
> Alternatively, given $$E$$ and $$\cal I$$, if (1) and (3) are satisfied, $$M = (E, {\cal I})$$ is a matroid.
>

### Gale Optimality

We say that a set $$B \in {\cal I}$$ is *Gale optimal* if all of its elements are not less than the corresponding elements of any other set $$I \in {\cal I}$$ when these elements are listed in non-increasing order. More formally, there exists a one-to-one mapping $$h:I \rightarrow B$$ such that $$w(e) \le w(h(e))$$ for all $$e \in I$$.

Note that a Gale optimal set is clearly a lexicographically maximum and, by *Theorem 1*, has optimal weight.

Given that, Gale's theorem provides a stronger result than *Theorem 1* regarding weighted matroids:

> **Theorem 2.** Let $$M = (E, {\cal I})$$ be a matroid. Then
>
> 4) For any weight function of elements on $$E$$, there exists a Gale optimal set $$B \in {\cal I}$$.
>
> Alternatively, given $$E$$ and $$\cal I$$, if (1) and (4) are satisfied, $$M = (E, {\cal I})$$ is a matroid.
>

### Weighted Matroid Greedy Algorithm

Property (4) allows to use a simple greedy algorithm to construct the Gale optimal set in $$\cal I$$.

In the first step, we look for the single-element set $$I = \{e_0\}$$ in $$\cal I$$ with the largest weight. By property (2) and (4), we can show that the Gale optimal set contains $$e_0$$. Next, we look for the largest element $$e_1$$ such that $$\{e_0, e_1\} \in {\cal I}$$. Again, we can show that such elements are contained in the Gale optimal set. We repeat this until we get a maximum independent set, which is also the Gale optimal set.

More generically, we have the following algorithm:

Let $$S$$ be our current solution, which starts as the empty set. At each step, we look for the element $$e$$ with maximum weight not in $$S$$ such that $$S + e$$ belongs to $$\cal I$$.

### The Maximum Spanning Tree problem

In the maximum spanning tree problem we are given a connected, undirected graph $$G = (V,E)$$ with non-negative weights $$w$$ on $$V$$ and we want to find a spanning tree (subset of $$E$$ that forms a tree) with the maximum cost.

Note that the minimum spanning tree, a more common form of the problem, can be reduced to the maximum version. This is only possible because every spanning tree has the same number of edges (this reduction cannot be made for example between the shortest and longest path problems).

Recall that a tree is a connected graph without cycles. The independent sets in a graphic matroid represent cycle-free subsets of arcs (aka forest) and thus the maximum independent set of a connected graph is a tree. If we assign non-negative weights to arcs, the optimal Gale independent set which corresponds to the maximum spanning tree.

The weighted matroid greedy algorithm for graphic matroids is also known as [Kruskal's algorithm](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm): It first sorts the edges by non-increasing order of weight and then adds an edge to the current solution if it doesn't form a cycle.

## Conclusion

In this post we learned the basics of matroids and weighted matroids. In future posts we'll study Matroid Intersection, Matroid Partition and other types of matroids like Oriented Matroids.

## References

* [[1]("http://en.wikipedia.org/wiki/Eugene_Lawler")] Wikipedia - Eugene Lawler
* [[2]("http://www.amazon.com/Combinatorial-Optimization-Networks-Matroids-Mathematics/dp/0486414531")]  Combinatorial Optimization – Networks and Matroids, Eugene Lawler - Chapter: Matroids and the Greedy Algorithm
