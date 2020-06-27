---
layout: post
title: "The PageRank algorithm"
tags: [combinatorial optimization, linear algebra, probability]
---

In this post we'll discuss the PageRank algorithm. The problem it tries to solve is ranking web pages based on their relevance. The general idea is that links to a page represent a good indicator to its relevance. It models the entire web as a giant directed graph, where links represent arcs, and then perform random walks following trough the links and assigns a score to each page based of the probability of landing at it.

This algorithm was developed by Sergey Brian and Larry Page during their PhD in Stanford. Besides the obvious reference to ranking of pages, PageRank also plays with the last name of Larry [1].

<figure class="center_children">
    <a href="https://kunigami.files.wordpress.com/5555/10/pagerank-cartoon.png"><img src="{{site.url}}/resources/blog/2014-11-24-the-pagerank-algorithm/5555_10_pagerank-cartoon.png" alt="pagerank-cartoon" /></a>
</figure>

### Theory

The basic modeling of the web as a graph $$G = (V, A)$$ is simple. The set of vertices $$V$$ is composed by each web page (document), and an edge $$(u, v) \in A$$ if the page corresponding to $$u$$ has a link the page corresponding to $$v$$. Let $$N = \mid V \mid$$ be the number of nodes.

The rank of each node represents the probability of being at a node at a given time if we are to perform a random walk in the graph. We can start at any node with equal probability, so the rank of each node starts as $$rank(i) = 1/N$$.

At each node, we can change to any one the adjacent nodes with equal probability, so this graph is essentially a Markov chain. We can represent the rank of a given node $$v$$ at time t through the following equation:

(1) $$Pr(v,t) = \sum_{(u, v) \in V} \dfrac{Pr(u, t-1)}{d(u)}$$

Where $$d(u)$$ is the out-degree of vertex $$u$$.

One limitation of this simple approach is that nodes that do not have outbound links will "drain" all the probability, because once we arrive at such nodes, there's no way out, so as time passes by, the probability of getting trapped in such nodes goes to 1.

A simple solution is to add a "teleport" probability, in which at each node we can teleport to any other node in the graph with a small probability. We control the chance of a teleport happening by the parameter $$\beta$$. More formally, the page rank equation is now the following:

(2) $$Pr(v,t) = \beta \left(\sum_{(u, v) \in V} \dfrac{Pr(u, t-1)}{d(u)} \right) + \dfrac{(1 - \beta)}{N}$$



Let $$M$$ be the adjacency matrix corresponding to the set of arcs $$A$$, that is, $$M$$ is a $$N \times N$$ matrix, and $$M_{(i, j)} = 1$$ if, and only if, $$(i, j) \in A$$. Let $$M'$$ be $$M$$ normalized its values normalized by the sum of each row. That is,

$$M'_{(i, j)} = \dfrac{M_{(i, j)}}{d(i)}$$

We can then rewrite (1) as:

(3) $$Pr(t) = {M'}^T Pr(t - 1)$$

and (2) as:

(4) $$Pr(t) = \beta {M'}^T Pr(t - 1) + \dfrac{(1 - \beta)}{N} \vec{1}$$

where $$\vec{1}$$ corresponds to a column vector with 1's. Let's simplify even further. Since for each iteration t, Pr is a probability distribution (that is, $$\sum_{v \in V} Pr(v, t) = 1$$), then we can rewrite (2) as:

(5) $$Pr(v,t) = \beta \left(\sum_{(u, v) \in V} \dfrac{Pr(u, t-1)}{d(u)} \right) +$$
$$\dfrac{(1 - \beta)}{N} \left(\sum_{v \in V} Pr(v, t-1) \right)$$

We can now define $$\hat{M} = \beta {M'}^T + \dfrac{(1 - \beta) E}{N}$$

where $$E$$ is a $$N \times N$$ matrix with all ones. With that in mind, we can re-write (5) simply as:

(6) $$Pr(t) = \hat{M} Pr(t-1)$$

In the next section we'll show $$\hat{M}$$ has some interesting properties that guarantees the page rank converges to a final value. Using that information, we'll be able to iterate until $$\mid Pr(t) - Pr(t - 1)\mid < \epsilon$$.



$$\hat{M}$$ **is stochastic**. A matrix is said (row or column) stochastic if the sum of each column or row is 1. For a given column of $$\hat{M}$$ there are $$d(i)$$ entries summing to 1 and which are multiplied by $$\beta$$ and also $$N$$ entries that sum to $$N$$ and multiplied by $$\frac{(1 - \beta)}{N}$$.

$$\hat{M}$$ **is irreducible**. A matrix is reducible if it can be transformed into a upper-diagonal matrix by rows/columns permutation. In the context of Markov chains, the transition matrix is irreducible if there is a non-zero probability of transitioning (even if in more than one step) from any state to any other state [3].

Alternatively, we can show a matrix $$M$$ is not reducible by the following procedure [2]: Construct a directed graph $$G(M)$$ where $$(i,j) \in A$$ iff $$M_{(i,j)} > 0$$. Then, $$M$$ is irreducible iff $$G(M)$$ is strongly connected. Because we're adding the teleport links, we can show $$G(\hat{M})$$ is a complete graph and clearly strongly connected.

$$\hat{M}$$ **is aperiodic**. If a matrix is such that $$M^{p} = M$$, we say it has period $$p$$. A matrix with maximum period 1 is said aperiodic. We can compute the period of a matrix $$M$$ by constructing the same directed graph as above. If $$M$$ is irreducible, then the period is the greatest common divisor of the length of the closest path for a given node. Since by adding the teleport links we also add self-arcs (i.e. there's a chance of staying in the same node), there's always a path of length 1, so the GCD is always 1, and thus is the period.

$$\hat{M}$$ **is positive recurrent**. Let $$f_{ij}^{(m)}$$ be the probability a node $$j$$ will be first visited at time $$m$$ if we start from the state $$i$$. Let $$\mu_{ij}$$ be the expected value of $$m$$ for a given $$i$$ and $$j$$.

We say a matrix is *recurrent* if $$\sum_{m=1}^{\infty} f_{ij}^{m} = 1$$ for all $$i, j$$, that is, we're sure every node will be eventually visited no matter where we start. A matrix is *positive-recurrent* if $$\mu_{ii} < \infty$$, that is, the expected number of steps for a random walk to come back to the state if started is not infinite.

If a Markov chain has a finite set of states (i.e. the vertices of the graph) and its transition matrix is irreducible, then we can show this transition matrix is also positive recurrent [4].

With these properties, we&#039;re ready to throw in the **Fundamental theorem of Markov Chains** [5]:

For any irreducible, aperiodic, positive-recurrent Markov chain, there's a unique stationary distribution $$\pi$$ such that

$$\pi^{(t + 1)} = \pi^{(t)}$$

This also means the final rank is an eigenvector with a corresponding eigenvalue $$\lambda = 1$$, since

$$\lambda Pr =  \hat{M} Pr$$

A similar but more general result is [Perron Frobenius theorem](http://en.wikipedia.org/wiki/Perron%E2%80%93Frobenius_theorem).

### Conclusion

I've been watching the Mining Massive Data Sets course in Coursera. It hasn't finished yet, but I found the idea of the PageRank pretty interesting from the mathematical point of view. I didn't do any actual coding here, but I'm planning to revisit this subject when I learn more about the Map reduce paradigm.

### References

* [[1](http://en.wikipedia.org/wiki/PageRank)] Wikipedia: Page Rank Theorem
* [[2](http://en.wikipedia.org/wiki/Perron%E2%80%93Frobenius_theorem)] Wikipedia: Perron Frobenius Theorem
* [[3](http://en.wikipedia.org/wiki/Irreducibility_%28mathematics%29)] Wikipedia: Irreducibility (mathematics)
* [[4](http://math.stackexchange.com/questions/297278/irreducible-finite-markov-chains-are-positive-recurrent)] Mathematics Stack Exchange: Irreducible, finite Markov chains are positive recurrent
* [[5](http://www.math.uchicago.edu/~may/VIGRE/VIGRE2008/REUPapers/Plavnick.pdf)] The Fundamental Theorem of Markov Chains: Aaron Plavnick 
