---
layout: post
title: "Protein Design"
tags: [bioinformatics, combinatorial optimization, computational complexity, integer programming]
---

We [previously learned]({{site.url}}/blog/2019/09/06/protein-folding-prediction.html) about the problem of predicting the folding of a protein, that is, given a chain of amino acids, find its final 3D structure. This time we're interested in the reverse problem, that is, given a 3D structure, find some chain of amino-acid that would lead to that structure once fully folded. This problem is called *Protein Design*.

In this post we'll focus on mathematical models for this problem, studying its computational complexity and discuss possible solutions.

### Mathematical Modeling

The 3D structure of a protein can be divided into two parts: the backbone and the side chains. In the model proposed by Piece and Winfree [3], we assume the backbone is rigid and that we'll try to find the amino-acids for the side chains such that it minimizes some energy function.

This means we're not really trying to predict the whole chain of amino acids, but a subset of those amino that will end up on the side chains.

The amino acids on the side-chain can have a specific orientation [2, 3], known as *rotamer*, which in turn can be represented by a single value, its *dihedral angle*. We can define some arbitrary order for these amino acids and label them with an index, which we call **position**.

At each position there are multiple rotamers possible and we need to select them to minimize the overall energy function. More formally, for each position i, let $$R_i$$ be the set of rotamers available and $$r_i \in R_i$$ the chosen rotamer.

The model assumes the cost function of the structure is pairwise decomposable, meaning that we can account for the interaction of each pair independently when calculating the total energy:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2019-09-30-protein-design/2019_09_screenshot-from-2019-09-20-15-46-22.png" alt="Screenshot from 2019-09-20 15-46-22" />
</figure>

Where $$E(r_i, r_j)$$ is the energy cost of the interaction between positions i and j, assuming rotamers $$r_i$$ and $$r_j$$ respectivelly. The definition of E can be based on molecular dynamics such as [AMBER](https://en.wikipedia.org/wiki/AMBER).

In [3], the authors call this optimization problem **PRODES** (PROtein DESign).

### PRODES is NP-Hard

Pierce and Winfree [3] prove that PRODES is NP-Hard. We'll provide an informal idea of the proof here.

First we need to prove that the decision version of PRODES is NP-Hard. The decision version of PRODES is, given a value K, determine whether there is a set of rotamers such that the energy cost is less or equal K. We'll call it **PRODESd**. We can then prove that PRODESd is NP-complete by showing that it belongs to the NP complexity class and by reducing, in polynomial time, some known NP-complete problem to it.

We claim that this problem is in NP because given an instance for the problem we can verify in polynomial time whether it is a solution (i.e. returns true), since we just need to evaluate the cost function and verify whether it's less than K.

We can reduce the 3-SAT problem, known to be NP-complete, to PRODESd. The idea is that we can map every instance of the 3-SAT to an instance of PRODESd and show that the result is "true" for that instance of 3-SAT if and only if the result is "true" for the mapped instance of PRODESd.

Let's start by formalizing PRODESd as a graph problem, with an example shown in the picture below:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2019-09-30-protein-design/2019_09_screenshot-from-2019-09-29-16-19-31.png" alt="Screenshot from 2019-09-29 16-19-31" />
    <figcaption> a) has 3 positions with their sets of rotamers. b) each rotamer becomes a vertex grouped into sets. We pick exactly one vertex per set and try to minimize the total cost of the edges associated with the selected vertices. Image source: [3]</figcaption>
</figure>Now, given a 3-SAT instance, we create a vertex set for each clause $$C_i$$ (containing a vertex for each literal), and a vertex set for each variable $$x_i$$ (containing vertices T and F). For each literal $$x_i$$ we add an edge of weight 1 to the vertex F of the set corresponding to variable $$x_i$$. Conversely, for each negated literal, $$\bar x_i$$, we add an edge of weight 1 to the vertex T. All other edges have weight 0.

For example, the instance $$(x_1 + \bar x_2 + x_3) \cdot ( \bar x_1 + x_2 + x_3) \cdot (\bar x_3)$$ yields the following graph where only edges of non-zero weight are shown:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2019-09-30-protein-design/2019_09_screenshot-from-2019-09-29-16-41-00.png" alt="Screenshot from 2019-09-29 16-41-00" />
    <figcaption> Source: [3]</figcaption>
</figure>We claim that this 3-SAT instance is satisfiable if and only if the PRODESd is true for K=0. The idea is the following: for each vertex set corresponding to a variable we pick either T or F, which corresponds to assigning true or false to the variable. For each vertex set corresponding to a clause we pick a literal that will evaluate to true (and hence make the clause true). It's possible to show that if 3-SAT is satisfiable, there's a solution for PRODESd that avoids any edge with non-zero weight.

### Integer Linear Programming

Now that we know that PRODES is unlikely to have an efficient algorithm to solve it, we can attempt to obtain exact solutions using [integer linear programming](https://kunigami.blog/category/computer-science/integer-programming/) model. Let's start with some definitions:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2019-09-30-protein-design/2019_09_screenshot-from-2019-09-29-18-43-51.png" alt="Screenshot from 2019-09-29 18-43-51" />
</figure>

We can define our variables as:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2019-09-30-protein-design/2019_09_screenshot-from-2019-09-29-18-44-27.png" alt="Screenshot from 2019-09-29 18-44-27.png" />
</figure>

The object function of our model becomes:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2019-09-30-protein-design/2019_09_screenshot-from-2019-09-29-18-45-05.png" alt="Screenshot from 2019-09-29 18-45-05" />
</figure>

Finally, the constraints are:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2019-09-30-protein-design/2019_09_screenshot-from-2019-09-29-18-39-34.png" alt="Screenshot from 2019-09-29 18-39-34" />
</figure>

Equation (1) says we should pick exactly one rotamer for each position. Constraints (2) and (3) enforce that $$x_{i, j} = 1$$ if and only if $$r_i = r_j = 1$$.

Note: the LaTeX used to generate the images above are available [here](https://github.com/kunigami/blog-examples/blob/master/protein-design/ilp.tex).

### Conclusion

The study of protein prediction led me to protein design, which is a much simpler problem, even though from the computational complexity perspective it's still an intractable problem.

The model we studied is very simple and makes a lot of assumptions, but it's interesting as a theoretical computer science problem. Still I'm not sure how useful it is in practice.

### References

* [[1](https://en.wikipedia.org/wiki/Protein_design)] Wikipedia - Protein design
* [[2](https://foldit.fandom.com/wiki/Rotamer)] Wikipedia - Rotamer
* [[3](https://academic.oup.com/peds/article/15/10/779/1521759)] Protein Design is NP-hard - Niles A. Pierce, Erik Winfree
