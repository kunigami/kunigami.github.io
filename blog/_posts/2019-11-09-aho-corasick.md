---
layout: post
title: "Aho-Corasick"
tags: [data structures, python]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2019-11-09-aho-corasick/2019_11_ahopic.jpg" alt="AhoPic" />
</figure>Alfred Aho is a Canadian computer scientist. He is famous for the book Principles of Compiler Design co-authored with Jeffrey Ullman. Aho also developed a number of utilities for UNIX which has widespread usage, including the AWK programming language (acronym for Aho, Weinberger and Kernigham) and variants of grep.

Unfortunately, I couldn't find much information about Margareth J. Corasick, except that she worked at Bell Labs and together with Aho, developed the Aho-Corasick algorithm, which we'll talk about in this post.

## Multiple-string search

The problem we're trying to solve is, given a text $H = [a_1, a_2, \cdots, a_n$ and a set of strings $S$, find all the occurrences of $s \in S$ as substrings in $H$. For example, if $H = ABAAAAB$ and $S = \curly{AB, AAA}$, we should output:

$$[AB (1), AAA (3), AAA (4), AB (6)]$$

For clarity, we add the index of the start of the match (in parenthesis).

## Trying Tries

How would we solve this problem using a trie? We can construct the trie from all the entries in $S$ and then for search for matching prefixes of $H$, which can be done in $O(m)$, with $m$ being the size of the longest string in $S$, that is, the height of the trie.

The problem is that we're interested in all substrings, not only prefixes, so we'd need to repeat this prefix search for each starting index in $H$, which would leave us with a complexity of $O(\abs{H} m)$.

We'll now analyze how to do it faster using the Aho-Corasick tree.

<!-- A trie can be seen as a state machine where each node represents a prefix of one of the entries in $S$ and the transition function tells us to which prefix to go next given an input character. The transition $g(s, a)$ tells us the node we navigate to if we're at node $s$ and receive input $a$.

Given a text $H = [a_1, a_2, \cdots , a_n]$, we can determine whether a prefix of $H$ exists in $S$ by traversing the trie constructed from $S$. We start at the root and follow the edges labeled $a_1$, $a_2$ and so on. If we land on a node representing an entry in $S$ we found such a prefix.

If we want -->

## The Aho-Corasick tree

The Aho-Corasick algorithm can be used to solve the multiple-string search problem in linear time in $O(\abs{H})$ plus the size of the output (i.e. the total number of characters that need to be printed when a match is reported).

Like with tries, it consists in pre-computing a tree structure $T$ from $S$ and then iterating over $H$ following its edges. Whenever it detects a match was found, it prints the corresponding matches.

The look-up structure is like a trie or prefix tree but with extra edges which we can use when we find a mismatch. These extra edges allow us to go back up in the tree but are smart to use the information from the prefix we are at so we don't have to start from scratch. The "trie edges" are called **goto** edges, while the back up edges are called **fail** edges.


### Goto Edges

As we mentioned, goto edges are the trie edges. We'll denote them by $g(r, a) \rightarrow s$, which represents the node $s$ we'll go to if we are at node $r$ and encounter a character $a$ while iterating over $H$.

### Fail Edges

Suppose we are doing the trie search starting at position $k$. We'll eventually reach a node $r$ such that the next character $a_j$ in $H$ doesn't have a corresponding edge $g(r, a_j)$. This means $r = [a_k,\cdots,a_{j-1}]$.

We would then restart the search at position $k + 1$. Suppose $s = [a_{k+1}, \cdots, a_{j-1}, a_j]$ is in $T$. We could add a short-cut edge from $r$ to $s$ via $a_j$. The rationale is that we already analyzed $[a_{k + 1},\cdots,a_{j-1}]$ to get to $r$, so it's wasteful to go over them again.

It might be that $[a_{k+1}, \cdots, a_{j-1}, a_j]$ is not in $T$, but perhaps $[a_{k+2}, \cdots, a_{j-1}, a_j]$ is? If not, then we try $a_{k+3}$, $a_{k+4}$ and so on, until we find the smallest index $k^\* > k$ such that $s^\* = [a_k^\*,\cdots,a_{j-1}, a_j]$ is in $T$. We then add an edge from $r$ to $s^\*$ via $a_j$, which is the fail edge $f(r, a_j) \rightarrow s^\*$.


### Traversal

Given the goto and fail edges, we have a proper state machine which we can use to find matches in $H$. Let's combine them into a single set of edges $e(s, a)$ (that is $e(s, a) = g(s, a)$ if it exists, otherwise $f(s, a)$).

Then the pseudo-code for the traversal is trivial:

{% highlight text %}
s = root
for a in H:
    s = e(s, a)
{% endhighlight %}

### Reporting Matches

Suppose that we are at node $r = [a_k, \cdots, a_{j-1}]$ with no available goto edge for $a_j$. Suppose that $[a_{k+1}, \cdots, a_{j-1}, a_j]$ is not in $S$ but $s_2 = [a_{k+2}, \cdots, a_{j-1}, a_j]$ is. Then there will be a fail edge $f(r, a_j) \rightarrow s_2$ and we'll follow that edge.

This means we are implicitly skipping searching for any prefixes starting at $a_{k+1}$! Suppose there is some substring $[a_{k+1}, \cdots, a_{\ell-1}, a_\ell]$ in $S$.

It must be that $\ell$ is less than $j$ otherwise there would be a node corresponding to $[a_{k+1}, \cdots, a_{j-1}, a_j]$ in $T$ which we assumed was not the case.

So when do we report $[a_{k+1}, \cdots, a_{\ell-1}, a_\ell]$? Because $\ell < j$, we know that the node $[a_{k}, \cdots, a_{\ell-1}, a_\ell]$ is in the tree (it's a parent of $r$), so as long as we report all the *suffixes* of a node in $S$ when visiting it, we should cover our bases. These suffixes are encoded in `output()` in the pseudo-code below.

{% highlight text %}
s = root
for a in H:
    s = e(s, a)
    print output(s)
{% endhighlight %}

We'll now see how to construct the fail edges and the output.

### Building the fail edges

We'll show how to construct the failure edges efficiently. We define auxiliary edges $ps(s)$ which represents the longest proper suffix of $s$ that is in the trie. Note that $f(r, a)$ is $ps(r + a)$ for when $g(r, a)$ doesn't exist.

The idea is that if we know $ps(r)$ for all nodes $s$ of depth up to $l$ in the trie, we can compute $ps(r + a)$, that is for the nodes in the next level.

Consider each node $r$ of depth $l$ and each symbol $a$. Let $s = r + a$. We know $ps(r)$ is in the trie, but is $ps(r) + a$ in the trie? Not necessarily, but maybe a suffix of $ps(r)$ is? We can try $ps(ps(r))$ and further suffixes until we find one that is in the trie, or we reach the empty string.

How can we make sure to only process node $s$ after all the edges from lower levels have been processed? We can process the nodes in bread-first order, which can be achieved using a queue. The pseudo-code could be:

{% highlight text %}
queue = [root]
while queue is not empty:
    r = queue.pop()
    for each a in symbols:

        let p = ps(r)
        while g(p, a) is none:
            p = ps(p)

        // s = r + a is not in the trie
        if g(r, a) is none:
          f(r, a) = p
        else:
          s = g(r, a)
          ps(s) = g(p, a)
          queue.push(s)

{% endhighlight %}

Note how instead of searching for the largest suffix by removing one character at a time and checking if it exists in the trie, we're "jumping" to suffixes via repeated application of $ps()$, which is what makes the algorithm efficient as we shall see. You might be asking if we're not missing any valid suffix when doing that, but no worries, we analyze the correctness of this in *Lemma 1* in the *Appendix*.

### Building the output

If we compute $output(s)$ in a bread-first order, we can assume we know $output(r)$ for nodes $r$ lower level than $s$. Assuming we already computed $ps(s)$, we have that $output(s) = output(ps(s)) + \curly{s}$ if $s \in S$:

{% highlight text %}
queue = [root]
while queue is not empty:
    r = queue.pop()
    for each c in r.children():
        output(s) = output(ps(s))
        if (s is match):
            output(s).add(s)
{% endhighlight %}

How do we know by jumping through suffixes via $ps(s)$ we are not missing any matches? We demonstrate that in *Lemma 2* in the *Appendix*.

## Implementation in Python

I've implemented the pseudo-code proposed here in Python, and made it available on [Github](https://github.com/kunigami/blog-examples/blob/master/aho-corasick/aho_corasick.py). The main implementation aspect is how we compute the output list. To avoid storing the matches multiple times, we model it as a linked list. More precisely, when computing $output(s)$ from $ps(s)$, if $ps(s)$ is itself a keyword, we point $output(s)$ to $ps(s)$. Otherwise we point it to $output(ps(s))$.

{% highlight python %}
if jump_node.entry is not None:
    child.output = jump_node
else:
    child.output = jump_node.output

{% endhighlight %}

When printing output(s) we just need to traverse the list:

{% highlight python %}
def get_matches(self):
    matches = []
    output = self
    while output is not None:
        if (output.entry is not None):
            matches.append(output.entry)
        output = output.output
    return matches
{% endhighlight %}

## Conclusion

It was a lot of work to study Aho Corasick's paper [1], understand the proof and re-write with my own words, but this provided me much better intuition behind the algorithm.

Like the KMP, I've known this algorithm before but never took the time to fully understand it.

My initial implementation was in Rust, but I got stuck when modeling the nodes because of self-reference. This led me to this [Stackoverflow answer](https://stackoverflow.com/questions/32300132/why-cant-i-store-a-value-and-a-reference-to-that-value-in-the-same-struct) which I haven't time to study, but seems like a good idea for a post.

## Related Posts

[Knuth-Morris-Prat algorithm]({{site.url}}/blog/2016/03/13/tree-ring-matching-using-the-kmp-algorithm.html) is an algorithm for solving the case where $\abs{S} = 1$.

## Appendix

**Correctness**


**Lemma 1.** *Let $s$ be a prefix in the look-up structure. Then $ps(s)$ is the longest proper suffix of $s$ that exists in the look-up structure $T$.*

Let's prove this by induction on the size of $s$. The base is for nodes of level 1, where $ps(s) = root$ (empty string), which is true, since the proper suffix of a single letter is empty.

Assuming that $ps(s')$ is the longest proper suffix of $s$ that exists in the look-up structure for $\abs{s'} < \abs{s}$, let's prove this is true for $ps(s)$ as well. Let's show first that $ps(s)$ is a proper suffix of $s$ in $T$. We know that $ps(r)$ is a suffix of $r$, and so is $ps(ps(r))$ since a suffix of a suffix of $r$ is also a suffix of $r$, and so on. Let's define $ps_k(r) = ps(ps_{k-1}(r))$ and $ps_n(r)$ such that $g(ps_n(r), a)$ exists. By construction we assigned $ps(s) = g(ps_n(r), a)$. Since $ps_n(r)$ is a suffix of $r$, and $r + a = s$, we know what $ps_n(r) + a$ is suffix of $s$. We'll show there's no other proper suffix or $s$ in loop-up structure larger than $ps(s)$. Suppose there is a $s^\*$ in $T$ such that $\abs{s^\*} > \abs{ps(s)}$. Let $r^\* = s* + a$. Note that $r^\*$ is a suffix of $r$ and it exists in $T$ (since $s^\*$ is in there and $T$ it's a prefix tree). It cannot be larger than $ps(r)$ because of our induction hypothesis. $r^\*$ has to be larger than $ps_n(r)$ since $\abs{ps(s)} = \abs{ps_n(r)} + 1$, $\abs{s^\*} > \abs{ps(s)}$ and $\abs{s^\*} = \abs{r^\*} + 1$.

The first observation is that $r^\* \neq ps_k(r)$ for any $k$. Otherwise the algorithm would have set $ps(s) = g(r^\*, a) = s^\*$. So there must be some $k$ for which $\abs{ps_k(r)} > \abs{r^\*} > \abs{ps_{k+1}(r)}$, this means that for $r’ = ps_k(r)$, there’s a suffix $r^\*$ such that $\abs{r^\*} > \abs{ps(r’)}$. But this is a contradiction of the induction hypothesis saying that $ps(r’)$ is the largest suffix of $r’$ in $T$.

**Lemma 2.** *$output(s)$ contains $y$ if and only if $y$ is in $S$ and a suffix of $s$.*

Let's prove one direction: if $output(s)$ contains $y$ then $y$ is in $S$ and a suffix of $s$. This is easy to see by construction, $output(s)$ contains $s$ if it's in $S$, and also the outputs of $ps(s)$, which is a suffix of $s$, and by induction are all in $S$ and suffix of $ps(s)$, and hence of $s$.

Now suppose $y$ is a keyword and suffix of $s$. Let's show that there's a $k$ for which $ps_k(s) = y$. If this was not the case, then by a similar argument we did in *Lemma 1*, there is $ps_{k-1}(s)$ such that $y$ is suffix of it, and $\abs{y} > \abs{ps_{k}(s)}$, which violates *Lemma 1*, since  $ps(ps_{k-1}(s)) = ps_{k}(s)$ is not the longest possible. Since $ps_k(s) = y$, by construction, we know that $output(y)$ will eventually be merged into $output(s)$.

**Lemma 3.** *At the end of the loop in the Algorithm 1, state $s$ is in the look-up structure $T$ and it is a suffix of $[a_1,a_2,\cdots,a_j]$.*

This is true for $a_1$: it would be either in state $[a_1]$ in $T$, or the empty state, both being suffix of $[a_1]$. Assuming this hypothesis holds for $j-1$, we were in a state $r$ before consuming $a_j$, and $r$ is the longest suffix of $[a_1,a_2,\cdots,a_{j-1}]$.

By construction we know that the end state $s$ is still in $T$, since we only followed valid edges of $T$ (gotos and fails). We need to show $s$ is the longest suffix of $[a_1,a_2,\cdots,a_j]$. For the sake of contradiction, assume there's $s^\*$ such that $\abs{s^\*} > \abs{s}$ in $T$ that is a suffix of $[a_1,a_2,\cdots,a_j]$. Let $r^\*$ be such that $s^\* = r^\* + a_j$. Note that $r^\*$ is in $T$ and is a suffix of $[a_1,a_2,\cdots,a_{j-1}]$ and by the inductive hypothesis $\abs{r^\*} < \abs{r}$. If we used a goto edge, then $\abs{r^\*} > \abs{r}$ which contradicts the inductive hypothesis. If we used a fail edge, then $s = f(r, a_j)$. Using the same argument from *Lemma 1*, we have that $r^\*$ is a suffix of $r$, and that $r^\*$ is not $ps_k(r)$ for any $k$, so it must be that for some $k$, $x = ps_k(r)$, $\abs{x} > \abs{r^\*} > \abs{ps(x)}$ and is a contradiction because $r^\*$ is a proper suffix of x but longer than $ps(x)$.

**Theorem 1.** *The Aho Corasick algorithm is correct.*

We need to show that every substring of text which is in $S$, will be reported. Any substring we were to report end at an index $j$ of $H$, so it suffices to show we report every suffix of $[a_1,a_2,\cdots,a_j]$ in $S$, for all $j$.

Let $s$ be the node we are at the end of each loop in *Algorithm 1*. Suppose there's a suffix $s^\*$ of $[a_1,a_2,\cdots,a_j]$ in $S$ that is not in $output(s)$. From *Lemma 2*, we know that every keyword in $S$ that is a suffix of $s$ is in $output(s)$, it follows that $s^\*$ is not a suffix of $s$, so $\abs{s^\*} > \abs{s}$, which cannot be from *Lemma 3*.

QED

**Complexity**

**Theorem 2.** *The look-up structure can be constructed in linear time of the number of characters in S.*

Let's define $size(S)$ as the total number of characters in $S$ and $A$ the set of symbols of the alphabet.

It's known that we can construct a trie from a set of string in $S$, so the goto edges are covered.

For constructing the $ps()$ function, the cost will be associated on how many times we execute the inner loop. We visit each node once and for each of the $\abs{A}$ symbols, we perform the inner loop. Since $\abs{f(s)} < \abs{s}$, the number of times we execute the inner-most loop for a given state is bounded by $\abs{s}$, so for each state we execute the inner-most loop $\abs{s} \abs{A}$ times. If we sum over all states, we have $size(S) \abs{A}$. Assuming $\abs{A}$ is constant and small, we can ignore it in the overall complexity. It's worth noting Aho and Corasick's original paper don't depend on $\abs{A}$.

The cost of constructing the $output()$ function is $O(1)$ if we were to use a shared linked list. In that case $output(s)$ can be constructing with a cell s and have it point to the list of $output(f(s))$, so again, this would be bounded by $size(S)$.

For the search algorithm, let’s ignore the cost of printing the output for now. We can follow an edge in $O(1)$ time and do so exactly once per character.

In the worst case there could be a lot matches at each position. For example, if $S = {a, aa, aaa, \cdots, a^(n/2)}$ and $H = a^n$.  For $n/2$ positions we'll need to output all entries in $S$, leading to a complexity of $size(S) n$.

Since these values need to be printed, there’s no way to be faster than that and this could be the dominating factor in the total complexity. In practice however the number of matches is much smaller.

### References

* [[1](https://cr.yp.to/bib/1975/aho.pdf)] Efficient String Matching: An Aid to Bibliographic Search
* [[2](https://en.wikipedia.org/wiki/Alfred_Aho)] Alfred Aho - Wikipedia
