---
layout: post
title: "Aho-Corasick"
tags: [data structures, python]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2019-11-09-aho-corasick/2019_11_ahopic.jpg" alt="AhoPic" />
</figure>Alfred Aho is a Canadian computer scientist. He is famous for the book Principles of Compiler Design co-authored with Jeffrey Ullman. Aho also developed a number of utilities for UNIX which has widespread usage, including the AWK programming language (acronym for Aho, Weinberger and Kernigham) and variants of grep.

Unfortunately, I couldn't find much information about Margareth J. Corasick, except that she worked at Bell Labs and together with Aho, developed the Aho-Corasick algorithm, which we'll talk about in this post.

### Multiple-string search

We have studied before the problem of finding a string in a text, for which the [Knuth-Morris-Prat algorithm]({{site.url}}/blog/2016/03/13/tree-ring-matching-using-the-kmp-algorithm.html) is well-known.

Aho and Corasick studied the problem of finding a set of strings in a text. More precisely, given a body of text H and a set of search terms S, we want to find all occurrences of each term of S in H.

They came up with the Aho-Corasick algorithm which is able to solve this problem in linear time on the size of H and the size of the output (i.e. the total number of characters that need to be printed when a match is reported).

It consists in pre-computing a look-up structure from S and then scanning the text using it. Whenever it detects a match was found, it prints the corresponding matches. Let's see first how to construct the structure.

### The look-up structure

The look-up structure is constructed in two phases, each to construct two sets of edges: goto and fail edges.

The **goto** edges are essentially the edges of a trie containing the entries in S. The trie can be seen as a state machine where each node represents a prefix of one of the entries in S and the transition function tells us to which prefix to go next given an input character. These edges are thus called **goto** arrows. We denote by**g(s, a)** the node we navigate to if we're at node s and receive input a.

Given a text H = [a1,a2,...,a_n], we can start at the root and follow the edges labeled a1, a2 and so on. If we land on a node representing an entry in S we print it.

Eventually though, we may reach a node s such that the next character a_j doesn't have a corresponding edge g(s, a_j). We know that s is a suffix of [a1,a2,...,a_{j-1}], say s = [a_k,...,a_{j-1}]. We also know there's no other unreported entry in S that starts at k but there might be one that starts at a_{k+1}. We could restart the search at k+1 and at the root of the tree, but can we do better?

Because S is fixed, no matter what text H we have, by the time we encounter a dead end at a node r, we know that the last characters seen are  [a_k,...,a_{j-1}] = r. Now suppose s = [a_{k+1},...,a_{j-1}, a_j] happens to be in the trie. Then we can continue our search from s, without having to backtrack. If k+1 doesn't work, we can try k+2, k+3, and so forth. More generally, say that we eventually found the smallest index k* &gt; k such that s* = [a_k*,...,a_{j-1}, a_j] is in the trie. In other words s* is the longest proper suffix of s in the trie. When we fail to find s = r + a_j in the trie, we can resume at s*. This suggests we could have a **fail** edge from s to s* in our trie to quickly resume the search without backtrack.

Technically, s doesn't exist in the trie, so we can't add an edge there. However, we can still add the failure edge in r, and denote it as **f(r, a_j) = s*** for all labels a_j not in a goto edge.

Given the goto and fail edges, we have a proper state machine which we can use to find matches in H. Let's combine them into a single set of edges e(s, a) (that is e(s, a) = g(s, a) if it exists, otherwise f(s, a)).

{% highlight text %}
s = root
for a in H:
    s = e(s, a)
{% endhighlight %}

We are still missing printing out the matches. Because of the shortcuts we take via the fail edges, *we never explicitly visit a suffix of a state s*. For this reason, when visiting a state s, we need to print all its suffixes that belong to S.

{% highlight text %}
s = root
for a in H:
    s = e(s, a)
    print output(s)
{% endhighlight %}

We'll now see how to construct the fail edges and the output.

**Building the fail edges**

We'll show how to construct the failure edges efficiently. We define auxiliary edges **ps(s)** which represents the longest proper suffix of s that is in the trie. Note that f(r, a) is ps(r + a) for when g(r, a) doesn't exist.

The idea is that if we know ps(r) for all nodes s of depth up to l in the trie, we can compute ps(r + a), that is for the nodes in the next level.

Consider each node r of depth l and each symbol a. Let s = r + a. We know ps(r) is in the trie, but is ps(r) + a in the trie? Not necessarily, but maybe a suffix of ps(r) is? We can try ps(ps(r)) and further suffixes until we find one that is in the trie, or we reach the empty string.

How can we make sure to only process node s only after all the edges from lower levels have been processed? We can process the nodes in bread-first order, which can be achieved using a queue. The pseudo-code could be:

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

Note how instead of searching for the largest suffix by removing one character at a time and checking if it exists in the trie, we're "jumping" to suffixes via repeated application of ps(), which is what makes the algorithm efficient as we shall see. You might be asking if we're not missing any valid suffix when doing that, but no worries, we analyze the correctness of this in Lemma 1 (Appendix).

**Building the output**

If we compute output(s) in a bread-first order, we can assume we know output(r) for nodes r lower level than s. Assuming we already computed ps(s), we have that output(s) = output(ps(s)) + {s} if s is in S:

{% highlight text %}
queue = [root]
while queue is not empty:
    r = queue.pop()
    for each c in r.children():
        output(s) = output(ps(s))
        if (s is match):
            output(s).add(s)
{% endhighlight %}

How do we know by jumping through suffixes via ps(s) we are not missing any matches? We demonstrate that in Lemma 2 (Appendix).

### Implementation in Python

I've implemented the pseudo-code proposed here in Python, and made it available on [Github](https://github.com/kunigami/blog-examples/blob/master/aho-corasick/aho_corasick.py). The main implementation aspect is how we compute the output list. To avoid storing the matches multiple times, we model it as a linked list. More precisely, when computing output(s) from ps(s), if ps(s) is itself a keyword, we point output(s) to ps(s). Otherwise we point it to output(ps(s)).

{% highlight python %}
if jump_node.entry is not None:
    child.output = jump_node
else:
    child.output = jump_node.output

{% endhighlight %}

When printing output(s) we just need to traverse the list,:

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

### Conclusion

It was a lot of work to study Aho Corasick's paper [1], understand the proof and re-write with my own words, but this provided me much better intuition behind the algorithm.

Like the KMP, I've known this algorithm before but never took the time to fully understand it.

My initial implementation was in Rust, but I got stuck when modeling the nodes because of self-reference. This led me to this [Stackoverflow answer](https://stackoverflow.com/questions/32300132/why-cant-i-store-a-value-and-a-reference-to-that-value-in-the-same-struct) which I haven't time to study, but seems like a good idea for a post.

### Appendix


**Correctness**


**Lemma 1.** *Let s be a prefix in the look-up structure. Then ps(s) is the longest proper suffix of s that exists in the look-up structure T.*

Let's prove this by induction on the size of s. The base is for nodes of level 1, where ps(s) = root (empty string), which is true, since the proper suffix of a single letter is empty.

Assuming that ps(s') is the longest proper suffix of s that exists in the look-up structure for \|s'\| &lt; \|s\| , let's prove this is true for ps(s) as well. Let's show first that ps(s) is a proper suffix of s in T. We know that ps(r) is a suffix of r, and so is ps(ps(r)) since a suffix of a suffix of r is also a suffix of r, and so on. Let's define ps_k(r) = ps(ps_{k-1}(r)) and ps_n(r) such that g(ps_n(r), a) exists. By construction we assigned ps(s) = g(ps_n(r), a). Since ps_n(r) is a suffix of r, and r + a = s, we know what ps_n(r) + a is suffix of s. We'll show there's no other proper suffix or s in loop-up structure larger than ps(s). Suppose there is a s* in T such that \|s*\| &gt; \|ps(s)\|. Let r* be = s* + a. Note that r* is a suffix of r and it exists in T (since s* is in there and T it's a prefix tree). It cannot be larger than ps(r) because of our induction hypothesis. r* has to be larger than ps_n(r) since \|ps(s)\| = \|ps_n(r)\| + 1, \|s*\| &gt; \|ps(s)\| and \|s*\| = \|r*\| + 1.

The first observation is that r* != ps_k(r) for any k. Otherwise the algorithm would have set ps(s) = g(r*, a) = s*. So there must be some k for which \|ps_k(r)\| &gt; \|r*\| &gt; \|ps_{k+1}(r)\|, this means that for r’ = ps_k(r), there’s a suffix r* suc that \|r*\| &gt; \|ps(r’)\|. But this is a contradiction of the induction hypothesis saying that ps(r’) is the largest suffix of r’ in T.

**Lemma 2.** *output(s) contains y if and only if y is in S and a suffix of s.*

Let's prove one direction: if output(s) contains y then y is in S and a suffix of s. This is easy to see by construction, output(s) contains s if it's in S, and also the outputs of ps(s), which is a suffix of s, and by induction are all in S and suffix of ps(s), and hence of s.

Now suppose y is a keyword and suffix of s. Let's show that there's a k for which ps_k(s) = y. If this was not the case, then by a similar argument we did in Lemma 1, there is ps_{k-1}(s) such that y is suffix of it, and \|y\| &gt; \|ps_{k}(s)\|, which violates Lemma 1, since  ps(ps_{k-1}(s)) = ps_{k}(s) is not the longest possible. Since ps_k(s) = y, by construction, we know that output(y) will eventually be merged into output(s).

**Lemma 3.** *At the end of the loop in the Algorithm 1, state s is in the look-up structure T and it is a suffix of [a1,a2,...,aj].*

This is true for a1: it would be either in state [a1] in T, or the empty state, both being suffix of [a1]. Assuming this hypothesis holds for j-1, we were in a state r before consuming aj, and r is the longest suffix of  [a1,a2,...,aj-1].

By construction we know that the end state s is still in T, since we only followed valid edges of T (gotos and fails). We need to show s is the longest suffix of [a1,a2,...,aj]. For the sake of contradiction, assume there's s* such that \|s*\| &gt; \|s\| in T that is a suffix of [a1,a2,...,aj]. Let r* be such that s* = r* + aj. Note that r* is in T and is a suffix of [a1,a2,...,aj-1] and by the inductive hypothesis \|r*\| &lt; \|r\|. If we used a goto edge, then \|r*\| &gt; \|r\| which contradicts the inductive hypothesis. If we used a fail edge, then s = f(r, aj). Using the same argument from Lemma 1, we have that r* is a suffix of r, and that r* is not ps_k(r) for any k, so it must be that for some k, x = ps_k(r), \|x\| &gt; \|r*\| &gt; \|ps(x)\|and is a contradiction because r* is a proper suffix of x but longer than ps(x).

**Theorem 1.** *The Aho Corasick algorithm is correct.*

We need to show that every substring of text which is in S, will be reported. Any substring we were to report end at an index j of H, so it suffices to show we report every suffix of [a1,a2,...,aj] in S, for all j.

Let s be the node we are at the end of each loop in Algorithm 1. Suppose there's a suffix s* of [a1,a2,...,aj] in S that is not in output(s). From Lemma 2, we know that every keyword in S that is a suffix of s is in output(s), it follows that s* is not a suffix of s, so \|s*\| &gt; \|s\|, which cannot be from Lemma 3.

QED

**Complexity**


**Theorem 2.** *The look-up stricture can be constructed in linear time of the number of characters in S.*

Let's define size(S) = the total number of characters in S and A the set of symbols of the alphabet.

It's known to that we can construct a trie from a set of string in S, so the goto edges are covered.

For constructing the ps() function, the cost will be associated on how many times we execute the inner loop. We visit each node once and for each of the \|A\| symbols, we perform the inner loop. Since\|f(s)\| &lt; \|s\|, the number of times we execute the inner-most loop for a given state is bounded by \|s\|, so for each state we execute the inner-most loop \|s\|*\|A\| times. If we sum over all states, we have size(S)*\|A\|. Assuming \|A\| is constant and small, we can ignore it in the overall complexity. It's worth noting Aho and Corasick's original paper don't depend on \|A\|.

The cost of constructing the output() function is O(1) if we were to use a shared linked list. In that case output(s) can be constructing with a cell s and have it point to the list of output(f(s)), so again, this would be bounded by size(S).

For the search algorithm, let’s ignore the cost of printing the output for now. We can follow an edge in O(1) time and do so exactly once per character.

In the worst case there could be a lot matches at each position. For example, if S = {a, aa, aaa, ..., a^(n/2)} and H = a^n.  For n/2 positions we'll need to output all entries in S, leading to a complexity of size(S)*n.

Since these values need to be printed, there’s no way to be faster than that and this could be the dominating factor in the total complexity. In practice however the number of matches is much smaller.

### References

* [[1](https://cr.yp.to/bib/1975/aho.pdf)] Efficient String Matching: An Aid to Bibliographic Search
* [[2](https://en.wikipedia.org/wiki/Alfred_Aho)] Alfred Aho - Wikipedia
