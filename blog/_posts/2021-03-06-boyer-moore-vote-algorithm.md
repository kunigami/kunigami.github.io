---
layout: post
title: "Boyer–Moore Majority Vote Algorithm"
tags: [online optimization, puzzles]
vanity: "2021-03-06-boyer-moore-vote-algorithm"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}


<figure class="image_float_left">
    <img src="{{resources_path}}/boyd-moore.jpg" alt="Robert S. Boyer and J Strother Moore thumbnail" />
</figure>

Robert Stephen Boyer and J Strother Moore are *Professor Emeritus* at The University of Texas at Austin. They're known for their [Boyer–Moore string-search algorithm](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm).

In this post we'll present the Boyer–Moore Majority Vote Algorithm which can find the major element (i.e. one which appears more than 50% of time) in a stream of data if one exists.

<!--more-->

## Boyer–Moore Majority Vote Algorithm

Suppose we're given a stream (i.e. an unbounded list) of integers and we want to find the element that occurs more than the rest of the other elements together if it exists.

Another way to pose this problem is, given an array of $N$ elements, find the element that occurss more than $\lfloor N/2 \rfloor$ times using constant additional memory.

The algorithm is very simple: we keep 2 local variables, $n$ and $c$. Variable $n$ represents the current candidate for majority and $c$ is a counter that reflects how frequent $n$ is, but it's not the frequency of $n$.

We start with $c = 0$ and process the elements $e_i$ of the array one at a time. If $c$ is 0, we set $n = e_i$ and $c = 1$. Otherwise, if $e_i$ equals $n$, we increment $c$, otherwise we decrement $c$.

At the end, if there's a major element in the array, it's guaranteed that it's stored in $n$. If there is no majority, the element in $n$ will not represent anything.

Thus, this algorithm cannot be used to decide whether there is a major element in the array, but will find one if it has. In the array case it's possible to verify that by doing an extra pass on the array and check if the frequency of $n$ is higher than $\lfloor N/2 \rfloor$ which can still be done with $O(1)$ memory.

The code in Python assuming `arr` has a major element:

{% highlight python %}
def major(arr):
    c = 0
    for e in arr:
        if c == 0:
            n = e

        c += 1 if e == n else -1
    return n
{% endhighlight %}

## A Soccer Analogy

The interesting part of the algorithm is to understand how it works. Let's build an intuition by considering a simple case where there's only 2 types of values in the array, say $a$ and $b$.

Let's build an analogy with soccer. We have Arsenal vs. Barcelona. The input array or stream represents the goals scored over time. An entry $a$ means that Arsenal scored, and $b$ that Barcelona scored. We now interpret $n$ as representing the team that is ahead on the score and $c$ the absolute difference between the scores. If $c$ is 0, it's a draw and $n$ can be assumed to be undetermined.

It's easy to see that the algorithm preserves these properties at every step. It's also easy to see that the team that scored the most will be ahead at the end and thus $n$ will contain the winnner.

Let's change the rules a bit now. Instead of keeping two scores, one for Arsenal and one for Barcelona, we'll only have one but we'll track which team is ahead. Now suppose a team scores a goal: if they're already ahead, we increment the score; if they're behind, they decrement the score; if it's a tie, the score is set to 1 and they're now ahead. Note that this is basically what the algorithm does.

This alternative version is less informative because while we know who won and by how much, we don't know the absolute scores of each team. However, these rules allow us extending our soccer game to 3 teams! Let's add Chelsea to the play. The rules are exactly the same. The team that is ahead with a positive score is the winner.

<figure class="center_children">
    <img src="{{resources_path}}/chelsearselona.png" alt="Logo of Arsenal, Barcelona and Chelsea"/>
</figure>


We now claim that if any team scored more goals than the other 2 combined will necessarily be the winner, although the converse is not necessarily true. To get an intuition, suppose that Arsenal scored more goals than Barcelona and Chelsea combined. Assume that somehow Barcelona and Chelsea colluded and became a single team, meaning that if Barcelona was ahead and Chelsea scored, it would increment the score instead of decrementing.

This essentially reduced the game to the 2-team version, in which Arsenal would still win. In the 3-team version, Arsenal could not do worse: Barcelona and Chelsea still decrement Arsenal's score when it's on the lead, but now they also play against each other, so Barcelona's goals would decrement Chelsea's score when it's on the lead, which only helps Arsenal.

It easy to extend to an arbitrary number of teams. As long as one of the teams scores more goals than the rest combined, they will be declared the winners.

## $\lfloor N/3 \rfloor$ majority

In our 3-play soccer match, if there's no team that scored more than the other two combined the final result would not reflect the team that scored the most. An example is $aaabbcc$, which would have Chelsea as the winner even though Arsenal scored the most.

Suppose then that instead of finding the element that occurs more than $\lfloor N/2 \rfloor$, we want to find one that occurs more than $\lfloor N/3 \rfloor$ time.

We can keep 2 pairs of score-leader as opposed to one [2]. When we process an element $e$ we have 3 scenarios:

1. If any of the pairs has score 0, $e$ becomes the lead of that pair with score 1.
2. If $e$ is already in either one of the pairs, we increment that one.
3. If both pairs are taken, then $e$ decreases the score of *both* of them.

We claim that if an element occurs more than each of the other 2, it will be in one of the pairs. For example, imagine the array $aaabbc$. We'll have after each step:

{% highlight text %}
a: (a, 1), (-, 0)
a: (a, 2), (-, 0)
a: (a, 3), (-, 0)
b: (a, 3), (b, 1)
b: (a, 3), (b, 2)
c: (a, 2), (b, 1)
{% endhighlight %}

$a$ occurs more than $b$ and $c$, so it is in one of the pairs at the end.

We can ask ourselves whether the order matters. Let's try $cbbaaa$:

{% highlight text %}
c: (c, 1), (-, 0)
b: (c, 1), (b, 1)
b: (c, 1), (b, 2)
a: (_, 0), (b, 1)
a: (a, 1), (b, 1)
a: (a, 2), (b, 1)
{% endhighlight %}

We'll call the even in which an element $x$ causes another $y$'s counter to be 0 as *ousting*. So in the second example, $a$ ousted $c$ in line 4.

It looks like the result doesn't change regardless of the order of the elements. Let's try to prove that. The idea is to define a canonical order in which the most frequent element come first as a group, then the second most and then the third. So the canonical order for $cbbaaa$ or $abcaba$ is $aaabbc$.

The first observation is that if $a$ and $b$ are on the lead, the order clearly doesn't matter since they're just incrementing their respective counter. For example: $aaabb$ and $ababa$ will both lead to $(a, 3), (b, 2)$. Furthermore, if the element that is not on the lead, say $c$, doesn't oust anyone, their order is also irrelevant. For example $aaaabbcb$ and $aaaabbbc$ will both lead to $(a, 3), (b, 2)$ because $c$ didn't oust anyone.

So the ousting is the event of interest for us. Suppose the first outsting event was when $c$ ousted $b$ at position $i$. For this to happen the number of $c$'s has to be equal to the number of $b$'s. The score looks like $(a, k_a), (-, 0)$. Now there are three possibilities for the next element in $i + 1$:

* $a$ - which is the element still standing - it will just increment their counter. In this case it's clear that we could have moved $a$ before the $c$'s.
* $b$ - this means $b$ will take its spot right away. Before $c$'s ousting we had $(a, k_a + 1), (b, 1)$ and then $(a, k_a), (-, 0)$ and now $(a, k_a), (b, 1)$. If we swapped $b$ at $i+1$ and $c$ at $i$, we would have had $(a, k_a + 1), (b, 2)$, and then $(a, k_a), (b, 1)$ which is the same result.
* $c$ - $c$ will take $b$'s spot that is $(a, k_a), (c, 1)$. This also means at this point there were more $c$'s than $b$'s, or more precisely $k_c = k_b + 1$. If the $c$ had shown up before $b$ in the array, we'd also have $(a, k_a), (c, 1)$ and have avoided the ousting.

The above analysis shows that we could re-order the elements to avoid this ousting event at position $i$. If we reason inductively, we can avoid oustings up to the last element (in which there's no next element for swapping), in which case an ousting can happen if $k_c = k_b$ or $k_c = k_a$.

This shows we can assume a canonical order for any input without changes to the result. If $a$ occurs more than each of the other 2, then it's easier to see it will stay standing at the end and with the highest score.

**More players.** We can generalize this for more teams using a reasoning similar to the one for the 2-player game. If we had $k$ teams and assuming they were colluding with the non-majority team, this would reduce to the 3-player version as long as $a$'s hold the majority and appears more than $\lfloor N/3 \rfloor$ times. A formal proof is needed here though.

For example, suppose we have 4 teams with scores $aaaaabbbcccdd$. We could form 3 coallitions: $aaaaa$, $bbbd$ and $cccd$ in which $a$ would still win. Note that $d$ had to be split otherwise $a$ would lose majority.

**$\lfloor N/k \rfloor$ majority.** We can extend the ideas above to find an element with the $k$-th majority. We define a $k-1$-array with score-leader pairs and return the leader with the highest score.

This can be done in $O(N)$ if we use a hash map to track the scores. Checking if $e$ is in the lead can be done in $O(1)$ and so is incrementing. Decrementing requires a pass over the entire hash map but the number of times we decrement is bounded by the number of increments we do, so it's still $O(N)$ overall.

## Misra-Gries's k-reduced bag

In [3] Misra and Gries introduce a simpler algorithm for finding the $\lfloor N/k \rfloor$ majority which is also easier to prove the correctness of. They introduce a k-reduced bag which results from removing k distinct elements from an array until it's not possible anymore.

They provide an example of `[1, 1, 2, 3, 3]` and $k=2$ where we can remove `[1, 2]`, then `[1, 3]` to end with `[1]`. A k-reduced bag is not unique.

If the array has size $N$, we can remove $k$ distinct elements from it at most $\lfloor N/k \rfloor$ times, since after that there will be less than $k$ elements left.

That means that if an element appears more than $\lfloor N/k \rfloor$ times, it's sure to be on the k-reduced bag since it's removed at most $\lfloor N/k \rfloor$ times.

The other observation is that there are at most $k$ distinct elements in the k-reduced bag, by definition.

If we can represent the k-reduced bag as a hashmap of element-frequency we can use $O(k)$ memory.

{% highlight python %}
def major(arr, k):

    bag = {}
    for x in arr:
        if x not in bag:
            bag[x] = 1
        else:
            bag[x] += 1

        if len(bag) == k:
            # found k distinct elements.
            # remove 1 of each
            ys = list(bag.keys())
            for y in ys:
                bag[y] -= 1
                # make sure hash map is O(k)
                if bag[y] == 0:
                    del bag[y]

    # returns element with the highest value
    return max(bag, key=lambda x:bag[x])
{% endhighlight %}

If we assume that adding, accessing and removing from `bag` can be done in $O(1)$, this algorithm is $O(N)$. The key is to observe that the number of times we execute the nested loop is bounded by $N$ because on each iteration we subtract from `bag` and we only add one per iteration of the outer loop.

## Conclusion

I learned about the $\lfloor N/2 \rfloor$ and $\lfloor N/3 \rfloor$ majority problems recently in programming puzzle sites. In particular, [2] led me to the *Boyer–Moore Majority Vote Algorithm* and showed a solution to the $\lfloor N/3 \rfloor$ version.

They don't provide a proof of correctness of the solution but it seemed relatively obvious. However, once I tried to write a proof down I realized how tricky it is. I tried to make some analogies but the result was a sketch of a proof at best.

I tried reading Boyer–Moore's original paper [4] but they only cover the $\lfloor N/2 \rfloor$ case. The paper points to Misra and Gries' paper [3] though which was a really nice finding.

## References

* [[1](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm)] Wikipedia: Boyer–Moore majority vote algorithm
* [[2](https://www.geeksforgeeks.org/n3-repeated-number-array-o1-space/)] GeeksforGeeks: N/3 repeated number in an array with O(1) space
* [3] Finding Repeated Elements - J. Misra and David Gries
* [4] MJRTY - A Fast Majority Vote Algorithm - R. Boyer and J Moore
