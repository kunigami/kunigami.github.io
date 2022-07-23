---
layout: post
title: "Baum-Welch Algorithm: Theory"
tags: [artificial intelligence, probability]
excerpt_separator: <!--more-->
vanity: "2022-07-19-baum-welch-algorithm"

---
{% include blog_vars.html %}

Leonard E. Baum was an American mathematician who worked for Institute for Defense Analyses (IDA) at Princeton [1]. He also worked in the finance industry. Later in life, despite vision impairments, he did research on number theory.

Lloyd Richard Welch [2] is an American mathematician who worked at Jet Propulsion Laboratory (JPL), IDA and Caltech (where he retired).

Baum and Welch developed the Baum-Welch algorithm for determining unknown parameters of Hidden Markov Models, which we'll study in this post.

We'll start by reviewing Hidden Markov Models, its definitions and notation. Secondly we'll state the parameter estimation problem in details. Then we'll go over the theory behind the Baum-Welch algorithm and finally we'll present a high-level procedure describing the algorithm and its runtime complexity.

We'll leave the actual implementation and experiments to a separate post: [Baum-Welch Algorithm: Python Implementation]({{blog}}/2022/07/23/baum-welch-algorithm-in-python.html).

<!--more-->

## Hidden Markov Model

We can think of a Hidden Markov Model as a probabilistic state machine. We are given a graph with nodes corresponding to hidden (or latent) states and edges corresponding to transitions between states, associated with a probability of going from one state to another.

Associated with each node is a set of possible visible states, or observations, which act as a proxy to the hidden state. There's a probability of observing a visible state given a hidden state.

Let's go into more details and introduce more formal notation.

**States.** There are $N$ (hidden) states in the model, $S = \curly{s_1, s_2, \cdots, s_N}$. Each node correspond to one of these states.

**Time**. As in a state machine, we have the concept of time. At any given instant $t$, we are in a state (or node) $q_t$. We start at $t = 1$ at any given state $q_1 = s_i$ with probability $\pi_i$ for $i \in \curly{1, 2, \cdots, N}$.

**Transition.** When going from instant $t$ to $t+1$, we make a state transition. The probability of going to state $s_j$ given we're at a state $s_i$ is given by the variable $a_{ij}$, where

$$\sum_{j = 1}^{N} a_{ij} = 1 \qquad \forall i \in \curly{1, 2, \cdots, N}$$

Note that we may end up staying on the same state if $a_{ii} > 0$.

The $N \times N$ matrix of $a_{ij}$ is denoted by $A$. The "Markov" in "Hidden Markov Model" stems from the fact that the probability of transition only depends on the current state, that is, independent from the past like in a [Markov chain](https://en.wikipedia.org/wiki/Markov_chain).

**Observation.** The "Hidden" in "Hidden Markov Model" models the fact that it might be hard or impossible to directly measure or observe a state $s_i$, and instead we use a proxy metric or observation that is ideally highly correlated with $s_i$.

There are $M$ observable states in the model, $V = \curly{v_1, v_2, \cdots, v_M}$. The probability of observing $v_k$ given we're in state $s_i$, $p(v_k \mid s_i)$, is encoded in the variable $b_{ik}$. The $N \times M$ matrix of $b_{ik}$ is denoted by $B$.

## Estimating Model Parameters

Recall that the [Viterbi Algorithm]({{blog}}/2021/01/25/viterbi-algorithm.html) can be used to estimate the sequence of hidden states $Q$ given the model $(S, V, A, B, \pi)$.

Given a sequence of observations $O = o_1, o_2, \cdots, o_T$, where $o_i \in V$, we can also estimate the parameters associated with probabilities, namely $A$ (probability of transition between hidden states), $B$ (probability of observation for a given state) and $\pi$ (probability of being at a state initially).

Let's define $\theta = (A, B, \pi)$. Suppose $Y$ is the sequence of random variables corresponding to the observations at each time $t$. We want to compute $\theta^{*}$ that maximizes the likelihood of observing $O$:

$$(1) \quad \theta^{*} = \arg \max_{\theta} \mathcal{L}(\theta \mid O)$$

Where $\mathcal{L}(\theta \| O) = P_{\theta}(Y = O)$.

## Baum-Welch Algorithm - Theory

The theory behind Baum-Welch Algorithm is essentially that of the [Expectation Maximization Algorithm](https://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm).

The major difference is the probability distribution in HMMs is a specific function of $\theta$, which in turns allows us to determine specific formulas that solve (1) analytically.

### Log-Likelihood

The first observation is that we can find $\theta^*$ that maximizes $\log (\mathcal{L}(\theta \mid O))$ instead of $\mathcal{L}(\theta \mid O)$ because $\log$ is a monotonic function. Optimizing the log is desirable because probabilities often involves products, which in log space turns into sums. So let's change our objective function.

Note: in this post we'll use assume $\log$ means the natural logarithm.

### Likelihood as a Random Variable

Given $\theta$, how can we compute $P_{\theta}(Y = O)$? The underlying model has hidden variables for which we don't know the values. Let $Q = q_1, q_2, \cdots, q_T$ ($q_i \in S$), be a sequence of random variables corresponding to the hidden states at each time $t$. Let's denote by $S^T$ be the set of all possible values $Q$ (i.e. by assuming that $Q$ is a $T$-dimensional vector) can assume.

We can thus treat $P_{\theta}(Y = O)$ as a function of the random variables $Q$, and hence we can compute its expected value $E[P_{\theta}(Y = O)]$ over all $Q' \in S^T$. The probability of observing a value $Q'$ given $Y=O$ and a specific value of $\theta = \theta^{(n)}$, is $P_{\theta^{(n)}}(Q = Q' \mid Y = O)$. So the expected value is:

$$E[P_{\theta}(Y = O)] = \sum_{Q' \in S^T} P_{\theta}(Y = O, Q = Q') P_{\theta^{(n)}}(Q = Q' \mid Y = O)$$

There's a subtlety here: the first term inside the sum is $P_{\theta}$ while the second is $P_{\theta^{(n)}}$. $P_{\theta^{(n)}}(Q = Q' \mid Y = O)$ is a constant, the actual probability, while $P_{\theta}(Y = O, Q = Q')$ is a function of the variable $\theta$.

This makes $E[P_{\theta}(Y = O)]$ a function of $\theta$ as well, so if we want to be super clear, we should write $E\[P_{\theta}(Y = O)\](\theta)$.

Because we're interested in the value of $\log P_\theta(Y = O)$, not $P_\theta(Y = O)$, we can use the *Law of the Unconscious Statistician* [6] to get:

$$(2) \quad E[\log P_{\theta}(Y = O)](\theta) = \sum_{Q' \in S^{T}} \log (P_\theta(Y = O, Q = Q')) P_{\theta^{(n)}}(Q = Q' \mid Y = O)$$

To summarize, we can now optimize $E\[\log P_{\theta}(Y = O)\](\theta)$ instead of $P_{\theta}(Y = O)$

### Expectation Maximization

We want to find $\theta^{*}$ that maximizes (2), that is,

$$\theta^{*} = \arg \max_{\theta} \sum_{Q' \in S^{T}} \log (P_\theta(Y = O, Q = Q')) P_{\theta^{(n)}}(Q = Q' \mid Y = O)$$

The conditional probability [4] states that $P(Q = Q' \mid Y = O) = P(Y = O, Q = Q') / P(Y = O)$. Since the denominator $P(Y = O)$ is the same for all $Q' \in S^{T}$ it can be factored out when computing $\arg \max_{\theta}$:

$$(3) \quad \theta^{*} = \arg \max_{\theta} \sum_{Q' \in S^{T}} \log (P_\theta(Y = O, Q = Q')) P_{\theta^{(n)}}(Y = O, Q = Q')$$

$P_\theta(Y = O, Q = Q')$ is given by:

$$P_{\theta}(Q = Q', Y = O) = \pi_{q_1} b_{q_1, o_1} \prod_{t=2}^{T} a_{q_{t-1}, q_t} b_{q_t, o_t}$$

Which can be derived by "simulating" a traversal over the HMM. Applying the logarithm:

$$\log P_{\theta}(Q = Q', Y = O) = \log \pi_{q_1} + \log b_{q_1, o_1} + \sum_{t=2}^{T} (\log a_{q_{t-1}, q_t} + \log b_{q_t, o_t})$$

If we plug this back into (3), we'll get a long expression of sums of sums. Let's focus on the summand involving $\pi$ terms first:

$$\sum_{Q' \in S^{T}} \log \pi_{q_1} P_{\theta^{(n)}}(Y = O, Q = Q')$$

### Re-marginalization

Note that $\sum_{Q' \in S^{T}}$ is still an intractable sum because $S^T$ is very large. The insight is that there are only a small set of values $q_1$ can take (since $q_1 \in S$), so we can group by distinct values of $q$ and re-marginalize $P_{\theta^{(n)}}(Y = O, Q = Q')$:

$$(4) \quad \sum_{Q' \in S^{T}} \log \pi_{q_1} P_{\theta^{(n)}}(Y = O, Q = Q') = \sum_{i \in S} \log \pi_{i} P_{\theta^{(n)}}(Y = O, q_1 = i)$$

We can use a similar line of thought for $A$:

$$\sum_{Q' \in S^{T}} \sum_{t=2}^{T} \log a_{q_{t-1}, q_t} P_{\theta^{(n)}}(Y = O, Q = Q') = \sum_{i \in S} \sum_{j \in S} \sum_{t=2}^{T} \log a_{ij} P_{\theta^{(n)}}(Y = O, q_{t-1} = i, q_t = j)$$

And for $B$:

$$\sum_{Q' \in S^{T}} \sum_{t=1}^{T} \log b_{q_t, o_t} P_{\theta^{(n)}}(Y = O, Q = Q') = \sum_{i \in S} \sum_{t=1}^{T} \log b_{i{o_t}} P_{\theta^{(n)}}(Y = O, q_t = i)$$


### Non-linear Optimization

We can model the maximization problem as the following non-linear optimization problem:

Maximize:

$$f(\theta) = \sum_{Q' \in S^{T}} \log (P_\theta(Y = O, Q = Q')) P_{\theta^{(n)}}(Y = O, Q = Q')$$

Subject to:

$$
\begin{equation}
\begin{split}

\sum_{i \in S} \pi_{i} & = 1 \\
\sum_{j \in S} a_{ij}  & = 1 \quad \forall i \in S \\
\sum_{k \in V} b_{ik}  & = 1 \quad \forall i \in S
\end{split}
\end{equation}
$$

$$
\begin{equation}
\begin{split}
0 \le \pi_{i} \le 1 & \quad \forall i \in S \\
0 \le a_{ij} \le 1  & \quad \forall i,j \in S \\
0 \le b_{ik} \le 1  & \quad \forall i \in S, k \in S
\end{split}
\end{equation}
$$


These constraints guarantee that $\pi$, $A$ and $B$ are valid probability distribution functions.

### Gradient Descend

We can solve this problem by using gradient descend. The idea is that if the objective function is differentiable we can compute the gradient and find which variables values make it 0, which corresponds to a local extrema.

However, we first need to get rid of the equality constraints. We can remove them via [Lagrangian multipliers](https://en.wikipedia.org/wiki/Lagrange_multiplier), which consists in moving them to the objective function multiplied by scalars.

It's possible to show that by adding the multipliers as variables to the model and computing their value such that the resulting gradient is 0 yields a local extrema in the original formulation.

For each of the constraints we'll have a multiplier, so the new objective function will be:

$$\ell(\theta) = f(\theta) - \lambda_{\pi} (\sum_{i \in S} \pi_{i} - 1) - \sum_{i \in S} \lambda_{A_i} (\sum_{j \in S} a_{ij} - 1) - \sum_{i \in S} \lambda_{B_i} (\sum_{k \in V} b_{ik} - 1)$$

Because the variables $\pi$, $A$, $B$ and the Lagrangian multipliers only occur as independent summands (e.g. we don't have one variable multiplied by another), computing the partial derivatives is relatively simple. For example, let's compute the partial derivative of $\ell(\theta)$ with respect to $\pi$ for a given $i$ by using (4):

$$\frac{\partial \ell(\theta)}{\partial \pi_i} = \frac{\partial}{\partial \pi_i} \sum_{i \in S} \log \pi_{i} P_{\theta^{(n)}}(Y = O, q_1 = i) - \lambda_{\pi} (\sum_{i \in S} \pi_{i} - 1) = 0$$

The terms with $\pi_j$ for $j \ne i$ will be eliminated and since $\frac{d log(x)}{dx} = \frac{1}{x}$ we get:

$$\frac{P_{\theta^{(n)}}(Y = O, q_1 = i)}{\pi_i} - \lambda_{\pi} = 0 \qquad \forall i \in S$$

Recall that $P_{\theta^{(n)}}(Y = O, q_1 = i)$ is a constant. We have $N$ unknown variables $\pi_i$ and $\lambda_{\pi}$ but only $N$ equations. We can use the partial derivative of $\ell(\theta)$ with respect to $\lambda_{\pi}$:

$$\frac{\partial \ell(\theta)}{\partial \lambda_{\pi}} = \sum_{i \in S} \pi_{i} - 1 = 0$$

It can be shown these are sufficient to determine $\pi_i$ as:

$$\pi_i = P_{\theta^{(n)}}(q_1 = s_i \mid Y = O)$$

We can perform similar calculations for $A$ and $B$, but we'll skip it for now. Refer to [7] for detailed derivations. Let's summarize how we can compute $\theta^{(n+1)} = \pi, A, B$ from a given estimate of $\theta^{(n)}$.

### Summary

We start with an initial random value for $\theta^{(1)} = (A^{(1)}, B^{(1)}, \pi^{(1)})$. Then, using the methods above, we can compute $A^{(n + 1)}$ from $\theta^{(n)}$ as:

$$(5)\quad a_{ij}^{(n + 1)} = \frac{\sum_{t=1}^{T-1} P_{\theta^{(n)}}(q_{t+1} = s_j, q_t = s_i \mid Y = O)}{\sum_{t=1}^{T-1} P_{\theta^{(n)}}(q_t = s_i \mid Y = O)}$$

Analogously $B^{(n + 1)}$ is defined as a function of $\theta^{(n)}$ as:

$$(6) \quad b_{ik}^{(n + 1)} = \frac{\sum_{t=1}^{T} P_{\theta^{(n)}}(y_t = v_k, q_t = s_i \mid Y = O)}{\sum_{t=1}^{T} P_{\theta^{(n)}}(q_t = s_i \mid Y = O)}$$

And as we stated above, $\pi^{(n + 1)}$ is:

$$(7) \quad \pi_i^{(n + 1)} = P_{\theta^{(n)}}(q_1 = s_i \mid Y = O)$$

And this is the overall idea of the Baum-Welch algorithm! In the next section we'll describe how to carry out this computation by using intermediate variables.

## Baum-Welch Algorithm - Details

### Computing $\alpha$: Forward Procedure

Before we start, let's introduce the following notation: $Y_{i:j} = \curly{y_i, \cdots, y_j}$ and  $O_{i:j} = \curly{o_i, \cdots, o_j}$ and $Y_{i:j} = O_{i:j}$ meaning $y_i = o_i, \cdots, y_j = o_j$. Also assume $Y_{t} = Y_{1:t}$ and $Y = Y_T$.

Let $\alpha_i(t)$ be the probability of observing $o_1, \cdots, o_t$ *and* being at state $i$ at time $t$, that is

$$\alpha_i(t) = P_\theta(Y_t = O_t, q_t = i)$$

For $t = 1$, the probability of the initial state being $i$ is given by $\pi_i$. The probability $y_1 = o_1$ is given by $b_{i, o_{1}}$. Since these are independent:

$$\alpha_i(1) = P_\theta(Y_1 = O_1, q_1 = i) = \pi_i b_{i, o_{1}}$$

Suppose we know how to compute $\alpha_i(t)$. We can compute $\alpha_i(t + 1)$ inductively. Prior to being in state $i$ at $t+1$, we could have been in any of the states in $t$, so we have to account for all these possibilities by adding up all $\alpha_j(t)$ multiplied by the probability of the transition to $i$, $a_{ji}$.

And then account for the of probability $y_{t+1} = o_{t+1}$, that is, $b_{i, o_{t+1}}$:

$$\alpha_i(t + 1) = P_\theta(Y_{t+1} = O_{t+1}, q_{t+1} = i) = b_{i, o_{t+1}} \sum_{j \in S} \alpha_j(t) a_{ji}$$

### Computing $\beta$: Backward Procedure

Let $\beta_i(t)$ be the probability of observing $o_{t+1}, \cdots, o_T$, *given* we started from $i$ at instant $t$, that is:

$$\beta_i(t) = P_\theta(Y_{t+1:T} = O_{t+1:T} | q_t = i)$$

We can compute $\beta_i$ inductively starting from $T$ downwards, hence the *backward* terminology. For $t = T$ we define

$$\beta_i(T) = 1$$

Supposing we know how to compute $\beta_i(t + 1)$. We can compute $\beta_i(t)$ by considering all possible next states $j \in S$ as stating points, and assume we observed $o_{t+1}$ there. The assumption that we observed $o_{t+2}, \cdots, o_T$ is included in $\beta_j(t + 1)$.

The probability of transitioning from $i$ to $j$ is given by $a_{ij}$, the probability of observing $o_{t+1}$ in that state is $b_{j, o_{t+1}}$ and the probability of observing $o_{t+2}, \cdots, o_T$ starting from $j$ at $t+1$ is $\beta_j(t + 1)$, which leads to:

$$\beta_i(t) = P_\theta(Y_{t+1:T} = O_{t+1:T} | q_t = i) = \sum_{j \in S} a_{ij} \beta_j(t + 1) b_{j, o_{t+1}}$$

### Computing $\gamma$

Let's define $\gamma_i(t)$ be the probability of the state at time $t$ being $i$, given the observation $O$ (for all $t$'s) and the current parameter $\theta$, that is:

$$\gamma_i(t) = P_\theta(q_t = i \mid Y = O)$$

This conditional probability can be written as [4]:

$$P_\theta(q_t = i \mid Y = O) = \frac{P_\theta(q_t = i, Y = O)}{P_\theta(Y = O)}$$

The numerator $P_\theta(q_t = i, Y = O)$ can be split into $P_\theta(q_t = i, Y_{t} = O_{t}, Y_{t+1:T} = O_{t+1:T})$. Assuming $q_t = i$, the events $Y_{t} = O_{t}$ and $Y_{t+1:T} = O_{t+1:T}$ are independent, so

$$P_\theta(q_t = i, Y = O) = P_\theta(q_t = i, Y_{t} = O_{t}) P_\theta(q_t = i, Y_{t+1:T} = O_{t+1:T})$$

Which are the definitions of $\alpha_i(t)$ and $\beta_i(t)$, respectively, so

$$(8) \quad P_\theta(q_t = i, Y = O) = \alpha_i(t) \beta_i(t)$$

The denominator can be computed with the law of total probability [5] as:

$$P_\theta(Y = O) = \sum_{j = 1}^{N} P_\theta(Y = O, q_t = j)$$

Which by (8) gives us:

$$P_\theta(Y = O) = \sum_{j = 1}^{N} \alpha_j(t) \beta_j(t)$$

Putting it all together:

$$\gamma_i(t) = \frac{\alpha_i(t) \beta_i(t)}{\sum_{j = 1}^{N} \alpha_j(t) \beta_j(t)}$$

### Computing $\xi$

Let's define $\xi_{ij}(t)$ as the probability of the state being $i$ at $t$ and at $j$ at $t+1$, given the observation $Y = O$, that is:

$$\xi_{ij}(t) = P_\theta(q_t = i, q_{t+1} = j \mid Y = O)$$

This conditional probability can be written as [4]:

$$P_\theta(q_t = i, q_{t+1} = j  \mid Y = O) = \frac{P_\theta(q_t = i, q_{t+1} = j, Y = O)}{P_\theta(Y = O)}$$

In the numerator $P_\theta(q_t = i, q_{t+1} = j, Y = O)$ the implicit range $[1:T]$ of $Y = O$ can be split into 3 sub-ranges: $[1:t]$, $[t:t+1]$ and $[t+2:T]$.

* The range $[1:t]$ gives us $P_\theta(q_t = i, Y_t = O_t)$, which is $\alpha_i(t)$.
* In the range $[t:t+1]$ we must account for the transition from $q_t = i$ to $q_{t+1} = j$, which is given by $a_{ij}$ and of observing $y_{t+1} = o_{t+1}$ given the hidden state $j$, which is $b_{j, o_{t+1}}$
* The range $[t+2:T]$ gives us $P_\theta(q_{t+2} = j, Y_{t+2:T} = O_{t+2:T})$, which is $\beta_j(t+1)$.

Thus,

$$(9) \quad P_\theta(q_t = i, q_{t+1} = j, Y = O) = \alpha_i(t) a_{ij} b_{j, o_{t+1}} \beta_j(t+1)$$

The denominator can be computed by applying the law of total probability twice [5] as:

$$P_\theta(Y = O) = \sum_{i' = 1}^{N} \sum_{j' = 1}^{N} P_\theta(q_t = i', q_{t+1} = j', Y = O)$$

Where each $P_\theta(q_t = i', q_{t+1} = j', Y = O)$ can be computed via (9). In this case $P_theta(Y = O)$ was also computed for $\gamma_i(t)$, so we can reuse.

Finally $\xi$ can be computed as:

$$\xi_{ij}(t) = \frac{\alpha_i(t) a_{ij} b_{j, o_{t+1}} \beta_j(t+1)}{\sum_{i' = 1}^{N} \sum_{j' = 1}^{N} P_\theta(q_t = i', q_{t+1} = j', Y = O)}$$

### Updates

We can now update $\theta = (A, B, \pi)$ from $\gamma$ and $\xi$.

$A$ from (5):

$$\quad a_{ij} = \frac{\sum_{t=1}^{T-1} \xi_{ij}(t)}{\sum_{t=1}^{T-1} \gamma_{i}(t)}$$

$B$ from (6):

$$\quad b_{ik} = \frac{\sum_{t=1}^{T} 1[y_t = v_k] \xi_{ij}(t)}{\sum_{t=1}^{T} \gamma_{i}(t)}$$

Where $1[y_t = v_k]$ is the indicator function:

$$
\begin{equation}
  1[y_t = v_k] =\left\{
  \begin{array}{@{}ll@{}}
    1, & \text{if}\ y_t = v_k \\
    0, & \text{otherwise}
  \end{array}\right.
\end{equation}
$$

$\pi$ from (7):

$$\quad \pi = \gamma_{i}(1)$$


## Complexity

The complexity depends on how long it takes the algorithm to convert. Each iteration requires computing the intermediate variables $\alpha$, $\beta$, $\gamma$ and $\xi$. Computing $\xi$ requires $O(N^2 T)$ operations whereas computing $B$ requires $O(NMT)$ operations.

Assuming that the number of hidden states is at least the number of observable ones, we get $O(N^2 T)$ for each step.

## Conclusion

This post has taken me a really long to write (I started the draft in May), mainly because I wanted to understand the theory behind the algorithm. Wikipedia is very sparse about this theory and I couldn't find if the authors ever published a paper on this.

Stephen Tu's note [7] was tremendously helpful in filling in the gaps, though I still had to go through some extra hoops to feel confident about my understanding.

I finally understood why Baum-Welch is a special case of the EM Algorithm but I still don't have a good intuition on why EM works. My current understand is as follows: $E[P_{\theta}(Y = O)]$ is a function of $\theta$, and finding $\theta^{*} = \arg \max_{\theta} E[P_{\theta}(Y = O)]$ is hard. To make it tractable we fix some $\theta$ with an estimated value and try to optimize the remaining equation.

More simplistically, suppose we want to find $x$ that maximizes $f(x) = g(x) h(x)$. It might be hard to find the solution analytically for this, but if we assume a constant value ($x_0$) for $x$ in part of that expression, we might be able to solve for $f(x) = g(x) h(x_0)$, because now $h(x_0)$ is a constant. It's still a mystery to me in which cases doing this is valid though.

## Related Posts

* [Probabilistic Graphical Model Lecture Notes - Week 9]({{blog}}/2012/05/21/pgm-lecture-notes-week-9.html) - I learned about Expectation-Maximization a long time ago, while Probabilistic Graphic Models (PGM). I had forgot most of it and my notes were rather sparse.
* [Lagrangean Relaxation - Theory]({{blog}}/2012/02/05/lagrangean-relaxation-theory.html) - At first sight, Lagrangian multipliers look very much like Lagrangian relaxation from Integer Linear Programming, in the sense that we move constraints to the objective function, but for Lagrangian multipliers we leverage the fact they're differentiable to compute the gradient, which we cannot do in ILP. In ILP Lagrangian relaxation is mostly used to obtain approximated solutions for the original problem.

## References

* [[1](https://en.wikipedia.org/wiki/Leonard_E._Baum)] Wikipedia: Leonard E. Baum
* [[2](https://en.wikipedia.org/wiki/Lloyd_R._Welch)] Wikipedia: Lloyd R. Welch
* [[3](https://en.wikipedia.org/wiki/Baum%E2%80%93Welch_algorithm)] Wikipedia: Baum–Welch algorithm
* [[4]({{docs}}/math/probability.html)] kuniga.me - Probability Cheat Sheet, Conditional probability
* [[5]({{docs}}/math/probability.html)] kuniga.me - Probability Cheat Sheet, Law of Total Probability
* [[6]({{docs}}/math/probability.html)] kuniga.me - Probability Cheat Sheet, Law of The Unconscious Statistician
* [[7](https://stephentu.github.io/writeups/hmm-baum-welch-derivation.pdf)] Stephen Tu - Derivation of Baum-Welch Algorithm for Hidden Markov Models
