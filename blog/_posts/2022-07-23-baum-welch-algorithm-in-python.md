---
layout: post
title: "Baum-Welch Algorithm: Python Implementation"
tags: [artificial intelligence, python]
excerpt_separator: <!--more-->
vanity: "2022-07-23-baum-welch-algorithm-in-python"
---

{% include blog_vars.html %}

In this post we provide an implementation of the Baum-Welch algorithm in Python. We discussed the theory in a previous post: [Baum-Welch Algorithm: Theory]({{blog}}/2022/07/19/baum-welch-algorithm.html).

<!--more-->

## Theory Recap

Let's first summarize the algorithm in high-level. We first generate an initial value for $\theta$, $\theta^{(1)}$. Then we obtain $\theta^{(n+1)}$ from $\theta^{(n)}$ until $P_\theta(Y = O)$ stops improving.

To obtain $\theta^{(n+1)}$ from $\theta^{(n)}$ we first compute these intermediate variables:

$$
\begin{equation}
\begin{split}

\alpha_i(1) & = \pi_i b_{i, o_{1}} & \\
\alpha_i(t + 1) & = b_{i, o_{t+1}} \sum_{j \in S} \alpha_j(t) a_{ji} \qquad & t = 1, \cdots, T-1\\
\\
\beta_i(T) & = 1 & \\
\beta_i(t) &= \sum_{j \in S} a_{ij} \beta_j(t + 1) b_{j, o_{t+1}} \qquad & t = 1, \cdots, T-1\\
\\
\gamma_i(t) &= \frac{\alpha_i(t) \beta_i(t)}{\sum_{j = 1}^{N} \alpha_j(t) \beta_j(t)} \qquad & t = 1, \cdots, T\\
\\
\xi_{ij}(t) &= \frac{\alpha_i(t) a_{ij} b_{j, o_{t+1}} \beta_j(t+1)}{\sum_{i' = 1}^{N} \sum_{j' = 1}^{N} \alpha_i(t) a_{i'j'} b_{j', o_{t+1}} \beta_j(t+1)} \qquad & t = 1, \cdots, T-1\\

\end{split}
\end{equation}
$$

It's worth noting that in practice the denominator of $\gamma_i(t)$ and $\xi_{ij}(t)$ are all the same, $P_\theta(Y = O)$ and hence we don't have to compute them repeatedly.

We can now update $\theta = (A, B, \pi)$ from $\gamma$ and $\xi$:

$$
\begin{equation}
\begin{split}
a_{ij} &= \frac{\sum_{t=1}^{T-1} \xi_{ij}(t)}{\sum_{t=1}^{T-1} \gamma_{i}(t)}\\
\\
b_{ik} &= \frac{\sum_{t=1}^{T} 1[y_t = v_k] \xi_{ij}(t)}{\sum_{t=1}^{T} \gamma_{i}(t)}\\
\\
\pi &= \gamma_{i}(1)

\end{split}
\end{equation}
$$

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

## Python Implementation

Implementing the Baum-Welch algorithm is relatively straightforward. It does involve a bunch of sums and indexes which are harder to debug, though.

The model is just a bag of parameters including the observations, represented by a `dataclass`:

{% highlight python %}
from numpy.typing import ArrayLike

@dataclass
class HMM:
   hidden_sts: ArrayLike
   visible_sts: ArrayLike
   trans_prob: ArrayLike
   obs_prob: ArrayLike
   ini_prob: ArrayLike

   obs: ArrayLike
{% endhighlight %}

The algorithm is implemented by the `BaumWelch` class, which is stateless but groups all methods for computing the auxiliary variables. The `run()` method keeps running the `iterate()` method until the likelihood stops improving:

{% highlight python %}
class BaumWelch:
def run(self, model):
   current_likelihood = self.get_likelihood(model)
   while True:
     new_model = self.iterate(model)

     new_likelihood = self.get_likelihood(new_model)
     if new_likelihood < current_likelihood:
       break
     model = new_model

     current_likelihood = new_likelihood
{% endhighlight %}

The function `iterate()` consists of computing the auxiliary variables and then creating a new model. Noting that $P_\theta(Y = O)$ is the likelihood and also the denominator for all $\gamma$ and $\xi$, so we can compute it separately and reuse.

{% highlight python %}
def iterate(self):
 alpha = self.calc_alpha()
 beta = self.calc_beta()

 likelihood = self.get_likelihood()

 gamma = self.calc_gamma(alpha, beta) / likelihood
 xi = self.calc_xi(alpha, beta) / likelihood

 return HMM(
   hidden_sts=self.model.hidden_sts,
   visible_sts=self.model.visible_sts,
   trans_prob=self.calc_trans_prob(gamma, xi),
   obs_prob=self.calc_obs_prob(gamma, xi),
   ini_prob=self.calc_ini_prob(gamma),
 )
{% endhighlight %}

Computing $\alpha$, $\beta$, $\gamma$ and $\xi$ is almost a direct implementation of their definitions above, and we leverage NumPy's API for more conciseness.

{% highlight python %}
def calc_alpha(self, model):
 alpha = np.empty(shape=(model.N, model.T))
 for i in model.states():
   alpha[i, 0] = model.ini_prob[i] * model.obs_prob[i, model.obs[0]]

 for t in range(1, model.T):
   for i in model.states():
     alpha[i, t] = sum(alpha[:, t - 1] * model.trans_prob[:, i]) * \
       model.obs_prob[i, model.obs[t]]
 return alpha

# backward step
def calc_beta(self, model):
 beta = np.empty(shape=(model.N, model.T))
 for i in model.states():
   beta[i, model.T-1] = 1

 for t in range(model.T-2, -1, -1): # T-2 to 0
   for i in model.states():
     beta[i, t] = sum(
       model.trans_prob[i, :] *
       beta[:, t + 1] *
       model.obs_prob[:, model.obs[t + 1]]
     )
 return beta

# un-normalized
def calc_gamma(self, model, alpha, beta):
 gamma = np.empty(shape=alpha.shape)
 for i in model.states():
   gamma[i, ] = alpha[i, ] * beta[i, ]
 return gamma

# un-normalized
def calc_xi(self, model, alpha, beta):
 xi = np.empty(shape=(model.N, model.N, model.T))
 for t in range(model.T - 1):
   for i in model.states():
     for j in model.states():
       a = model.trans_prob[i, j]
       b = model.obs_prob[j, model.obs[t + 1]]
       xi[i, j, t] = alpha[i, t] * a * b * beta[j, t+1]
 return xi
{% endhighlight %}

Updating $A$, $B$ and $\pi$ is also a most direct implementation of the equations above:

{% highlight python %}
def calc_trans_prob(self, model, gamma, xi):
 trans_prob = np.empty(shape=model.trans_prob.shape)
 for i in model.states():
   den = sum(gamma[i, 0:model.T-1])
   for j in model.states():
     trans_prob[i, j] = sum(xi[i, j, 0:model.T-1]) / den
 return trans_prob

def calc_obs_prob(self, model, gamma, xi):
 obs_prob = np.empty(shape=model.obs_prob.shape)
 for i in model.states():
   for k in model.observations():
     num = sum(
       gamma[i, t] for t in range(model.T) if model.obs[t] == k
     )
     obs_prob[i, k] = num / sum(gamma[i, :])
 return obs_prob

def calc_ini_prob(self, gamma):
 return gamma[:, 0]
{% endhighlight %}

The full code is on [Github]({{github}}/baum_welch.py) and is less than 200 lines.

## Experiments

Here we describe running the implementation against some small examples.

### Wikipedia's Example

Wikipedia [3] has this simple example:

> Suppose we have a chicken from which we collect eggs at noon every day. Now whether or not the chicken has laid eggs for collection depends on some unknown factors that are hidden. We can however (for simplicity) assume that the chicken is always in one of two states that influence whether the chicken lays eggs, and that this state only depends on the state on the previous day. Now we don't know the state at the initial starting point, we don't know the transition probabilities between the two states and we don't know the probability that the chicken lays an egg given a particular state

It also proposed an initial value for the $\theta$ param which corresponds to the following code:

{% highlight python %}
model = HMM(
   hidden_sts=["state 1", "state 2"],
   visible_sts=["no eggs", "eggs"],
   trans_prob=np.array([
     [0.5, 0.5],
     [0.3, 0.7],
   ]),
   obs_prob=np.array([
     [0.3, 0.7],
     [0.8, 0.2],
   ]),
   ini_prob=[0.2, 0.8],
   obs=[0, 0, 0, 0, 0, 1, 1, 0, 0, 0]
)
{% endhighlight %}

After running the Baum-Welch algorithm we obtain the updated param $\theta$:

{% highlight text %}
Transition Probability:
[
 [0.5004038  0.4995962 ]
 [0.14308799 0.85691201]
]

Observation Probability:
[
 [0.01, 0.99],
 [1.00, 0.00],
]

Initial Probability:
[0.00 1.00]
{% endhighlight %}

This model essentially tells us that we'll for sure start in hidden *state 2*, with probability of switching to *state 1* of ~15%. Once in *state 1*, we have 50/50 chance of staying vs. changing state.

The observation matrix tells us that the observation on the state is deterministic. If in *state 1* we'll emit "eggs", while in *state 2* we'll emit "no eggs".

Intuitively this seems consistent with the observation: there are 10 observation events, 80% of which are "no eggs" and 20% are "eggs". The transition probability says we're more likely to be on *state 2* on any given time, in which case we'll be observing "no eggs" only.

The computed likelihood is 1.41% which feels low, but unless the model perfectly models the underlying phenomena, more observations will tend to reduce the likelihood.

### Toy Example

I tried to create a case that can be perfectly modeled by an HMM. It has the same domain as the Wikipedia example but the observation can be generated by a model where we *always* transition states and where the observation is deterministic from the hidden state.

We start with our initial $\theta$ slightly biased towards the idea model (40/60 split):

{% highlight python %}
model = HMM(
   hidden_sts=["state1", "state2"],
   visible_sts=["yes", "no"],
   trans_prob=np.array([
     [0.4, 0.6],
     [0.6, 0.4],
   ]),
   obs_prob=np.array([
     [0.6, 0.4],
     [0.4, 0.6],
   ]),
   ini_prob=[1.0, 0],
   obs=[0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
)
{% endhighlight %}

The algorithm converges to what we would expect and the computed likelihood is 100%, which is a good sanity check.

### Choice of Initial Value

From a few runs with the examples above, I found the algorithm very sensitive to the initial choice of $\theta$. One of my first attempts was to make no assumptions and start all probability matrices with a uniform distribution but the algorithm couldn't make progress.

It might be necessary to try multiple initial choices, perhaps each encoding some bias towards different dimensions to avoid having to rely too much on a single choice.

## Conclusion

The main motivation for studying the Baum-Welch is to apply it to speech recognition. My next step is to understand how to adapt it for such use case. In [2] Rabiner suggests either handling observation vectors with continuous values (as opposed to discrete ones we've studied so far) or using *Vector Quantization* (VQ).

## References

* [[1]({{blog}}/2022/07/19/baum-welch-algorithm.html)] NP-Incompleteness: Baum-Welch Algorithm: Theory
* [2] A Tutorial on Hidden Markov Models and Selected Applications in Speech Recognition - L. Rabiner
* [[3](https://en.wikipedia.org/wiki/Baum%E2%80%93Welch_algorithm)] Wikipedia: Baum–Welch algorithm
