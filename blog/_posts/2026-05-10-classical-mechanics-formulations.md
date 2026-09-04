---
layout: post
title: "Classical Mechanics Formulation"
tags: [mechanics]
vanity: "2026-05-10-classical-mechanics-formulations"
excerpt_separator: <!--more-->
---

Like axioms?

Mechanics is about predicting the future. More specifically we need to determine the function $q(t)$, which tells us the position of a particle at an instant $t$.

Once we have this function, we can determine the velocity at any point by differentiating it with respect to time. We can also get the acceleration by differentiating the velocity with respect to time.

What is given depends on the formulation.

## Newtonian

For the Newtonian formulation we need the following:

### 1. State Variables

This represents the possible states of the system. The dimension of the system typically matches the dimension of the variables provided as input, in our case, the initial position $q$ and the momentum $p$ (which we'll use instead of velocity - we explain why later).

In 3 dimensions the pair $q, p$ adds up to $\mathbb{R}^{6}$. The system can contain $N$ particles, in which case the dimension scales accordingly, $\mathbb{R}^{6N}$.

We can think of our model as a function, in which $q$ and $v$ are variables, so the state is analogous to the domain of a function. Note that this does not specify the coordinate system (e.g. cartesian or polar).

#### 2. Time

Time is not part of the state, even though it can be used as parameter of functions.

#### 3. Momentum

The *momentum* is defined as:

$$
p(t) = m(t) \frac{dq}{dt}(t)
$$

or in the more compact physics notation:

$$
p = m \dot{q}
$$

One may ask why $p$ is a variable on the system given we can derive $p(t)$ from $q(t)$. The problem is that we don't know $q(t)$ (the function): that's what we're trying to find out! What we are given is $q$ at a point in time ($t = 0$) and we can't tell what $p$ is at that time, so it must be provided as well.

### 4. Force

Let $F$ be a function of $q(t), p(t)$ and $t$. This is called a *force function*. Then

$$
\frac{dp}{dt}(t) = F(q(t), p(t), t)
$$

or in the more compact notation:

$$
\dot{p} = F
$$

for constant mass, we get the more famous $F = ma$:

$$
m \ddot{q} = F
$$

Usually this is the equation used for Newtonian mechanics, but I opted to work with momentum and skip velocity because even when we use velocity we need to introduce momentum to talk about conservation of momentum.

### 5. Input

So the problem we're trying to solve is: we want to know what $q(t)$ will be at any point $t$. What we're given are the points $q(0)$, $p(0)$ and the function $F$.

We also need to specify the function $F$. For example, for gravity, we have $F(q, p, t) = -mg$, which you'll notice it's a constant with respect to its inputs. For a spring setup, we have $F(q, p, t) = -kq$.

## Lagrangian

For the Lagrangian formulation we need the following:

### 1. Configuration Variables

### 2. Lagrangian

The Lagrangian is a scalar function of $q, q'$ and $t$, similar to the force function in the Newtonian formulation. We define the **action functional** as:

$$
S[q] = \int_{t_0}^{t_1} L(q(t), q'(t), t) dt
$$

Note that here $q$ is a function, so $S$ is a [functional](https://www.kuniga.me/blog/2026/04/26/functionals.html) which returns a scalar. One way to see $q$ is as a path from $q(t_0)$ to $q(t_1)$. We can "perturb" this path and obtain another $q_\epsilon$ that coincides with $q$ on $t_0$ and $t_1$. Such path can be represented by:

$$
q_\epsilon(t) = q(t) + \epsilon\eta(t)
$$

Where we can think of $\eta$ as a function yielding some direction and $\epsilon$ a small displacement. We can then define the **variation of $S$** as:

$$
\delta S[q, \eta] = \lim_{\epsilon \rightarrow 0} \frac{S[q + \epsilon\eta] - S[q]}{\epsilon}
$$

Finally the constraint imposed by this formulation is that

$$
\delta S[q, \eta] = 0 \quad \mbox{for all } \eta
$$

or more succinctly:

$$
\delta S = 0
$$

this is saying that tiny perturbations on the path $q$ will not change the "score" $S[q]$ we associate with it.

This is the infinite-dimensional (or functional) analogous to having $\nabla f(x) = 0$ for a stationary point $x$ of $f$. Here we're trying to find a function $q(t)$ which is a stationary "point" for $S$, by having $\delta S[q] = 0$. An equivalent constraint is the so called *Euler-Lagrange* formula:

$$
\frac{\partial L}{\partial q} = \frac{d}{dt} \frac{\partial L}{\partial q'}
$$

<details>
This notation is a bit confusing, so we need to clarify a bit. As we've seen $L$ is really a function $L(q(t), q'(t), t)$ so the output of $q(t)$ and $q'(t)$ are variables, which we confusingly set as $q$ and $q'$, respectively but we'll call them $x$ and $y$ for clarify. So we can differentiate $L(x, y, t)$ with respect to $x$, leaving us with another function:

$$
\frac{\partial L}{\partial x} (x, y, t)
$$

Similar for $q' = y$:

$$
(A) \quad \frac{\partial L}{\partial y} (x, y, t)
$$

Now we can diferentiate with respect to $t$, but since both $x$ and $y$ are functions of $t$, we can see $(A)$ as a function of $t$, so we don't need partial derivatives:

$$
\frac{d}{dt} \frac{\partial L}{\partial y} (x, y, t)
$$

</details>

In classical mechanics the Lagrangian is typically defined as the difference between kinetic energy and potential energy. That is, given a function $T(q, \dot{q}, t)$ corresponding to the *kinetic energy* and $V(q, \dot{q}, t)$ to the *potential energy*. We have:

$$
L(q, \dot{q}, t) = T(q, \dot{q}, t) - V(q, \dot{q}, t)
$$

### Input

Like in the Newtonian case, we're trying to find $q(t)$ but now we're given $L$ (or both $T$ and $V$). The solution to the Euler-Lagrange equation is a family of path/functions so we need initial conditions to determine one of them, to $q(0)$ and $q'(0)$ must be given.


## Hamiltonian

Re-learning physics
* Lens of ML models, linear integer programming
