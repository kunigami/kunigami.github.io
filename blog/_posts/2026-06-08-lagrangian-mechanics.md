---
layout: post
title: "Lagrangian Mechanics"
tags: [mechanics]
vanity: "2026-06-08-lagrangian-mechanics"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/lagrange.png" alt="Thumbnail of Joseph-Louis Lagrange" />
</figure>


I've been reading the book *The Theoretical Minimum* [1] by Leonard Susskind and George Hrabovsky and the first topic I don't recall learning in school is Lagrangian mechanics.

In this post I'd like to cover this formulation and connect it with the mathematical concepts we studied previously.

<!--more-->

## The Lagrangian

Suppose we have a function $q: \mathbb{R} \rightarrow \Omega$. The region $\Omega$ is called a **configuration space**. If we assume the domain $\mathbb{R}$ is time and $\Omega$ the set of valid positions of a particle in space, then one way to interpret the function $q(t)$ is an oracle that tells us the position of a particle at an instant $t$.

Note that $\Omega$ can encode the position of multiple particles. If talking about one particle in space, we'd have $\mathbb{R}^3$, but for a $N$ particle system, we would have $\mathbb{R}^{3N}$.

Let's denote by $\dot{q}$ the derivative of $q$ with respect to $t$. Following our previous interpretation, $\dot {q}(t)$ corresponds to the velocity of the particle at an instant $t$.

The *Lagrangian* is a scalar function $L : \Omega \times \Omega \times \mathbb{R} \rightarrow \mathbb{R}$. In physics, it depends on $q, \dot{q}$ at an instant of time $t$, so we write

$$
L(q(t), \dot{q}(t), t)
$$

but it's common to write in the abbreviated form:

$$
L(q, \dot{q}, t)
$$


## The Action Functional

We define the **action functional** as:

$$
S[q] = \int_{t_0}^{t_1} L(q(t), \dot{q}(t), t) dt
$$

Note that $S$ is a function of $q$, which is also a function, so $S$ is a [functional](https://www.kuniga.me/blog/2026/04/26/functionals.html) which returns a scalar. One way to see $q$ is as a curve from $q(t_0)$ to $q(t_1)$. And then we can also interpret $S[q]$ as a "score" for the curve $q$. A more mathematical interpretation of $S$ is of a generalized length. That is, if $q$ is a curve, then

$$
\int_{t_0}^{t_1} \abs{\dot{q}(t)} dt
$$

Is the length of this curve, so we could think of the Lagrangian as a more generalized version of length.

## First Variation and the Fréchet Derivative

Now suppose we "wiggle" curve $q$ and obtain another $q_\epsilon$ that coincides with $q$ on $t_0$ and $t_1$. Such path can be represented by:

$$
q_\epsilon(t) = q(t) + \epsilon\eta(t)
$$

Where we can think of $\eta$ as a function yielding some direction and $\epsilon$ an infinitesimal scalar. This is the infinite-dimensional analogue of translating a finite vector towards a direction $v$:

$$
q_\epsilon = q + \epsilon v
$$

Now suppose we want to compute the change in $S$ by moving towards direction $\eta$. We can then define the **(first) variation of $S$** as:

$$
(1) \quad \delta S[q; \eta] = \lim_{\epsilon \rightarrow 0} \frac{S[q + \epsilon\eta] - S[q]}{\epsilon}
$$

We can connect it to the Fréchet derivative [2]. Recall that the Fréchet derivative at $q$ is the linear map $DS_q: \Omega \rightarrow \mathbb{R}$ such that

$$
\lim_{\norm{h} \rightarrow 0} \frac{\norm{S[q + h] - S[q] - DS_q[h]}}{\norm{h}} = 0
$$

For every $h \in \Omega$, so it should hold for $h = \eta$ and thus $\delta S[q; \eta] = DS_q[\eta]$. This is the infinite dimension analogue of computing the differential for a multi-valued function $Df_q$ and then applying it to a value $x$: $Df_q(x)$.

## Gradient and Riesz Representation

In [2], we discussed the *Riesz Representation Theorem* which states that if $f$ is a linear functional over a Hilbert space $H$, then there's a unique vector $y$ in $H$ such that:

$$
f(x) = \langle x, y \rangle, \quad \forall x \in H
$$

Now consider the Fréchet derivative $DS_q$. Since it's a linear map and we're assuming $q$ is over the Euclidean space which is a Hilbert one, we can use the theorem and conclude that there's a unique element in $\Omega$ such that:

$$
DS_q[\eta] = \langle g, \eta \rangle, \quad \forall \eta \in H
$$

Now we denote $g$ as $\nabla S(q) = g$, so we could write:

$$
\delta S[q; \eta] = \langle \nabla S(q), \eta \rangle,
$$

For finite dimensions, if $Df_x$ is the derivative of $f$ at $x$, then:

$$
Df_x(v) = \langle \nabla f(x), v \rangle = \nabla f(x) \cdot v
$$


## Principle of Stationary Action

This is also known as the *principle of least action*, but this is mathematically inaccurate because it implies the minimization of some function but it could be maximization too.

It says that:

$$
(2) \quad \delta S[q; \eta] = 0 \quad \forall \eta
$$

with $\eta(t_0) = \eta(t_1) = 0$, which encodes the fact that the wiggled curve coincides with the original curve's endpoints. One way to interpret this is that small wiggling of $q$ doesn't change the score $S$.

For finite dimensions, this is analogous to the expression:

$$
\nabla f(x) = 0
$$

since that implies

$$
D f_x(v) = 0
$$

for all $v$.

## Euler-Lagrange Equations

Now let's plug the Lagrangian into $S$ and see what we get. We start by computing $S[q_\epsilon]$:

$$
S[q_\epsilon] = \int_{t_0}^{t_1} L(q_\epsilon(t), \dot{q_\epsilon}(t), t) dt
$$

By some algebra and calculus we obtain that:

$$
\delta S[q; \eta] = \int_{t_0}^{t_1} \left( \frac{\partial L}{\partial q} + \frac{d}{dt} \frac{\partial L}{\partial \dot{q}} \right) \eta  dt
$$

<details>

Using $q_\epsilon(t) = q(t) + \epsilon \eta(t)$ we have:

$$
\dot{q}_\epsilon(t) = \dot{q}(t) + \epsilon \dot{\eta}(t)
$$

So $S[q_\epsilon]$ is (omitting the parameter $(t)$ for simplicity):

$$
S[q + \epsilon \eta] = \int_{t_0}^{t_1} L(q + \epsilon \eta, \dot{q} + \epsilon \dot{\eta}, t) dt
$$

Now we can compute the first variation $(1)$, and using the linearity of integrals we get:

$$
\delta S[q; \eta] = \lim_{\epsilon \rightarrow 0} \int_{t_0}^{t_1} \frac{L(q + \epsilon \eta, \dot{q} + \epsilon \dot{\eta}, t) - L(q, \dot{q}, t)}{\epsilon} dt
$$

We have that

$$
L(q + \epsilon \eta, \dot{q} + \epsilon \dot{\eta}, t) =  L(q, \dot{q}, t) + \epsilon \frac{\partial L}{\partial q} \eta + \epsilon \frac{\partial L}{\partial \dot{q}} \dot{\eta} + o(\epsilon)
$$

where $o(\epsilon)$ means this term is neglible compared to $\epsilon$ (e.g. a polynomial where the smallest order is $\epsilon^2$), so that $\lim_{\epsilon \rightarrow 0} o(\epsilon) / \epsilon = 0$. Dividing by $\epsilon$ and taking the limit:

$$
\lim_{\epsilon \rightarrow 0} \frac{L(q + \epsilon \eta, \dot{q} + \epsilon \dot{\eta}, t) - L(q, \dot{q}, t)}{\epsilon} = \frac{\partial L}{\partial q} \eta + \frac{\partial L}{\partial \dot{q}} \dot{\eta}
$$

Thus:

$$
\delta S[q; \eta] = \int_{t_0}^{t_1} \left( \frac{\partial L}{\partial q} \eta + \frac{\partial L}{\partial \dot{q}} \dot{\eta} \right) dt
$$

Now we do integration by parts for the second partial derivative:

$$
 \int_{t_0}^{t_1} \frac{\partial L}{\partial \dot{q}} \dot{\eta} dt = \left[ \frac{\partial L}{\partial \dot{q}} \eta  \right]^{t_1}_{t_0} - \int_{t_0}^{t_1} \frac{d}{dt} \left( \frac{\partial L}{\partial \dot{q}} \right) \eta dt
$$

since $\eta(t_1) = \eta(t_0)$, the first term vanishes so we end up with:

$$
\delta S[q; \eta] = \int_{t_0}^{t_1} \left( \frac{\partial L}{\partial q} \eta + \frac{d}{dt} \frac{\partial L}{\partial \dot{q}} \eta \right) dt = \int_{t_0}^{t_1} \left( \frac{\partial L}{\partial q} + \frac{d}{dt} \frac{\partial L}{\partial \dot{q}} \right) \eta  dt
$$

</details>

Let's define:

$$
E(L) = \frac{\partial L}{\partial q} + \frac{d}{dt} \frac{\partial L}{\partial \dot{q}}
$$

By $(2)$ we have:

$$
(3) \qquad \int_{t_0}^{t_1} E(L) \eta dt = 0 \qquad \forall \eta
$$

The *Fundamental Lemma of the Calculus of Variations* states that:

**Lemma 1.** Let $f: [a, b] \rightarrow \mathbb{R}$ be a continuous function. If

$$
\int_{a}^b f(t) \eta(t) dt = 0
$$

for every smooth function satisfying $\eta(a) = \eta(b) = 0$, then

$$
f(t) = 0 \qquad \forall t \in [a, b]
$$

Applied to our case, this means $E(L) = 0$ and hence:

$$
(4) \quad \frac{\partial L}{\partial q} + \frac{d}{dt} \frac{\partial L}{\partial \dot{q}} = 0
$$

which is the **Euler-Lagrange equations**.

## Sobolev Spaces

We mentioned at the beginning that $q$ is a function $q: \mathbb{R} \rightarrow \Omega$ but we haven't really specified what kind of function this is. We didn't need it to understand the derivations all the way up to the Euler-Lagrange equations but we shall do it now to keep things rigorous.

In [Sobolev Spaces](https://www.kuniga.me/blog/2026/05/02/sobolev-spaces.html) we learned about weak derivatives and different function spaces and their notations such as the space of smooth functions $C^\infty(D)$ for some domain $D$. The function space implicitly assumed in mechanics is $C^2([t_0, t_1])$, the space of twice differentiable functions over the interval $[t_0, t_1]$.

The need for twice-differentiability is due to this term in the Euler-Lagrange equation $(4)$:

$$
\frac{d}{dt} \frac{\partial L}{\partial \dot{q}}
$$

Which expands to:

$$ = \frac{\partial^2 L}{\partial q \partial \dot{q}} \dot{q} + \frac{\partial^2 L}{\partial \dot{q}^2} \ddot{q}  + \frac{\partial^2 L}{\partial t \partial \dot{q}}
$$

So the term $\ddot{q}$ requires $q$ to be twice-differentiable. This condition can be too restrictive in the real world, since $q$ might not be differentiable everywhere along $t_0$ to $t_1$.

A more relaxed function space is used, $H^1$, which as we've seen in [3] is the Sobolev space $W^{1, 2}$. This means that if a function $f$ belongs to this space:

* It also belongs to $L^2$, i.e. it's a square integrable function;
* It has the first weak derivative, and it also belongs to $L^2$
* $H^1$ is a Hilbert space, so $f$ has an inner product;

The Lagrangian is often defined as the kinetic energy minus potential energy:

$$
L(q, \dot{q}, t) = T(q, \dot{q}, t) - V(q, t)
$$

and the kinetic energy is:

$$
\frac{1}{2} m \dot{q}^2
$$

So the derivative of $q$ squared appears inside the integral of $S[q]$. So we just require that $q$ has a weak derivative and that it is square integrable, which is one of the properties of $H^1$.

Now how about $\ddot{q}$ in the Euler-Lagrange? Turns out that when we start from a weaker assumption such as $q$ being from a Sobolev space $H^1$, we don't actually solve the Euler-Lagrange directly. This equation is only available when we can make stronger assumptions about $q$, but otherwise we need to work with $(3)$.

## Manifolds

There's one more piece we didn't discuss yet: what $\Omega$ is. We mentioned that for a single particle in 3D it could be $\mathbb{R}^3$, but a more general object than the Euclidean space is a smooth manifold.

A smooth manifold is a geometric object that locally behaves like $\mathbb{R}^n$. The sphere is such an example, where in an infinitesimal patch it behaves like a plane.

Since I haven't studied manifolds yet, I'll leave this discussion for another time.

## Conclusion

In this post we covered Lagrangian mechanics and made connections with math concepts such as [functionals](https://www.kuniga.me/blog/2026/04/26/functionals.html) and [Sobolev Spaces](https://www.kuniga.me/blog/2026/05/02/sobolev-spaces.html). I've seen these terms thrown around before and it's nice to finally get a sense of what they are.

Even for manifolds, which I haven't studied yet, I now have a better understanding where it comes into picture.

The book *The Theoretical Minimum* doesn't go as deep on the mathematics, but I enjoyed the process of digging into it.

## References

* [1] The Theoretical Minimum, Leonard Susskind and George Hrabovsky
* [[2](http://localhost:4000/blog/2026/04/26/functionals.html)] NP-Incompleteness - Functionals
* [[3](http://localhost:4000/blog/2026/05/02/sobolev-spaces.html)] NP-Incompleteness - Sobolev Spaces
