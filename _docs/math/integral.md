---
layout: doc
title: "Integral Cheat Sheet"
---



## Definition

There are multiple definitions of integrals, but we'll use the **Darboux integral** which is often mistakenly called *Riemann integral*.

Consider any partition $P$ of the line interval $[a, b]$ as:

$$a = x_0 \lt x_1 \lt \dots \lt x_n = b$$

Each $[x_{i-1}, x_{i}]$ for $i = 1, \dots, n$ is called a *subinterval* of $P$, which is also denoted by $(x_0, x_1, \dots, x_n)$.

Let $f: [a, b] \rightarrow \mathbb{R}$ and $P$ a partition of $[a, b]$. We define the upper $M_i$ and lower bounds $m_i$ of $f$ at a given subinterval as follows:

$$\begin{align}
M_i &= \sup \curly{f(x) : x \in [x_{i-1}, x_{i}]} \\
m_i &= \inf \curly{f(x) : x \in [x_{i-1}, x_{i}]}
\end{align}
$$

The **upper and lower Darboux sums** are defined as:

$$\begin{align}
U(f,P) &= \sum_{i = 1}^n M_i (x_{i-1} - x_{i}) \\
L(f,P) &= \sum_{i = 1}^n m_i (x_{i-1} - x_{i})
\end{align}
$$

If we think of numerical analysis, $U(f,P)$ is an upper bound value for the area under the curve $f(x)$, by approximating it via rectangles. Similarly $L(f, P)$ is the lower bound.

Now let $\cal{P}$ the set of all possible partitions of $[a, b]$. The upper and lower Darboux integrals are defined as:

$$\begin{align}
U(f) &= \inf \curly{U(f, P) : P \in \cal{P}} \\
L(f) &= \sup \curly{L(f, P) : P \in \cal{P}}
\end{align}
$$

Intuitively, $U(f)$ is the tighest upper bound for the area under the curve $f(x)$ and $L(f)$ is the tighest lower bound. If $U(f) = L(f)$, then we have an exact estimate for the area under the curve $f(x)$. The integral exists and $f$ is said to be **Darboux integrable**.

## Identities

### Divergence Theorem

Let $f$ be a differentiable function with domain $\mathbb{R}^n$ and $\nabla f$ the gradient:

$$
\nabla f = \left(\frac{\partial f}{\partial x_1}, \cdots, \frac{\partial f}{\partial x_n} \right)
$$

let $\Omega$ be a subset of $\mathbb{R}^n$ and $\delta \Omega$ its boundary. Then the **divergence theorem** states:

$$
\int_\Omega \nabla f dx = \int_{\delta \Omega} (f \cdot \mathbf{n}) dS
$$

Here $dx$ is a infinitesimal box in $\Omega$, $dS$ is an infinitesimal bit in the boundary $\delta \Omega$ (dimension $n- 1$) and $\mathbf{n}$ the normal vector at that point.

The intuition is that if we add the flow at each point inside a volume, it will cancel out and the only parts remaining will be the flow at the boundary.
