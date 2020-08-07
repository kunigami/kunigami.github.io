---
layout: post
title: "Lagrangean Relaxation - Theory"
tags: [graph theory]
---

In this post we will comment briefly on relaxations of Integer Linear formulations and focus on Lagrangian relaxations. Then we will demonstrate the application of this technique in the [Facility Location Problem](https://en.wikipedia.org/wiki/Facility_location). Unless stated otherwise, we are talking about minimization problems.

## Relaxations

In Integer Linear Programming, we use the term *relaxation* to indicate that some of the constraints of the original problem have been relaxed. Probably the best known example of relaxation is linear relaxation. In such case, we remove the integer constraints from the model.

More formally, if we have a formulation Frepresented by

$$z = \min \{c (x): x \in P \}$$

$$P$$ being the set of points that satisfy the constraints of this formulation, $$c(x)$$ the objective function and $$z$$ the value of the optimal solution. We say that a formulation $$F_R$$, given by

$$z_R = \min \{f (x): x \in R \}$$

is a relaxation if every point $$P$$ is in $$R$$, that is $$P \subseteq R$$ and if it $$f (x) \le c(x)$$ for $$x \in P$$.

Note that due to these properties, the optimal relaxation solution is a dual bound, i.e., a lower bound in our case (an upper bound if maximizing) for the original formulation ie $$z_R \le z$$. Dual bounds can be used, for example, to improve the performance of the [Branch and Bound algorithm](https://en.wikipedia.org/wiki/Branch_and_bound).

The advantage of using relaxations is that in some cases they are easier to solve. For example, linear relaxations can be solved in polynomial time, while the integer version, in general, is NP-difficult.

We can also relax a formulation by removing some of its constraints. Note, however, that the more we relax the formulation the worse the quality of the dual bound tends to be (consider the extreme case where we remove all inequalities from the model: it is still a relaxation, but the obtained dual bound serves no purpose).

The ideal is that we find a relaxation that is easy to solve and that gives tight dual bounds, that is, very close to the optimum value of the original formulation.

## Lagrangean Relaxation

There is a technique called *Lagrangian relaxation* [1], which consists of removing some of the constraints from the original formulation, but tries to embed these inequalities in the objective function.

The idea is to penalize the objective function when the removed constraints are violated. The "weight" of these penalties is controlled by coefficients called *Lagrangian multipliers*.

More precisely, suppose we have a formulation with $$m$$ constraints and $$n$$ variables:

$$z^* = \min cx$$

Subject to

$$\begin {array} {llcl} & A_1x & \le & b_1 \\& A_2x & \le & b_2 \\& x & \ge & 0 \\& x & \in & Z^n \end {array}$$

Where it $$A_1x \le b_1$$ represents the inequalities that we want to relax and $$A_2x \le b_2$$ the inequalities that we will maintain. The Lagrangian relaxation is then given by

$$z(u) = \min cx - u(b_1 - A_1x)$$

Subject to

$$\begin {array} {llcll} & A_2x & \le & b_2 & \\& x & \ge & 0 & \\& x & \in & Z^n & \\& u & \ge & 0, & u \in R^m_1 \end {array}$$

Where $$u$$ is a vector representing the Lagrangian multipliers (one for each inequality removed). For example, consider the inequality jthat will be removed:

$$\sum_{i = 1}^n a_{ij} x_i \le b_j$$

On removing inequality $$j$$, the following factor is subtracted from the objective function

(1) $$u_j (b_j - \sum_{i = 1}^n a_{ij} x_i)$$

Note that the more "violated" the inequality is, the more negative the term (1) will become. Since the object function is of minimization and we are subtracting, intuitively the optimal solution to the problem will tend not to violate the inequalities, improving the result of the dual limiting.

We choose to remove the inequalities so that the resulting problem is easier to solve for a fixed value of Lagrangian multipliers. However, now we have to solve another problem: find a set of Lagrangian multipliers $$u^*$$ that we minimize $$z(u)$$.

## Application to the problem of facility location

The problem of the location of facilities is widely studied in the literature. One of my undergrad research was to study approximate algorithms for this problem.

We basically have a bipartite graph where one partition corresponds to the set of customers $$C$$ and the other to the set of facilities $$F$$. Each customer $$i$$ can potentially be serviced by the factory $$j$$, at cost one $$c_{i, j}$$. In addition, a cost is paid $$c_j$$ for opening the factory $$j$$, given by $$y_j$$. The goal is to serve all customers at the lowest cost.

We will present an entire linear formulation for this problem. Let $$x_{i, j}$$ a binary variable be worth 1 s if the customer iis served by the factory jand the variable $$y_j$$ a binary variable worth 1 if and only if the factory $$j$$ has been opened. We have the following formulation:

$$z = \min \sum_{i \in C, j \in F} x_{ij} c_{ij} + \sum_{j \in F} y_j c_j$$

Subject to

$$\begin {array} {llcll} (2) & \sum_{j \in F} x_{ij} & \ge & 1 & \forall \, i \in C \\(3) & x_{ij} & \le & y_j & \forall i \in C, \forall j \in F \\& x_{i, j}, y_{j} & \in & \{0, 1 \} & \forall i \in C, \forall j \in F \end {array}$$

Inequality (2) says that each customer must be served by at least one factory and inequality (3), that a factory must be open so that it can serve a customer.

This is a very simple formulation and we basically have two groups of inequalities. We will try to relax the inequality group indicated by (2). We will have:

$$z_1(u) = \min \sum_{i \in C, j \in F} x_{ij} c_{ij} +
\sum_{j \in F} y_j c_j - \sum_{i \in C} u_i(\sum_{j \in F} x_{ij} - 1)$$

Subject to

$$\begin {array} {llcll} & x_{ij} & \le & y_j & \forall i \in C, \forall j \in F \\& u_i & \ge & 0 & \forall i \in C \\& x_{i, j}, y_{j} & \in & \{0, 1 \} & \forall i \in C, \forall j \in F \end {array}$$

With some algebra it is possible to “factor” the sum of the objective function:

$$z_1(u) = \min \sum_{j \in F} (\sum_{i \in C} (x_{ij} (c_{ij} - u_{i})) + c_j y_j)) + \sum_{ i \in C} u_i$$

As the term $$\sum_{i \in C} u_i$$ is constant, we can solve the problem without it and then add it to the value of the solution obtained. In addition, note that each factory contributes independently to the objective function and in each constraint only variables associated with a given factory appear.

In this way, we can decompose this formulation for each factory $$j$$:

$$\min \sum_{i \in C} (x_{ij} (c_{ij} - u_{i})) + c_j y_j$$

Subject to:

$$\begin {array} {llcll} & x_{ij} & \le & y_j & \forall i \in C \\& x_{i, j}, y_{j} & \in & \{0, 1 \} & \forall i \in C \end {array}$$

Each of these formulations can be solved by inspection. Define $$c'_{ij} = c_{ij} - u_{i}$$. If we do $$y_j = 0$$, we will not serve any customer, leading to a solution of cost 0. If $$y_j = 1$$, it only pays to serve customers with $$c'_{ij} < 0$$. Thus, the optimal cost of this formulation is:

$$\min \{0, c_j + \sum_{c '_{ij} <0} c' _{ij} \}$$

The other alternative is to relax constraints (3). In that case, after some algebra, we will have:

$$z_2 (u) = \min\sum_{i \in C, j \in F} x_{ij} (c_{ij} - u_{ij}) + \sum_{j \in F} y_j (c_j + \sum_{i \in C} u_{ij})$$

Subject to:

$$\begin {array} {llcll} & \sum_{j \in F} x_{ij} & \ge & 1 & \forall \, i \in C \\& u_{ij} & \ge & 0 & \forall i \in C, j \in F \\& x_{i, j}, y_{j} & \in & \{0, 1 \} & \forall i \in C, \forall j \in F \end {array}$$

Note that as there are no constraints involving both $$x$$ and $$y$$, we can solve the model for each type of variable separately. For a given variable $$y_j$$, if both $$c_j \ge 0$$ and $$u \ge 0$$, then is not worthwhile to open any factory, otherwise it is.

In the case of $$x$$, we can resolve independently for each $$i$$, by inspection. Define $$c'_{ij} = c_{ij} - u_{ij}$$.

For each $$i$$ we have to choose at least one $$j$$ to set $$x_{ij} = 1$$. If $$c'_{ij} > 0$$, it's worthwhile to set $$x_{ij} = 1$$, otherwise $$x_{ij} = 0$$. The corner case is when $$c'_{ij} \ge 0$$ for all $$j \in F$$. In this case, we have to do set $$x_{ij^*} = 1$$, where $$j^* = \mbox{argmin}_{j \in F} \{c'_{ij} \}$$.

We have two formulations that can be easily resolved. However, we must take into account which has the strongest formulation. In other words, given $$z_1^* = \max \{z_1(u): u \ge 0 \}$$ and $$z_2^* = \max \{z_2(u): u \ge 0 \}$$, we want the dual limit that comes closest to $$z$$. However, I cannot say which of the two relaxations presented is the strongest.

## Conclusion

Lagrangean relaxation is a powerful technique to obtain dual bounds for combinatorial problems that can be modeled as integer linear programs. These bounds can be used to try to improve the performance of branch-and-bound algorithms.

Furthermore, for certain problems, it is possible to adjust the solution obtained via relaxation in order to obtain a candidate solution to the original problem, hoping that the quality of the solution will not be much worse.

This post introduced the theory behind Lagrangian relaxation. In a future post I will present a heuristic that aims to find good Lagrangian multipliers.

## References

* [[1](http://athena.uwindsor.ca/users/b/baki%20fazle/73-605.nsf/0/a0f56d9f30b19b7485256a6300120aaf/$FILE/Fisher_Lagrangian.pdf)] The Lagrangian relaxation method for solving integer programming problems - ML Fisher Management Science , 1981.
