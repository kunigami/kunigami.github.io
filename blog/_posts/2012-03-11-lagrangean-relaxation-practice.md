---
layout: post
title: "Lagrangean Relaxation - Practice"
tags: [combinatorial optimization, graph theory]
---

In a [previous post]({{site.url}}/blog/2012/02/05/lagrangian-relaxation-theory.html), we studied the theory of Lagrangean relaxations. In this post we are going to present a heuristic that finds Lagrangean multipliers.

We will apply the algorithm to the Facility Location problem.

## Algorithm

The best known algorithm for finding good Lagrangean multipliers is given by Beasley [1], and is known by the subgradient method.

<figure class="center_children">
    <a href="https://en.wikipedia.org/wiki/Newton's_method_in_optimization"><img src="{{site.url}}/resources/blog/2012-03-11-lagrangean-relaxation-practice/gradient_descent.png" alt="https://en.wikipedia.org/wiki/Newton's_method_in_optimization" /></a>
    <figcaption>Gradient Descent for optimization</figcaption>
</figure>

Geometrically, if we had two Lagrangean multipliers, they would represent points on a plane and the values ​​of the optimal solutions of the Lagrangean dual for one of these points (multipliers) would form level curves. What the algorithm does is to look for a point with the closest value to the optimal primal solution so it goes in a direction (gradient) in which there is improvement of this function.

If there is no improvement for a long time, it may be that the “step” we are taking for each iteration is very large and there may be a better solution in the middle of that step. Therefore, we will gradually decrease the step, until eventually it becomes smaller than a minimum size.

* `01.` $$\hat x^*$$ *// Best dual solution*
* `02.` while True:
* `03.` $$\quad x^* \leftarrow \mbox{lagrangean_dual()}$$
* `04.` $$\quad \mbox{if } f'(x^*) \ge z_{LB}:$$
* `05.` $$\quad\quad z_{LB} \leftarrow f'(x^*)$$
* `06.` $$\quad\quad n_{stuck} \leftarrow 0$$
* `07.` $$\quad \mbox{else}:$$
* `08.` $$\quad\quad n_{stuck} \leftarrow n_{stuck} + 1$$
* `09.` $$\quad\hat x \leftarrow \mbox{lagrangean_heuristic()}$$ *// Feasible primal solution*
* `10.` $$\quad \mbox{if } f(\hat x) \le z_{LB}$$:
* `11.` $$\quad\quad z_{LB} \leftarrow f(\hat x)$$
* `12.` $$\quad\quad \hat x^* \leftarrow \hat x$$
* `13.` $$\quad \mbox{if } z_{LB} \leftarrow z_{UB}:$$ *// Found optimal solution*
* `14.` $$\quad\quad \mbox{STOP}$$
* `15.` $$\quad u_{\ell}^{k+1} \leftarrow \mbox{update_multipliers()}$$
* `16.` $$\quad \mbox{if } n_{stuck} = N:$$
* `17.` $$\quad\quad n_{stuck} \leftarrow 0$$
* `18.` $$\quad\quad \pi \leftarrow \pi / 2.0$$
* `19.` $$\quad\quad \mbox{if } \pi \le T_{\pi}:$$
* `20.` $$\quad\quad\quad \mbox{STOP}$$
* `21.` $$\quad k \leftarrow k + 1$$


Where function `update_multipliers()` is given by:


* `1.` $$\mbox{update_multipliers}(\pi, z_{LB}, z_{UB}, u_{\ell}^k, x^*)$$
* `2.` $$\quad G_\ell \leftarrow b_\ell - \sum_{j=1}^{m} a_{\ell j} x_{\ell j}^* \quad \forall \ell \in R$$
* `3.` $$\quad T \leftarrow \frac{\pi (1.05 z_{UB} - z_{LB})}{\sum_{\ell \in R} G_{\ell}^2}$$
* `4.` $$\quad u_{\ell}^{k + 1} \leftarrow \max\{0, u_{\ell}^k + TG_{\ell} \}\quad \forall \ell \in R$$
* `5.` $$\quad \mbox{return } u_{k + 1}$$


In this algorithm, the value of a primal solution is used, which is obtained from the relaxation using a heuristic.

## Application to the Facility Location problem

We will use the relaxation from our [previous post]({{site.url}}/blog/2012/02/05/lagrangian-relaxation-theory.html) that removes the constraints which forces customers to be covered by at least one installation.

**Lagrangean heuristics.** Since we are working with the version without capacity restriction, given a set of open factories it is trivial to find the optimal solution: just connect each customer to the cheapest open facility. It is this heuristic that we will use to obtain a primal solution from a relaxed solution (`lagrangean_heuristic()`).

**Lagrangean multipliers.** Note that the algorithm requires an initial set of Lagrangean multipliers. In our case, we did $$u_i = \frac{1}{n}$$, where $$n$$ is the number of relaxed inequalities.

For the other algorithm parameters we used: $$\pi = 2$$, $$T_\pi = 0.2$$ and $$N = 30$$.

I've implemented it in C ++, trying to separate the problem (class `FacilityLocation` implementing class `LagrangeanModel`) from the algorithm (class `GradientMethod`). As usual, the code is on [github](https://github.com/kunigami/blog-examples/tree/master/lagrangean-relaxation).

## Computational results

To test the algorithm, I used the instances of the Capacitated Facility Location problem from [OR-Library](http://people.brunel.ac.uk/~mastjjb/jeb/orlib/capinfo.html) [3]. Since we are dealing with the uncapacitated version, I ignored the capacity data.

They also provide [the optimal solutions values](http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/uncapopt.txt) so that we can compare the quality of the solutions obtained via relaxation. The following table contains the instance name, the number of facilities, the number of customers, the gap (%) between the optimal and the best primal value obtained, the distance (%D) between the best primal and dual values ​​(Gap DP) and finally time taken in seconds.

We observed that the optimal solution was found for most of the instances. The worst result was for the instance `capb`, due to the difficulty of the algorithm in finding good Lagrangean multipliers (note the distance between the primal and dual values).

Note that even for large instances, probably not feasible to be solved exactly, they are solved in less than 6 seconds (i7 2.2GHz processor, without parallelism).

## Conclusion

Although we have to deal with a formulation of the problem to decide which restrictions to relax and get the relaxed formulation, we do not need to work with the original formulation explicitly (in general solving ILPs, mainly for large formulations, is not very fast). Thus, depending on the complexity of the resulting problem, Lagrangean relaxation can be quite efficient.

We have seen that in practice the solutions obtained via Lagrangean heuristics can be very good. The main problem is that the choice of parameters can greatly influence the quality of these solutions.

I had already done work on Lagrangean relaxation during a class in college, on a problem called *Axis Parallel Minimum Stabbing Number Spanning Tree*.

## References

* [1] JE Beasley - Modern Heuristics for Combinatorial Optmization Problem, Chapter 6 (Lagrangean Relaxation). Blackwell Scientific Publications, 1993.
* [2] Full Linear Programming Report (MO420) - Guilherme Kunigami
* [3] OR-Library
