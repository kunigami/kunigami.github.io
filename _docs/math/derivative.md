---
layout: doc
title: "Derivatives Cheat Sheet"
---

## Partial Derivative

Suppose $f(x, y)$ is a function of two real variables. Then the partial of derivative $f$ with respect to $x$, denoted by $\frac{\partial f}{\partial x}$ is defined as

$$\frac{\partial f}{\partial x} = \lim_{h \rightarrow 0} \frac{f(x + h, y) - f(x, y)}{h}$$

In LaTex we can use the `\partial` command for the "curly d".

### Chain Rule

Let $f(x, y)$ be a function of two variables that are themselves functions of $t$, $x(t)$ and $y(t)$. Then the derivative of $f$ with respect to $t$ is:

$$\frac{df}{dt} = \frac{\partial f}{\partial x} \cdot \frac{dx}{dt} + \frac{\partial f}{\partial y} \cdot \frac{dy}{dt}$$

The symbol "$\cdot$" represents the multiplication of the two functions. The arguments of these functions are omitted but can be inferred.

Example:

* $f(x, y) = x^2 + y^2 + xy$
* $x(t) = t^2$
* $y(t) = t^3$

Then

* $\frac{\partial f}{\partial x}(x, y) = 2x + y$
* $\frac{dx}{dt}(t) = 2t$
* $\frac{\partial f}{\partial y}(x, y) = 2y + x$
* $\frac{dy}{dt} = 3t^2$

In this case we have:

$$\frac{df}{dt}(t) = (2x(t) + y(t)) 2t + (2y(t) + x(t)) 3t^2$$

Here we explicitly included the arguments of the functions.
