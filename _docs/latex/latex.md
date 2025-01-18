---
layout: doc
title: "LaTeX"
---

Syntax for LaTeX I often use for my writings.

## Set of Equations

**Source:**

{% highlight latex %}
\begin{align}
a &= b \\
c + 1 &= d \\
\end{align}
{% endhighlight %}

**Rendered:**

$$
\begin{align}
a &= b \\
c + 1 &= d \\
\end{align}
$$

## Conditional

**Keywords:** *if*

**Source:**

{% highlight latex %}
\begin{equation}
  a_{ij}=\left\{
  \begin{array}{@{}ll@{}}
    0, & \text{if}\ i=j \\
    1, & \text{otherwise}
  \end{array}\right.
\end{equation}
{% endhighlight %}

Note: the period `.` after `right` is not a typo, it's necessary.

**Rendered:**

$$
\begin{equation}
  a_{ij}=\left\{
  \begin{array}{@{}ll@{}}
    0, & \text{if}\ i=j \\
    1, & \text{otherwise}
  \end{array}\right.
\end{equation}
$$

## Symbols

### Set theory

| Name | Rendered | LaTeX |
| - | - | - |
| Exists | $\exists$ | `$\exists$` |
| Union  | $\cup$ | `$\cup$` or `$\bigcup$` |
| Intersection | $\cap$ | `$\cap$` or `$\bigcap$` |

### Calculus

| Name | Rendered | LaTeX |
| - | - | - |
| Partial derivative | $\partial$ | `$\partial$` |
| Vector             | $\overrightarrow{v}$ | `$\overrightarrow{v}$` |
