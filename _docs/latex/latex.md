---
layout: doc
title: "LaTeX"
---

Syntax for LaTeX I often use for my writings.

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

* Exists ($\exists$): `$\exists$`
* Union ($\cup$): `$\cup$` or `$\bigcup$`
* Intersection ($\cap$): `$\cap$` or `$\bigcap$`
