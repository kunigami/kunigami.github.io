---
layout: doc
title: "Matplotlib Cheatsheet"
---

{% include blog_vars.html %}

Syntax for common tasks I run into often. Assumes Python 3.

# Index
{:.no_toc}

1. TOC
{:toc}

# Import

{% highlight python %}
import matplotlib.pyplot as plt
{% endhighlight %}

# Templates

## Single plot

{% highlight python %}
fig, ax =  plt.subplots(figsize=(18, 5))
{% endhighlight %}

## Multiple Plots

One row, two columns:

{% highlight python %}
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(18, 5))
{% endhighlight %}

# Chart Types

## Line

{% highlight python %}
ax.plot(xs, ys)
{% endhighlight %}

## Vertical Line

{% highlight python %}
ax.plot(x, color='red')
{% endhighlight %}

# Properties

## Dimensions

Do on creation, via `figsize`:

{% highlight python %}
fig, ax =  plt.subplots(figsize=(18, 5))
{% endhighlight %}

Width 18 and height 5 are assumed to be inches.

## Title

{% highlight python %}
ax.set_title('My title')
{% endhighlight %}
