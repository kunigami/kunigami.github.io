---
layout: doc
title: "Numpy Cheatsheet"
---

Assuming numpy is imported as:

{% highlight python %}
import numpy as np
{% endhighlight %}

# Vector (1D)

Un-initialized vector of size N:

{% highlight python %}
np.empty(shape=N)
{% endhighlight %}

Similar versions for vectors filled with 1s: `ones`, ans 0s: `zeros`. Note that `np.empty(shape=N)` is different from `np.empty(shape=(N, 1))`. The former has `x[N]`, while the latter `x[N, 1]`.

To flatten a matrix `m(Nx1)` to a vector `v(N)`:

{% highlight python %}
v = m.reshape(shape=N)
{% endhighlight %}


## Matrix (2D)

### Initialization

Un-initialized matrix of size N x M:

{% highlight python %}
m = np.empty(shape=(N, M))
{% endhighlight %}

Similar versions for vectors filled with 1s: `ones`, and 0s: `zeros`.

Number of rows:

{% highlight python %}
len(m)
{% endhighlight %}

Number of columns:

{% highlight python %}
len(m[0])
{% endhighlight %}

Initialize from nested Python lists:

{% highlight python %}
m = np.array([
    [0.1, 0.3, 0.5],
    [0.9, 0.7, 0.5],
])
{% endhighlight %}

### Transformations

Transpose:

{% highlight python %}
t = m.transpose()
{% endhighlight %}

Einstein Summation:

This article is a great introduction:

https://ajcr.net/Basic-guide-to-einsum/
