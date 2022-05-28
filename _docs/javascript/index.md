---
layout: doc
title: "Python Cheatsheet"
---

{% include blog_vars.html %}

Syntax for common tasks I run into often. Tested in Google Chrome 102 (circa 2022-05).

# Index
{:.no_toc}

1. TOC
{:toc}


# Data Structures

## Array

### Create

Fixed size:

{% highlight js %}
const arr = new Array(10);
{% endhighlight %}

### Add

keywords: insert, append

Append to the end:

{% highlight js %}
arr.push(e)
{% endhighlight %}

### For loop

{% highlight js %}
let s = 0
arr.forEach(e => s += e);
{% endhighlight %}

### Size

keywords: length

{% highlight js %}
arr.length
{% endhighlight %}

## Set

### Create

Empty:

{% highlight js %}
const s = new Set();
{% endhighlight %}

From Array:

{% highlight js %}
const s = new Set([1, 2]);
{% endhighlight %}

### Add

keywords: insert, append

{% highlight js %}
s.add(x);
{% endhighlight %}

### For loop

{% highlight js %}
for (let e of s) {
  print(e);
}
{% endhighlight %}

### Membership check

{% highlight js %}
s.has(x);
{% endhighlight %}

### Remove

{% highlight js %}
s.delete(x);
{% endhighlight %}

### Get first element

$O(1)$:

{% highlight js %}
const first = s.values().next().value;
{% endhighlight %}
