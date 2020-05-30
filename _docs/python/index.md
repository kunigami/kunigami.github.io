---
layout: doc
title: "Python Cheatsheet"
---

Syntax for common tasks I run into often. Assumes Python 3.

## Collections

### Map over list

{% highlight python %}
xs = [1, 2, 3]
ys = [x * x for x in xs]
{% endhighlight %}

### Sort dictionary

Dictionaries are unordered sets, so you likely want to work with a list after sorting.

{% highlight python %}
d = {'a': 3, 'b': 2}
xs = sorted(d.items(), key=lambda x: x[1])
{% endhighlight %}

## Files

### Read file

{% highlight python %}
with open(filename, "r") as file:
    print(file.readLines())
{% endhighlight %}
