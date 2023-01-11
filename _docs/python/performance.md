---
layout: doc
title: "Python Performance"
---

{% include blog_vars.html %}

# Start Up Times

## Profiling

### Detecting Expensive Modules

We can determine which module is taking the most time to import by running a binary with the `-X importtime flag`:

{% highlight text %}
pip install tuna
python -X importtime tdigest/tdigest.py 2> /tmp/import.log
tuna /tmp/import.log
{% endhighlight %}

Source: [Revisiting Python: Modules]({{blog}}/_posts/2021-12-24-revisiting-python-modules.md).
