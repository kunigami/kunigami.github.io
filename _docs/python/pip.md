---
layout: doc
title: "Pip Cheatsheet"
---

{% include blog_vars.html %}

# Index
{:.no_toc}

1. TOC
{:toc}

# Setup

## PYTHONPATH

The directory where pip installs packages is based on `PYTHONPATH`, so make sure it's the same one Python reads from (e.g. in Jupyter).

{% highlight text %}
export PYTHONPATH="~/homebrew/lib/python3.9/site-packages/:$PATH"
{% endhighlight %}
