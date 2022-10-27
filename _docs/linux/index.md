---
layout: doc
title: "Linux Cheatsheet"
---

{% include blog_vars.html %}

# How-To

## Save stdout/stderr to file while still displaying in terminal

{% highlight text %}
command 2>&1 | tee file.txt
{% endhighlight %}
