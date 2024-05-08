---
layout: doc
title: "Venv Cheatsheet"
---

{% include blog_vars.html %}

## Start venv

{% highlight text %}
python3 -m venv /home/kunigami/python/
source /home/kunigami/python/bin/activate
{% endhighlight %}

Note that `/home/kunigami/python/bin/activate` is a bash script.

## Exit venv

{% highlight text %}
deactivate
{% endhighlight %}
