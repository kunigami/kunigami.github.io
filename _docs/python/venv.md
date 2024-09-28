---
layout: doc
title: "Venv Cheatsheet"
---

{% include blog_vars.html %}

## Setup vend

This step is only needed the first time you create the virtual environment.

{% highlight text %}
python3 -m venv /home/kunigami/python/
{% endhighlight %}

## Start venv

{% highlight text %}
source /home/kunigami/python/bin/activate
{% endhighlight %}

Note that `/home/kunigami/python/bin/activate` is a bash script.

## Exit venv

{% highlight text %}
deactivate
{% endhighlight %}
