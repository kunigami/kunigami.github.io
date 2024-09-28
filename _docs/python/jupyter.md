---
layout: doc
title: "Jupyter Cheatsheet"
---

{% include blog_vars.html %}

## Start in a clean environment

Create a venv following [this notes](https://www.kuniga.me/docs/python/venv.html):

{% highlight text %}
mkdir ~/my_env
python3 -m venv ~/my_env
source ~/my_env/activate
{% endhighlight %}

Install the kernel module

{% highlight text %}
pip install ipython ipykernel
{% endhighlight %}

Create a kernel for the venv:

{% highlight text %}
ipython kernel install --user --name=my_env
{% endhighlight %}

Start Jupyter:

{% highlight text %}
jupyter notebook
{% endhighlight %}

In the UI, make sure to select the `my_env` kernel. You can install modules via `pip` now, without messing with other environments.
