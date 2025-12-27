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

## Find-replace inside a file

{% highlight text %}
sed "s/search/replace/g" filename
{% endhighlight %}

NOTE: the delimiter `/` is customizable, so you can also do:

{% highlight text %}
sed "s@search@replace@g" filename
{% endhighlight %}

Which is very useful if replacing entries like filename.
