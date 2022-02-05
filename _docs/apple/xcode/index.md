---
layout: doc
title: "Xcode 13.1"
---

{% include blog_vars.html %}


## Build

When running through the UI, the output directory when building in Xcode is:?

{% highlight bash %}
~/Library/Developer/Xcode/DerivedData/<Project>-<hash>/Build/Products/Debug/
{% endhighlight %}

**Tip.** Create a symbolic link as a shotcut:

{% highlight bash %}
ln -s /Users/kunigami/Library/Developer/Xcode/DerivedData/ xcode-out
{% endhighlight %}
