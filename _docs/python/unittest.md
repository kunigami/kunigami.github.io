---
layout: doc
title: "Unit Test"
---

# Patching

Example code:

`my/module/program.py`:

{% highlight python %}
from lib import func
{% endhighlight %}

`test.py`:

{% highlight python %}
@patch('my.module.program.func')
{% endhighlight %}
