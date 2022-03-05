---
layout: doc
title: "Audio in Python Cheatsheet"
---

## Read wav file

{% highlight python %}
import scipy.io.wavfile
[sample_rate, samples] = scipy.io.wavfile.read(filename)
{% endhighlight %}
