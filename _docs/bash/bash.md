---
layout: doc
title: "Bash Cheatsheet"
---

I really dislike working with Bash. I never get familiar with the syntax and there are a lot of idiosyncrasies and hard-to-read params. I tend to prefer using the `subprocess` module in Python. That said, sometimes it is incovenient to not use Bash. Assumes Bash v 3.2.

Bash can be very strict with the syntax (especially around new lines, spaces and quotes) which lead to frustrating syntax errors. I'll add notes when syntax is important

# Conditionals

Syntax:

{% highlight bash %}
if [ ... ]
then
    ...
fi
{% endhighlight %}

NOTES:

* It's is necessary to have empty spaces after `[` and before `]`!
* `then` **cannot** be on the same line as `if`

## Check if string is empty

{% highlight bash %}
x=""
if [ -z $x ]
then
   echo "empty"
else
    echo "not-empty"
fi
{% endhighlight %}
