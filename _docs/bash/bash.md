---
layout: doc
title: "Bash Cheatsheet"
---

I really dislike working with Bash. I never get familiar with the syntax and there are a lot of idiosyncrasies and hard-to-read params. I tend to prefer using the `subprocess` module in Python. That said, sometimes it is incovenient to not use Bash. Assumes Bash v 3.2.

Bash can be very strict with the syntax (especially around new lines, spaces and quotes) which lead to frustrating syntax errors. I'll add notes when syntax is important.

See [shellcheck](https://github.com/koalaman/shellcheck) for this purpose.

# Assignment

NOTES:

* No space before or after `=`!
* No `$` on the LHS!

Example:

{% highlight bash %}
x=10
{% endhighlight %}


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

To flip the check, use `-n`.

NOTE:

* Argument must be quoted: `"$x"`

{% highlight bash %}
x="hello"
if [ -n "$x" ]
then
   echo "not-empty"
else
    echo "empty"
fi
{% endhighlight %}

## And (&&)

Syntax: `[ <cond1> ] && [ <cond2> ]`

{% highlight bash %}
x=10
if [ $x -gt 1 ] && [ $x -lt 100 ]
then
    echo "correct"
fi
{% endhighlight %}
