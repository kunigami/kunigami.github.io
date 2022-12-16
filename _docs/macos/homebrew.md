---
layout: doc
title: "Homebrew"
---

# Index
{:.no_toc}

1. TOC
{:toc}

## Setup

This setup install homebrew in user directories, which is safer to mess with. The downside is larger install times since it has to build packages from source.

1) Git-clone it into the `~/homebrew` directory:

{% highlight text %}
git clone https://github.com/Homebrew/brew ~/homebrew
{% endhighlight %}

2) Make sure to include the `bin/` and `sbin/` directories in `$PATH`, by adding this to `~/.bash_profile`:

{% highlight text %}
PATH="$HOME/homebrew/bin:$HOME/homebrew/sbin:$PATH"'
{% endhighlight %}

3) Update

{% highlight text %}
brew update
{% endhighlight %}

## Commands
