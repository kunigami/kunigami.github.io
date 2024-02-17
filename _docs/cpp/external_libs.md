---
layout: doc
title: "C++ External Libraries Cheatsheet"
---

# Folly

## Setup

### Download

{% highlight text %}
git clone https://github.com/facebook/folly.git
{% endhighlight %}

### Install dependencies

On MacOS it uses homebrew, no need to use `sudo` (as the README suggests):

{% highlight text %}
cd folly
./build/fbcode_builder/getdeps.py install-system-deps --recursive
{% endhighlight %}

### Build

Create a directory inside your home, `folly/`. Run:

{% highlight text %}
python3 ./build/fbcode_builder/getdeps.py --allow-system-packages build --install-dir /home/me/folly/
{% endhighlight %}

### Setting paths

On MacOS, add to `.bash_profile`:

{% highlight text %}
export CPLUS_INCLUDE_PATH=/home/me/folly/include:$CPLUS_INCLUDE_PATH
export LIBRARY_PATH=/home/me/folly/lib:$LIBRARY_PATH
{% endhighlight %}

### Linking

During compilation:

{% highlight text %}
g++ -std=c++17 main.cpp -lfolly
{% endhighlight %}
