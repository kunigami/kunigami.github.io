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
python3 ./build/fbcode_builder/getdeps.py --allow-system-packages build --install-dir /home/me/folly/ --scratch-path /tmp/folly
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

`CMakeLists.txt`:

{% highlight text %}
target_link_libraries(binary PUBLIC /home/kunigami/folly/lib/libfolly.a)
target_link_libraries(binary PUBLIC /home/kunigami/github/glog-0.5.0/buikd/libglog.so)
target_link_libraries(binary PUBLIC /lib/x86_64-linux-gnu/libgflags.so)
target_link_libraries(binary PUBLIC /usr/lib/x86_64-linux-gnu/libdouble-conversion.so)
target_link_libraries(binary PUBLIC home/kunigami/github/fmt/build/libfmt.a)
target_link_libraries(binary PUBLIC /usr/lib/x86_64-linux-gnu/libboost_context.so)
target_link_libraries(binary PUBLIC /usr/lib/x86_64-linux-gnu/libevent.so)
{% endhighlight %}

Notes (as of 2025/04/27):
* Ubuntu's latest `fmt` is version 9, so  I had to install from source (see *Fmt*)
* Ubuntu's latest `glog` is version 0.6, which wasn't incompatible with Folly's usage, which uses 0.5. To install:

{% highlight text %}
wget https://github.com/google/glog/archive/v0.5.0.tar.gz
tar -xzf v0.5.0.tar.gz
cd glog-0.5.0/
mkdir build
cmake ..
make
# libglog.so -> libglog.so.0.5.0 is available in ...glog-0.5.0/build/libglog.so
{% endhighlight %}


# Fmt

Choose an appropriate directory, e.g. `~/github/fmt/build/libfmt.a`.

{% highlight text %}
$ git clone //https://github.com/fmtlib/fmt.git
$ mkdir build
$ cd build
$ cmake ..
$ make
{% endhighlight %}
