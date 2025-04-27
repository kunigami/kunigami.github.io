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
set(HOME "/home/kunigami")

add_executable(binary main.cpp)
target_include_directories(binary PRIVATE ${HOME}/folly/include)

target_link_libraries(
        binary PRIVATE
        ${HOME}/folly/lib/libfolly.a
        ${HOME}/github/glog/glog-0.5.0/build/libglog.so
        /lib/x86_64-linux-gnu/libgflags.so
        /usr/lib/x86_64-linux-gnu/libdouble-conversion.so
        ${HOME}/github/fmt/build/libfmt.a
        /usr/lib/x86_64-linux-gnu/libboost_context.so
        /usr/lib/x86_64-linux-gnu/libevent.so
        /usr/lib/x86_64-linux-gnu/libunwind.so.8
)
{% endhighlight %}

Notes (as of 2025/04/27):
* Ubuntu's latest `fmt` is version 9, so  I had to install from source (see *Fmt*)
* Ubuntu's latest `glog` is version 0.6, which wasn't incompatible with Folly's usage, which uses 0.5. To install:

{% highlight text %}
# First, go to the desired directory (e.g. ~/github)

$ wget https://github.com/google/glog/archive/v0.5.0.tar.gz
$ tar -xzf v0.5.0.tar.gz
$ cd glog-0.5.0/
$ mkdir build
$ cmake ..
$ make
# Available in ~/github/glog-0.5.0/build/libglog.so
{% endhighlight %}


# Fmt

{% highlight text %}
# First, go to the desired directory, (e.g. ~/github).

$ git clone //https://github.com/fmtlib/fmt.git
$ mkdir build
$ cd build
$ cmake ..
$ make
# Available in ~/github/fmt/build/libfmt.so
{% endhighlight %}
