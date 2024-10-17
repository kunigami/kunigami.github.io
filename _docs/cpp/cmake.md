---
layout: doc
title: "CMake"
---

# Overview

Cmake is a Makefile generator.

It expects a configuration file named `CMakeLists.txt` at the root directory. We need to create a ephemeral directory (i.e. not to be committed), e.g. `build/`:

{% highlight cmake %}
cd <project root>
mkdir build
cd build
cmake ..
{% endhighlight %}

This will not compile the code, but it does a bunch of checks, such as whether the listed files exist. It will generate the `Makefile` which we can then use to compile the code:

{% highlight cmake %}
make
{% endhighlight %}

# CMakeLists.txt

Template:

{% highlight cmake %}
cmake_minimum_required(VERSION 3.15...3.30)

project(
    PetProject
    VERSION 1.0
    DESCRIPTION "My pet project"
    LANGUAGES CXX
)

add_library(mylib STATIC lib.cpp lib.h)

add_executable(binary main.cpp)
target_link_libraries(binary PUBLIC mylib)
{% endhighlight %}

## Specifying C++ Standard Version

Equivalent to `-std=20` in gcc/llvm.

{% highlight cmake %}
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
{% endhighlight %}

## Adding custom `make` subcommand

`make run`:

{% highlight cmake %}
add_custom_target(run
    COMMAND binary  # i.e. ./binary
    DEPENDS binary  # target name
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "runs binary"
)
{% endhighlight %}

## Variables

Set:

{% highlight cmake %}
set(MY_VAR "value")
{% endhighlight %}

Read:

{% highlight cmake %}
set(MY_VAR "lib")
add_library(mylib STATIC ${MY_VAR}.cpp ${MY_VAR}.h)
{% endhighlight %}

List type:

{% highlight cmake %}
set(MY_VAR_LIST "v1" "v2")
# equivalent to
set(MY_VAR_LIST "v1;v2")
{% endhighlight %}
