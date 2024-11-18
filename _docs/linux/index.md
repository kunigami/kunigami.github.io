---
layout: doc
title: "Linux Cheatsheet"
---

{% include blog_vars.html %}

# Terminology

**Unix** was an OS created at AT&T. Today the term is used to refer to the family of OSes that comply to a specification called SUS (Single UNIX Specification). Examples of Unix (compliant) OSes are: macOS, IBM AIX, Oracle Solaris. Linux is not Unix compliant.

**Unix-like** (aka UN*X) are OSes that behave like Unix (but are not Unix compliant). There's no technical specification for being Unix-like, so the term is subjective. Examples include: FreeBSD and Linux.

**POSIX** (Portable Operating System Interface) is a competing specification for Unix systems. Many of UNIX-compliant OSes are also POSIX-compliant. Notably, Linux is also not POSIX-compliant, but analogously to Unix-like, it's also a POSIX-like OS.

A **Linux distribution** is an OS that uses the Linux kernel. Examples include: Debian, Fedora, Ubuntu. There's no OS called Linux, it refers to the family of Linux distributions or to one unspecified member of that family. Note that FreeBSD is not Linux (i.e. it has its own kernel).

**GNU** (GNU is not Unix) was a Unix-like OS (but not Unix, hence the name) created by Richard Stallman but in practice is not used in its entirety. However, many tools developed for this system were adopted by many Linux distributions, so sometimes these OSes are referred to as GNU/Linux. Examples of tools from GNU are: GCC (GNU Compiler Collection), glibc (GNU C library) and Bash.

# How-To

## Save stdout/stderr to file while still displaying in terminal

{% highlight text %}
command 2>&1 | tee file.txt
{% endhighlight %}

## Find-replace inside a file

{% highlight text %}
sed "s/search/replace/g" filename
{% endhighlight %}

NOTE: the delimiter `/` is customizable, so you can also do:

{% highlight text %}
sed "s@search@replace@g" filename
{% endhighlight %}

Which is very useful if replacing entries like filename.
