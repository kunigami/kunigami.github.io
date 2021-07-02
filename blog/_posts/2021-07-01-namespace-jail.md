---
layout: post
title: "Namespace Jailing"
vanity: "2021-07-01-namespace-jail"
tags: [operating systems, bash, c]
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

In a previous post we investigated a jail system using chroot with the conclusion that it was not a safe implementation. In this post we'll study a safer alternative using Linux namespaces.

<!--more-->


## Linux Namespaces

The idea of Linux namespaces is actually very close to that of a sandbox. We want to create subsystems within a system which are isolated, so if they'retampered with, it cannot affect the hosting sytem.

Linux allows sandboxing different pieces of its system. For example the user namespace consists of a separate set of users and groups.

There are at least 8 different namespaces available, but for the purposes of our simple sandbox, we'll focus on 2: the user and mount namespaces.

## Cloning


## References

* [[1](https://en.wikipedia.org/wiki/Chroot)] Wikipedia - Chroot
