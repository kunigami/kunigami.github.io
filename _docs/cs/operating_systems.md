---
layout: doc
title: "Operating Systems Cheat Sheet"
---

{% include blog_vars.html %}

Common definitions and terminology in operating systems.

For Linux specifics, see the [Linux Cheat Sheet]({{site.url}}/docs/linux/).


# Index
{:.no_toc}

1. TOC
{:toc}

## Lock

* Binary State: A lock is either locked or unlocked
* Blocking: If a thread attempts to acquire a lock already held by another thread, it is usually blocked until the lock is released.

## Mutex

It's a type of lock. Stands for **mut**ually **ex**clusive lock.

When a thread tries to acquire a already locked Mutex, the kernel puts that thread to sleep and it will be awakened once the lock is unlocked.

## Semaphore

A semaphore is a more generalized version of a lock because it supports up to `N` threads to "enter" on a exclusive region.

If `N = 1`, it behaves like a lock, albeit an expensive one.

## Spin Lock

It's a type of lock that keeps trying to acquire a lock in a loop. It doesn't require the thread to be context-switched, but consumes CPU cycles.
