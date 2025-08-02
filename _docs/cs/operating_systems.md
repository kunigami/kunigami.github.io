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

# Components

## Kernel

### Interrupts

Signals that tell the CPU to pause and execute some function, the *interrupt handler*. Examples: key press, network packet arrival, system calls and exceptions.

Interrupts are asynchronous: the execution of the handlers are queued by the kernel.

### System Call

System call is a special type of interrupt that a process can call to run kernel space functions, e.g. `read()`, `socket()`, `mmap()`.

## Process

## Thread

* Threads share the same memory address and file descriptors but have dedicated stack and registers.

# Memory

## Paging

When the system runs out of physical memory and it has to spill chunks of memory (page) to disk.

## Swapping

In Linux this is the same as paging.

# Concurrency

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
