---
layout: doc
title: "Operating Systems Cheat Sheet"
---

{% include blog_vars.html %}

Common definitions and terminology in operating systems, in particular those using the Linux kernel.

For sys-admin kind of notes on Linux, see the [Linux Cheat Sheet]({{site.url}}/docs/linux/).


# Index
{:.no_toc}

1. TOC
{:toc}

# Linux Terminology

**Unix** was an OS created at AT&T. Today the term is used to refer to the family of OSes that comply to a specification called SUS (Single UNIX Specification). Examples of Unix (compliant) OSes are: macOS, IBM AIX, Oracle Solaris. Linux is not Unix compliant.

**Unix-like** (aka UN*X) are OSes that behave like Unix (but are not Unix compliant). There's no technical specification for being Unix-like, so the term is subjective. Examples include: FreeBSD and Linux.

**POSIX** (Portable Operating System Interface) is a competing specification for Unix systems. Many of UNIX-compliant OSes are also POSIX-compliant. Notably, Linux is also not POSIX-compliant, but analogously to Unix-like, it's also a POSIX-like OS.

A **Linux distribution** is an OS that uses the Linux kernel. Examples include: Debian, Fedora, Ubuntu. There's no OS called Linux, it refers to the family of Linux distributions or to one unspecified member of that family. Note that FreeBSD is not Linux (i.e. it has its own kernel).

**GNU** (GNU is not Unix) was a Unix-like OS (but not Unix, hence the name) created by Richard Stallman but in practice is not used in its entirety. However, many tools developed for this system were adopted by many Linux distributions, so sometimes these OSes are referred to as GNU/Linux. Examples of tools from GNU are: GCC (GNU Compiler Collection), glibc (GNU C library) and Bash.

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

## Heap

The heap historically meant the data segment of the program, containing global and static variables. These days the heap is more generally synonym with dynamically allocated memory. The data-segment heap can be grown via the syscall `brk()`. Alternativelly, memory allocators such as [jemalloc](https://www.kuniga.me/blog/2025/07/15/jemalloc.html) use anonymous mapped memory, via `mmap()`, to dynamically allocate memory, which is not part of the data-segment heap.

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
