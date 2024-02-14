---
layout: doc
title: "C++ Concurrency Cheatsheet"
---

# Index
{:.no_toc}

- TOC
{:toc}

# API

## Thread

### Includes

{% highlight c++ %}
#include <thread>
{% endhighlight %}

### Creation

Thread from lambda

{% highlight c++ %}
std::thread my_thread([](){
  std::cout << "thread function\n";
});
{% endhighlight %}

Assignment version

{% highlight c++ %}
my_thread_ = std::thread([](){
  std::cout << "thread function\n";
});
{% endhighlight %}

### Termination

Block until thread ends

{% highlight c++ %}
my_thread.join();
{% endhighlight %}

## Locks

RAII lock

{% highlight c++ %}
std::mutex m;
{
  std::lock_guard<std::mutex> lock(m);
  // locked region
}
{% endhighlight %}

## Condition variables

Have thread 2 block until thread 1 signals it.

{% highlight c++ %}
std::condition_variable cv;

// thread 1
ready = true;
cv.notify_one();

// thread 2
std::unique_lock lk(m);
cv.wait(lk, []{return ready;});

{% endhighlight %}

# Theory

## The "synchronizes-with" relationship

This says that if we have write to an atomic variable and then read it, all the operations that happened before the write will have taken effect once we read from the atomic variable, including to non-atomic variables.

Example:

{% highlight c++ %}

int n;
std::atomic<int> barrier(0);

// executed by thread 1
void f() {
    n = 10;
    // Write to atomic
    barrier.store(1, std::memory_order_release);
}

// executed by thread 2
void g() {
    int ready = barrier.load(std::memory_order_acquire);
    if (ready == 1) {
        // At this point we're guaranteed to "see"
        // n == 10
    }
}
{% endhighlight %}
