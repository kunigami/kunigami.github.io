---
layout: post
title: "Tokio: Synchronous Execution"
tags: [rust]
vanity: "2026-11-17-tokio-sched"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/tokio.svg" alt="Tokio logo" />
</figure>

This is the third post in our series about Tokio, a popular Rust library for writing and running asynchronous code. The other posts in the series are:

* [Overview](https://www.kuniga.me/blog/2026/10/06/rust-tokio.html) - covering Tokio's primitives and how Rust's await works
* [Scheduling](https://www.kuniga.me/blog/2026/11/17/tokio-sched.html) - how asynchronous work is scheduled in Tokio

In this post, we'll talk about synchronous execution. While Tokio is meant for asynchronous code such as I/O, we occasionally need to interleave it with blocking, CPU-bound work, which can lead to subpar performance if scheduled as asynchronous work.

<!--more-->

## Motivation

Let's consider a contrived async function where we read a file asynchronously and then compute its checksum. 

{% highlight rust %}
async fn f() -> Result<(), Box<dyn std::error::Error>> {
    let data = tokio::fs::read("large-file.bin").await?;

    let mut checksum = 0_u64;
    for byte in data {
        checksum = checksum.wrapping_mul(31).wrapping_add(byte as u64);
    }
    checksum

    println!("Checksum: {checksum}");
    Ok(())
}
{% endhighlight %}

Recall how the [worker executes](https://www.kuniga.me/blog/2026/11/17/tokio-sched.html) the future of this function: 

{% highlight rust %}
loop {
    if should_poll_events() {
        poll_for_events(); // Check I/O readiness and expired timers.
    }

    let task = get_task();
    match task {
        Some(task) => {
            poll_task(task);
        }
        None => {
            park(); // Sleep so it doesn't waste CPU 
        }
    }

    if shutting_down() {
        break;
    }
}
{% endhighlight %}

The code that computes the checksum will cause the worker to spend a lot of time at `poll_task()`, preventing it from running `poll_for_events()` in a timely manner. This is not a problem if there are only a few CPU-bound operations because there will be other workers to run the I/O readiness checks, but if the workers are saturated with CPU-bound work, I/O work results will be delayed.

The solution is for synchronous work to be executed in a separate thread pool, via the `spawn_blocking()` API. It takes in a non-asynchronous function but returns an asynchronous one that can be awaited. Our example could be changed to:

{% highlight rust %}
async fn f() -> Result<(), Box<dyn std::error::Error>> {
    let data = tokio::fs::read("large-file.bin").await?;

    let checksum = tokio::task::spawn_blocking(move || {
        let mut hash = 0_u64;
        for byte in data {
            hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
        }
        hash
    })
    .await?;

    println!("Checksum: {checksum}");
    Ok(())
}
{% endhighlight %}

What happens is that the checksum computation will be run in a separate thread pool and behave as if it were I/O work: from the perspective of the worker, this function is executing elsewhere, so the current function will suspend and, when it's ready, it will be rescheduled, so that the `println!()` is executed on the worker.

Let's now delve into how this is carried out. 

## Implementation 

Like its async counterpart, Tokio also maintains a thread pool for synchronous work which reads from a queue. By default, this pool starts empty, but when `spawn_blocking()` adds work to the queue, it will check for any thread not busy executing work. If it finds none, it will add a thread to the pool, up to 512 threads (configurable via `max_blocking_threads()`).

When the queue is empty, the worker blocks on a condition variable so as not to waste CPU cycles, but it times out at 10s (configurable via `thread_keep_alive()`), after which it gets reaped.

`spawn_blocking()` returns a future, which captures the waker handle passed by the async worker (see *Suspending and Resuming* in [[1](https://www.kuniga.me/blog/2026/11/17/tokio-sched.html)]), and when the sync worker is done, it wakes up the task.

## Conclusion

In our previous [post](https://www.kuniga.me/blog/2026/11/17/tokio-sched.html), we noted that

> Also note that if async functions do heavy CPU work, they will block the worker at `poll_task()`, which can then impact how often it can call `poll_for_events()`.

We didn't propose a solution to that issue because at that time I hadn't heard of `spawn_blocking()`! This is an interesting problem with Q&A learning with AI: we don't know what we don't know, so we need to exercise our curiosity and our critical thinking.

## References

* [[1]](https://www.kuniga.me/blog/2026/11/17/tokio-sched.html) - NP-Incompleteness: Tokio: Scheduling