---
layout: post
title: "Tokio: Scheduling"
tags: [rust]
vanity: "2026-11-17-tokio-sched"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/tokio.svg" alt="Tokio logo" />
</figure>

This is the second post in our series about Tokio, a popular Rust library for writing and running asynchronous code. In the [first post](https://www.kuniga.me/blog/2026/10/06/rust-tokio.html) we learned a bit about Tokio's primitives and how Rust's await works.

In this post we'll dive into Tokio's scheduler, in particular the multi-threaded model.

<!--more-->

We'll start by covering concepts and components of the system, then cover specific flows in more detail.

## Components

### Runtime

We talked about the Tokio runtime in the [previous post](https://www.kuniga.me/blog/2026/10/06/rust-tokio.html). In that post we focused on the `block_on()` function which is essentially a loop that runs the future corresponding to an async function. It runs this loop until it finishes, so there's no scheduling involved.

However, we only typically run it at the root of the application. For the application to benefit from asynchronous operations, somewhere we must call `spawn()` or equivalent APIs. In this case the Tokio runtime performs scheduling. We'll focus on this flow in this post.

Unless stated otherwise, everything we'll discuss in this post is within the scope of a single runtime, rather than the process. For most cases a process runs a single runtime so the distinction doesn't matter, but it's possible to have multiple runtimes per process.

### Thread Pool

The runtime keeps a pool of threads on which the runtime will schedule tasks. By default, the number of threads corresponds to the number of (logical) CPU cores on the host, but it's possible to override this number, which stays fixed throughout the application. Each thread is called a **worker**.

### Futures vs Tasks

As we saw in the overview post, a *future* is basically a class the compiler generates (implementing the `Future` trait) with a `poll()` method that implements a state machine. A task, on the other hand, is a Tokio implementation that references a future and has extra metadata. The task is what gets scheduled.

When we call `tokio::spawn()`, we pass a future, and Tokio creates the task and returns a `JoinHandle`.

{% highlight rust %}
async fn fetch() -> String { /* ... */ }

let future = fetch();                // Creates a future; no execution yet
let handle = tokio::spawn(future);   // Creates a scheduled task
{% endhighlight %}

A task can be in one of several states that are relevant to this post: **runnable**, **running**, **suspended** or **completed**.

### Queues

There are two levels of queues. One for each worker and a global one. The worker queue has a hardcoded capacity of 256 items. The reason to have local queues is to reduce contention on a global queue.

### The Worker Loop

The worker is what runs on a thread. Conceptually it's a loop that reads tasks from queues and executes them:

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

We'll cover some of the worker's flows in more detail later. A worker can be in one of these states: **searching**, **running** or **parked**. It's running when calling `poll_task()`, parked after calling `park()`. Otherwise it's searching (while inside the loop).

## Flows

Let's now cover specific flows.

### Enqueueing

Let's understand where tasks and futures get scheduled to run in different scenarios.

**No queueing.** First, consider the case when we do:

{% highlight rust %}
async fn foo() {
    let handle = tokio::spawn(bar());
    handle.await.unwrap();
}

fn main() {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(foo());
}
{% endhighlight %}

The future corresponding to `foo` will run on the main thread, so it doesn't go through a worker.

**Global queueing.** Inside our async function `foo()` it calls `tokio::spawn()` and that's when the task gets added to the global queue. Workers can race to get tasks from this queue, so we need synchronization, which can add contention.

**Local queueing.** As we mentioned before, to avoid contention workers have their local queues. Tasks are added to this queue when a future running on a worker calls `tokio::spawn()`. Workers prefer reading tasks from their own queue, but to avoid starving tasks in the global queue, they occasionally read tasks from the global one.

**LIFO slot.** There's a small optimization on the local queue that aims to help with temporal locality. Suppose task `A` is being run, and it causes a task `B` to be scheduled. It's not added to the local queue yet; instead, it goes to a special place called the LIFO slot. The worker will check this slot first before checking the queue. However, if another task `C` gets scheduled before `B` gets picked, then `B` is placed at the end of the queue and `C` goes into the LIFO slot.

Another way to describe this process is that we add an item to the head of the queue first instead of the tail, but if a second item gets added there, the first one gets kicked back to the end of the queue. The idea of this optimization is that when a task gets scheduled, there's a good chance we'll run it ASAP, but if that's not the case, we go with the usual scheduling.

Going forward, when we talk about adding items to a local queue, we'll be implicitly assuming this is happening.

**Work sharing.** If a worker's queue gets full, half of the items in it are moved to the global queue, in hopes that the work can be redistributed across other workers.

If a worker's queue is empty, it can steal work from another. It randomly picks other workers and tries to steal half of the items in that queue. It doesn't consider the queue size, so it's possible a worker is busy with 200 tasks in its queue, but the idle worker helps a random one with only 2 tasks, for example.

As we'll see later, only runnable tasks are enqueued.

### Suspending and Resuming

Consider the following example, where we call `sleep()`, which will suspend the task for 1 second.

{% highlight rust %}
tokio::spawn(async {
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;
});
{% endhighlight %}

Let's assume that `sleep()` has a custom future with a timer object in it, which will implement the sleep. In our previous [post](https://www.kuniga.me/blog/2026/10/06/rust-tokio.html) we provided one possible way to implement a timer.

Let's see what's happening behind the scenes. If this task is running on a worker, recall that the worker will call the future's `poll()` method, passing a `Context` object. One of the fields in this context is a waker, which the future for `sleep()` will copy. The waker has a reference to the task as well. Then the future returns `Pending`, which will cause the worker to process the next task.

In our example in the previous post, the future's `poll()` had this implementation:

{% highlight rust %}
this.timer.poll_read_ready(cx)
{% endhighlight %}

Internally this will pass the waker back to Tokio's I/O. The timer can communicate with Tokio because it's an instance of `AsyncFd`, which is part of the Tokio framework:

{% highlight rust %}
timer: AsyncFd::with_interest(file, Interest::READABLE)
{% endhighlight %}

This will add the file descriptor to Tokio's runtime registry, which is shared across all workers. Note that the task doesn't get added back to the queue. From the worker's perspective, it's done with it. The continuation will be done via the timer that now holds a copy of the waker.

Once the time passes, the file descriptor will be marked as ready. Then, when the worker runs its loop:

{% highlight rust %}
loop {
    if should_poll_events() {
        poll_for_events(); // Check I/O readiness and expired timers.
    }
    // ...
}
{% endhighlight %}

In `poll_for_events()` it uses [epoll()](https://www.kuniga.me/blog/2025/05/16/async-io.html) to find out if any events are ready. If none are ready, it exits (i.e. it doesn't block). In our example it will detect that this file descriptor is available, mark the task as runnable and schedule it on this worker's queue (see *Enqueueing*). Note that this is not necessarily the same worker that was running the task when it was suspended.

Also note that if `async` functions do heavy CPU work, they will block the worker at `poll_task()`, which can then impact how often it can call `poll_for_events()`.


### Parking

Let's cover the case in which the worker finds no task to run, when it calls `park()` in our pseudocode in *The Worker Loop*.

{% highlight rust %}
...
match task {
    Some(task) => {
        poll_task(task);
    }
    None => {
        park(); // <--- here
    }
}
...
{% endhighlight %}

The first worker to park will be assigned to "watch" for I/O events. It basically uses `epoll()` like in `poll_for_events()` but in this case it blocks. This way, this thread will be put to sleep and not waste CPU cycles. To make sure only one thread is listening for I/O events at a time, it's behind a lock. So if there's another running worker that calls `poll_for_events()`, it will fail to acquire the lock and skip.

Similarly if another worker gets idle too and calls `park()`, it will also try to acquire the epoll lock and fail. Instead, it will wait on a condition variable (similar to `condition_variable` in [C++](https://www.kuniga.me/blog/2022/10/25/review-effective-modern-cpp.html)), so it will block until something else wakes it up by notifying the condition variable. There are a few cases when this happens:

* No worker is searching for work and a new task is added to the global queue (via `spawn()`).
* Only one worker is in searching mode and it transitions to running mode.

The first case indicates new work is available but there's no available worker to pick it up, so one should be put back into action. The second case is for when there is additional work but the only worker that was searching is now busy.

Note that there is no central coordinator to notify workers. In the first case the thread on which `spawn()` is called will notify, and in the second the worker transitioning to running mode will notify before doing that. Also note that only one worker is woken up at a time.


## Conclusion

In this post we learned more about how Tokio distributes work between threads. It's a lot simpler than I imagined, because the logic is handled by the workers without a central coordinator.


## Related Posts

Not surprisingly, Tokio reminds me a lot of [Folly coroutines](https://www.kuniga.me/blog/2025/06/18/folly-coroutines.html) + `CPUThreadPoolExecutor` and `IOThreadPoolExecutor` [executors](https://www.kuniga.me/blog/2025/06/07/folly-executors.html). In Folly coroutines though the scheduling policy is decoupled from the coroutine machinery.

The LIFO slot optimization to leverage locality is a similar idea to [CPU Caches]({{blog}}/2020/04/24/cpu-cache.html).
