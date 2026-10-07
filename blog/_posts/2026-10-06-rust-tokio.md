---
layout: post
title: "Tokio: Overview"
tags: [rust]
vanity: "2026-09-26-tokio"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/tokio.svg" alt="Tokio logo" />
</figure>

Tokio is a Rust library for writing and running asynchronous code. It was started by Carl Lerche, Alex Crichton, and Aaron Turon in 2017, interestingly even before Rust had the `async` / `await` syntax.

I plan to do a series of posts on Tokio. In the first installment, we provide an overview of Tokio, focusing on the user-facing semantics, covering enough detail so that it doesn't feel like magic.

<!--more-->

## Basic Example: Sleep

Let's start with a very simple example:

{% highlight rust %}
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;
    println!("done");
}
{% endhighlight %}

The code is simple: we first call `tokio::time::sleep()` which sleeps for 1 second and then we print "done". 

### Tokio Macro

Now let's dissect this code. First we resolve the macro `#[tokio::main]`. Conceptually it's equivalent to:

{% highlight rust %}
fn main() {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    runtime.block_on(async {
        tokio::time::sleep(std::time::Duration::from_secs(1)).await;
        println!("done");
    });
}
{% endhighlight %}


### Futures

A *future* is what an `async` function is transformed into by the compiler. We can think of it as a state machine, for example:

{% highlight rust %}
enum MainFuture {
    Start,
    Sleeping {
        sleep: tokio::time::Sleep,
    },
    Done,
}
{% endhighlight %}

The compiler also makes this enum class implement the `Future` trait, which has the method `poll()`, which essentially executes the state machine:

{% highlight rust %}
impl Future for MainFuture {
    type Output = ();

    fn poll(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<()> {
        loop {
            match &mut *self {
                MainFuture::Start => {
                    let sleep =
                        tokio::time::sleep(Duration::from_secs(1));

                    *self = MainFuture::Sleeping { sleep };
                }

                MainFuture::Sleeping { sleep } => {
                    match Pin::new(sleep).poll(cx) {
                        Poll::Pending => {
                            return Poll::Pending;
                        }

                        Poll::Ready(()) => {
                            println!("done");
                            *self = MainFuture::Done;
                            return Poll::Ready(());
                        }
                    }
                }

                MainFuture::Done => {
                    panic!("polled after completion");
                }
            }
        }
    }
}
{% endhighlight %}

It's pretty verbose but we can think of `MainFuture` being in one of the states: `Start`, `Sleeping`, `Done`. When we call `poll()` it runs in a loop, potentially performing state transitions until it returns either `Poll::Pending` or `Poll::Ready`.

Note that `sleep` is a future too, and `MainFuture::poll()` calls `sleep::poll()` inside its own loop. We can imagine that sleep's `poll()` will return `Poll::Pending` until the timeout has happened.

### Runtime

Now we focus on what happens when `runtime.block_on()` is called. We can conceptually think of it as:

{% highlight rust %}
fn block_on<F: Future>(future: F) -> F::Output {
    loop {
        match future.poll(...) {
            Poll::Ready(result) => return result,
            Poll::Pending => let_runtime_do_work(),
        }
    }
}
{% endhighlight %}

Considering the transformed code we have:

{% highlight rust %}
fn main() {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    let future = MainFuture::Start;
    runtime.block_on(future);
}
{% endhighlight %}

So it's basically an outer loop that runs until the future returns `Poll::Ready`. Note the function `let_runtime_do_work()` when we run into `Poll::Pending`: Tokio's runtime might do some other work in the meantime, for example schedule other tasks.

## Suspension and State

In non-async code, when we have a function call like:

{% highlight rust %}
fn foo() {
    let x = 10;
    bar();
    println!("{x}");
}
{% endhighlight %}

We typically create `x` in the stack frame of function `foo`, so that when we call `bar()` and pop it from the call stack, `x` is "restored". The same idea applies when we suspend the current coroutine:

{% highlight rust %}
async fn foo() {
    let x = 10;
    bar().await;
    println!("{x}");
}
{% endhighlight %}

Except that now the compiler is generating an explicit future type, so we can actually make `x` part of that future struct. Conceptually:

{% highlight rust %}
enum FooFuture {
    Start,
    WaitingForBar {
        x: i32,
        bar_future: BarFuture,
    },
    Done,
}
{% endhighlight %}

And the `poll()` method is roughly:

{% highlight rust %}
impl Future for FooFuture {
    type Output = ();

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<()> {
        loop {
            match self.state {
                Start => {
                    let x = 10;
                    let bar_future = bar();

                    // (1)
                    self.state = WaitingForBar {
                        x,
                        bar_future,
                    };
                }

                WaitingForBar { x, bar_future } => {
                    match bar_future.poll(cx) {
                        Poll::Pending => {
                            return Poll::Pending;
                        }

                        Poll::Ready(()) => {
                            // (2)
                            println!("{x}");
                            self.state = Done;
                            return Poll::Ready(());
                        }
                    }
                }

                Done => panic!("polled after completion"),
            }
        }
    }
}
{% endhighlight %}

So here we can see `x` is part of the future, so when it's moved around, the local state of the async function is preserved at `(1)` and available once it's done with `bar()` at `(2)`.

## Multiple Tasks

In the first example we considered, there was a single task, even though we had the async lambda call another async function recursively:

{% highlight rust %}
async {
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;
    println!("done");
}
{% endhighlight %}

This is because `async function = future != task`. The key point is that the future of this root call was driving the execution of the sleep future, but this was transparent to the Tokio runtime. But if we look at what `let_runtime_do_work()` does (conceptually) we have:

{% highlight rust %}
fn let_runtime_do_work() {
    loop {
        // Run work that can make progress now.
        while let Some(task) = next_runnable_task() {
            task.poll();
        }

        // Nothing can currently make progress.
        // Wait for I/O, timers, or another wakeup.
        wait_for_event();

        // Processing the event may make tasks runnable,
        // including the root future.
        if root_is_runnable() {
            return;
        }
    }
}
{% endhighlight %}

So here we also have a loop that checks for `next_runnable_task()`, indicating we can have multiple in-flight tasks at a time. We can create one via `tokio::spawn()`, so if we had:

{% highlight rust %}
runtime.block_on(async {
    tokio::spawn(async {
        do_something_long().await;
        println!("spawned done");
    });

    println!("main done");
});
{% endhighlight %}

In this case the task created via `tokio::spawn()` will run independently from the top-level task. In fact the top-level task will likely finish before the spawned one. This is fine in terms of lifetime because ownership moves to the Tokio runtime when we do `tokio::spawn(future)`.

If we do want the top-level task to "await" the spawned one, we can do:

{% highlight rust %}
runtime.block_on(async {
    let handle = tokio::spawn(async {
        do_something_long().await;
        println!("spawned done");
    });

    println!("main done");

    handle.await.unwrap();
});
{% endhighlight %}

We'll still have two independent tasks running, but now we have a new future `handle` which the top-level task will poll until the corresponding task is finished.

We can make an analogy with threads:

{% highlight rust %}
let handle = std::thread::spawn(|| {
    do_something_long();
    println!("spawned done");
});

println!("main done");

handle.join().unwrap();
{% endhighlight %}

One simplified way to think about the differences is that for threads it's the OS kernel that schedules them while for async it's the Tokio runtime doing the scheduling in user space. 


## Parallelism vs. Concurrency

As we've seen above, using `tokio::spawn` can give us parallelism, much like threads. We can also achieve *concurrency* (more like Python asyncio). For example:

{% highlight rust %}
async fn foo() {
    // ...
}

async fn bar() {
    // ...
}

tokio::join!(foo(), bar());
{% endhighlight %}

Here the macro `join!` effectively polls both `foo()` and `bar()` until both are ready. It implements logic similar to what the compiler generates for `Future::poll()`. Note that no extra task is created: this happens within the current task's future.

I think a different syntax would make it a little more clear, say if we had a special async function called `join`:

{% highlight rust %}
let f = join(foo(), bar());
f.await;
{% endhighlight %}

This form looks a lot more similar to the first example we covered. 


## Multiple Runtimes

As we've seen, the macro expands to something containing this line of code:

{% highlight rust %}
let runtime = tokio::runtime::Runtime::new().unwrap();
{% endhighlight %}

Nothing prevents us from using multiple runtimes, but it's uncommon, like how in Python it's uncommon to use multiple event loops. We also need to be aware of sandwiching non-async code between async ones. For example:

{% highlight rust %}
fn synchronous_library_function() {
    // (1)
    let runtime = Runtime::new().unwrap();

    runtime.block_on(async {
        fetch_data().await;
    });
}

async fn fetch_data() {
    // async I/O...
}

#[tokio::main] 
async fn main() {
    synchronous_library_function(); 
}
{% endhighlight %}

This will panic because you can't construct a runtime on a running runtime (implicit in the `tokio::main` macro).

## Custom Future

We typically work with async code but it's mostly plumbing work. We do `await`, `join`, but at the end of these calls there must be some code that can actually do non-CPU work and work with Tokio to not waste CPU cycles in the meantime.

I find it very useful to study such an example to get a better sense of what it is. Let's write our own version of a sleep that doesn't block the thread. 

We'll use Linux's [timerfd](https://man7.org/linux/man-pages/man2/timerfd_create.2.html) which creates a file descriptor that is not readable until a specific amount of time has elapsed. The idea is that Tokio will use [epoll](https://www.kuniga.me/blog/2025/05/16/async-io.html) to be notified when the file descriptor is ready.

We create a `File` object using the API `timerfd_create()` and associate a TTL with it via `timerfd_settime()`. This is mostly API boilerplate implemented via the function `create_timerfd` in the details below.

<details>

{% highlight rust %}
fn create_timerfd(duration: Duration) -> io::Result<File> {
    // SAFETY: This syscall takes only integer flags and returns a fresh fd.
    let fd = unsafe {
        libc::timerfd_create(libc::CLOCK_MONOTONIC, libc::TFD_NONBLOCK | libc::TFD_CLOEXEC)
    };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: fd is valid and newly created; File becomes its sole owner.
    let file = unsafe { File::from_raw_fd(fd) };
    let spec = create_spec(duration)?;
    // SAFETY: file owns a live timerfd, spec is initialized, and a null
    // old_value pointer requests that the previous timer setting be ignored.
    if unsafe { libc::timerfd_settime(file.as_raw_fd(), 0, &spec, std::ptr::null_mut()) }
        < 0
    {
        return Err(io::Error::last_os_error());
    }
    Ok(file)
}
{% endhighlight %}

</details>

Tokio has a handler for file descriptors which we can initialize and specify which event we're interested in: 

{% highlight rust %}
AsyncFd::with_interest(file, Interest::READABLE)
{% endhighlight %}

In the example above, we're saying the event of interest is when the file becomes readable. This creates an object we'll store but importantly it has a side effect: it registers this source with Tokio's async I/O driver, so it can use epoll to monitor it. 

We keep the returned object in our own future structure:

{% highlight rust %}
pub struct TimerFuture {
    timer: AsyncFd<File>,
    completed: bool,
}
{% endhighlight %}

We need an explicit `completed` flag because when we read from the timerfd file that is `READABLE`, it goes back to unreadable. In our case, because this is a single-use timer, we don't want it to go back to the previous state. 

We can create `TimerFuture` from a duration as:

{% highlight rust %}
impl TimerFuture {
    pub fn new(duration: Duration) -> io::Result<Self> {
        let file = create_timerfd(duration)?;

        Ok(Self {
            timer: AsyncFd::with_interest(file, Interest::READABLE)?,
            completed: duration.is_zero(),
        })
    }
}
{% endhighlight %}

And implement the `Future` trait for it, which requires the `poll()` function:

{% highlight rust %}
impl Future for TimerFuture {
    type Output = io::Result<()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();

        // (1)
        if this.completed {
            return Poll::Ready(Ok(()));
        }

        // (2)
        let guard = match this.timer.poll_read_ready(cx) {
            Poll::Pending => return Poll::Pending,
            Poll::Ready(Err(error)) => return Poll::Ready(Err(error)),
            Poll::Ready(Ok(guard)) => guard,
        };

        // (3)
        let mut buffer = [0_u8; 8];
        let result = match guard.get_inner().read(&mut buffer) {
            Ok(8) => Ok(()),
            Ok(_) => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "incomplete timerfd expiration count",
            )),
            Err(error) => Err(error),
        };
        this.completed = true;
        Poll::Ready(result)
    }
}
{% endhighlight %}

Since our timer is single-use, block `(1)` checks if the future is completed and returns `Ready()` if so. 

The block `(2)` shortcuts the polling if it gets `Poll::Pending` and `Poll::Ready` with errors, but `cx` contains an indirect reference to the task that is polling this future and it will basically tell the Tokio runtime to poll the task once its status changes.

So, despite the name, `poll()` is only called two times if this is the only future the task is polling, because once it receives `Poll::Pending` it will not poll again blindly. The picture changes if the task polls multiple futures at the same time via `join`, for example, since each time a future wakes up the task it will poll the other futures too. 

Finally in `(3)` we read the file's content into an 8-byte buffer. It should contain the number of timer expirations since the file was last read. This tells us that a timerfd could in theory be used for repeated timers, but in our case we only want to use it once, hence we do `this.completed = true`. 

We can then use it as a normal "async" function:

{% highlight rust %}
async fn wait_for_timer() -> std::io::Result<()> {
    use std::time::Duration;
    use timer_future::TimerFuture;

    TimerFuture::new(Duration::from_millis(250))?.await
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    wait_for_timer().await
}
{% endhighlight %}

## Conclusion

In this post we learned a bit about how Tokio operates at a high level, focusing mostly on semantics rather than implementation details. We learned about futures and tasks, how futures implement the `poll()` interface and what the `#[tokio::main]` macro does. I got a much better mental model of what's going on.

We covered two pitfalls: assuming `join!` will lead to parallelism and sandwiching non-async code between async ones.

We also wrote a new future type from scratch to get a better understanding of what happens at the "leaves" of the async call graph.

## Related posts

I have now studied async/coroutines in four languages: [JavaScript](https://www.kuniga.me/blog/2019/07/01/async-functions-in-javascript.html), [Python](https://www.kuniga.me/blog/2020/02/02/python-coroutines.html), [C++](https://www.kuniga.me/blog/2025/06/18/folly-coroutines.html) and now Rust. Each language implements coroutines and async functions in different ways and with different semantics, so I never feel I can immediately understand them in a new language.


