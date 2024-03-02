---
layout: post
title: "Understanding std::call_once() in C++"
tags: [c++]
vanity: "2024-03-01-understanding-call-once-in-cpp"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}


<figure class="image_float_left">
  <img src="{{resources_path}}/thumbnail.png" alt="a bunch of lines and circles abstract art" />
</figure>

In this post, we'll delve into the function `std::call_once()` in the C++ STL: why it's useful, how efficient it is and how it can be implemented. We'll provide an simple implementation based on locks and a more advanced one based on futexes.

Finally we do a benchmark to compare their performance vs. libraries such as GCC, Clang and Folly.

<!--more-->

I recently needed to call a function that computes some result, memoizes it, so that in subsequent calls it gets the memoized result. It went something like this:

{% highlight c++ %}
int compute() {
    static std::optional<int> result;
    if (!result) {
        result = computeImpl();
    }
    return result;
}
{% endhighlight %}

However, during code review it was pointed out this was not thread-safe. After some searching (a.k.a. asking ChatGPT4) I learned about the `std::call_once()` function. It takes a `std::once_flag` and a lambda that is guaranteed to be called once even in a multi-threaded environment.

The above example would look like:

{% highlight c++ %}
std::once_flag flag;
int result;

int compute() {
    std::call_once(flag, [&]() {
        result = computeImpl();
    });
    return result;
}
{% endhighlight %}

Here we don't need to make `result` optional because we don't need to determine whether it has been initialized.

If a thread calls `std::call_once()` with the same `flag` while another thread is executing the lambda, it will block until `result` the lambda is finished and hence `result` is properly set.

If a thread calls `std::call_once()` with the same `flag` after the lambda has been executed, `std::call_once()` will return right away.

## Implementation

I was curious how to implement `std::call_once` (and `std::once_flag`) and in [2] user Matteo Italia provided a nice solution.

I'm reproducing it in here with some cleanups to improve readability. The `std::once_flag` class can be implemented as:

{% highlight c++ %}
class once_flag {
private:
    std::mutex m;
    std::atomic<bool> has_run;
public:
    constexpr once_flag() : has_run(false) {}

    once_flag(const once_flag&) = delete;
    once_flag& operator=(const once_flag&) = delete;

    template<typename Callable, typename... Args>
    friend void call_once(once_flag& flag, Callable&& f, Args&&... args);
};
{% endhighlight %}

Notice that most of it is boilerplate. The main takeaway is that `std::once_flag` is essentially composed of a mutex and an atomic boolean.

Now to the `call_once()` implementation:

{% highlight c++ %}
template<typename Callable, typename... Args>
void call_once(once_flag& flag, Callable&& f, Args&&... args) {
    if (flag.has_run) { // (1)
        return;
    }
    std::lock_guard<std::mutex> lock(flag.m); // (2)
    // This might have been changed from the
    // time we checked.
    if (flag.has_run) { // (3)
        return;
    }
    f(std::forward<_Args>(args)...);
    flag.has_run = true;
}
{% endhighlight %}

In `(1)` we check whether the flag `has_run` has been set. This is thread-safe because `has_run` is atomic. If not, then we acquire a lock to perform the computation `(2)`.

It's possible that by the time we're done acquiring the lock, another thread already invoked `f()` and set `flag.has_run`, so we need to check it one more time in `(3)`.

If it's still false, we're guaranteed no other thread will compute it until we release the lock. Since it's a RAII lock, this will only happen once `call_once()` ends. Worth noting that the implementation in [2] uses a `std::unique_lock` without explicitly locking it, and the author mentions in a comment it has RAII semantics so I believe they meant to use `std::lock_guard` as we did here.

### Exceptions

If `f()` throws an exception, according to [1]:

> If that invocation throws an exception, it is propagated to the caller of `std::call_once()`, and flag is not flipped so that another call will be attempted.

So in this case it should work as intended. If `f()` throws, we skip setting `flag.has_run` and since we exit the `call_once()`, the lock is released. If there's another thread waiting to acquire the lock, it will manage to do it and retry `f()`.

### Efficiency

This code only requires locking until a successful execution of `f()` and the setting of `flag.has_run`. Once that happens, it consists of checking an atomic boolean variable which is most systems should be implemented without locks.

There's a worst case scenario where many threads will invoke `std::call_once()` and if the function takes long enough, all but one thread will block when trying to acquire the lock. Once the thread executing the function finishes and unlocks, each of the remaining threads will have to acquire the lock, only to execute `(3)` and realize it doesn't need to execute `f()`. Since only one thread can get the lock at any time, this process will repeat until the last thread exits.

### Thread-safety of once_flag

In [2] Matteo mentions the need to make the constructor of `once_flag` `constexpr` and points to a Boost discussion [6] that says non-`constexpr` constructors are not thread-safe.

I get this fact, but I couldn't find an example where the thread safety of the constructor matters, in particular when the copy constructor is deleted as is the case with `once_flag`.

## Library Implementations

The version suggested by Matteo is very close to [`folly::call_once()`](https://github.com/facebook/folly/blob/323e467e2375e535e10bda62faf2569e8f5c9b19/folly/synchronization/CallOnce.h) [4]. Some notable details from folly:

* It annotates the check `(1)` with the equivalent of GCC's `__builtin_expect` to hint to the compiler that this is a very likely branch to be taken.
* It uses `std::memory_order_relaxed` when reading the atomic boolean and `std::std::memory_order_release` when writing the atomic boolean. These flags control the memory consistency. By default reads and writes to atomic variables use `std::memory_order_seq_cst` which is the safest but less efficient level. It's possible to relax the constraints when you know how about the relationship of reads and writes. This is a complicated subject I hope to write about one day.

The GCC libstdc++-v3 uses a [single global mutex](https://github.com/gcc-mirror/gcc/blob/5c30ecfa81cb64c8c5b52f561f54acf2d87d57ea/libstdc%2B%2B-v3/src/c%2B%2B11/mutex.cc#L52) across all calls to `std::call_once()`.

## Implementation with Futexes

In a private forum I saw someone mentioned `std::call_once()` is a good application for futexes. What is a futex? [Eli Bendersky's blog](https://eli.thegreenplace.net/2018/basics-of-futexes/) provides a very good introduction [5], but essentially futex stands for *Fast userspace mutex* and it's a lower level API that the STL uses to implement things such as `std::mutex`.

The key behavior which makes it good for `std::call_once()` is that all threads are awoken at once by the kernel, so they can all do the flag check without having to acquire locks sequentially. This helps with the worst case scenario mentioned above in *Efficiency*.

However, this API is only available as a Linux kernel system call, so this solution is not portable. Instead we can use an abstraction for Futexes such as Folly's [Futex](https://github.com/facebook/folly/blob/main/folly/detail/Futex.h). Which also has a simpler API for waiting and waking than the one from [Linux](https://man7.org/linux/man-pages/man2/futex.2.html).

It defines a `Futex` class, which is essentially an atomic unsigned 32-int variable. The waiting API is `futexWait()`:

{% highlight c++ %}
FutexResult result = futexWait(&futex, expectedValue);
{% endhighlight %}

It will block the thread until:

* **Case 1.** The value in `futex` is not `expectedValue`, in which case `FutexResult::VALUE_CHANGED` is returned.
* **Case 2.** Some other thread calls `futexWake()` (see next), in which case `FutexResult::AWOKEN` is returned.

Note that *Case 1* can happen if the value is not the expected when calling `futexWait()`, in which case it returns immediately.

The API for `futexWake()` is:

{% highlight c++ %}
int numberAwoken = futexWake(&futex, numberToAwake);
{% endhighlight %}

We can specify the number of threads to awake. If we want to awake all threads, we can just set `numberToAwake = INT_MAX`.

Using folly's `Futex`, our `call_once()` code can be as follows:

{% highlight c++ %}
// Atomic int:
// 0 - unlocked, not done
// 1 - locked
// 2 - unlocked, done
using once_flag = folly::detail::Futex<std::atomic>;

template<typename Callable, typename... Args>
void call_once(once_flag& flag, Callable&& f, Args&&... args) {
    while (true) {
        // If stage contains 0 (unlocked), set it to 1
        // Otherwise keep the value
        //
        // Set the old value of fut into expected
        std::uint32_t expected = 0;
        std::atomic_compare_exchange_strong(&flag, &expected, 1);
        if (expected == 0) { // it enters an exclusive region
            try {
                f(std::forward<Args>(args)...);
                flag = 2; // done
            } catch (...) {
                flag = 0; // failed, let another thread try it
            }
            // unblock dormant threads
            int count = folly::detail::futexWake(&flag, INT_MAX);
        } else if (expected == 2) { // already evaluated
            break;
        } else { // did not acquire, wait
            // Wait until flag is set to 1 or awoken
            auto result = folly::detail::futexWait(&flag, 1);
            switch (result) {
                case folly::detail::FutexResult::VALUE_CHANGED:
                    // This can happen if the value changed before
                    // the wait took effect. Try again.
                    continue;
                case folly::detail::FutexResult::AWOKEN:
                    // Awoken by the thread that was performing
                    // the computation. We have to loop again
                    // because if might have failed.
                    continue;
                case folly::detail::FutexResult::TIMEDOUT:
                    throw std::runtime_error("timeout");
                case folly::detail::FutexResult::INTERRUPTED:
                    throw std::runtime_error("interrupted");
            }
        }
    }
}
{% endhighlight %}

Notice that the `once_flag` is now a simple alias to the `Futex` (that is, a atomic integer), with the additional need to explicitly initialize it to 0:

{% highlight c++ %}
once_flag flag{0};
auto f = [&]() -> void {
    std::cout << "computing\n";
    result = 42;
};

std::thread t1([&](){
    std::cout << "thread 1\n";
    call_once(flag, f);
});
std::thread t2([&](){
    std::cout << "thread 2\n";
    call_once(flag, f);
});
t1.join();
t2.join();
std::cout << result << "\n";
{% endhighlight %}

We can do a small optimization: instead of always calling `std::atomic_compare_exchange_strong()`, we can do a simple read from the flag first, because once the function is computed, it will always return true:

{% highlight c++ %}
while (true) {
    if (flag == 2) { // always true after initial computation
        return;
    }
    ...
    std::atomic_compare_exchange_strong(&flag, &expected, 1);
    ...
}
{% endhighlight %}

I also tried using `__builtin_expect` on that call and more relaxed memory models (`std::memory_relaxed`) but didn't see significant performance gains on the benchmark (see next).

## Benchmark

I was curious to see how these different implementations compare. I extended [Folly's benchmark](https://github.com/facebook/folly/blob/ff3463a6b459a4046d2bef3b231e32c8a3265d0e/folly/synchronization/test/CallOnceBenchmark.cpp) for `call_once` comparing the STL and Folly's implementations and the futex-based and lock-based implementations provided here.

The benchmark is very simple: it basically starts `N` threads and have them attempt to call a function wrapped via `call_once`:

{% highlight cpp %}
template <typename CallOnceFunc>
void bm_impl(CallOnceFunc&& fn, size_t iters) {
  for (size_t i = 0u; i < size_t(FLAGS_threads); ++i) {
    threads.emplace_back([&fn, iters, &sync] {
      for (size_t j = 0u; j < iters; ++j) {
        fn();
      }
    });
  }
  for (std::thread& t : threads) {
    t.join();
  }
}
{% endhighlight %}

I was wondering if the sequential creation of threads might stagger the execution of `fn()` above and reduce contention. I tried adding a barrier ([`std::latch`](https://en.cppreference.com/w/cpp/thread/latch)) right before the inner `for`-loop to exarcebate the concurrent access to the exclusive region but it didn't seem to have a visible effect.

I also tried sleeping for 1 second in the function wrapped in `call_once` to make sure it's not finished computing too soon. The *relative* performance results didn't seem to change.

### Results and Analysis

On MacOS, I pull the head of Folly's repo as of 2024/02/16 (commit `65fb952918572592fa7dd2478f3b582b26e66b3f`) and compiled with Clang 15 (`-O3`), running on a MacBook M1 Pro. I used 100,000,000 iterations and 32 threads. The results are as follows:

| Implementation        | Time / Iter  | Iter / Sec |
| --------------------- | ------------ | ---------- |
| `StdCallOnceBench`    | `2.92ns`     | `341.93M`  |
| `FollyCallOnceBench`  | `2.91ns`     | `324.35M`  |
| `FutexCallOnceBench`  | `2.39ns`     | `418.23M`   |
| `LockCallOnceBench`   | `2.56ns`     | `390.23M`   |

So they seem to have pretty comparable performance when accounting for noise and variance, though the futex-based one is slightly faster.

I also tried it on a Ubuntu. Pulling the head of Folly's repo as of 2024/02/24 (commit `ff3463a6b459a4046d2bef3b231e32c8a3265d0e`) and compiled with GCC 9.4 (`-O3`), running on a Intel i5-8400 2.80GHz. The results are as follows:

| Implementation        | Time / Iter  | Iter / Sec |
| --------------------- | ------------ | ---------- |
| `StdCallOnceBench`    | `9.64ns`     | `103.77M`   |
| `FollyCallOnceBench`  | `5.39ns`     | `185.44M`   |
| `FutexCallOnceBench`  | `4.00ns`     | `249.82M`   |
| `LockCallOnceBench`   | `5.41ns`     | `184.82M`   |

The lock-based implementation is pretty close to Folly's and the futex-based was a bit faster. However, I experienced enough variance depending on the setup that I'm not confident in claming which one is faster. All versions are significantly faster than GCC though!

I'm surprised the lock-based implementation performed so well, since it's the simplest and without optimizations.

The optimization mentioned at the end of *Implementation with Futexes* was very important. Without it the futex implementation was 100x worse than the others.

## Conclusion

I love digging into topics and learning a lot more details than I expected! Initially I just wanted to understand the performance of `std::call_once()` but ended up learning about `constexpr` constructors and futexes in the process.

## Appendix A - Setup

I followed the instructions on Folly's `README.md` to install the dependencies. It required some work to figure out the compilation commands, without setting up a build system like CMake.

For MacOS:

{% highlight text %}
g++ -std=c++17 benchmark.cpp futex_call_once.h lock_call_once.h -lfolly -lgflags -lglog -lfmt -lfollybenchmark -lgtest -ldouble-conversion -O3
{% endhighlight %}

For Linux (Ubuntu):

{% highlight text %}
g++ -std=c++17 benchmark.cpp futex_call_once.h lock_call_once.h -lfollybenchmark -lfolly -lfmt -lboost_system -lglog -lgtest -lboost_regex -lboost_filesystem -lgflags -ldl -pthread -ldouble-conversion -O3
{% endhighlight %}

Running:

{% highlight text %}
./a.out --bm_min_iters=100000000 --threads=32
{% endhighlight %}

## References

* [[1](https://en.cppreference.com/w/cpp/thread/call_once)] C++ reference - std::call_once
* [[2](https://codereview.stackexchange.com/questions/117468/stdonce-flag-and-stdcall-once-implementation
)] Code Review: std::once_flag and std::call_once implementation
* [[3](https://github.com/gcc-mirror/gcc/blob/5c30ecfa81cb64c8c5b52f561f54acf2d87d57ea/libstdc%2B%2B-v3/include/std/mutex)] libstdc-v3 - mutex
* [[4](https://github.com/facebook/folly/blob/main/folly/synchronization/CallOnce.h)] Folly - call_once
* [[5](https://eli.thegreenplace.net/2018/basics-of-futexes/)] Eli Bendersky's website - Basics of Futexes
* [[6](https://lists.boost.org/Archives/boost/2013/03/201556.php)] Boost Mailing List: C++11 once_flag enabled when constexpr is not available
