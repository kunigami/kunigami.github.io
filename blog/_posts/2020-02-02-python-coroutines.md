---
layout: post
title: "Python Coroutines"
description: "Study of Python Coroutines for asynchronous programming"
tags: [python]
---

In this post we'll cover Python coroutines. First we'll study David Beazley's great tutorial [1] to learn about generators and coroutines, and how to build a multitask system from the ground-up.

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2020-02-02-python-coroutines/2020_02_3052214847_dbd27f9ccc_w-1.jpg" alt="3052214847_dbd27f9ccc_w (1)" />
    <figcaption> Goddess Durga - source: <a href="https://www.flickr.com/photos/rosenkranz/3052214847/in/photostream/">Flickr</a></figcaption>
</figure>

We'll then take a look at a Python module called asyncio which makes use of coroutines to enable asynchronous I/O.

## Generators

Python generators are functions what allow return the execution back to the caller such that when called again it will resume its execution from the same place. It's the same concept of JavaScript generators, which we [talked about before]({{site.url}}/blog/2019/07/01/async-functions-in-javascript.html).

The syntax for yielding the execution back is via the `yield` keyword. Here's an example of a custom implementation of `range()`:

{% highlight python %}
def range_gen(start, end, step):
    i = start
    while i < end:
        yield i
        i += step

for i in range_gen(1, 10, 2):
    print(i)
{% endhighlight %}

Because the execution can be yielded at any time in the code, we can use infinite loops:

{% highlight python %}
def naturals():
    n = 0
    while True:
        yield n
        n += 1

for i in naturals():
    print(i)
    if i > 10:
        break
{% endhighlight %}

Another way to use generators is to "pipe" (in the Linux way), one generator into another, like in the example where we read lines from a dictionary file and find those ending in "ion":

{% highlight python %}
def get_words():
    f = open('/usr/share/dict/words', 'r')
    for line in f:
        yield line.strip()

def filter_step(lines):
    for line in lines:
        if (line[-3:] == 'ion'):
            yield line

def print_step(lines):
    for w in filtered_words:
        print(w)

words = get_words()
filtered_words = filter_step(words)
print_step(filtered_words)
{% endhighlight %}

This also highlights one advantage of using generators abstraction: the lower memory footprint since it can process one line at a time. Sure, we could achieve the same with regular functions, but they would need to be combined in a single block:

{% highlight python %}
def get_words_and_filter_and_print(lines):
    f = open('/usr/share/dict/words', 'r')
    for line in f:
        if (line[-3:] == 'ion'):
            print(line)

get_words_and_filter_and_print()
{% endhighlight %}

Which is efficient but doesn't allow modularity.

Combined with the fact that generators can be infinite loops, one can model functionality like tail and grep as generators (e.g. tail -f \| grep foo) which is the exact example Beazley provides in his presentation [1].

## Coroutines

In [PEP 342](https://www.python.org/dev/peps/pep-0342/) it explains the motivation behind coroutines:
> Coroutines are a natural way of expressing many algorithms, such as simulations, games, asynchronous I/O, and other forms of event-driven programming or co-operative multitasking. Python's generator functions are almost coroutines -- but not quite -- in that they allow pausing execution to produce a value, but do not provide for values or exceptions to be passed in when execution resumes. They also do not allow execution to be paused within the `try` portion of `try/finally` blocks, and therefore make it difficult for an aborted coroutine to clean up after itself.
There are four main changes:
* `yield` is an expression instead of statement, which means you can assign the return type of yield to a variable.
* `send()` method which can be used to send value to the yield expression. `send()` is a general version of `next()`, which is equivalent to `.send(None)`.
* `throw()` method which raises an exception at the place where the last yield was called.
* `close()` method which raises a `GeneratorExit` at the place where the last yield was called. Effectively used to force terminating the generator.
Here's a simple coroutine that just prints what it receives:

{% highlight python %}
# Dummy function that prints what it receives
def echo():
    print("initialized")
    while True:
        line = (yield)
        print(line)

gen = echo()
gen.send(None)
gen.send("hello")
gen.send("world")
{% endhighlight %}

Notice that we have to call an empty `send()` to "initialize" the coroutine. According to *PEP 342*:
> Because generator-iterators begin execution at the top of the generator's function body, there is no yield expression to receive a value when the generator has just been created. Therefore, calling send() with a non-None argument is prohibited when the generator iterator has just started (...). Thus, before you can communicate with a coroutine you must first call next() or send(None) to advance its execution to the first yield expression.
In the example below we highlight how this flow can be confusing. The first `send()` receives the value from yield, but it is only able to send a value through a subsequent `send()`.

{% highlight python %}
def echo():
    x = yield 'ready'
    print(x)

f = echo()
f.send(None) # Moves to the yield line, gets 'ready'
f.send('hi') # Sends hi but throws StopIteration
{% endhighlight %}

Since in a coroutine most of the times the first `send()` will be to initialize the coroutine, Beazley provides a decorator to abstract that:

{% highlight python %}
def coroutine(fn):
    def start(*args, **kwargs):
        gen = fn(*args, **kwargs)
        next(gen)
        return gen
    return start
{% endhighlight %}

### Piping

We can chain multiple coroutines the same we did with generators but now the order is somewhat reversed. To see why, consider our generator pipe example rewritten with coroutines. In here we pass `print_step()` to `filter_step()` and `filter_step()` to `get_words()`. Contrast this with the generator version where we start with get_words, pass its results to `filter_step()` and then `print_step()`.

{% highlight python %}
def get_words(step):
    f = open('/usr/share/dict/words', 'r')
    for line in f:
        step.send(line.strip())

@coroutine
def filter_step(step):
    while True:
        line = (yield)
        if (line[-3:] == 'ion'):
            step.send(line)

@coroutine
def print_step():
    while True:
        line = (yield)
        print(line)

p = print_step()
f = filter_step(p)
get_words(f)
{% endhighlight %}

### Event Handling

Beazley provides an example using an XML parser to manipulate XML and convert to JSON. The API of xml.sax relies on the visitor pattern, where at important steps of the AST traversal (e.g. startElement, character, endElement) it calls specific methods of a [content handler](https://docs.python.org/3/library/xml.sax.handler.html#contenthandler-objects).

The example uses an adapter layer ([cosax.py](http://www.dabeaz.com/coroutines/cosax.py)), to allow using coroutines instead the content handler. It also combines multiple smaller coroutines to produce a more complex ones, demonstrating the composability of coroutines.

### Coroutines as tasks

After some preliminary overview of Operating Systems Beazley builds a prototype of a multitasking system by modeling coroutines as tasks (no threads or processes).
* *Task* - wrapper around coroutine
* *Scheduler* - can create new tasks and schedule tasks - keeps a loop as long as there are tasks. Existing tasks can cause tasks to be scheduled. JavaScript event loop.
Yielding inside a task is like an OS interruption (trap).

**System Calls**


One of the interesting implementation of the multitasking system is system calls. The task/coroutine issues a system call by providing it as a value to the yield (e.g. `yield GetTid()`). This is received by the scheduler which can provide the necessary context to the specific system call implementation.

{% highlight python %}
# Excerpt from http://www.dabeaz.com/coroutines/pyos4.py

# coroutine that will be wrapped in a task
def foo():
    mytid = yield GetTid()
    ...

# A system call
class GetTid(SystemCall):
    def handle(self):
        # The scheduler "injects" task in the
        # system call
        self.task.sendval = self.task.tid

class Scheduler(object):
    ...
    def mainloop(self):
       ...
       # RHS of foo's yied
       result = task.run()
       if isinstance(result, SystemCall):
           result.task  = task
           result.sched = self
           result.handle()
{% endhighlight %}

The tutorial then covers one of the most important parts of the multitasking system which is the ability to do asynchronous I/O operations (e.g. reading and writing to a file).

The idea is to introduce a "system call" corresponding to performing I/O. When a task invokes that, it doesn't get rescheduled but put in a staging area (`self.read_waiting` in the code below). The scheduler has then a continuously run task that polls the OS to check if any of the file descriptors corresponding to the tasks in the staging area are ready, in which case the task is rescheduled.

{% highlight python %}
class ReadWait(SystemCall):
    def __init__(self, file):
        self.file = file

    def handle(self):
        fd = self.file.fileno()
        self.sched.waitforread(self.task, fd)

class Scheduler(object):

    # Add task and file descriptor to the list
    # to be checked on polling
    def waitforread(self, task, fd):
       self.read_waiting[fd] = task

    # select.select is an OS API to determine
    # when a file descriptor is "ready"
    def iopoll(self,timeout):
        if self.read_waiting or self.write_waiting:
           read_fds, write_fds, _x = select.select(
             self.read_waiting,
             # ...
           )
           for fd in read_fds: self.schedule(self.read_waiting.pop(fd))
           # ...

    # Models polling as a regular task - whenever
    # it gets run by the scheduler it counts as a
    # poll
    def iotask(self):
        while True:
            if self.ready.empty():
                self.iopoll(None)
            else:
                self.iopoll(0)
            yield

{% endhighlight %}

Beazley uses that system to implement a simple server that receives data via a socket:

{% highlight python %}
def server(port):
    sock = socket(AF_INET,SOCK_STREAM)
    sock.setsockopt(SOL_SOCKET,SO_REUSEADDR,1)
    sock.bind(("",port))
    sock.listen(5)
    while True:
        yield ReadWait(sock)
        client, addr = sock.accept()
        yield NewTask(handle_client(client, addr))
{% endhighlight %}

**Trampolining**


One of the limitations of the scheduler that is developed up to this point is that the `yield` has to happen at the top-level of coroutine, that is, it cannot invoke other functions that yield, which limits refactoring and modularization.

To overcome this limitation, the author proposes a technique called [trampolining](https://en.wikipedia.org/wiki/Trampoline_(computing)). Here's an example where the execution of `add()` is done by the top level code, not by `main()`:

{% highlight python %}
def add(x, y):
    yield x + y

def main():
    result = yield add(2, 2)
    print(result)
    yield

main_cr = main()
# add() is not executed
add_cr = main_cr.send(None)
# it has to be explicitly executed
result = add_cr.send(None)
main_cr.send(result)
{% endhighlight %}

We can do a similar thing in the multitasking system. Because the coroutines can recursively call other coroutines, we need to keep a callstack. We can do it inside `Task` by keeping an explicit stack:

{% highlight python %}
class Task(object):
    def run(self):
        while True:
            try:
                result = self.target.send(self.sendval)

                if isinstance(result, SystemCall):
                    return result

                # sub-routine call
                if isinstance(result, types.GeneratorType):
                    # put the caller on the stack
                    self.stack.append(self.target)
                    self.sendval = None
                    self.target  = result
                else:
                    if not self.stack:
                        return
                    self.sendval = result
                    # resume execution to caller
                    self.target = self.stack.pop()
            # current coroutine finished
            except StopIteration:
                # invalid state
                if not self.stack:
                    raise
                self.sendval = None
                self.target = self.stack.pop()

# Example coroutine and sub-coroutine
def Accept(sock):
    yield ReadWait(sock)
    yield sock.accept()

def server(port):
    # ...
    while True:
        # coroutine that calls another
        client, addr = yield Accept(sock)
        yield NewTask(handle_client(client, addr))

{% endhighlight %}

In the example above, when a coroutine that is wrapped in `Task` makes another coroutine call, for example:

`client, addr = yield Accept(sock)`

Within `Task`, `result` will be a generator, so the current coroutine will be put on the stack and the execution will pass to that generator instead:

`self.target = result`

When that sub-coroutine yields the execution, the original caller is resumed

`self.target = self.stack.pop()`

Note that this all happen transparently to the scheduler itself.

This concludes the study of the multitask system from the ground up.

## Modern Asynchronous I/O

The tutorial we just studied is from 2009 and while still relevant, Python went through many changes since then.

We'll now cover some of the development since the generator-based coroutines were introduced and that culminated in a standard module for handling asynchronous I/O.

### Delegating to subgenerators: yield from

[PEP 380](https://www.python.org/dev/peps/pep-0380/) introduced a new syntax for delegating the execution to another generator until that generator is finished. Simeon [5] provides an example where using yield from simplifies the code. In the code below `generator()` calls `generator2()` and `generator3()` in a loop to "exhaust" their execution.

{% highlight python %}
def generator2():
    for i in range(10):
        yield i

def generator3():
    for j in range(10, 20):
        yield j

def generator():
    for i in generator2():
        yield i
    for j in generator3():
        yield j

{% endhighlight %}

The same can be accomplished with yield for, which does that implicitly:

{% highlight python %}
def generator():
    yield from generator2()
    yield from generator3()
{% endhighlight %}

### Async / Await

[PEP 492](https://www.python.org/dev/peps/pep-0492/) Introduces the async and await syntax. The motivation contains these two reasons among others:
> It is easy to confuse coroutines with regular generators, since they share the same syntax; this is especially true for new developers.
> Whether or not a function is a coroutine is determined by a presence of yield or yield from statements in its body, which can lead to unobvious errors when such statements appear in or disappear from function body during refactoring
To address these issues, a function containing the `async` is considered a *native coroutine* (as opposed to a generator-based coroutine):

{% highlight python %}
async def g_async():
    return 10

# <class 'coroutine'>
print(type(g_async()))

def g_coro():
    yield 10

# <class 'generator'>
print(type(g_async()))


{% endhighlight %}

A native coroutine can await generator-based coroutines in which case it has the same  behavior as the wait for. Borrowing from the example above:

{% highlight python %}
async def generator_async():
    await generator2()
    await generator3()
{% endhighlight %}

### The asyncio module

[PEP 3156](https://www.python.org/dev/peps/pep-3156/) Describes a system to support asynchronous I/O, which is now known as the `asyncio` module. It proposes a coroutine-based multitasking system similar to the one Beazley describes. It has support to common I/O operations like those involving files, sockets, subprocesses, etc.

We won't dive into much detail in this post, but we can see parallels to the scheduler we  studied previously. Here's an example where `asyncio.run()` schedules the coroutine `main()` which in turn executes the sub-coroutines `sleep()` one after another:

{% highlight python %}
# Python 3.7+
import asyncio

async def sleep(id):
    print("will sleep", id)
    await asyncio.sleep(4)
    print("slept", id)

async def main():
    await sleep(1)
    await sleep(2)

asyncio.run(main())
{% endhighlight %}

This is not taking advantage of the non-blocking capabilities of asyncio. Why? Recall that await is equivalent to yield from and that causes the current coroutine to wait the call to finish until it continues. If we run this code we get:

`will sleep:  1
slept:  1
will sleep:  2
slept:  2`

What we want is to schedule both sub-coroutines at the same time, and asyncio allows that via the `gather()` method:

{% highlight python %}
# Python 3.7+
import asyncio

async def sleep(id):
    print("will sleep", id)
    await asyncio.sleep(4)
    print("slept", id)

async def main():
    await asyncio.gather(sleep(1), sleep(2))

asyncio.run(main())

{% endhighlight %}

If we run this code we get:

`will sleep 1
will sleep 2
slept 1
slept 2`

Which means that the first sleep executed but yielded the execution back to the scheduler after the `sleep()` call. The second `sleep()` got run, printing *"will sleep 2"*.

## Conclusion

I've used async/await and event loops in other languages such as Hack and JavaScript, but only recently ran into it in Python. I kept seeing mentions of coroutines and that led me to study it in more details. Overall I felt like I learned a lot.

David Beazley's tutorial is really good. They're thorough and provide lots of analogies with operating systems concepts.

I also liked the presentation medium: the slides are self-contained (all the information is present as text) and he shows the whole code at first then repeats the code multiple times in subsequent slides, highlighting the important pieces in each of them. This is difficult to achieve in flat text like a blog post.

## References

* [[1](http://www.dabeaz.com/coroutines/Coroutines.pdf)] David Beazley - A Curious Course on Coroutines and Concurrency
* [[2](https://www.python.org/dev/peps/pep-0342/)] PEP 342 - Coroutines via Enhanced Generators
* [[3](https://www.python.org/dev/peps/pep-0380/)] PEP 380 -- Syntax for Delegating to a Subgenerator
* [[4](https://www.python.org/dev/peps/pep-0492/)] PEP 492 -- Coroutines with async and await syntax
* [[5](http://simeonvisser.com/posts/python-3-using-yield-from-in-generators-part-1.html)] Simeon Visser - Python 3: Using "yield from" in Generators - Part 1
* [[6](https://www.python.org/dev/peps/pep-3156/)] PEP 3156 -- Asynchronous IO Support Rebooted: the "asyncio" Module
