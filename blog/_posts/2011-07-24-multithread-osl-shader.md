---
layout: post
title: "Multithread OSL Shader"
tags: [computer graphics]
vanity: "2011-07-24-multithread-osl-shader"
---
{% include blog_vars.html %}


These last few weeks I was trying to implement multithread support for the OSL shader. BRL-CAD supports using pthreads to parallelize work.

The first time I got the OSL shader running, I had problems, probably due to concurrent reading and writing of data in memory. I had postponed this issue and was working with only one thread.

What my `osl_render()` function does is fill in a structure called `RenderInfo` with data like the intersection point, the normal, a pointer to the shader for that surface, etc. Once this structure is completed, it makes a call to the `QueryColor` method of the `OSLRender` class. The object of this class is global, which means that it is in-memory location shared by threads. As calling the `QueryColor` method modifies the object, we have the problem of concurrent access to this method.

## First Attempt

The first thing I tried was to protect this function call with semaphores. BRL-CAD has an internal library of utilities, including the implementation of a semaphore.

Basically what I did was the following:

{% highlight cpp %}
OSLRender oslr;
...
bu_semaphore_acquire(BU_SEM_SYSCALL);
oslr.QueryColor();
bu_semaphore_release(BU_SEM_SYSCALL);
{% endhighlight %}

`BU_SEM_SYSCALL` is actually a macro representing an integer. When a thread calls the function `bu_semaphore_acquire(BU_SEM_SYSCALL)`, it blocks access to other threads for the same integer.

With that the code started to work for more than one thread! However, I was too conservative when blocking the `QueryColor` call. I believe that 90% of the work of the `osl_render()` function is done there. So, by putting a semaphore in this function the parallelization goes down the drain. In fact, I've done tests with 1 and 2 processors and the times to render a test image were exactly the same.

## Second Attempt

I went to study the OSL code a little more and found that it supports multithreaded applications. Basically, each thread must maintain a local structure called `ThreadInfo`. When calling the OSL shader system, we pass this information, which will be modified.

### Work around!

In order to maintain a `ThreadInfo` for each thread, we need to somehow identify them. The problem is that we don't have access to this information directly. The only way I found without having to change the internal structure of the BRL-CAD raytracer was the following: every time the `osl_render()` function is called, a structure called `struct application` is passed. This structure has a field called `resource`, which is a pointer to a piece of memory unique to each thread.

This piece is allocated at thread creation and does not change throughout their lifetime. Thus, using the address of that piece of memory is an (ugly) way to identify them. Another problem is that, although there is a shader initialization function (called `osl_setup()`), the threads are created after that, which forces me to initialize them in the `osl_render()` function.

The solution I arrived at was the following:

{% highlight cpp %}
/* Every time a thread reaches osl_render for the
   first time, we save the address of their own
   buffers, which is an ugly way to identify them */
std::vector<struct resource *> visited_addrs;
/* Holds information about the context necessary to
   correctly execute a shader */
std::vector<void *> thread_infos;

int osl_render(struct application *ap, ...){
    ...
    bu_semaphore_acquire(BU_SEM_SYSCALL);

    /* Check if it is the first time this thread is
       calling this function */
    bool visited = false;
    for(size_t i = 0; i < visited_addrs.size(); i++){
        if(ap->a_resource == visited_addrs[i]){
            visited = true;
            thread_info = thread_infos[i];
            break;
        }
    }
    if(!visited){
        visited_addrs.push_back(ap->a_resource);
        /* Get thread specific information from
           OSLRender system */
        thread_info = oslr->CreateThreadInfo();
        thread_infos.push_back(thread_info);
    }
    bu_semaphore_release(BU_SEM_SYSCALL);
    ...
}
{% endhighlight %}

### Thread-safe class

With `ThreadInfo`'s, we can stop worrying about concurrent access to the object of the `OSLRender` class. To guarantee that no writing will be done on this object, we can declare the methods as `const`, as in the example below:

{% highlight cpp %}
struct A {
    int x;
    // OK
    void set_x(int _x) { x = _x; };
    // ERROR, trying to modify object member
    // whether method const
    void const_set_x(int _x) const { x = _x; };
};
{% endhighlight %}

## Tests

With this new implementation, times have improved a lot with more processors. The table below shows the runtimes for rendering an example scene, using up to 4 processors.


<figure class="center_children">
  <img src="{{resources_path}}/chart.png" alt="Cornell box with the large box as mirror, the smaller box as a mirror and green checkered shader." />
  <figcaption>Figure 1: Execution time vs. number of processors</figcaption>
</figure>

The gains were practically linear, and for 4 processors the parallelization measure was 1.25 (if it were perfectly linear it would be 1.0).

## Next Steps

I talked to my mentor about developing that incremental view, but he didn't seem to like the idea very much. This would require quite a big change in the system, as the BRL-CAD renderer was designed for ray-tracing and not path-tracing.

For now I'm studying ways to adapt the OSL code to support ray-tracing, but I don't know if this is feasible. By the way, I still get the terms ray-tracing, path-tracing, photon mapping and all these lighting algorithms mixed up and I intend to write a post covering these topics pretty well soon.
