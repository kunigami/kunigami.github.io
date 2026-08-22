---
layout: post
title: "Velox: Memory"
tags: [distributed systems, databases]
excerpt_separator: <!--more-->
vanity: "2026-08-04-velox-memory"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/velox-logo.svg" alt="Velox Logo" />
</figure>


[Velox](https://github.com/facebookincubator/velox) is an open source C++ library by Meta that can be used to perform computation common to distributed engines like Presto.

Its offerings include columnar operations, a rich type system, an expression parser and a smart resource management such as memory [1]. In this series of posts we'll go over different components of Velox.

In this post we'll study memory management done by Velox.

<!--more-->

Previous posts on the series:

* [Vectors]({{blog}}/2026/06/01/velox-vectors.html)
* [A Simple Application]({{blog}}/2026/06/18/velox-application.html)
* [UDFs]({{blog}}/2026/07/23/velox-udfs.html)

Velox has a custom memory allocator system because the same process can run short-lived queries from different clients. To avoid one query causing others to OOM, it cannot rely on OS-level limits such as cgroups and thus uses its own arbitrage.

## Components

There are 4 main components involved in memory allocation: the manager, the allocator, the arbitrator and the pools. The pools themselves are subdivided into root pool, "inner" pool and leaf pool.

The memory allocator (`MemoryAllocator`) is the one interfacing with the OS and making the actual allocation.

The memory arbitrator (`MemoryArbitrator`) is an interface, and is implemented by classes like `SharedArbitrator`. Its purpose is to distribute quotas to the pools, but it doesn't perform the memory allocation.

Memory pools (`MemoryPool`) are organized in a tree hierarchy: at the top there is the root pool, in between there are the inner pools and at the leaves the leaf pools. The root and inner pools are of aggregate type, meaning they are logical grouping of the leaf pools and they're mostly used to keep aggregated statistics such as total memory used by the subtree. The leaf pool is the one Velox internals interface with.

The idea is that each query corresponds to a root pool. A given process can be executing a number of queries. Each query runs one or more tasks, each having a pool as a child of the query root pool. Then for each node in the plan (see [Velox Application]({{blog}}/2026/06/18/velox-application.html)) we have another pool. Each node might be converted into one or more operators and one instance of each operator will exist for each driver (in the parallel mode). Finally, each such instance of the operator has the leaf pool.

The memory manager (`MemoryManager`) is the singleton which can be used to construct the allocator and pools.

<figure class="center_children">
  <img src="{{resources_path}}/class-diagram.png" alt="See caption" />
  <figcaption>Figure 1. Relationship between the different components involved in memory allocation.</figcaption>
</figure>

## Allocation

Allocation typically happens at the leaf pool and it might bubble up all the way to the memory arbitrator. Let's go through the flow.

Each leaf pool tracks how much memory it's using (we'll name it `used`) and how much it has allocated to it (we'll name it `reserved`), and both start at 0. When the caller requests `requested` amount of memory from the leaf pool, it will check if `used + requested <= reserved`. If yes, then it just updates `used += requested`.

If `used + requested > reserved`, it will ask the root pool to increase its reservation. The request goes through its parents because then they can update the aggregated reserved values. Once it reaches the root, it will check if it's within its limit.

The root pool has two relevant properties: `capacity` and `maxCapacity`. The `capacity` is analogous to the leaf pool's `reserved`: if the root pool exceeds this limit, it requests the arbitrator to increase its `capacity`. The `maxCapacity` is a limit on `capacity`. It's a static amount that is set when creating the root pool (unbounded by default). If the increase in `capacity` would exceed this value, the pool will try reclaiming memory (see *Reclamation*).

The arbitrator also contains a limit, `capacity`, which is set by the application. If a request for more memory would exceed this value, it will attempt memory reclamation (see *Reclamation*). Note that the arbitrator limit is global.

So far, all this escalation from leaf pool → root pool → arbitrator is accounting. It checks if the allocation can be performed, but the actual allocation happens via the memory allocator and is initiated by the leaf pool.

The memory allocator also has a limit, also set by the application. If this limit is exceeded, it will result in the error `VELOX_MEM_ALLOC_ERROR`. Note that memory allocation can be requested by callers directly to the allocator, without using pools.

The arbitrator capacity must be less than that of the allocator. It's not exactly the same because of the last point: not all memory allocated is accounted for by the arbitrator, only the ones requested from the pools.

### Custom Allocator vs. Malloc

The allocator has 2 flavors: malloc or mmap, which uses `malloc()` and `mmap()` to allocate memory, respectively. Malloc is the default mode.

Recall that `malloc()` can itself be customized by linking a library such as [jemalloc]({{blog}}/2025/07/15/jemalloc.html) during compilation. As we've seen libraries like jemalloc do a lot of the heavy lifting on memory allocation, balancing performance and efficiency (reduce fragmentation) and making use of modern hardware and OS (e.g. memory layout and multi-thread). So for this flavor, the Velox memory allocator mostly delegates to `malloc()`.

For the `mmap()` version, since it requests larger pages to the OS, Velox has to add its custom logic to do the fine grained allocation and we won't cover it here.

One of the major downsides of this allocator compared to jemalloc is when there are a lot of threads, contention can be significant. As we've seen, jemalloc avoids this by using arenas (see *Multi-threads and Arenas* in [jemalloc]({{blog}}/2025/07/15/jemalloc.html)).


## Deallocation

Freeing memory is almost the exact opposite of allocation. When called on a leaf pool, it reduces `used`. If the amount of free memory (`reserved - used`) crosses a threshold (the quantized sizes), then `reserved` is reduced and this amount flows back to the root pool.

## Reclamation

As we discussed, there are 2 cases in which memory reclamation can happen:

First is when the `maxCapacity` of a root pool would be exceeded, in which case it will choose a leaf pool from which it can reclaim memory. First it checks if the pool can reduce its `reserved` and give it back to the root. If not, it tries to find a leaf pool in its subtree to spill its state to disk. Once an operator spills to disk its `used` memory drops and it follows the *Deallocation* process. Only certain operators can do spilling (e.g. `HashBuild`, `OrderBy`, etc.).

The second case is when the `capacity` on the arbitrator is reached. It will choose a root pool from whom to reclaim memory and then the process is the same as above.

If reclamation cannot be done, a query gets killed. In the first case it's the query making the request; in the second the arbitrator will select the query to be killed. After reclamation is done, the arbitrator can re-allocate the recouped memory to fulfill the original request.

## Other

### Cache

Velox supports cache by building on top of the memory framework. The component is called `AsyncDataCache` and it allocates from the `MemoryAllocator` directly (not through pools).

This cache has a secondary (optional) layer backed by SSD. When enough bytes have been stored in memory, it eventually flushes to SSD. Data is kept in memory as well, but when memory reclamation arrives those can be easily dropped.

Eviction happens when no more data can be stored in RAM, which is the same trigger we've seen for the memory allocation.

## Conclusion

In this post we've learned about the memory management done by Velox. It was one of the most mysterious parts of it, and I think I have a much better grasp on it. It's not as complex as I imagined, but it's a lot more complicated than I though with lots of components.

Writing also provided some insights on the motivation, for example, being designed to run multiple independent queries in the same process, which seems to be modeled after the Presto query engine (which is the system it aimed to replace).

It also clarified my understanding of memory pools and the two underlying allocator systems, malloc and custom.

## Related Posts

We already discussed [jemalloc]({{blog}}/2025/07/15/jemalloc.html) which itself is a user-space, custom memory allocator that internally implements [Buddy Memory Allocation]({{blog}}/2020/07/31/buddy-memory-allocation.html).

The summary [[Book] Systems Performance]({{blog}}/2025/10/10/review-systems-performance.html) mentions that allocators don't return memory to the kernel once free is called, but keep them around. Another relevant behavior is that the virtual to physical mapping for memory is done lazily by the OS: it only happens when memory is written to. In a sense allocation and reclamation in Velox are also lazy.
