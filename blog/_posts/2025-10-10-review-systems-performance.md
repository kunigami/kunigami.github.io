---
layout: post
title: "Review: Systems Performance"
tags: [review, distributed systems, operating systems]
excerpt_separator: <!--more-->
vanity: "2025-10-10-review-systems-performance"
---

{% include blog_vars.html %}


<figure class="image_float_left">
  <img src="{{resources}}/books/systems-performance.jpg" alt="Book cover." />
</figure>


In this post I’ll share my notes on the book *Systems Performance* by Brendan Gregg.

In summary, this book covers major components of an operating system such as CPU, memory, disk and network and for each of them provides methodologies and tools for analyzing performance.

<!--more-->

The book focuses on two families of operating systems: Solaris and Linux. The concepts are largely shared between them but some tooling and metrics are specific to one of the other.

The book has over 700 pages and 13 chapters. This is a pretty thick volume, but I skipped anything related to Solaris and any details about a tool called DTrace. The author seems to be an expert on this tool and spends a considerable number of pages on scripts for it, but a cursory search tells me DTrace is not very popular or developed for Linux systems.

The audience of the book is for performance engineers, who need to find performance issues without knowledge of the application. One example is of a cloud provider where they might need to help customers with their performance problems but they don't have expertise on the client's code.

A lot of the chapters provide an overview of operating systems topics such as CPU, Memory, Disk and Network. In compiling my notes, I also did extra searching to help me understand some parts better, so the content presented here is not strictly from the book.

In what follows I list some topics I learned from the book and found interesting.

## Methodology

The book provides suggestions of processes or methodologies to follow when debugging performance issues. Many of them feels like common sense such as having a runbook or checklist to follow during an investigation to make sure one is not missing anything obvious. This includes metrics to look at, tools to use, and processes to try, e.g. microbenchmarking.

Another is comparing metrics from before and after since it can help narrow down when something changed and what. If aggregate metrics don't provide enough detail, we might want to trace individual events (via perf and DTrace).

The two main processes I learned from the book are the *USE Method* and *Workload Characterization*.

**USE Method.** Stands for Utilization, Saturation and Errors. This is a quick checklist for ruling out issues. Errors are usually the easiest to detect. Utilization is measured in percentage, but once a system is at peak utilization, we need to look at saturation metrics (e.g. queueing).

**Workload Characterization.** Focuses on describing the input (load) to the system. Questions we might ask:

* Who is causing the load? e.g. process ID
* What are the load characteristics? e.g. throughput
* How is the load changing over time?

One outcome is to realize some load was not meant to be there, for example, an unexpected backup mechanism causing high disk and CPU utilization. As the author says:

> Sometimes the best performance wins are the result of eliminating unnecessary work.

The author suggests having a checklist for load characterization questions to avoid making assumptions and discarding culprits.

## Operating Systems

**Kernel and CPU work.** For most CPU operations performed by the user process such as arithmetic, control flow, memory reads/writes, the kernel is not involved. It gets involved for I/O, interrupts or when page faults happen during memory access.

**Jiffy.** Historically, a jiffy was the "virtual clock tick" of the Linux kernel, corresponding to the smallest unit of time supported and was about 4ms, so kernel scheduling would be a multiple of this. Nowadays the kernel has high resolution timers with sub-jiffy granularity.

**Processes and threads.** The kernel doesn't distinguish between user processes and threads. Both are called tasks, but the task corresponding to user threads typically share more stuff (e.g. memory address space).

**Lazy Evaluation.** The kernel maps virtual memory to physical on demand, when a write occurs, not when memory is first allocated. This means we can get a `std::bad_alloc` not when we call `malloc()` but when we perform a write.

When `fork()`-ing a process the kernel uses copy-on-write (COW). Instead of immediately copying the code, data, stack and heap, it only does so when one of the processes performs a write.

**Paging vs. Swapping.** In Linux swapping and paging are the same thing and it means moving pages (4KB) from memory to disk.

The book provides an interesting post from Linus Torvalds:

> I'm doing a (free) operating system (just a hobby, won't be big and professional like gnu) for 386(486) AT clones. (...)

## CPU

**Sockets, Cores, Hardware Threads.** Sockets is the physical slot where a CPU die connects to the motherboard. A given CPU die typically has multiple cores and each core might have one or more hardware threads. Each CPU core has its own [L1 cache]({{blog}}/2020/04/24/cpu-cache.html), but threads share them. **Logical CPUs** corresponds to the total number of hardware threads.

**CPU stalls.** Is when the CPU is waiting for data from the main memory.

**Instruction width.** Corresponds to how many instructions a CPU can process within a clock cycle (parallelism).

**CPU Saturation.** Is when there is more work to do than CPUs to run them on. The kernel maintains a queue that can be queried via:

{% highlight text %}
$ vmstat 1
{% endhighlight %}

It prints a snapshot every second. The first column, `r`, indicates the size of a queue at any given time. If it's constantly higher than the number of CPUs it indicates saturation.

**NUMA Architecture.** stands for *Non-Uniform Memory Access*. Instead of having a centralized RAM card equally shared by the CPUs, in NUMA architecture each CPU socket has its own RAM card.

CPUs can access memory from other sockets (*remote memory*), but it's slower since it has to go through more hops.

**Scheduling policies.** Controls which algorithm the kernel uses to assign tasks to CPUs. It can be checked with:

{% highlight text %}
chrt -p <id>
{% endhighlight %}

The default is `SCHED_OTHER` which is known as *CFS* or Completely Fair Scheduling.

**time (tool).** I've been using `time` probably since I learned how to program and embarrassingly I *just* learned what all these numbers mean:

{% highlight text %}
real    0m11.524s
user    0m2.163s
sys     0m0.878s
{% endhighlight %}

`user` is CPU time in user space, `sys` is CPU time in kernel space and `real` is wall clock, which is the only one I've made use in the past. If we subtract `user` and `sys` from `real` we can get a sense of how much time it was waiting on I/O, which is this case it was significant!

**ps (tool).** I learned that we don't pass dashes to `ps`, e.g. `ps aux` because it's a tool from BSD, which has a different convention for CLIs!

## Memory

**Terminology.** *Resident memory* is the portion of virtual memory that is mapped to the physical RAM. *RSS* stands for *Resident Set Size* and is the size of the resident memory.

**Anonymous and non-anonymous.** Anonymous memory is not backed by a file or disk. This includes the heap and stack. Example of non-anonymous memory is code, shared libraries and memory-mapped files.

**Memory utilization.** User space allocators such as [jemalloc](https://www.kuniga.me/blog/2025/07/15/jemalloc.html) typically don't return memory back to the kernel, even when calling `free()`. It keeps the memory around in case more is requested later.

This means that if tracking memory utilization we might not see it going down. A more useful approach is to track/profile memory allocations.

**Memory reclamation.** The kernel can reclaim memory back when there's memory pressure. First, it tries to evit memory backed by disk (non-anonymous). In this case, since the memory acts as a cache for the files on disk, it can drop them. In case no such memory can be evicted, it tries to swap anonymous page to disk. Finally, if it can't perform swaps it might attempt to kill specific processes (OOM killer).

**Shared Libraries.** If multiple processes use the same library, the kernel doesn't load a copy of them in memory for each process. It instead loads it once and maps the corresponding virtual address of each process to the same position in physical memory.

The tool `ps` reports resident memory for each process under the column `RSS` which includes the memory of its shared libraries. So if a shared library is shared by multiple processes, it's accounted for multiple times.

**pmap (tool).** This tool can be used to display anonymous and non-anonymous mappings for a process via

{% highlight text %}
pmap -x <pid>
{% endhighlight %}

It's most useful for the non-anonymous because it shows how much memory the main binary and shared library occupy. It breaks down them by write mode:

{% highlight text %}
Address           Kbytes     RSS   Dirty Mode  Mapping
0000000000001000  428564  138584   47248 r---- my_bin
0000000000003000  868184  377740       0 r-x-- my_bin
0000000000004000     580     516      16 r---- my_bin
...
0000000000011000  103284   89064       0 r---- libmy.so
0000000000013000   73060   60208       0 r-x-- libmy.so
0000000000014000    1392    1392      16 r---- libmy.so
...
0000000000026000     256     256     256 rw--- [ anon ]
{% endhighlight %}


## Filesystem

**Block vs. Extent.** Block-based filesystems store data in fixed-sized blocks. It can lead to a lot of blocks for large files, which adds overhead due to metadata per block. Extent-based filesystems tries to use a single block, called *extent*, defined by an offset + size, and if the file grows it *extends* the existing block instead of allocating new ones. This reduces overhead of having too many small blocks.

If it can't, because of some existing block, it then starts a new extent in a new location. It is still possible to have fragmentation this way, so some filesytems pre-allocate space to allow for growth.

**Copy-on-Write.** In filesystems implementing this, block data is immutable. If modifications need to be made, a new block with the modification is created elsewhere and pointers updated.

This model is more fault tolerance since if a crash happens during the write, the old data is intact. It can lead to performance overhead if the block is big.

**Volumes and Pools Storage.** I had trouble understanding the difference between these two. A mental model that makes sense to me is that: a volume is synonymous with virtual disk or block device. There's a 1:1 relationship between a filesystem and a volume.

A volume can be backed by exactly one physical disk or a portion of a physical disk. It can also be backed by multiple disks. In this latter case, the layer that abstracts this away from the volume - so that it "seems" a single disk - is the pool storage.

The way the book presents these concepts suggests these are mutually exclusive, which is confusing.

**Benchmarking.** When benchmarking disk performance we need to account for in-memory cache. If the reads are small, the data might be served entirely from cache and bypass the disk entirely. A way to clear page caches is via:

{% highlight text %}
echo 1 > /proc/sys/vm/drop_caches
{% endhighlight %}

## Disk

Most of the new things I learned were about SSDs (Solid State Drives).

**Flash.** is organized into pages (4-16KB) and grouped into blocks (1-16MB). Reads and writes are done based at the page granularity. Writes do not happen in-place. Instead they first read the page data, modify and then write in a different location, no matter how small the write is. Also, writes can only happen in erased pages and pages can only be erased in batch (blocks). So generally writes are slower than reads (up to 2-3x), as opposed to HDDs in which read and write performance are more similar.

**Lifespan.** SSDs are known for having limited amount of writes it supports. A low-end SSDs might support 1k writes on a given block. High-end SSDs can support up to 100k.

To reduce this problem, SSD controllers try to spread the load so that it doesn't keep writing on the same block over. The SSD might also be overprovisioned: internally it has more capacity than stated, so that the controller can afford having a few blocks go bad.

**Storage Interfaces.** These are standards defining how a storage device communicates with the host, not only the protocols but the hardware interfaces. For HDDs, SATA and SAS are often used. SATA stands for Serial ATA (*Advanced Technology Attachment*) and SAS stands for Serial Attached SCSI (*Small Computer System Interface*). SAS is more performant and robust, and is used by Enterprise systems. SATA is more common in consumer devices.

SSDs typically use the NVMe (Non-Volatile Memory Express) procotol, generally on top of the PCIe (*Peripheral Component Interconnect Express*) bus.

**Hard Disk Performance.** I learned about a funny experiment where Gregg shouts at a rack of hard disks and shows that if affects their performance ([Youtube](https://www.youtube.com/watch?v=tDacjrSCeq4
)).

## Network

**Network Interface Card.** or NIC. It's a component that connects the computer with the physical network. It has one or more ports (e.g. eth0, eth1, etc.), and a network controller: a microprocessor that transfers packets from the ports to the OS.

**Buffering.** TCP can buffer packets both at the sender and receiver level. Buffering can also happen lower in the stack, such as routers and switches.

**Three-way Handshake.** TCP specifies the three-way handshake protocol for a client-server to establish a connection. The client first sends a SYN packet to the server. The server replies with a SYN + ACK packet and finally the client sends an ACK packet.

The kernel keeps separate queues for connections that haven't been ACKed by the client and those that have. The reason is that it won't create sockets for connections on the first queue, to protect against a type of DDoS attack in which clients flood the server with SYN requests.

Once a connection is established (i.e. server receives ACK packet from client), it gets moved to the second queue and a socket is created. Then the program in user space can process an entry from the queue [`accept()`]({{blog}}/2020/03/07/sockets.html) that connection.

If there's a flood of SYN connections and the first queue gets full, it can drop packets. The rationale is that legitimate clients will do a retransmit. Packet drops can be used as a proxy for network saturation and explain connection latency.

**TCP Algorithms.** This book presents several algorithms for throughput improvement and congestion control. I'd like to study them in more depth, so I'll add it to my queue of topics to write about.

**Tools.** There are many tools listed for inspecting network including `netstat`, `sar`, `ifconfig`, `ip`, `nicstat`, `ping`, `traceroute` and `tcpdump`.

The way traceroute works is quite clever. IP packets have a TTL field, which is not time-based but "hop"-based: every time a router forwards a packet, it decrements the TTL by 1. If it's 0, it sends an error message back (*Time Exceeded*) with metadata about itself. So traceroute sends a package with TTL 1 to get the first hop, then another with TTL 2 and so on, until it stops getting error message. This allows it to trace the exact route taken to a destination!

**ICMP.** stands for *Internet Control Message Protocol* and we can see it as a type of packet for metadata (as opposed to data packets like TCP/IP). This is the type of packet used by ping and traceroute.

It can be used to probe for error handling because routers can return error messages via this packet. In the case of traceroute, if the TTL is 0, it returns a *Time Exceeded* ICMP packet.

## Virtualization

**OS Virtualization** is when there's a single OS, but it supports multiple tenants.

In **Hardware Virtualization** tenants are different OSes, so for example Window and Linux OSes can coexist in the same physical host. The component responsible for coordinating this is called a **hypervisor** and can be of type 1 or type 2.

A *Type 1* hypervisor, also known as "bare-metal", is its own specialized OS, called the host OS. It's more performant but requires hardware support. For example, Google Compute Engine and Amazon AWS use a regular Linux as the host OS with a kernel module called KVM (*Kernel-based Virtual Machine*) and a user-space program called QEMU (*Quick EMUlator*), while VMWare provides a custom lightweight OS called VMware ESX.

There is a type of virtualization called *paravirtualization* in which does not require hardware support but the host OS must be modified to support virtualization. Apparently AWS used to use this, but it got phased-out.

A *Type 2* hypervisor runs as a user space process on the host OS, such as *VMware Workstation* and *VirtualBox*, so it emulates everything outside of the kernel, so it can be slow.

From Systems Performance point of view, OS virtualization is much better, since it has fewer layers and the same tooling and metrics can be used for analysis.

**Balloon Driver.** This mechanism can be used by the hypervisor to transfer available memory from one OS to the other. Each guest OS has a balloon driver process. The hypervisor can instruct it to allocate memory (inflate) in the guest OS A without using it. Then in the baloon driver in the guest OS B can free memory (deflate) in the guess OS B, which then becomes available to that guest.

## Conclusion

I didn't get a lot of value from the performance analysis from this book. Perhaps because I'm not a performance engineer and I'm most of the time dealing with performance on the application level and at work we have a lot of custom in-house tooling.

I did find it quite a good refresher on OS concepts. I learned many new things about network and virtualization in particular.

The tools present overlap a lot and feel like an encyclopedia. The methodology feels scattered and abstract, even when applied to specific dimensions such as CPU and memory. On that node, my favorite chapter was the last one, where the author provides a case study on how we root caused some performance issue. I wish he provided more examples instead of going for completeness, but perhaps the goal of the book is to be used as reference.
