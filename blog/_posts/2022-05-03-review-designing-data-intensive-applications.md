---
layout: post
title: "Review: Designing Data Intensive Applications"
tags: [review, distributed systems]
excerpt_separator: <!--more-->
vanity: "2022-05-03-review-designing-data-intensive-applications"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/ddia.jpeg" alt="Designing Data Intensive Applications book cover" />
</figure>

In this post we will review the book *Designing Data Intensive Applications* by Martin Kleppmann.

If I had to summarize this book in a sentence I'd say it discusses several topics in database and distributed systems with a industry applications in mind. This is opposed to textbook versions of these topics which tend to be a lot more theoretical and academic.

This book covers a broad range of topics which makes it challenging to summarize in enough detail. Instead I'll go over the major themes and comment on specific things I learned and found interesting.

<!--more-->

## Organization of the book

The book is divided into 3 parts of a total of 12 chapters. In part I, the author covers some database topics, in part II he discusses distributed systems and in part III batch and stream processing in the context of data pipelines.

## Selected Notes

Here I’m going to over each chapter providing a brief summary and then a list of bullet points with my random notes.

### Chapter 1 - Reliable, Scalable and Maintainable Applications

This chapter discusses three of the most important non-functional aspects of data applications: reliability, scalability and maintainability.

Notes:

* Security is part of reliability.
* *Fault vs. failure*. Fault is a component of the system deviates from a spec, while failure is when the system as a whole stops providing a service.
* Hardware errors are usually independent, software errors are usually correlated.
* *Latency vs response time*. response time is the total time to process a request, latency is the time the request waited to be handled.
* *Tail latency amplification.* when a slow sub-request makes the whole request slow.
* Abstraction is one of the best tools to reduce accidental complexity.

### Chapter 2 - Data Models and Query Language

This chapter describes a few data models like the relational, document (key-value, NoSQL) and also graph models. It also describes different ways to access the data: imperative language, SQL and also DSLs, Domain Specific Languages, for the graph models.

Notes:

* NoSQL was a catchy (but misleading) Twitter hashtag for non-relational databases.
* Graph-like data models can make some queries very simple to express, e.g. using [Cypher](https://neo4j.com/developer/cypher/).
* *Schema on read vs. Schema on write.* this is an interesting way to contrast relational DBs and key-value stores. Relational DBs have a strict schema which is enforced on writes, while key-value stores don't enforce a schema so it has to be enforced at the application level during reads.

### Chapter 3 - Storage and Retrieval

This chapter describes the data structures used to store indexes of DBs, such as Hash Indexes, B-Trees and [LSM Trees](https://www.kuniga.me/blog/2018/07/20/log-structured-merge-trees.html). It presents the differences between **OLTP** (Online Transaction Processing) and **OLAP** (Online Analytic Processing) databases.

Notes:

* A B-tree with height 4, branching factor of 500 and pages of size 4kB can store 256TB!
* In memory DBs are faster not because they avoid reading from disk but because they don’t need to serialize/deserialize the data.
* *Stars and Snowflakes.* In a star schema consists of a fact table, e.g. user actions, which are related to multiple dimension tables, e.g. user info, country info. The snowflake schema is a generalization in which the dimension tables themselves can refer to sub-dimensions (we can picture a snowflake as a fractal star).
* Fact tables are fat tables: most of times not all columns are queried and many analytics operations require scanning all rows, so column-oriented storage is more efficient.
* *Data Cubes.* pre-computed tables for specific aggregations.

### Chapter 4 - Encoding and Evolution

This chapter describes ways to encode data in a way that supports evolution (i.e. schema can change in back-compatible ways). Examples include [Protocol Buffers](https://developers.google.com/protocol-buffers) and [Thrift](https://thrift.apache.org/). It also touches upon inter-node communication concepts such as REST and RPC.

* Data structures from memory cannot automatically be represented in disk, especially if they use pointers.
* Using automatic serialization like Python's [pickle](https://docs.python.org/3/library/pickle.html#module-pickle) can lead to security vulnerability.
* *Service Oriented Architecture* (SOA) got re-branded to microservices.
* REST is not a protocol, but a philosophy. APIs using its principles is called RESTful.
* RPC is more common for internal requests, while REST is more common for public APIs.

### Chapter 5 - Replication

Replication means duplicating data in multiple machines for increased throughput and fault-tolerance. This chapter discusses single-leader replication (master-slave), multi-leader replication and leaderless replication. It also delves into the issue of replication lag.

Notes:

* *Split brain*. The problem in which a leader that was thought dead comes back to life and competes with the new elected leader.
* *Read-your-writes consistency.* A consistency guarantee that a given user will not experience replication lag for their own writes.
* The set of server DBs plus local DBs in mobile phones in offline mode act as multi-leader database.

### Chapter 6 - Partitioning

Partitioning means segmenting the data into multiple machines for scalability and potentially increased throughput. This chapter discusses different partition strategies, re-balancing of partitions and routing of requests to the right partitions.

Notes:

* Consistent hashing is rarely used in practice for DBs.
* Partition in the presence of secondary indexes is tricky.
* The mapping between data and machine for routing can be either centralized (e.g. stored in zookeeper) or de-centralized ([gossip protocol](https://en.wikipedia.org/wiki/Gossip_protocol)).

### Chapter 7 - Transactions

Transactions allow multiple operations to be performed atomically, that is, either all operations succeed or none of them does. This chapter discusses transactions in a single machine and considers the different guarantees and tradeoffs that we can achieve. It delves into weak isolation and serializability.

Notes:

* The belief that transactions means low performance and low availability is false.
* ACID - Atomicity Consistency Isolation and Durability
  * Consistency is mostly performed by the application as invariants - should not be part of ACID
  * Isolation is a guarantee that any two transactions can be assumed to have happened  serially (i.e. serializability), but it's rarely used in practice (too costly).
* *Snapshot Isolation.* When all reads within a transaction see a consistent snapshot of the database. So if any other operation updated the data while the transaction is running it won't see these changes. This is a weaker guarantee than serializability.
* *Write Skew.* When a race condition happens in different objects but it violates some invariant that connects these 2 objects. This cannot be prevent by snapshot isolation.
* *2-Phase Lock.* Can be used to achieve serializability, but is slow in degenerate cases.

### Chapter 8 - The Trouble with Distributed Systems

In this chapter the author discusses transactions in the context of distributed systems. There are a lot more things to go wrong: partial failures, unreliable networks and unreliable clocks. In addition, distributed algorithms and protocols are based off models, which makes assumptions about what kind of failures can and cannot happen, which might not correspond 100% to reality.

Notes:

* Datacenters can query switches to check if a node is down (this is not possible in the general internet).
* Timeout is the only 100% reliable way to declare whether a node is down, but choosing the right timeout is tricky. Needs to be done experimentally.
* *Sync vs Async Networks.* Internet is an async network. Telephone network is sync: no queues and no delays but wasted capacity.
* Thread safety mechanisms do not translate to distributed systems.
* Most practical applications are not Byzantine fault tolerant.
* Theory vs practice: algorithm might be proven correct for a model but would not work in practice.


###  Chapter 9 - Consistency and Consensus

This chapter describes different consistency models: eventual consistency and strong consistency (linearizability). It also talks about ordering guarantees, distributed transactions and consensus. The author then makes a connection between ordering, linearizability and consensus.

Notes:

* *Linearizability.*: Behaves as if there was only one copy of the data and all operations on it are atomic.
* The author on the CAP theorem:

> Although CAP has been historically influential, it has little practical value for designing systems.

###  Chapter 10 - Batch Processing

In this chapter the author discusses map-reduce and makes an analogy with the unix philosophy: do one thing and do it well, and compose simple functions via pipes.

Notes:

* *Sushi principle.* Raw data is better. This funny quip was in the context that storing the source of truth as unstructured logs and having the structured data derive from it is much easier, for example in the case of needing to schema changes of the structured data, we just need to "re-derive" it from the structured logs.
* The original Map-Reduce framework proposed by Google is "overly" fault tolerant because at Google batch jobs were often preempted in favor of higher priority tasks. Most companies don't operate this way and don't need pay this overhead price.

###  Chapter 11 - Stream Processing

Opposed to batch processing is stream processing where the input is considered unbounded, and it arrives gradually over time. We can reduce stream processing to batch by accumulating the data for an amount of time (say a day) but this adds a delay.

Notes:

* *Event vs. Processing Time.* in stream processing we need to work with 2 time domains: one corresponding to when the event was generated (logged) and another when it's being processed by the engine. They might not align and the events might arrive out of order or be severely delayed. Doing time-based aggregations like moving windows can be tricky.
* *Change Data Capture.* we can model a relational database as a stream of events corresponding to updates to the DB. The result of processing all those events is the relational database.

###  Chapter 12 - Future of Data Systems

In this chapter the author shares a more personal take. From my understanding he advocates for a big architectural shift, on the lines of storing unstructured raw logs and having all other structured data be derived from it, via batch and stream processing. This would avoid many complicated issues around ordering (the logs define a total order) and consistency (one source of truth).

The author also touches on data privacy and the ethics of dealing with real-world data.

## Conclusion

Overall I learned a lot from this book. The author makes difficult topics digestible via examples and diagrams. I found the Chapters 7, 8 and 9 the most difficult.

I recently started working with stream processing and noticed Chapter 11 is not very comprehensive, though there's only so much detail one can cover in a general distributed systems book. I already have *Streaming Systems* by Akidau, Chernyak and Lax lined up.

## Related Posts

Many of the posts I wrote under distributed systems has been mentioned in the book:

* [Consistent Hashing]({{blog}}/2019/04/12/consistent-hashing.html)
* [Log Structured Merge Trees]({{blog}}/2018/07/20/log-structured-merge-trees.html)
* [Notes on Zookeeper]({{blog}}/2015/08/07/notes-on-zookeeper.html)
* [Paper Reading - Spanner: Google's Globally-Distributed Database]({{blog}}/2017/04/27/paper-reading-spanner-google's-globally-distributed-database.html)
* [The Paxos Protocol]({{blog}}/2014/04/14/the-paxos-protocol.html)
