---
layout: post
title: "Review: Software Architecture - The Hard Parts"
tags: [distributed systems, software engineering]
excerpt_separator: <!--more-->
vanity: "2025-03-22-review-llvm-core-libs"
---

{% include blog_vars.html %}


<figure class="image_float_left">
  <img src="{{resources}}/books/the-hard-parts.jpg" alt="Book cover." />
</figure>


In this post I’ll share my notes on the book *Software Architecture: The Hard Parts* by Neal Ford, Mark Richards, Pramod Sadalage and Zhamak Dehghani.

In summary, this book presents trade-offs between different ways to implement microservices. The authors present general guidance on how to split services and databases, and the tradeoffs involved.

The book has about 400 pages and 15 chapters and in this post I go over each of the chapters and then provide a summary and impressions at the end.

<!--more-->

## Chapter 1 - What Happens When There Are No "Best Practices"?

This first chapter explains the motivation of the book. First the "hard" means "difficult" as one would expect, indicating that choosing the best option is no trivial matter. It also conveys "hard" in juxtaposition with "soft"ware, the idea being that it provides high-level advice that is invariant with specific implementation and trends (it changes less frequently, like hardware).

It also introduces concepts such as *architectural decision records* (ADR) and *architectural fitness functions* used throughout the book.

Finally, it sets the stage for the example of the ticketing system that will be used alongside the subsequent chapters.

## Chapter 2 - Discerning Coupling in Software Architecture

Nice quote by Paracelsus, on how trade-offs is usually about finding the right balance:

> All things are poison, and nothing is without poison; the dosage alone makes it so a thing is not a poison.

This chapter is mostly about **coupling**, which the book defines as:

> Two parts of a software system are coupled if a change in one might cause a change in the other.

It also introduces the concept of **architecture quantum**, which basically corresponds to services in a microservice; and **static** vs. **dyanmic** coupling. Static coupling are dependencies you might see listed in build files, such as shared libraries and OS. Databases are considered static coupling. Dynamic coupling are runtime dependencies such as one service calling another.

Maybe one simplistic way of distinguishing between static and dynamic coupling is that without static dependencies your service can't start. Without dynamic dependencies it can, but it cannot run.

Static coupling binds two services into the same quantum, so for independent services to be in different quantum they must also have independent databases.

## Chapter 3 - Architectural Modularity

This chapter presents the benefits of having more modular components (as opposed to a monolith) and discusses two alternatives: the **service-based architecture** where the services have coarse granularity and the database shared by the services; and the **microservice architecture** where the services map to domains, having fine granularity, and even the data is split along these lines.

It presents a few dimensions to consider when choosing the right granularity (modularity drivers), including availability, maintainability, scalability and testability.

## Chapter 4 - Architectural Decomposition

Once it has been decided to decompose a monolithic application, one must determine whether it's even feasible, based on some metrics that correlate to coupling.

If it is, the authors suggest that even if microservices is the goal, moving to the more coarse grained service-based architecture as a first stepping stone might be more feasible.

## Chapter 5 - Component-Based Decomposition Patterns

This chapter describes a more detailed procedure to go from a monolith to microservices.

It goes from identifying groups of files or classes that are semantically related and group them into components, then identify classes that provide shared functionality (e.g. notification), and move them into their own components.

Then finding the dependencies between these components and finally group these components into larger groups called domain, aiming to minimize cross domain dependencies.

Each domain will then map to a service and turn into a service-based architecture and later then can be further split into smaller pieces.

## Chapter 6 - Pulling Apart Operational Data

In this chapter the authors touch on the splitting of the data. This is a necessary step to go from service-based architecture to microservices.

They present some reasons for splitting (*data disintegrators*), for example scalability, fault tolerance; but also for not splitting / merging them together (*data integrators*), for example transactions.

Finally, they provide a series of steps similar to *Chapter 5* for how to go from a single database to multiple ones. One advantage of having independent databases is that we can have mixed types such as relational, key-value, graph, etc. I had never heard of term *NewSQL DBs*, but turns out I knew of one such example, [Google Spanner](https://www.kuniga.me/blog/2017/04/27/paper-reading-spanner-google's-globally-distributed-database.html).

## Chapter 7 - Service Granularity

This is like the data integrators and disintegrators from *Chapter 6*, but for the services. Some reasons to split are scalability, fault tolerance and security.

More granular services enable more scalability because usually only a subset of the functionality must scale (the hot path), so if it lives in its own service, it can be scaled independently. Similarly, if a service goes down, it might not affect the others, which is desirable if this service is not on the critical path.

Reasons to keep services together is the need for transactions or if there's a need for complex coordination between the services (i.e. all services are on the critical path).

## Chapter 8 - Reuse Patterns

This chapter discusses how to deal with code that is shared by multiple services. Options include code replication (poor choice), shared library, shared service or service mesh.

The main downside of libraries is managing dependencies and handling version deprecation. For a shared service the main downside is performance.

A service mesh is a hybrid between shared library and service. It consists of a side car where the deployment is controlled by a centralized team (like in the service case) and it doesn't incur into the performance issues. The authors recommend using the service mesh for operational shared functionality such as monitoring and logging.

## Chapter 9 - Data Ownership and Distributed Transactions

The idea is to have each database be owned by exactly one service. The easiest scenario is when only one service writes to the DB, in which case it is made the owner. Reads from this DB by other services must go through this owner via RPC calls.

If multiple services write to the DB there are several options:

* Split a table (normalization) into multiple ones and into separate DBs if possible;
* Choose the service closest to the DB domain-wise or the one with highest throughput;
* Do nothing and assume co-ownership;
* Merge the services - if we can't separate the data, it could be a sign the services belong to the same domain.

If most services write to the DB, then we can create a new dedicated service for handling the reads and writes, which would then become the owner of the DB.

Once the data is split, achieving ACID transaction is very hard (that's why it's a disintegrator), so microservices often opt for eventual consistency. This can be achieved via:

* Background synchronization: a separate process periodically keeps the DBs in sync. The main disadvantage is the delay in achieving the consistency (depending on the period).
* Orchestrated requests: this consists of an orchestrator that handles both the requests but also takes care of the eventual consistency. It can be tricky to implement when errors occur and some sort of rollback is needed.
* Event-based: in this model, changes (events) necessary to achieve eventual consistency are added to a distributed queue and processed by a background process. It can also be hard to handle errors.

## Chapter 10 - Distributed Data Access

Once data ownership is resolved, how does a service access data it does own? We mentioned doing it indirectly through the owning service but there are alternatives, especially to work around the performance of that approach: replicate the data into the DB the requesting service owns or having a in-memory cache with the data, and have a separate process periodically update the cache. In both cases the major disadvantage is in how to achieve consistency.

## Chapter 11 - Managing Distributed Workflows

Once the services are separated, there are two main ways of handling requests: centralized (orchestration) or distributed (choreography). The latter option is more scalable since responsibility is distributed across services, but handling complex workflows and errors is difficult.

## Chapter 12 - Transactional Sagas

Three dimensions of a microservice are considered: communication (synchronous or asynchronous), consistency (atomic or eventual) and coordination (orchestrated or choreography).

This leads to 8 possible combinations, which the authors call **sagas** and provide names for, e.g. the *Epic saga* which corresponds to: synchronous, atomic and orchestrated. Some of them are not very useful and exist just for completeness, e.g. the *Fantasy Fiction*: asynchronous, atomic and orchestrated (generally pairing asynchronous communication with atomic consistency is bad).

This chapter also talks about using a state machine to help implement eventual consistency, which provides a more cohesive view of the distributed parts. It feels like this should have been discussed in *Chapter 11*.

## Chapter 13 - Contracts

Contracts specify how services communicate between each other. On the loose side of the spectrum is JSON, while on the strict side is gRPC with GraphQL and REST in between. This seems analogous to the tradeoffs between untyped and strictly typed languages.

One interesting concept is *consumer driven contracts*. In this case the consumer specifies what information it needs and pass that contract to the provider, which can then use this contract in its integration tests.

If this contract is specific enough, it can be very useful! We recently had an issue at work where one service changed the exception type thrown, which broke the client. If the dependency on a specific exception was encoded in a contract, it would have prevented the issue. I don't know how feasible this is to implement though.

## Chapter 14 - Managing Analytical Data

A logical step once services and data are distributed is to also split the analytical data. This chapter discusses the data warehouse and data lake patterns but suggests the data mesh pattern, which aligns more naturally with microservices.

The idea is that each microservice provides its own analytical data which can be queried by analytical engines. Like in the service mesh (*Chapter 8*), shared operational functionality such as privacy is implemented as a sidecar.

The writing of this chapter felt less objective and more jargony than the rest of the book. Some examples:

> Dash mesh is a *sociotechnical* approach (...)


> (...) necessary to ensure that domains provide their data in a way that *delights* the experience of data consumers (...).

> Allows *excellent* decoupling between analytical and operational data

and sometimes vague:

> Finding ways to support eventual consistency, (...), allows many patterns that don't impose other difficulties.

Also it's unclear how a query engine can work with a federated set of data, and how well it scales. It seems like details are in one of the author's book on this topic, so this chapter felt a bit like an upselling.

## Chapter 15 - Build Your Own Trade-Off Analysis

While the book provides options and pros-and-cons, each problem is unique and decisions cannot be automated. This chapter provides some guidance on how to make tradeoffs.

On qualitative vs. quantitative analysis:

> We recommend you hone the skill of performing qualitative analysis, as few opportunities for true quantitative analysis exist in architecture.

MECE stands for *Mutually Exclusive* and *Collective Exhaustive* which in short is to make sure we compare apples to apples (e.g. cannot compare a simple message queue with an enterprise service bus, since the latter has way more functionality).

Make sure not to include points irrelevant to the problem at hand (out-of-context trap). For example, if your system doesn't need scalability, it should not be a factor in the trade-off comparison.

One approach I found particularly useful is to do trade-offs on the end user domain. Instead of thinking about latency aspects of synchronous vs. asynchronous communication, how does that choice impact the end user experience?

## Conclusion

I was misled by the book title because I didn't associate distributed architectures with microservices, but upon reading it, it makes sense. I probably wouldn't have read this book if it used "microservices" instead and since I did learn useful things, I turned out to be a positive misleading.

The use a ficticious story of a ticketing system as example and applying the concepts of each chapter to it makes it much easier to understand them.

Despite having over 400 pages, the book is not very dense in information, there's some repetition of concepts across chapters and heavy use of images and tables. Some of the images are very useful, especially when it shows the before and after. Others felt unecessary, like in *Chapter 12, Transactional sagas*, where for each of the 8 possible combinations of dimensions, the corresponding cube is shown.

At points it feels like the restructuring is guided by blind principles (decouple decouple decouple), rather than practical need. In *Chapter 9* for example, they decide to split the database into different instances based on domain, and have exactly one service be the owner of that instance. It required massive undertaking for migrating the data, contortionisms to make the system support real world use cases while also adhering to the principles. And in the end it feels like it swapped one set of problems with another.

For a book that has 4 authors, the writing felt pretty consistent and for the most part cohesive. On the other hand, some pieces did seem to be in the wrong chapter or there was discussed in multiple chapters, and in *Chapter 14* we can clearly identify the writer.

Overall I learned useful information. At work, we partner with a control plane team and I see many ideas listed in this book implemented there. Under the definitions of this book they use a service-based architecture because all the data is centralized in one DB.

They also have an orchestrator which uses state machines for handling requests and all DB access is done through a dedicated service. It also uses a mix of asynchronous and synchronous communication. Eventual consistency is implemented closer to the *orchestrated requests* and it's a known difficulty to handle partial failures.
