---
layout: post
title: "Notes on Zookeeper"
tags: [distributed systems]
---

<figure class="image_float_left">
    <a href="https://www.mapr.com/sites/default/files/zookeeper-image.png"><img src="{{site.url}}/resources/blog/2015-08-07-notes-on-zookeeper/2016_07_zookeeper-logo.png" alt="zookeeper-logo" /></a>
</figure>

Since I've read about the Paxos protocol, I've been wanting to learn more about Zookeeper. I've collected some notes of this system based on the official documentation and blog posts about the subject.

We'll understand in a high-level how Zookeeper works, which guarantees it provides and some applications we can construct using the base API.

## Introduction

Zookeeper is a distributed coordination service for distributed applications. It was developed at Yahoo! by Ben Reed and Flavio Junqueira in 2006 and open-sourced in 2007.

It can be deployed in a set of servers (called **ensemble**), in which one is elected a **leader** and the others are called **followers**. If a leader fails, one of the followers is promoted to a leader.

Each server running zookeeper maintains a copy of a tree structure. The nodes in the tree, called **znodes**, can hold data and are referred by filesystem-like paths, that is, a list of node names concatenated with "/". Each server also has a replica of the tree in a persistent storage, for recovery.

Clients can connect to one of these hosts to perform CRUD (Create, Read, Update, Delete) operations on the nodes of the tree. All write requests goes to the leader. It then broadcasts the update proposals to the followers, that have to obtain a quorum (at least half) to agree on the proposal, so we have guarantees the data in all followers is consistent (Zookeeper uses the [Paxos protocol]({{site.url}}/blog/2014/04/14/the-paxos-protocol.html) for that purpose). Every update in a znode increases its version number.

A given znode can be marked as **sequential** and/or **ephemeral**. If it's marked as sequential, a sequence number will be appended to the node name when creating it, which Zookeeper guarantees is unique. If it's marked as ephemeral, the znode will be deleted when the client that created the node ends the connection.

Clients can subscribe to changes on a given node of the tree via **watches** by providing callbacks, which are called when the event it subscribes to is fired. The events are only fires once, no clients have to setup the watch again after the first firing.

Similar to a filesystem, it's possible to associate create, read, update and delete permissions to znodes. These permissions are refereed to as ACL (access control list).

## Guarantees

Zookeeper is a simple system, but it provides a set of guarantees that is otherwise hard to get right in practice:

* *Sequential consistency* - If a client requests a sequence of updates to the tree, they will happen in the same order
* *Atomic updates* - Either the update succeeds or fails, with no partial updates
* *Single System Image* - The client will see the same tree no matter to which server it connects to
* *Reliability* - Updates made to the tree will never be lost
* *Timeliness* - The tree the clients view are up-to-date within a certain amount of time.

## Applications

One of the main interesting things about Zookeeper is that it provides a very small API that does general things well, so it can be used for different purposes. We'll now cover some of them described in [[1](http://zookeeper.apache.org/doc/trunk/recipes.html)].

**Barrier.** Barriers are useful for synchronizing distributed nodes. They all block until a condition is met and the barrier is removed. One way to implement this mechanism in Zookeeper is to have all clients watch for a given znode, and delete the znode when the barrier is complete.

The Zookeeper recipe wiki [[1](http://zookeeper.apache.org/doc/trunk/recipes.html)] describes a recipe for implementing double barriers.

**Distributed queue.** A distributed queue is a data structure similar to a regular queue, but it's available to a distributed set of clients, which can either enqueue an element at the end of the queue or retrieve an element from the front.

We can use Zookeeper to implement a simple distributed queue. We create a root znode representing the tree. Inserting an element in the queue corresponds to create a new node under that znode, while retrieving an element corresponds to removing the corresponding znode.

To make sure the order is respected, we can mark the nodes created as sequential, so they have a number consistent with their creation order. For insertion, Zookeeper will handle race conditions for us with the sequential nodes.

To dequeue an element we need to handle race conditions in which two clients are trying to dequeue an element at the same time. One simple way to do this is by having a client getting the list of elements in the queue (get all children from the root node), sorting them and try to remove the element with the smallest sequence number. If the znode is already deleted by another client, keep trying with the next node in the sequence, until it tries all elements it has downloaded. After that it may need to get a new list of elements, since they can have been inserted since the first call. If it returns empty, then it can throw an exception due to the empty queue.

In [[2](http://blog.cloudera.com/blog/2009/05/building-a-distributed-concurrent-queue-with-apache-zookeeper)], Henry Robson writes about Zookeeper and how to write a distributed queue using this Python API. He covers the design mentioned above in more details.

One special interesting consideration is dealing with failures during insertion. When performing an enqueuing, the connection between the client and the Zookeeper server might go away before the client can tell if it succeeded. Retrying might lead to a node being inserted twice and even checking if the node is not in the queue can't guarantee it was not inserted, since it might have been dequeued already. One solution is using a flag that the client can set atomically only when a node is successfully enqueued. When dequeuing a node, the client ignores it if it doesn't have the flag set. This way, if the enqueuer client can't guarantee a node was inserted, it won't be processed, so it can retry until it manages to set the flag.

[[3](https://cwiki.apache.org/confluence/display/CURATOR/TN4)] discusses some drawbacks of using Zookeeper to implement a queue.

**Distributed lock.** Distributed clients can try to acquire the lock. If the lock is available, the client successfully acquires it. Otherwise they block until the lock is released.

We can use a similar idea we had for the queue, creating a root znode representing the lock and when a client tries to acquire a lock, it does so by creating an ephemeral sequential node. It then gets the children of the directory and if it has the lowest sequence number, it has acquired the lock. Otherwise, it picks the highest sequence number that is less than itself in the directory and set a watch on that znode and waits.

When the watched znode is deleted, the current znode will wake up and can try to acquire the lock again. Note that with this design, at most one node is watching another node. If we otherwise chose to watch any changes in the directory, every time a node was deleted, it would wake up all waiting nodes, which would try to acquire the lock at the same time, causing a [thundering herd problem](https://en.wikipedia.org/wiki/Thundering_herd_problem).

The recipes [[1](http://zookeeper.apache.org/doc/trunk/recipes.html)] wiki page also describes a distributed read-write lock design.

**Master election.** A set of clients can use a zookeeper ensemble to elect a master among them. First, each client creates an ephemeral sequential znode and whoever has the lowest sequence number gets to be the leader. The other clients find out which znode has the immediately before their znode in the sequence and set a watch on them.

The clients will keep a connection open and keep sending heartbeats signaling they're alive. If the current master crashes or hangs, their corresponding connection will be closed, the ephemeral znode deleted and the client that was watching this node can be promoted to master.

[Curator](http://curator.apache.org/index.html) is an open source project started by Netflix and aims to implement common recipes on top of Zookeeper and provide them as a library.

## References

* [[1](http://zookeeper.apache.org/doc/trunk/recipes.html)] ZooKeeper Recipes and Solutions
* [[2](http://blog.cloudera.com/blog/2009/05/building-a-distributed-concurrent-queue-with-apache-zookeeper)] Cloudera - Building a distributed concurrent queue with Apache ZooKeeper
* [[3](https://cwiki.apache.org/confluence/display/CURATOR/TN4)] Apache Curator - Tech Note 4

## Conclusion

In this post we learned a little bit about Zookeeper and saw some applications that can be built on top of it.

The motivation for this post was a tech that Patrick Hunt - one of the earlier contributors to Zookeeper and now at Cloudera - gave at AirBnB:

[https://www.youtube.com/watch?v=mMG-wagNBwY](https://www.youtube.com/watch?v=mMG-wagNBwY)
