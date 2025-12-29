---
layout: post
title: "The Paxos Protocol"
tags: [distributed systems]
---

<figure class="image_float_left">
    <a href="http://kunigami.files.wordpress.com/2016/03/lamport.png"><img src="{{site.url}}/resources/blog/2014-04-14-the-paxos-protocol/2016_03_lamport.png" alt="lamport" /></a>
</figure>

[Leslie Lamport](http://en.wikipedia.org/wiki/Leslie_Lamport) is an american computer scientist. He's known for his work in distributed systems and for the LaTeX system.

He was recently the recipient of the [2014 Turing award](http://amturing.acm.org/award_winners/lamport_1205376.cfm) for his work in distributed systems. Among his contributions, the Paxos protocol for solving the consensus problem is one of the most famous. Several real-world applications are built using the Paxos protocol such as Yahoo Zookeeper and Google's Chubby.

In this post, which we'll talk we'll provide a brief overview of this protocol.

## Introduction

Consider a set of processes running in a network, where failures may happen. The **[consensus problem](http://en.wikipedia.org/wiki/Consensus_(computer_science))** requires these distributed processes to agree on a single value.

More precisely, processes communicate with each other through asynchronous messages. These messages  can take arbitrarily long to be delivered, can be duplicated and can be lost, but we assume no [Byzantine failures](http://en.wikipedia.org/wiki/Byzantine_fault_tolerance) may happen.

Also, processes may fail, but it can be restarted and restore information from before the failure.

To solve the consensus problem, Leslie Lamport devised a protocol named **Paxos**. There are actually many variants of this algorithm with different trade-offs but here we'll discuss one of the simplest versions.

The protocol is named after a Greek island named Paxos. According to [3], Leo Guibas, a greek professor at Stanford, suggested the name.

## Definitions

There are three roles that processes can have in the protocol: the proposer, the acceptor and the learner.

A **proposer** is the process that makes proposals of values. Most of the time, only one process acts as proposer (we'll discuss later when we can have multiple proposers), thus it's also called the **leader**.

An **acceptor** can accept (multiple) proposals. A subset of acceptors that represents a majority is known as **quorum**. When a proposal has been accepted by a quorum, we say that the proposal was **chosen**.

After a proposal is chosen, the proposer informs the processes known as **learners** about the chosen value.

## Protocol

In the Paxos protocol the proposals have unique **labels**, which are natural numbers used to order the proposals.

The Paxos protocol has 2 phases. In the first phase, the leader will basically try to learn if any value has already been accepted by the acceptors. It will then pick a value that will guarantee the correctness of the algorithm.

In the second phase, it will propose the picked value from phase 1 to the acceptors. After a quorum of acceptors acknowledge the leader that they accepted the proposal, the leader can inform the learners.

More specifically, the protocol is as follows:

### Phase 1 - Prepare

**Proposer**

  First, the proposer picks up an unique number $$n$$ and broadcasts a *prepare request* with label $$n$$ to the acceptors.

**Acceptor**

When an acceptor receives a prepare request with label $$n$$, it will ignore it if it has already received a prepare request with label greater than $$n$$. Otherwise, it will promise not to accept any request (prepare or accept) with label less than $$n$$ from now on. It will then return a proposal with the highest label less than $$n$$ that it has already accepted (or null if it has no such proposal).

### Phase 2 - Accept

**Proposer**

If the proposer receives responses from a quorum of the acceptors, it selects, among all the proposals returned, the one with the highest label. Let $$v$$ be the value of such proposal (if no proposal was returned, the proposer is free to pick up any value). The proposer then sends an *accept request* to each of those acceptors with value $$v$$, and label $$n$$.

If it didn't get a response from a majority of acceptors, it may repeat phase 1 after a timeout.

**Acceptor**

If an acceptor receives an accept request with value $$v$$ and label $$n$$ it accepts this request, unless it has already responded to some other prepare request with a label higher than $$n$$.

If the acceptor accepts the request, it sends a response notifying the proposer. If the proposer gets a confirmation from a quorum, it can now broadcast the value to the learners.

## Properties

Intuitively, when a proposal with value $$v$$ gets accepted by a majority of acceptors $$S$$, then the next value a proposer will get from phase 1 is necessarily $$v$$, since any other majority includes some element from $$S$$. Inductively, at any point the value obtained from phase 1 by a proposer will be $$v$$, thus we have the following property [2]:

> **Property 1.** If a proposal $$v$$ and label $$n$$ is chosen, then every proposal issued with a higher label have value $$v$$.

## What could go wrong?

We can think of some scenarios of failures to understand why some steps are necessary for the correctness of the algorithm.

**Failure of a learner or failure of less than half of the acceptors.** In this case the protocol is still capable to obtain a quorum and inform the alive learners about the proposed value. By relying on a majority instead of all acceptors, the algorithm becomes more fault-tolerant.

**Failure of leader** A leader may fail in different stages of the protocol. If it fails after getting a response from a quorum, but before broadcasting the accept requests, the new leader will start fresh, because acceptors only store information after they accept an accept request.

The leader might be sending accept requests, and a quorum of acceptors accept it, but while the leader is sending the value value $$v$$ to the learners, it fails. So what happened here is that a proposal was chosen but the new leader might not know about it. It might try to propose a new value to the acceptors, but due to *Property 1*, if it's accepted we can assume it has the same value $$v$$, so we can guarantee that all learners will get the same value.

**Multiple leaders.** Since a leader can fail, the system needs to be able to elect a new leader. The problem is that the original leader might recover in which case we may have multiple leaders. Even in this case, *Property 1* guarantees the correctness of the algorithm, but it may never terminate.

**Failure and recover of an acceptor.** The leader might be sending accept requests, and the minimum possible quorum accepts it, but one of the acceptors fail before sending the accept response. The original leader will think it didn't get a quorum. Consider the case in which another leader proposed a different value and managed to get a quorum of acceptors. By this time, the acceptor that failed can recover and finally send the accept response to the first leader. In this scenario we wouldn't have achieved a consensus. But again, due to *Property 1*, it's guaranteed that the value proposed by the second leader is the same as the one accepted by the first quorum.

## Conclusion

It has been a while since I've been wanting to start studying distributed algorithms. I had a distributed systems course during my undergrad, but I didn't learn much at the time.

The paper "Paxos Algorithm made simple" [2] was very hard for me to understand and I'm not completely sure I did. Complementing with other resources on the internet helped me getting a better grasp at the protocol [1, 4, 5].

## References

* [[1]("http://en.wikipedia.org/wiki/Paxos_(computer_science)")] Wikipedia - Paxos
* [[2]("http://research.microsoft.com/en-us/um/people/lamport/pubs/paxos-simple.pdf")] Paxos Algorithm made simple
* [[3]("http://research.microsoft.com/en-us/um/people/lamport/pubs/pubs.html#lamport-paxos")] My Writings - Leslie Lamport
* [[4]("https://www.cs.columbia.edu/~du/ds/assets/lectures/lecture17.pdf")] Distributed Systems - Lecture 17: Agreement in Distributed Systems: Three Phase Commit, Paxos
* [[5]("http://the-paper-trail.org/blog/consensus-protocols-paxos/")] Consensus Protocols: Paxos
