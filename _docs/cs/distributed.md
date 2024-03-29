---
layout: doc
title: "Distributed Systems Cheat Sheet"
---

{% include blog_vars.html %}

Common definitions and terminology in distributed systems.

# Index
{:.no_toc}

1. TOC
{:toc}

## Two-Phase Commit (2PC)

It's a protocol to guarantee consistency of distributed transactions. It guarantees that either all transactions will succeed or none will be performed (they'll rollback). The protocol requires the election of a coordinator among the participant transactions and such election can be performed using the Paxos protocol.

In the first phase, the leader must obtain an agreement from all participants and after that it starts the second phase and sends a message to each participant informing them to proceed with the transaction.

## Wound-wait lock

*Wound-wait* lock is a mechanism used between transactions to prevent deadlock [1]. More specifically, assume we have transactions $T_1$ and $T_2$ with associated unique timestamps and $T_2$ is currently holding a lock. Let $t(T)$ be the timestamp of a transaction $T$. We have two scenarios: either $t(T_1) \le t(T_2)$ or $t(T_1) \ge t(T_2)$. In the first case, $T_1$  is older than $T_2$  and the protocol says that $T_2$  should abort, rollback and re-tries later with the same timestamp. We say $T_1$  *wounds* $T_2$ . In the second case, $T_1$  is younger than $T_2$  and it's allowed to *wait* until the resource is available.

A converse approach is the **wait-die** mechanism. The comparison of these methods is explained [here](http://stackoverflow.com/questions/32794142/what-is-the-difference-between-wait-die-and-wound-wait).

## Write-Ahead Log

A write ahead log consists in writing to an append-only file before a final commit operation is performed.

An analogous idea can be seen in [LSM Tress]({{blog}}/2018/07/20/log-structured-merge-trees.html).

## References

* [[1](http://dl.acm.org/citation.cfm?id=320260)] System level concurrency control for distributed database systems
