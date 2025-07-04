---
layout: doc
title: "Databases Cheat Sheet"
---

{% include blog_vars.html %}

Common definitions and terminology in distributed systems.

# Index
{:.no_toc}

1. TOC
{:toc}

## Transactions

**ACID.** stands for the initials of the 4 guarantees a transaction has:

* Atomicity - all-or-nothing.
* Consistency - invariants must continue to be valid after the transaction.
* Isolation - concurrent transactions behave as if executed sequentially.
* Durability - once a transaction commits, the changes are permanent.
