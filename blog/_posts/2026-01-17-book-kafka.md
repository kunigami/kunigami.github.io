---
layout: post
title: "[Book] Kafka: The Definitive Guide"
tags: [distributed systems]
excerpt_separator: <!--more-->
vanity: "2026-01-17-book-kafka"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources}}/books/kafka.jpg" alt="Book cover." />
</figure>

In this post I'll share my notes on the book *Kafka: The Definitive Guide. Real-Time Data and Stream Processing at Scale* by Gwen Shapira, Todd Palino, Rajini Sivaram and Krit Petty.

This book covers many aspects of the popular open-source Kafka, a distributed queue.

<!--more-->

## Book Summary

I read the 2nd edition of the book. It has 457 pages including Appendix, divided in 14 chapters. I skimmed most of the code snippets and chapters explaining configuration parameters since I was mostly interested in Kafka's high-level architecture (I don't use Kafka day-to-day).

**Architecture.** *Chapter 1* provides a high-level overview of Kafka. *Chapter 3* and *4* dives into Producers and Consumers, respectively. *Chapter 6* dives into the core of Kafka.

**Features.** In terms of feature set (functional and non-functional) and applications, *Chapter 7* describes Kafka's reliability, and *Chapter 8* explains how to achieve exactly-once semantics. *Chapter 9* explains how to move data from/to Kafka to/from some other database, while *Chapter 14* explains stream processing, i.e. how to transform data from Kafka.

**Operations.** *Chapter 2* explains how to install it, including dependencies such as [Zookeeper](https://www.kuniga.me/blog/2015/08/07/notes-on-zookeeper.html). *Chapter 5* explains Kafka how to manage operations (e.g. creating topics) programmatically. *Chapter 12* covers similar ground, but more manually, via CLI. *Chapter 10* explains how to replicate data across data centers, *Chapter 11* how to secure Kafka data and *Chapter 13* how to monitor Kafka.

As I said, I was mostly looking into learning about how Kafka works. So the most useful chapters for my needs were *Chapters 1, 3, 4, 6, 7, 8* and *14*. These are the ones covered in my notes.

## Kafka Overview

At the most basic level, Kafka is a distributed queue: you can have multiple hosts write data to it (producers) and have multiple hosts read from it (consumers) in a FIFO manner. One key feature from Kafka is that reading the data doesn't remove the data from the queue. This allows multiple consumers to read the same data and even replay old data as long as its within Kafka's retention.

**Messages and Batch.** A *message* is equivalent to a row in a database. For performance reasons, the unit of data in Kafka is a *batch* of data. How a batch is encoded is transparent to Kafka (of course it must be consistent between producers and consumers) but a typical serialization used is [Apache Avro](https://avro.apache.org/).

Relatedly, data can have an underlying schema, but these are also transparent to Kafka. The schema must be stored externally to be used by both producers and consumers.

**Topics, Partitions and Segments.** Kafka is actually a set of queues, not a single one. Each queue is called a *topic*.

For each topic there's a corresponding set of `N` *partitions*. Each  partition is replicated in one or more host (broker). The broker that contains the "master" replica is called the *leader*, while those containing the "slave" replicas are called the *followers*.

Note that a partition is a logical concept. At a broker level, it's implemented by a set of files called *segments*.

**Broker and Cluster.** Is synonym with a host, but refers specifically to hosts holding partitions (producers and consumers are not brokers). Note that a host stores partitions from multiple topics.

*Cluster* is a logical grouping of *brokers*. In practice it seems like a cluster corresponds to a cluster deployment on a datacenter. The key constraint is that communication between brokers in a cluster must be fast.

Cross-cluster replication is possible, but it's not used in the production path.

**Consumers and Producers.** Producers are a set of client hosts that write data to a specific topic. Multiple producers can write to the same topic.

Consumers read data from a topic. In general there's too much data in a topic for a single host to be able to process it, so multiple consumers must be used to read the data in parallel. These form a *consumer group*, in which each member reads a unique portion of the data from a topic. Multiple consumer groups can read data from the same topic.

**Checkpointing.** Consumers may crash, the consumer group might scale up or down, so Kafka must know exactly up to which point a given consumer read its data. This is known as a checkpoint.

Since multiple consumer groups can read data from a topic, Kafka needs to store multiple checkpoints. This is stored colocated with the actual data in the broker.

**Compaction.** Kafka has the feature where messages can define a key (usually a column of the message) and only the last message of a given key is kept. This makes is kind of a key-value store, but the main purpose of this is to save space, so if a key is not needed, space can be reclaimed.


## Architecture

### Writing Data

The client wanting to write to Kafka must use a Kafka producer library. In the producer configuration, the user must provide the server address of at least one broker. This broker serves as a initial point of contact to return metadata about the other brokers in the cluster. With this information, the producer can talk directly to brokers it wants to write data to. There's no intermediate gateway of sorts.

**Partitioning.** The producer requires the user to provide a key column which it will use to determine to which broker it should send the data to. This means the assignment is sticky: messages with the same key always go to the same partition. This also means it's subject to hotkeys: if all keys are the same, only one broker will receive all the messages.

**Batching.** This producer doesn't send data right away, it batches it for performance. Should the sending fail there are retry mechanisms provided. The user is responsible for handling client failures. If no persistence mechanism exists, if the client crashes the buffered data is lost.

Note within a batch all messages in a batch must belong to the same partition.

**Serialization.** The producer handles serialization from user format to bytes and the user can provide a custom serializer. It attaches a piece of metadata describing the schema of the data. It is not the full schema because the overhead would be prohibitive but rather a schema ID. It assumes the schema can be looked up from this ID. This enables non-back compatible schema evolution: if a producer serializes with schema ID X, and then later starts to write with schema ID Y, the consumer will know to deserialize the data with schema X or Y.


### Reading Data

The client wanting to read from Kafka must use a Kafka consumer library. As mentioned before, each client is a member of a consumer group. It's possible to scale in or out a consumer group by removing or adding more clients to the group. The consumer library will know how to re-distribute reads to them.

Sometimes we need key affinity, for example if the client aggregates the data and needs all the data with the same key to go to it. It's possible to configure static assignment.

The way it works is that the consumer is by staying in an infinite loop and constantly calling `poll()` on the consumer client.

{% highlight scala %}
while (true) {
  ConsumerRecords<String, String> records = consumer.poll(timeout);

  for (ConsumerRecord<String, String> record : records) {
    int updatedCount = 1;
    if (custCountryMap.containsKey(record.value())) {
      updatedCount = custCountryMap.get(record.value()) + 1;
    }
    custCountryMap.put(record.value(), updatedCount);
    JSONObject json = new JSONObject(custCountryMap) ;
    System.out.println(json.toString());
  }
}
{% endhighlight %}


**Checkpoints.** As discussed earlier, Kafka uses checkpoints to know at which point of the queue a given consumer is reading. By default, Kafka commits a checkpoint for the data returned by a `poll()` when the next `poll()` is called. Assuming you don't call the next `poll()` until you make sure the data is committed to the sink, this guarantees at-least-once semantics.

The consumer also has the flexibility to decide when Kafka saves a checkpoint, by turning off automatic commits and calling an explicit method, `commitSync()`. There's an async version `commitAsync()` which doesn't block during checkpoint but also doesn't handle retries, so some care must be taken to ensure the right semantics. Exactly-once-semantics will be covered later.

Some care must also be taken when repartitioning happens. Suppose we add a new member to the consumer group. Then some existing members will "lose" the partitions they're reading from, so they must commit a checkpoint before that happens. It's possible to subscribe to such events.

The broker that keeps tab of offsets is called the *group coordinator*, and it is also the partition leader of a topic called `__consumer_offsets` (partitioned by the consumer group ID). This topic is only used for durability in case the leader crashes and the offset map must be reconstructed.

### Cluster Membership

Kafka uses Zookeeper as a source of truth to which brokers are part of a cluster. Brokers must periodically send a heartbeat to the Zookeeper ensemble otherwise they're considered dead.

Zookeeper is also used to elect a leader among the brokers, called the **controller**. The controller is responsible for deciding which brokers are the leader of a partition and which are the followers, so it must know when brokers leave or join the cluster.

### Replication

The replication factor Kafka suggests is 2-3 but not more than that. Writes happen to the leader replica and typically reads are also only from the leader, but the latter can be changed to improve performance. The problem is that Kafka uses eventual consistency, so the replicas are usually not in sync.

The follower constantly asks for data from the leader, so the leader has an idea on how far behind each follower is. Followers lagging more than a configured time (10s by default) behind are considered *out of sync*, otherwise they're *in-sync* replicas.  On leader failure, only in-sync replicas are candidates for leader election.

### Retention

Each partition is implemented via a set of files. By default each file contains either 1 GB or 1 week worth of data, whichever is smaller. When either of these limits is reached, Kafka starts writing to a new file.

The file contains metadata of the date range contained in it, so an asynchronous process can constantly delete files where all messages in it are out of retention.

### Indexes

Kafka stores an index that maps logical topic offsets to actual files and file offsets. By default the granularity of the index is 4KB, i.e. for every 4KB of data written a entry is added to the index. This index also allows lookup by timestamp.

An index is a tradeoff between storage and compute. If we store every single message in the index it would take too much space. No index would make lookups take too long.

This index is needed because consumers can specify a time in the past for replay or to recover from a checkpoint.

### Compaction

To implement the compaction, a background thread process a segment. It builds a in-memory hash table indexed by keys, containing the offset of the most recent message for that key. Then it does a second pass and it filters out any message that has a corresponding entry in the hash map with a smaller offset, and creates a new segment. The book is very confusing about this process, suggesting this is done in one pass.

Note that this is an "eventual" compaction. Consumers reading from the past might still see "dirty" messages. It's assumed that clients reading from compacted topics are themselves writing to some sort of key value store that also retains only the last entry.

A similar mechanism is used even for non-compacted topics, when data must be deleted. The producer might indicate a message called a *tombstone*, which is basically a message with the key to be deleted and a null value. Consumers are supposed to handle this tombstone: if they process this message

## Exactly-Once Semantics

By default, Kafka offers at-least-once semantics. This means that on consumer restart or repartitions, it guarantees that all data it stores will be sent to the consumer but it might send the same data more than once.

Achieving exactly-once is very difficult because the process of reading, processing and writing data must become atomic, so Kafka must be involved in the whole process. Since Kafka must be aware of the writes, a limitation of this process is that the sink of the application must also be a Kafka topic.

*Chapter 8* describes the setup necessary.

### Idempotent Producers

To start, the producer must be made idempotent. This means that each message will contain extra metadata: the producer ID and a sequence number, which will be used to uniquely identify a message and identify gaps. For each partition, the broker will keep the last N messages from each producer. If it gets a sequence already in this N messages, it will simply deduplicate.

If it gets a sequence number out of that range it will fail. If the last sequence number it has is `x`, and it gets `x + 2`, it will detect a gap in the sequence and fail as well.

### Transaction

The transaction Kafka implements is by handling both the writes to the sink and the checkpointing. The producer writing to the sink must be marked as transactional. Here's an example of a write with transactions (compare this with the simpler example in *Reading Data*):

{% highlight scala %}
producer.initTransactions();

while (true) {
  try {
    ConsumerRecords<Integer, String> records = consumer.poll();
    if (records.count() > 0) {
      producer.beginTransaction();
      for (ConsumerRecord<Integer, String> record: records) {
        ProducerRecord<Integer, String> customizedRecord = transform(record);
        producer.send(customizedRecord);
      }
      Map<TopicPartition, OffsetAndMetadata> offsets = consumerOffsets();
      producer.sendOffsetsToTransaction(offsets, consumer.groupMetadata());
      producer.commitTransaction();
    }
  } catch (KafkaException e) {
    producer.abortTransaction();
    resetToLastCommittedPositions(consumer);
  }
}
{% endhighlight %}

### Init Transactions

To prevent multiple producers with the same ID writing to Kafka, it uses a fencing mechanism. It stores a map of producer ID -> timestamp. When a producer calls `initTransactions()` it sets the current timestamp for its ID in that map. It will keep using this same timestamp when writing messages.

When writing a message, the broker will check if the timestamp on the message matches the one in the map. If not it will reject. This prevents multiple producers from being alive at the same time. If a new producer starts and registers a higher timestamp, it overtakes all the other existing producers.

When a producer calls `initTransactions()` Kafka also selects one of the brokers to be the *transaction coordinator* (hash of the producer ID, so the mapping is sticky). It will keep in-memory metadata about the transaction, such as state and which partitions are participating in the transaction. This broker is also the partition leader of a special internal topic named `__transaction_state` (partitioned by producer ID) which can be used to restore the state.

### Begin Transaction

To start a transaction, the producer calls `producer.beginTransaction()`. This doesn't talk to Kafka, but the producer client is aware that the next message it sends will be inside a transaction and it will tell Kafka that.

When the producer does send a message the coordinator will first add a message in the topic `__transaction_state` indicating a transaction started. The message is then processed by the respective brokers as normal, but they mark the message as uncommitted (so that doesn't get returned to consumers).

The coordinator writes a message to `__transaction_state` with all partitions that have changed by the current write.

In addition to sending the messages, we also need to update the offsets for the consumers. Note that it's the producer that commits the offset now, not the consumer: `producer.sendoffsetsToTransaction(offsets, consumer.groupMetadata())`.

The *group coordinator* will keep non-committed offsets in a special place and once the transaction is committed it will merge these offsets into its main map.


### Commit Transaction

When the producer calls `producer.commitTransaction()`, the coordinator writes a message in the topic `__transaction_state` with the state `PREPARE_COMMIT`. Then it sends a request to the leader of all partitions to add a special message representing a "commit" marker. So when broker returns its messages, only those preceding the last "commit" marker are considered valid.

Since the offset topic `__consumer_offsets` has also been changed during the transaction, it's also notified and will add the "commit" marker.

### Abort Transaction

Similarly, if the transaction needs to aborted, the producer calls `producer.abortTransaction()` and the coordinator go through the same process but instead of a "commit" marker, it will have a "abort" marker. Messages between starting at a marker and ending in the "abort" marker are considered deleted.

Differently from the success case, in an aborted transaction, we need the consumer to call `resetToLastCommittedPositions()`. While the `__consumer_offsets` also received the "abort" marker and will know not to return the pending offsets, on the client side we already moved to the next offset, so need to reset.

## Data Pipelines

Kafka is the input source for stream processing frameworks such as Flink, but it also provides its own stream processing framework known as *Kafka Connect*. This is mostly useful for sending data from Kafka to another source type (e.g. MySQL) or vice-versa, with simple transformations.


## Conclusion

I got what I wanted from this book: I learned a lot about Kafka! The part I found the most interesting and difficult was the transactions. I found it amusing how Kafka uses special topics for a bunch of internal operations including checkpointing and transactions, but also cluster-replication. It's how in Linux everything is a file and in Kafka everything is a topic.

As I was trying to write down my understanding, I found some difficult topics such as compaction and transactions were missing details so I had to complement with external research.

As I tried to summarize the chapters I realized that related chapters don't seem to be grouped together and some like *Chapter 5* and *12* feel like they should be one. This seems to be a common artifact of multi-author books, but didn't impact the content of the book.
