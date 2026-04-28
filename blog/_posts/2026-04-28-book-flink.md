---
layout: post
title: "[Book] Stream Processing with Apache Flink"
tags: [distributed systems]
excerpt_separator: <!--more-->
vanity: "2026-04-28-book-flink"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources}}/books/flink.jpg" alt="Book cover." />
</figure>

In this post I'll share my notes on the book *Stream Processing with Apache Flink* by Fabian Hueske and Vasiliki Kalavri.

This book covers many aspects of the popular open-source Apache Flink, a stream processing engine.

<br /><br />

<!--more-->

## Book Summary
{:.no_toc}

I read the 1st edition of the book. It has 292 pages including the appendix and index, divided into 11 chapters. I skimmed most of the discussions about configuration since I'm mostly interested in Flink's high-level architecture (I don't use Flink day-to-day).

**Stream Processing.** *Chapters 1* and *2* provide an introduction to general stream processing concepts. The contents overlap a bit with the book [Streaming Systems](https://www.kuniga.me/blog/2022/07/26/review-streaming-systems.html) by Akidau et al.

**Architecture.** *Chapter 3* covers the architecture of Flink at a high level, though the chapters introducing specific features go in more depth on some components.

**Features.** *Chapters 5* to *8* cover the **many** different features Flink offers. Including the Scala API.

**Operations.** *Chapters 4, 9* and *10* are mostly for operations and configuration. I skipped most of the contents.

## Table of Contents

1. TOC
{:toc}


## Overview

**Terminology.** A **job** is the highest level unit an end user interfaces with. So for example when users write a Scala code or a SQL query, it gets mapped to a single job.

A job gets compiled into an execution graph, a DAG of **operators**. Each operator is in theory executed by multiple **tasks** (hosts X threads) but in practice operators get fused so the same task ends up processing a chunk of the sub-DAG.



## Architecture

### Components

**JobManager.** This is a process that runs in one of the hosts/nodes of the cluster. For fault-tolerance, standby instances of it might run in other hosts. This component is responsible for many things such as compiling a query into a DAG, decides how to split it into tasks, decides where tasks are run, task health monitoring, rebalancing work across tasks.

**TaskManager.** This is a process running on each host (it's possible to have more than one task manager per host, but let's assume it's 1:1). Each task manager has a thread pool (task slots) that is able to handle work from different jobs, assigned by the job manager.

Tasks also need to send data to one another if they're processing the DAG of the same job and they don't live in the same host.

**ResourceManager.** Like JobManager, it's logically a single process. This one acts as a broker between JobManager asking for resources and TaskManagers providing them. It knows how many task slots each host has and can return that list of hosts to the JobManager.

It can also pass along requests by the JobManager for more task slots, by then requesting the underlying system (e.g. Kubernetes) to add more workers to the cluster.

### Data Model

Operator



### Data Transfer

Data can be transferred within the same thread, across threads and across hosts. The intra-thread case is when operators are fused and data is passed between operators. The inter-thread or intra-host is when an operator has to be split. The inter-host case is when tasks need to communicate with multiple other tasks such as in shuffling operations.

### Watermark

The watermark is a timestamp that should be interpreted as "no event with timestamp lower than this watermark will appear". We studied watermarks via the paper *Watermarks in Stream Processing* in a previous [post](https://www.kuniga.me/blog/2022/12/29/watermarks.html). Watermark is just a heuristic: we cannot for sure predict whether all events with that timestamp have been seen. However, events that have timestamp lower than the watermark are called *late events*.

In Flink watermarks are emitted alongside normal events. Operators can transform watermarks in different ways. If an operator has multiple upstream operators, the way it merges watermarks is by taking the minimum between them and re-emitting it.

Other operators such as window/stateful make use of watermark. It's used to determine when to close windows and emit events.

One downside of emitting watermarks as events is that if one of the tasks is not receiving events it doesn't know which watermark to emit since it's event-dependent and watermark progression can get stuck.

### State

There are two types of state: key-based (*keyed state*) or operator-based (*operator state*). The keyed state as the name implies is like a key-value store. When an operator is key-based, Flink guarantees key-affinity (i.e. the same keys are always processed by the same task) by introducing a shuffle operation beforehand.

The overall way state is implemented is that Flink uses a local key-value store like RocksDB to persist the state and periodically snapshots to an external storage. We'll cover more about this flow in *Snapshots*.

**Keyed state.** The value of the state can be a scalar (*ValueState*), a list (*ListState*) or a map (*MapState*). The reason to expose these as opposed to just working with a strongly typed object is due to the different optimizations they afford.

The scalar is the most general since everything can be serialized to one. But suppose you have a map value and you want to just update one of its keys. If using a scalar, Flink would need to read the entire map, deserialize, update one key, serialize and write back. By using a map state, Flink might decide to store the entries of the map as separate entries in the key-value store.

**Operator state.** has 3 types: List, Union and Broadcast. These mostly affect the semantics on repartitioning. The list state declares that the entries on the list are largely independent. Once repartitioning happens, Flink might decide to split the list across multiple tasks.

The Union is also a list, but upon restart Flink will union the lists across all tasks and provide each task with this unioned view. The broadcast state is typically sent by an upstream broadcast operator, and read-only for the downstream. On restart each task receives a copy of the broadcast state.

**State Evolution.** Flink supports some schema evolution if the application uses a backward-compatible serialization such as Avro. It also supports topology evolution as long as operator names are stable (so that their state is preserved). Applications can explicitly label an operator, for example:

{% highlight scala %}
stream
  .map(...)
  .uid("user-enrichment")
  .name("User Enrichment Step")
{% endhighlight %}


### Checkpointing

The JobManager decides to occasionally checkpoint the state of the application. It sends a message to all TaskManagers that then inject a checkpoint barrier, a special message like the watermark.

Upon receiving the barrier, each operator takes a snapshot of its state (the ones discussed above, or offsets for source operators). They have been continuously writing to a local RocksDB and now they need to update that state to an external storage such as S3. This process is asynchronous.

Once the TaskManager determines all operators have uploaded their state, they inform the JobManager. Once the JobManager determines that all tasks have uploaded their state, it can mark the snapshot as completed.

**State exactly-once semantics.** This 2-phase checkpointing allows for exactly-once semantics of the state (caveat: it assumes operations do not have side effects outside the state). Suppose it checkpointed successfully at time T, then it processed some data and the process crashed. Now it needs to rewind to the old checkpoint, meaning each operator will restore its state from the external storage, then it reprocesses the data.

From the state perspective, no data has been processed more than once. Note that by default Flink doesn't coordinate flushes to the sink with checkpoints: it constantly flushes data downstream, so the exactly-once semantics is only for state. It's possible to achieve end-to-end exactly once semantics, but the sink must support it.

### E2E Exactly-Once Semantics

Note: for this section, it's worth being familiar with [how Kafka works](https://www.kuniga.me/blog/2026/01/17/book-kafka.html).

One common way to achieve end-to-end exactly-once semantics in Flink is by using Kafka as a source and sink in EO mode. An important aspect is that Flink does not rely on Kafka to store the topic offset but it stores it as part of its state.

The sink must have exactly once mode enabled and downstream consumers must only read committed logs (see *Exactly-Once Semantics* in [Kafka: The Definitive Guide](https://www.kuniga.me/blog/2026/01/17/book-kafka.html)).

The flow is: Suppose Flink just performed a checkpoint of its state. It then starts a new transaction on the Kafka sink. As it reads data from Kafka source, and processing it, it writes data to the transaction. Once it successfully checkpoints again, it commits the Kafka transaction.

If the process crashes any time before the transaction is closed, it will simply abort the transaction and replay the data from the previously stored offset. One corner case is if Flink crashes after completing its checkpoint but before committing the sink transaction.

To support this case, the transaction handle is stored as part of the checkpointed state. So upon recovery, the first thing it needs to check is whether there is a pending transaction, in which case it would commit it. Then it doesn't need to reset to the previous checkpoint.

## API

Let's start with a simple example reading data from a socket, transforming it and printing to stdout:

{% highlight scala %}
// Read data
val input: DataStream[String] = env.socketTextStream("localhost", 9999)

// Deserialize
val sensorData: DataStream[SensorReading] = input
  .map { line =>
    val parts = line.split(",")
    SensorReading(
      parts(0).trim, // id
      parts(1).trim.toLong, // timestamp
      parts(2).trim.toDouble // temperature
    )
  }

// Transform
val avgTemp: DataStream[SensorReading] = sensorData
  .map( r => {
    val celsius = (r.temperature - 32) * (5.0 / 9.0)
    SensorReading(r.id, r.timestamp, celsius)
  })
  .keyBy(_.id)
  .timeWindow(Time.seconds(5))
  .apply(new TemperatureAverager)

// Print
avgTemp.print()
{% endhighlight %}

This example covers several components of a pipeline: reading a serialized message from source, deserializing, transforming and writing somewhere (in this case stdout).

Next we consider the API in more detail.

### Stateless Transformations

**Map.** Is a method of a datastream that takes a function object which itself transforms one message into another message. Example

{% highlight scala %}
stream.map( r => r.id )
{% endhighlight %}

**Filter.**  Same idea as the `map()`. Takes one message, returns true/false. If false, the message is filtered out. Example:

{% highlight scala %}
stream.filter( r => r.temperature >= 25 )
{% endhighlight %}

**FlatMap.** A more general version of map takes one row but can return a different number of rows. Can be used to implement both `map()` and `filter()`. Example:

{% highlight scala %}
stream.flatMap { r =>
  if (r.temperature >= 25) List(r.id)
  else List.empty
}
{% endhighlight %}

**Custom Functions.** Instead of passing callbacks to the mentioned APIs, it's also possible and sometimes necessary to implement interfaces and pass function objects instead. For example, for the `.filter()` method we can implement:

{% highlight scala %}
class FlinkFilter extends FilterFunction[String] {
  override def filter(value: String): Boolean = {
    value.contains("flink")
  }
}
...
var flinkTweets = tweets.filter(new FlinkFilter)
{% endhighlight %}

There are corresponding interfaces `MapFunction` and `FlatMapFunction` functions for the other 2 APIs. You can also pass parameters to the function upon construction:

{% highlight scala %}
var flinkTweets = tweets.filter(new KeywordFilter("flink"))
{% endhighlight %}

But there's "compile" time parameters that will be resolved before the DAG is constructed. For runtime initialization, one can use the *Rich*- versions, e.g. `RichFilterFunction` which have the `open()` and `close()` methods.

### Keyed Transformations

**KeyBy.** This method on a normal `DataStream` transforms it into a `KeyedStream` which is logically partitioned by a key function provided, e.g.

{% highlight scala %}
stream.keyBy(r => r.id)
{% endhighlight %}

**Rolling Aggregations.** A rolling aggregation doesn't require a window of time, the values are accumulated forever, but events are emitted for every row. For example:

{% highlight scala %}
stream
  .keyBy(r => r.key)
  .sum(r => r.value)
{% endhighlight %}

For each event `r`, it will add `r.value` to the corresponding sum for `r.key` and emit the result.

**Reduce.** This API allows providing custom aggregation functions. The major restriction is that the accumulator type must be the same as the record type. For example:

{% highlight scala %}
stream
  .keyBy(r => r.key)
  .reduce((acc, r) => Record(acc.key, acc.value + r.value))
{% endhighlight %}

Note that this API does not have an "initializer" step. The variable `acc` is set with the first event it sees.

**KeyedProcessFunction.** The most general functions on `KeyedStream` are those implementing `KeyedProcessFunction`. The book provides a complete example, but for brevity, we mention the two methods that must be implemented: `processElement()` and `onTimer()`.

In `processElement()` we receive one message and decide what to do with it. This method has access to the (keyed) state, so it can update the state based on this message. This method cannot output messages downstream. Instead it schedules a timer that when fired calls the `onTimer()` method.

The way to use a custom `KeyedProcessFunction` is passing it to the `process()` method:

{% highlight scala %}
val alerts =
  readings
    .keyBy(_.id)
    .process(new MyKeyedProcessFunction)
{% endhighlight %}

**Stateful Map Functions.** It's possible to use keyed state in the generic flat map function, in particular via the `RichFlatMapFunction`. In this case the state is managed by the function itself and has no timer semantics - the only "hook" point is when `flatMap()`.

{% highlight scala %}
class TemperaturAlertFunction()
  extends RichFlatMapFunction[SensorReading, (String, Double, Double)] {

  ...
  override def open(parameters: Configuration): Unit = {
    var lastTempDesc = new ValueStateDescriptor[Double]("lastTemp", classOf[Double])
    lastTempState = getRuntimeContext.getState[Double](lastTempDesc)
  }

  ...
  override def flatMap(reading: SensorReading, out: Collector[(String, Double, Double)]): Unit = {
    ...
    val lastTemp = lastTempState.value()
    ...
  }
}
{% endhighlight %}

The state used is of type *ValueState*. As discussed in *Architecture > State > Keyed State*, we can also have *ListState* or *MapState*. The API will be similar, for example `ValueStateDescriptor` becomes `ListStateDescriptor` and `getRuntimeContext.getState()` becomes `getRuntimeContext.getListState()`.


### Windowed Transformations

A special case of keyed transformations is the windowed transformations, on top of `WindowedStream`. This type of stream is returned once we call methods such as `window()`. The `ProcessWindowFunction` interface has the method `process()` which among other things takes the key and a list of messages within a window.

It's a layer of abstraction above the `KeyedProcessFunction`. In this case we don't have to deal with timers explicitly. The method `window()` takes a function that describes the window, for example, for a sliding window of 1h and step 15min:

{% highlight scala %}
val alerts = readings
  .keyBy(_.id)
  .window(SlidingEventTimeWindows.of(Time.hours(1), Time.minutes(15)))
  .process(new MyProcessWindowFunction)
{% endhighlight %}

This can load a lot of data in memory. An alternative is to use either `ReduceFunction` or `AggregateFunction`. The former works more like the `reduce()` for keyed streams, where it receives the accumulation for a particular window and a new element to be added to the window. The latter is a bit more general, you can have different types between the accumulator and the event, but it's more complex to implement.

It's also possible to customize what is passed to the `.window()` API by implementing a `WindowAssigner`. This determines to which window an event is assigned to. One of the methods in `WindowAssigner` is `getDefaultTrigger()` which returns a `Trigger`.

The trigger tells us when to emit events from the window, and it's possible to provide a custom trigger that overrides the default for the window, e.g.

{% highlight scala %}
val alerts = readings
  .keyBy(_.id)
  .window(new MyWindow)
  .trigger(new MyTrigger)
  .process(new MyProcessWindowFunction)
{% endhighlight %}

### Multistream Transformations

**Union.** Merges data of two or more streams of the same type. Events are processed in FIFO order and all events are emitted as they arrive. Example:

{% highlight scala %}
var unioned = stream1
  .union(stream2, stream3)
{% endhighlight %}

**Connect.** It combines two streams into a special type of stream called `ConnectStreams[T1, T2]`. The streams are still technically separated, and are handled by different functions, but the key is that they're processed by the same operator, so they can share state. Example:

{% highlight scala %}
var connected = stream1.connect(stream2)

connected.map1(f1).map2(f2)
{% endhighlight %}

So if we decide to store events from `stream1` in a state inside `map1()`, then `stream2` would have access to it. Typically we want to shard events by key:

{% highlight scala %}
var connected = stream1
  .keyBy(keyFunc1)
  .connect(stream2.keyBy(keyFunc2))
{% endhighlight %}

To make sure they're processed by the same task since state is task-scoped.

### Join Transformations

**Interval Join.** This joins matching events from different streams that are within a period of time of each other:

{% highlight scala %}
var connected = stream1.keyBy(_.id)
  .intervalJoin(stream2.keyBy(_.id2))
  .between(Time.seconds(-5), Time.seconds(10))
  .process{ (r, a) => (r, a) }
{% endhighlight %}


Where `.process()` takes a function (lambda or a function of type `ProcessJoinFunction`) which takes a pair of matching events. The detail is that this join keeps a buffer of the left and right events that are within range. When a new event $e$ arrives, say from the first stream, it emits all pairs containing $e$ and events from the second stream in the buffer.

**Window Join.** This combines two streams into a (custom) window and performs a join between them. For example:

{% highlight scala %}
var connected = stream1.keyBy(_.id)
  .join(stream2.keyBy(_.id2))
  .where(_.id)
  .equalTo(_.id2)
  .window(TumblingEventTimeWindows.of(Time.seconds(10)))
  .apply{ (r, a) => (r, a) }
{% endhighlight %}

Where `.apply()` takes a function (lambda or a function of type `JoinFunction`) which takes the cross product of the list of elements from the first stream with those of the second within a window of time.

### Distribution Transformations

By default, when using `keyBy()`, Flink will distribute data to the right tasks based on the provided key (expression). There are ways to customize that using APIs such as `shuffle()`, `rebalance()` and `rescale()`.

### Late Event Handling

Due to watermark, some events will be considered late. There are many options on how to handle them: the simplest is discarding them, but an alternative is sending them to a different sink (dead letter queue).

This can be done via the `.sideOutputLateData()`:

{% highlight scala %}
stream
  .keyBy(_.id)
  .timeWindow(Time.seconds(10))
  .sideOutputLateData(new LateEventSink)
  .process(new MyProcessFunc)
{% endhighlight %}

When customizing `ProcessFunction` we can also handle late events since this function has access to the watermark.

### Operator State

For non-keyed state, we discussed a few options in *Architecture* > *State* > *Operator State*: List, Union and Broadcast. A flat map function can use them by using the trait `ListCheckpointed`, which requires implementing the `restoreState` and `snapshotState`. For example:

{% highlight scala %}
class MyStatefulFunction()
  extends RichFlatMapFunction[SensorReading, (Long)]
  with ListCheckpointed[java.lang.Long] {

  ...
  override def restoreState(state: List[Long]) = { ... }
  ...
  override def snapshotState(chkpntId: Long, ts: Long): List[Long] = { ... }
}
{% endhighlight %}

To use other types of state such as union, we must use the more general trait, `CheckpointedFunction`:

{% highlight scala %}
class MyStatefulFunction()
  extends RichFlatMapFunction[SensorReading, (Long)]
  with CheckpointedFunction {

  ...
  override def initializeState(ctx: FunctionInitializationContext): Unit = {
    val desc = new ListStateDescriptor[String]("rules", classOf[String])
    unionState =
      ctx.getOperatorStateStore.getUnionListState(desc)
  }
  ...
  override def snapshotState(ctx: FunctionSnapshotContext): Unit = { ... }
}
{% endhighlight %}

## Conclusion

I've been working with stream processing for about 4 years and never took the time to learn about the most popular open source stream processing system.

One thing that I found surprising is how expressive Flink is, and how many different levels of abstractions are supported in the APIs. This also means there are many ways to do the same thing, which I tend to not be a big fan of.
