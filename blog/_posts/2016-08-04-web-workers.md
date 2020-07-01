---
layout: post
title: "Web Workers"
tags: [javascript]
---

In this post we study Web Workers, a technology that allows JavaScript code to run in separate threads. We'll start by exploring the API with some toy examples and at the end discuss some applications.

## Introduction

By default JavaScript runs in a single (the main thread), which can be a problem for user experience if expensive operations need to be performed in code which would affect responsiveness of the UI.

The thread that runs the web worker code has some environment limitations, which includes no access to the DOM or global objects such as window. [[3](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Functions_and_classes_available_to_workers)] contains a list of functions and methods available to a Web Worker.

Besides that, memory is not shared between the threads, having to be explicitly serialized (so it can be cloned) and passed via a method (`postMessage()`). This can lead to performance issues if the amount of data to be copied is large. In *Transferable Objects* we'll discuss an alternative.

Workers might spawn their own workers.

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2016-08-04-web-workers/4444_01_chihiro-creatures.jpg" alt="For some reason, the term &quot;web workers&quot; reminds me of these creatures from Spirited Away" />
    <figcaption> For some reason, the term "web workers" reminds me of these creatures from *Spirited Away* :)</figcaption>
</figure>

Let's work a simple example. Imagine we have two files, main.js and worker.js (in the same directory):

`main.js:`
{% highlight js %}

// Initializes the worker with the
// JavaScript code in worker.js
var myWorker = new Worker("worker.js");

// Post a message to the worker
myWorker.postMessage("Hello worker!");

// Callback that gets called when a
// message is received from the worker.
myWorker.onmessage = function(/*MessageEvent*/ message) {
  console.log(
    'Message received from worker script: ',
    message.data
  );
}

{% endhighlight %}

`worker.js`
{% highlight js %}

onmessage = function(/*MessageEvent*/ message) {
  console.log(
    'Message received from main script: ',
    message.data
  );
  postMessage("Hello main!");
}

{% endhighlight %}

## Transferable Objects

By default data is copied when sending information back and forth between the main thread and the worker, using a process called structural cloning.

The serialization/deserialization can be expensive, for which case there is an alternative: **transferrable objects**. More specifically, we can work with ArrayBuffers, which can be "transferred" instead of cloned, which is a more performant operation, more precisely, O(1).

We'll first cover `ArrayBuffers` and then see how to apply it in a context of Web Workers.



According to [5]:

> The `ArrayBuffer` is a data type that is used to represent a generic, fixed-length binary data buffer. You can't directly manipulate the contents of an ArrayBuffer; instead, you create a typed array view or a DataView which represents the buffer in a specific format, and use that to read and write the contents of the buffer.

ArrayBuffer basically represents an unstructured array of bits, which, to have any meaning/interpretation, needs a view, which can be an array of 32-bit unsigned ints or 16-bit unsigned ints. In the example below we create an array buffer of 100 bytes.

{% highlight javascript %}


// Number of bytes or number of elements
var buffer = new ArrayBuffer(100);

// A 32-bit unsigned int array of length 10 (i.e. 40 bytes), starting
// from byte 0 at the array buffer
var int32View = new Uint32Array(buffer, 0, 10);
// A 16-bit unsigned int array of length 20 (i.e. 40 bytes), starting
// from byte 0 at the array buffer
var int16View = new Uint16Array(buffer, 0, 20);

// Fill in the 16-bit array with 0, 1, 2...
for (var i = 0; i < int16View.length; i++) {
  int16View[i] = i;
}

// The memory is shared because we're reading from the same chunk of
// the byte array.
for (var i = 0; i < int32View.length; i++) {
  console.log("Entry " + i + ": " + int32View[i]);
}

{% endhighlight %}

This is a very interesting model. In `ArrayBuffers` one explicitly work with the serialized form of the data and create views on top of them. I'm used to work with the views-first, that is, create a class representing some data and eventually add serialization/deserialization methods. One advantage of working with serialized data is that we don't need to write the serialization methods, only the deserialization. The major disadvantage is that you need to know upfront how much memory you'll have to use.



We can extend the example above to be used between a worker and the main thread.

`worker.js`:

{% highlight text %}

var buffer = new ArrayBuffer(100);
var int16View = new Uint16Array(buffer, 0, 20);

for (var i = 0; i < int16View.length; i++) {
  int16View[i] = i;
}

console.log('array buffer size', buffer.byteLength); // 100
postMessage(buffer, [buffer]);
console.log('array buffer size?', buffer.byteLength); // 0

{% endhighlight %}

and in the `main.js`:

{% highlight js %}

...
myWorker.onmessage = function(e) {
  buffer = e.data;
  // Number of bytes or number of elements
  var int32View = new Uint32Array(buffer, 0, 10);

  for (var i = 0; i < int32View.length; i++) {
    console.log("Entry " + i + ": " + int32View[i]);
  }
}

{% endhighlight %}

By logging the output to the console, we can see the main thread received the values written to the array buffer by the worker and after the worker transferred the buffer data, it was emptied out.

Note in the `postMessage()` API, we provide buffer as a the first parameter and then it also appears in the list indicating it should be transferred, not copied. Having to pass it twice is a bit confusing in my opinion, but this is to allow the example below, in which the objects transferred are nested inside another structure (in this case an object) and we want to transfer both `buffer1` and `buffer2` but not the top-level object. I'm not sure which use case the API designers had in mind, though.

{% highlight js %}

postMessage(
  {'key1': buffer1, 'key2': buffer2},
  [buffer1, buffer2]
);

{% endhighlight %}

## Error Handling

If any errors are uncaught by the worker, it can be caught from the main thread through the onerror callback:

{% highlight js %}

myWorker.onerror = function(e) {
  console.log('an error occurred:', e);
  e.preventDefault();
}

{% endhighlight %}

Where e is an instance of [ErrorEvent](https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent). We can simulate an error on the worker.js code:

{% highlight js %}

throw new Error("Some error occurred");

{% endhighlight %}

## Terminating

The main thread can terminate the worker

{% highlight text %}

worker.terminate();

{% endhighlight %}

or the worker can terminate itself:

{% highlight text %}

close();

{% endhighlight %}

## Applications

A lot of the examples using Web Workers involve doing some fake expensive calculation in the worker thread, but I haven't found any real-world application.

StackOverflow offers [some ideas](http://stackoverflow.com/questions/2773682/what-are-the-use-cases-for-web-workers), including one that is dimissed as bad uses of Web Workers (polling) or from projects that are long defunct (Mozilla Skywriter). The main issue is that most of time heavy processing is done on the server.

One idea that came to mind is to use web-workers in [React](https://facebook.github.io/react/). React defers a lot of DOM work to the end by working with the concept of a virtual DOM. Web-workers don't have access to the DOM but they do have it for virtual DOMs. Turns out this idea has been explored already [7, 8] but there were some technical difficulties in implementing events.

## Conclusion

In this post we studied Web Workers and some examples utilizing it. I learned a few other related topics like ArrayBuffers, and [Compositor Workers](https://github.com/w3c/css-houdini-drafts/blob/master/composited-scrolling-and-animation/Explainer.md). I was a bit disappointed with the lack of compelling applications using Web Workers. I'll try it out in some of my projects and see if I can any benefits from it.

## References

Some of the code presented in this post is available on [Github](https://github.com/kunigami/jsdraft/tree/master/02_worker).

* [[1](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers)] MDN - Using Web Workers
* [[2](http://www.html5rocks.com/en/tutorials/workers/basics/)] HTML5 Rocks - The Basics of Web Workers
* [[3](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Functions_and_classes_available_to_workers)] MDN - Functions and classes available to Web Workers
* [[4](https://developers.google.com/web/updates/2011/12/Transferable-Objects-Lightning-Fast)] Google Developers - Transferable Objects: Lightning Fast!
* [[5](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Typed_arrays)] MDN - JavaScript typed arrays
* [[6](http://stackoverflow.com/questions/2773682/what-are-the-use-cases-for-web-workers)] StackOverflow - What are the use-cases for Web Workers?
* [[7](http://blog.nparashuram.com/2015/12/react-web-worker-renderer.html)] React Custom Renderer using Web Workers
* [[8](https://github.com/facebook/react/issues/3092)] GibHub React Issues: Webworkers #3092
* [[9](https://github.com/w3c/css-houdini-drafts/blob/master/composited-scrolling-and-animation/Explainer.md)] Compositor Workers Proposal
