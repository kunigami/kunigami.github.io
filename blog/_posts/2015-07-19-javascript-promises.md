---
layout: post
title: "JavaScript Promises"
tags: [es6, javascript, promise]
---

<figure class="image_float_left">
    <a href="https://kunigami.files.wordpress.com/3333/06/logo.png"><img src="{{site.url}}/resources/blog/2015-07-19-javascript-promises/3333_06_logo.png" alt="logo" /></a>
</figure>

In this post we'll introduce promises in JavaScript. Promises is a new feature included as part of the [ES6 spec draft](https://people.mozilla.org/~jorendorff/es6-draft.html#sec-promise-constructor) [[1](https://people.mozilla.org/~jorendorff/es6-draft.html#sec-promise-constructor)].

First, we'll give an idea of what promises are good for and then go over simple examples to understand how promises work and also cover some extra methods from the API.

There was an initial standard proposal called [Promises/A](http://wiki.commonjs.org/wiki/Promises/A) and a subsequent improvement of that proposal called [Promises/A+](https://promisesaplus.com/) [[2](https://github.com/promises-aplus/promises-spec)].

Google Chrome implements the Promises/A+ standard [[3](https://developers.google.com/api-client-library/javascript/features/promises)]. The code snippets in this post were test on the regular Chrome, version 43.

## Introduction

Promises are an abstraction to make working with asynchronous code in a more expressive manner [[4](https://blog.domenic.me/youre-missing-the-point-of-promises/)]. In synchronous code, we think in terms of return's for normal execution and throw's for exceptions. In asynchronous world, the code flow is structured around callbacks, for example, onSuccess or onFailure callbacks.

As a toy example, consider the case where we have to call 3 functions, each depending on the previous one, but each of them can fail for whatever reason and we need to handle exceptions. In the synchronous case, it's straightforward:

{% highlight js %}

try {
    var a = fetchSomeStuff();
    var b = fetchSomeStuffDependingOnA(a);
    var c = fetchSomeStuffDependingOnB(b);
} catch (ex) {
    handleException(ex);
}

{% endhighlight %}

If these functions are asynchronous, we'd have to handle these dependencies via the callbacks, creating a nested set of calls (aka *callback hell*):

{% highlight js %}

fetchSomeStuff(
    /* onSuccess*/ function (a) {
        fetchSomeStuffDependingOnA(
            a,
            /* onSuccess */ function (b) {
                fetchSomeStuffDependingOnB(
                    b,
                    /* onSuccess */ function (c) {
                        /* Code goes on here */
                    },
                    /* onFailure */ function (ex) {
                        handleException(ex);
                    }
                )
            },
            /* onFailure */ function (ex) {
                handleException(ex);
            }
        );
    },
    /* onFailure */ function (ex) {
	handleException(ex);
    }
);

{% endhighlight %}

We could definitely work around the depth of calls by using auxiliary functions, but Promises make use cases like this easier:

{% highlight js %}

fetchSomeStuff().then(
    function (a) {
        return fetchSomeStuffDependingOnA(a);
    }
).then(
    function (b) {
        return fetchSomeStuffDependingOnB(b);
    }
).then(
    function (c) {
        /* Code goes on here */
    }
).catch(
    function (ex) {
        handleException(ex);
    }
);

{% endhighlight %}

In this case, we'd have to change the functions to return promise objects.

We'll next cover small examples exploring the behavior of promises to understand how they work.

## Examples

**Creating a promise**

The promise constructor expects a callback (also called *executor*). This callback on its turn expects takes two other functions as arguments, `resolve()` - to be called when a normal execution is ended - and `reject()` - called when an exception occurs. A sample executor example could be:

{% highlight js %}

function executor(resolve, reject) {
  // do some work
  if (success) {
    resolve(10 /* some value */);
  } else {
    reject(new Error("some error"));
  }
}

{% endhighlight %}

which succeeds half of the time and fails the other half. We then use this function to create a new promise object:

{% highlight js %}

var promise = new Promise(executor);

{% endhighlight %}

**Calling a promise**

After instantiating a promise, we can call the `then()` method from the promise, passing two functions, which we'll refer to *onFulfill* and *onReject*. The `onFulfill()` function will be called when the promise invokes `resolve()` is called and the `onReject()` function one when `reject()` is called.

{% highlight js %}

promise.then(
  /* onFulfill */
  function(value) {
    console.log('resolved with value: ');
    console.log(value);
  },
  /* onReject */
  function(error) {
    console.log('rejected with error: ');
    console.log(error);
  }
)

{% endhighlight %}

Now that we saw the basic syntax for creating and handling promises, let's delve into more details.

**Resolving a promise**

When we pass a function (executor) to instantiate a promise, it gets immediately executed. For example, in the code below, it will print "running executor" first and then "constructed".

{% highlight js %}

var promise = new Promise(function(resolve, reject) {
    console.log("running executor");
});
console.log("constructed");

{% endhighlight %}

In the example above we're not using neither resolve or reject. If we can include a call to `resolve()` and then invoke the `then()` method:

{% highlight js %}

var promise = new Promise(function(resolve, reject) {
    console.log("resolving");
    resolve(10);
});
console.log("constructed");

promise.then(function (value) {
    console.log("resolved with " + value);
});

{% endhighlight %}

We'll see that the messages are printed in this order:

{% highlight text %}

> resolving
> constructed
> resolved with 10

{% endhighlight %}

Note that the `resolve()` function was called when constructing the promise, but the action was deferred until we passed a callback to the `then()` method. If we think in terms of events, we set up the listener after the event was fired, but the action of the listener was fired nevertheless.

When a promise is first created, it's in a state called **pending**. When the `resolve()` function is called, it changes its state to **fulfilled**, when `reject()` is called, it changes its state to **rejected**. When a promise is either fulfilled or rejected, we say it's *settled*.

In the example above, by the time we called `then()`, the promise was already fulfilled, so the action fired right away.

We can simulate calling `then()` while the promise is pending, that is, before `resolve()` or `reject()` is called, by using the `setTimeout()` function:

{% highlight js %}

var promise = new Promise(function(resolve, reject) {
    setTimeout(
        function() {
	    console.log("resolving");
            resolve(10);
        },
        1000
    );
});
console.log("constructed");

promise.then(function (value) {
    console.log("resolved with " + value);
});

{% endhighlight %}

The messages are now printed in this order:

{% highlight text %}

> constructed
// Waits 1 second
> resolving
> resolved with 10

{% endhighlight %}

Here the promise was in the pending state and then after a second it got fulfilled, so the callback passed to `then()` was fired.

With events and listeners, we have to guarantee an event is not fired before the listener is setup, otherwise we have to store that event and process later. This is one problem promise solves for us, as pointed out by [[5](http://www.html5rocks.com/en/tutorials/es6/promises/)].

**Reactions can be queued**

Note that the `then()` function can be called multiple times for the same promise and each time the callbacks passed to it are enqueued and when the promise is settled, they fire in order. For example:

{% highlight js %}

var promise = new Promise(function(resolve, reject) {
    setTimeout(
        function() {
	    console.log("resolving");
            resolve(10);
        },
        1000
    );
});
console.log("constructed");

promise.then(function (value) {
    console.log("resolved with " + value);
});

promise.then(function (value) {
    console.log("resolved again with " + value);
});

{% endhighlight %}

We'll see both callbacks passed to then are invoked:

{% highlight text %}

> constructed
// Waits 1 second
> resolving
> resolved with 10
> resolved again with 10

{% endhighlight %}

**Promises can't be unsettled**

The `resolve()` and `reject()` calls only take effect if the promise is in a pending state. Remember that the first time we call `resolve()` or `reject()` the promise changes its state from pending, so all subsequent calls to `resolve()` or `reject()` will be ignored:

{% highlight js %}

var promise = new Promise(function(resolve, reject) {
  resolve(10);
  // Promise is fulfilled, so all the subsequent
  // calls will be ignored
  reject(new Error("error"));
  resolve(20);
});

promise.then(function (value) {
    console.log("resolved with " + value);
});

{% endhighlight %}

**Chaining promises**

The `then()` method of a promise returns another promise. The value of the promise is the value returned by the handling functions passed to `then()`. If the handling functions executed normally, then the returned promise is fulfilled with that same value. If the handling function throws, the returned promise is reject with the error thrown.

An example of a the normal execution:

{% highlight js %}

var promise = new Promise(function(resolve, reject) {
    resolve(10);
});
var nextPromise = promise.then(function (x) {
    return x + 1;
});
nextPromise.then(function (x) {
    console.log(x);
});

{% endhighlight %}

It's as if `nextPromise` was created with:

{% highlight js %}

var nextPromise = new Promise(
   var x = 10; // from the first promise
   x += 1;     // from the 'then()' call
   resolve(x);
);

{% endhighlight %}

Here's another example where the promise handlers throws exceptions:

{% highlight js %}

var promise = new Promise(function(resolve, reject) {
    resolve(10);
});
var nextPromise = promise.then(function (x) {
    throw new Error("error");
    return x + 1;
});
nextPromise.then(
    function (x) {},
    function (e) {
        console.log(e);
    }
);

{% endhighlight %}

**Resolving a promise with another promise**

So far we've been using numbers as arguments to the `resolve()` function in a promise, but any JavaScript value could be used. There is one special case though, which is when the value provided is another promise (there's another special case when the value is an non-promise object with a `then()` method - which is also called **thenable**).

Suppose we have a promise A in which we call `resolve()` with another promise `B`. When we call `A.then()` the the value passed to the `onFulfill()` callback of the `then()` method will be the value resolved from the promise `A`.

Consider the following example:

{% highlight js %}

function getPromise() {
    return new Promise(function(resolve, reject) {
         resolve(10);
    });
}

var outerPromise = new Promise(function (resolve, reject) {
    var innerPromise = getPromise();
    resolve(innerPromise);
});

outerPromise.then(function(x) {
    console.log(x);  // prints 10
});

{% endhighlight %}

Here, `outerPromise` calls resolve with another promise that resolves with a number. The `onFulfill()` callback passed to `then()` will receive 10.

**Chaining promises with promises**

We can combine the two last examples to demonstrate how promises can increase the expressiveness of JavaScript callbacks, as we saw in the beginning of the post:

{% highlight js %}

function createPromise(x) {
    return new Promise(function(resolve, reject) {
        resolve(x + 1);
    });
}

var firstPromise = new Promise(function (resolve, reject) {
    resolve(10);
});

firstPromise.then(function(x) {
    var secondPromise = createPromise(x);
    return secondPromise;
}).then(function(x) {
    console.log(x); // Prints 11
});

{% endhighlight %}

Remember that when the callbacks provided to `then()` returns normally, the value is used to create another promise which automatically calls `resolve()` with that value. If that vale is a promise, then the value provided to `onFulfill()` of the next `then()` will be the value from `resolve()`, which in this case will be 11.

**Other methods from the API**

So far we've considered only promises that need to be executed sequentially. For those that can be executed in parallel, the Promise class contains the `all()` method, which will take an array of promises and returns a new promise, which will wait until all input promises are settled. If they were all fulfilled, then it will can resolve with an array with the resolve values. If any of them if rejected, it will call `reject()` with the error of the first promised to be rejected.

In our examples, we mostly exclusively focused on the "normal" execution flow, in which `resolve()` and `onFulfill()` are called. The exception case is very similar, with the `reject()` and `onReject()` functions being called. One difference is that `reject()` might be triggered implicitly, for example if an exception is thrown within a promise or one of the reactions callbacks.

If we want, we can only provide the `onFulfill()` callback to the `then()` method, but if we want to provide only the `onReject()`, we'll need to pass an empty function as `onFulfill()`. To cover this case, promises also have the `catch()` which does essentially this.

## Conclusion

We saw that promises can be used to make code dealing with callback more legible and easier to reason about. There are some complexities encapsulate in promise objects and its methods, so it can be a bit daunting to understand how they work behind the scenes. We covered some small examples and proceeded in steps to make it easier to digest.

When writing this post, I've initially tried reading the ES6 spec, but it was a bit too abstract to follow. I've also found the [promise/A+ spec](https://github.com/promises-aplus/promises-spec) which contains pseudo-code more similar to JavaScript and only describes the `then()` method behavior.

## References

* [[1](https://people.mozilla.org/~jorendorff/es6-draft.html#sec-promise-constructor)] Draft ECMA-262, 6th Edition - Rev 37.
* [[2](https://github.com/promises-aplus/promises-spec)] Promises/A+ Spec
* [[3](https://developers.google.com/api-client-library/javascript/features/promises)] API Client Library for JavaScript: Using Promises
* [[4](https://blog.domenic.me/youre-missing-the-point-of-promises/)] Hidden Variables: You're Missing the Point of Promises
* [[5](http://www.html5rocks.com/en/tutorials/es6/promises/)] HTML5 Rocks - JavaScript Promises
