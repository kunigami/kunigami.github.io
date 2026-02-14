---
layout: post
title: "Async Functions in JavaScript"
tags: [javascript]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2019-07-01-async-functions-in-javascript/2019_06_js.png" alt="js" />
</figure>

Async functions are a JavaScript construct introduced in the [ES2017 spec](https://www.ecma-international.org/ecma-262/8.0/#sec-async-function-definitions). Put it simply, async functions allow writing asynchronous code using synchronous syntax.

In this post we'll discuss async functions in JavaScript, covering some other concepts such as iterators, generators that can be used to implement async functions in cases they're not supported by the runtime.

### A Glimpse

Before we start delving into the details, let's get a sense on why async functions are useful, especially in the context of Promises.

In case you don't know about Promises in JavaScript or need a refresher, I recommend checking that out first. If you'd like, we talked about [Promises in a previous post]({{site.url}}/blog/2015/07/19/javascript-promises.html) which might serve as an introduction.

As we've learned, they're very handy for reducing the so called "callback hell". Here's a contrived example prior to Promises:

{% highlight javascript %}
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

We saw that with Promises we can simplify this to:

{% highlight javascript %}
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

Which is much cleaner. Now, the async syntax allows an even cleaner code by making it look synchronous:

{% highlight javascript %}
try {
  a = await fetchSomeStuff();
  b = await fetchSomeStuffDependingOnA(a);
  c = await fetchSomeStuffDependingOnB(b);
} catch (ex) {
  handleException(ex);
}

{% endhighlight %}

We can now proceed into some details on how async functions work. To start, let's learn about intermediate concepts, namely *iterators* and *generators*.

### JavaScript Iterators

An iterator is basically an object that has a method `next()`, which returns an object with fields `value` and `done`, the latter being a boolean indicating whether there's a next value to be iterated on. An iterator is more like a design or code pattern: it's not explicitly supported by the JavaScript runtime in any special way.

{% highlight javascript %}
function makeIterator(n) {
    return {
      cnt: n,
    
      next: function() {
        this.cnt--;
        return {value: this.cnt, done: this.cnt < 0};
      },
    };
}

it = makeIterator(30);

while (true) {
  const {value, done} = it.next();
  if (done) {
    break;
  } 
  console.log(value);
}

{% endhighlight %}

**Iterable** on the other hand is a contract that an object can be used as iterator. To indicate this we add a special field containing Symbol.iterator, which maps to a function that returns this object (similar to an interface in an OOP language) - and this constructed is handled as a special case.

In the example below we create an example iterator and use it with the for-of construct:

{% highlight javascript %}
function makeIterator(n) {
    return {
      cnt: n,
    
      next: function() {
        this.cnt--;
        return {value: this.cnt, done: this.cnt < 0};
      },
    
      [Symbol.iterator]: function() { return this }
    };
}

it = makeIterator(30);

for (value of it) {
    console.log(value);
}

{% endhighlight %}

### JavaScript Generators

Generators are a syntax sugar for iterators in what it allows us to not keep track of a "global" state (in the example above via `this.cnt`). It does so by allowing the function to yield the execution back to the caller and resume from where it stopped when it's called again. Behind the scenes, it creates an object with the same structure as the iterator object we defined above, namely with a `next()` method. It's much clearer with an example:

{% highlight javascript %}
function* makeGenerator(n) {
  for(cnt = n - 1; cnt >= 0; cnt--) {
    yield cnt;
  }
}

it = makeGenerator(30);

while (true) {
  const {value, done} = it.next();
  if (done) {
    break;
  } 
  console.log(value);
}

{% endhighlight %}

First, we indicate the function is a generator with the * modifier (i.e. function*). Here we don't have to explicitly define the `next()` function and we don't need to keep track of the variable `cnt` outside of the function - it will be resumed from the state it had when we called `yield`.

As with iterators, we can make generators *iterable* by implementing a contract. In this case we create an object with a special field containing `Symbol.iterator` which maps to the generator function:

{% highlight javascript %}
function makeGenerator(n) {
    return {
        *[Symbol.iterator]() {
    	    for(cnt = n - 1; cnt >= 0; cnt--) {
      	    yield cnt;
        }
    },
  };
}

it = makeGenerator(30);

for (value of it) {
    console.log(value);
}
{% endhighlight %}

### Async Functions &lt;&gt; Promises

We're now ready to come back to async functions. We can think of async functions as syntax sugar for Promises. Suppose a function `f()` exists that returns a Promise. If we want to use the result of that Promise and return a new one, we could do, for example:

{% highlight javascript %}
function f() {
    return Promise.resolve(10);
}
  
function g() {
    return f().then(r => {
        return r + 1;
    });
}
  
g().then(r => console.log(r));
{% endhighlight %}

Instead, we could replace `g()` with an async function, which "understands" Promises and returns them, making it possible to easily mix with Promise code. The code above would look like:

{% highlight javascript %}
function f() {
    return Promise.resolve(10);
}

// Implicitly returns a Promise 
async function g() {
    r = await f();
    return r + 1;
}
  
g().then(r => console.log(r));
{% endhighlight %}

Note how we swapped a Promise-based implementation with an async one without making any changes to the call stack that expected Promises throughout.

**Handling errors.** Async functions have a familiar syntax for error handling too. Suppose our function `f()` rejects with some probability:

{% highlight javascript %}
function f() {
    return Math.random() > 0.5 ? 
       	Promise.resolve(10) : 
        Promise.reject(new Error('code x'));
}

function g() {
    return f().then(r => {
        return r + 1;
    }).catch(e => {
    	return new Error('error:' + e.message);
    });
}
  
g()
    .then(r => console.log(r))
    .catch(e => console.error(e));
{% endhighlight %}

If we are to replace `g()` with an async version, using the try/catch syntax:

{% highlight javascript %}
function f() {
    return Math.random() > 0.5 ? 
       	Promise.resolve(10) : 
        Promise.reject(new Error('code x'));
}

async function g() {
	try {
  	    return await f();    
    } catch (e) {
        throw new Error('error: ' + e.message);
    }
}
  
g()
    .then(r => console.log(r))
    .catch(e => console.error(e));
{% endhighlight %}

### Async Functions as Generators

As of this writing most major browsers support async functions on their latest versions except Internet Explorer. For a while though, if developers wanted to use async functions they needed to rely on transpilation (i.e. translate their async-based code into browser-compatible code). One of the most popular tools for this is [Babel](https://babeljs.io/en/repl), which transpiles code with async functions into one using generators and some helpers.

We can study that code to learn how to implement async-like functions using generators. Consider this simple example chaining two Promises using an async function.

{% highlight javascript %}
function f1() {
    return Promise.resolve(21); 
}
function f2(x) {
    return Promise.resolve(x * 2); 
}

async function g() {
    r = await f1();
    return await f2(r);
}

g().then(x => console.log(x));
{% endhighlight %}

If we translate it using Babel we get some generated code. I removed parts dealing with error handling and inlined some definitions to make it easier to read. Here's the result:

{% highlight javascript %}
function _asyncToGenerator(fn) { 
    return function () { 
        var self = this;
        var args = arguments; 
        return new Promise(function (resolve, reject) { 
            // Instantiates the generator
            var gen = fn.apply(self, args); 
            function _next(value) {
                // Next step of the generator
                var info = gen.next(value); 
                var newValue = info.value; 
    
                if (info.done) { 
                    resolve(newValue); 
                } else {
                    newValue.then(_next); 
                }
            } 
            // Calls the generator recursively until it's done
            _next(undefined); 
        }); 
   }; 
}

function f1() {
   return Promise.resolve(21); 
}
function f2(x) {
    return Promise.resolve(x * 2); 
}
  
function g() {
    _g = _asyncToGenerator(function* () {
      r = yield f1();
      s = yield f2(r);
      return s;
    });
    return _g.apply(this, arguments);
}
  
g().then(r => console.log(r));
{% endhighlight %}

Let's see what is happening here. First, we note that our async function got translated into a generator, basically replacing the await with yield. Then it's transformed somehow via the `_asyncToGenerator()` function.

In `_asyncToGenerator()` we're basically invoking the generator recursively (via `gen.next()`) and at each level we chain the Promise returned by a yield call with the result of the recursion. Finally we wrap it in a Promise which is what the async function does implicitly.

**Intuition.** Let's try to gain an intuition on what's happening here on a high level. The ability of resuming execution of a function at particular points (via yield in this case) is what enables us to avoid passing callbacks every where. Part of why we need pass the callback is that we need to carry the "code" around as a callback, but by having the run time keep the code around solves this problem. For example, in a Promise world, code 1 and code 2 are wrapped in the arrow functions:

{% highlight javascript %}
p.then(() => {
  /* code 1*/
}).then(() => {
  /* code 2 */
});

{% endhighlight %}

In a world where we can remember where we were when an async execution happened, we can in-line the code:

{% highlight javascript %}
/* code 1*/
/* async. yield execution to others */
/* .... */
/* return here */
/* code 2 */

{% endhighlight %}

This translation relies on the existence of generators being fully supported by the runtime. In a world where generators didn't exist as first class citizens, how could we implement them via helpers and also transpilation? We could probably use some sort of iterators and `switch`es to simulate resuming execution at specific points in code, but this is out of the scope of this post and left as food for thought.

## Related Posts

Mentioned by [Coroutines in C++ - The API]({{blog}}/2024/06/03/coroutines-in-cpp.html).

### Conclusion

In this post we learned about some more language features that help with code authoring and readability, namely generators and async functions. These are very useful abstractions that ends up being added to programming languages such as Python, C#, and Hack.