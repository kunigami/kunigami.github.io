---
layout: post
title: "Javascript Prototyping"
tags: [javascript, prototype]
---

<figure class="image_float_left">
    <a href="http://kunigami.files.wordpress.com/3333/05/javascript-logo.png"><img src="{{site.url}}/resources/blog/2014-05-18-javascript-prototyping/3333_05_javascript-logo.png" alt="JavaScript-logo" /></a>
</figure>

Javascript is a programming language based on the concept of prototypes. It also allows us to write object-oriented code using prototypes, but if we don't understand how they work it can get confusing.

In this post we'll talk about prototypes and we can use them to write code in a more familiar object-oriented way.

### Objects

Objects in javascript are key-value pairs, called **properties**, the key, or property name, is either a string or a number and the property value can have arbitrary types, including strings, numbers, functions and other objects. If the property is a function, we can also refer to it as a **method**.

The basic way to create an object is:

{% highlight javascript %}

// Creates an empty object
> var car1 = {};
// Creates an object with some properties
> var car2 = {
  make: "Honda"
};
// We can dynamically add properties
> car2.model = "Civic";
// Properties can be functions
> car2.honk = function() {
  return "honk!";
}
> car2.honk();
"honk!"

{% endhighlight %}

There is a special object already defined called `Object`. One of its properties is `getOwnPropertyNames`, which is a function that takes an object and returns an array containing the name of properties defined in that object:

{% highlight javascript %}

> Object.getOwnPropertyNames(Object)
[
  "prototype",
  "getOwnPropertyNames",
  ...
]

{% endhighlight %}

Note that it returns `getOwnPropertyNames`, since it's a property. Another interesting property returned is `prototype`, which is an object.

All objects will inherit the properties in the `prototype` object. Let's see which properties are there:

{% highlight javascript %}

> Object.getOwnPropertyNames(Object.prototype)
[
  "constructor",
  "hasOwnProperty",
  "isPrototypeOf",
  "toString",
  "propertyIsEnumerable", ...
]

{% endhighlight %}

Let's take a look on the properties of a regular object:

{% highlight javascript %}

> var a = {};
> Object.getOwnPropertyNames(a);

{% endhighlight %}

It won't list the properties inherited from `Object.prototype` because `getOwnPropertyNames`, as the name suggests, only lists properties defined at that object, not those inherited. But we can verify it exists by doing:

{% highlight javascript %}

> a.toString();
"[object Object]"

{% endhighlight %}

We can override the properties inherited

{% highlight javascript %}

> a.toString = function() {
  return "Custom toString";
};
> a.toString();
"Custom toString"

{% endhighlight %}

We can also add more properties to `Object.prototype` so it's available to all other objects:

{% highlight javascript %}

> var a = {};
> Object.prototype.custom = function() {
  return "Custom property";
};
> a.custom();
"Custom property"

{% endhighlight %}

Observe that even though we added the `custom` property after creating object `a`, the method `a.custom()` exists is available. This is because when resolving the property from its name it first tries to look if the property is defined in the object itself, otherwise it looks at the properties in the prototype (delegation).

We can have arbitrary objects as prototypes for another object. `Object` has a method called `create()` that instantiate an object given a prototype:

{% highlight javascript %}

var a = {
  "propertyName1": "value1"
}
var b = Object.create(a);

{% endhighlight %}

We can then verify whether an object is prototype of another. This considers indirect prototype inheritance as well, so, since `Object.prototype` is prototype of `a`, `Object.prototype` is prototype of `b` too.

{% highlight javascript %}

> a.isPrototypeOf(b);
true
> b.isPrototypeOf(a);
false
> Object.prototype.isPrototypeOf(a);
true
> Object.prototype.isPrototypeOf(b);
true

{% endhighlight %}

### String, Array and Function

In javascript, strings, arrays and functions are objects! That's why when we run the `Object.prototype.toString` function we get:

{% highlight javascript %}

// We'll see why we need to use call() shortly
Object.prototype.toString.call({})
> "[object Object]"
Object.prototype.toString.call("")
> "[object String]"
Object.prototype.toString.call([])
> "[object Array]"
Object.prototype.toString.call(function() {})
> "[object Function]"

{% endhighlight %}

The difference is that while objects inherit from `Object.prototype`, strings, arrays and functions inherit from `String.prototype`, `Array.prototype` and `Function.prototype` respectively. Since these prototypes are objects, strings, arrays and functions inherit from `Object.prototype` as well:

{% highlight javascript %}

> Object.prototype.isPrototypeOf("");
true
> Object.prototype.isPrototypeOf([]);
true
> Object.prototype.isPrototypeOf(function() {});
true

{% endhighlight %}

Let's take a closer look into functions. We can declare and call a function like this:

{% highlight javascript %}

> var myFunction = function() {
  return x + 1;
}
> myFunction(2)
3

{% endhighlight %}

Let's take a look at which properties are inherited from `Function.prototype`:

{% highlight javascript %}

> Object.getOwnPropertyNames(Function.prototype)
[
  "arguments",
  "caller",
  "constructor",
  "bind",
  "call",
  "apply",
  ...
]

{% endhighlight %}

One of them is `call`, which we used above to invoke the function `Object.prototype.toString()`. It takes several arguments. It will call the function and bind the first argument to `this` within that function and the remaining of the arguments will be used as the actual arguments for the function. For example, if a function is defined as

{% highlight javascript %}

var add = function(y, z) {
  return this.x + y + z;
}

{% endhighlight %}

We can invoke it by:

{% highlight javascript %}

> var obj = {x: 4};
> add.call(obj, 5, 6)
15

{% endhighlight %}

An alternative way to do that is using the function `apply`, where you pass an explicit argument to be bound to `this`, and the arguments in a array:

{% highlight javascript %}

> var obj = {x: 4};
> add.apply(obj, [5, 6])
15

{% endhighlight %}

Having the parameter `this` is important so we can have stateful function, which enables us to simulate object oriented patterns.

### Object oriented paradigm

We can simulate a class with functions and prototypes. The function represents a constructor of the class, which will have the same name as the function, and methods are defined by adding functions to the prototype:

{% highlight javascript %}

var MyClass = function(x) {
  this.x = x;
}
MyClass.prototype.myMethod = function() {
  return this.x;
}

{% endhighlight %}

To make new instances, we can invoke the function with the operator `new`:

{% highlight javascript %}

> var obj = new MyClass(10);
> obj.myMethod();
10

{% endhighlight %}

To understand how the `new` operator works, we can implement a method that does the same:

{% highlight javascript %}

> var myNew = function(f) {
  // We can't do arguments.slice(1), because arguments
  // is not an array. Note that args is.
  var args = Array.prototype.slice.call(arguments, 1);

  // Create a new object using f's prototype
  var obj = Object.create(f.prototype);

  // Invoke f as if it was a constructor, having
  // object as the state (this)
  f.apply(obj, args);

  return obj;
}

// Syntax is slightly different
var obj = myNew(MyClass, 10);
> obj.myMethod();
10

{% endhighlight %}



We can simulate inheritance by instantiating an object and add properties to it:

{% highlight javascript %}

// Automatically inherits MyClass.prototype
var MyDerivedClass = new MyClass();

MyDerivedClass.myDerivedMethod() {
  return "this only exists in MyDerivedClass";
}
// We can override methods as well
MyDerivedClass.myMethod() {
  return this.x * 2;
}
var obj2 = new MyDerivedClass();

{% endhighlight %}

Crockford [1] doesn't like this pattern. His main argument is that if we forget the `new` keyword when instantiating we can cause unexpected behavior, because if no object is bound to `this`, it uses the global variable `this`:

{% highlight javascript %}

> var x = MyClass();
> this.x;
10

{% endhighlight %}

His suggestion is not to use the class pattern at all, but rather work with prototypes inheritance directly.

### Appendix: ES6 Classes

ECMAScript Edition 6 (ES6) is the next standard for the javascript language. The regular object-oriented class syntax is supported, but most browsers today run ES5 or lower.

We can already write ES6 syntax and use code transformers known as transpilers. Google has an open-source library called [Traceur](https://github.com/google/traceur-compiler/wiki/GettingStarted) to transform ES6 syntax into ES5.

We can define a class representing a point:

{% highlight javascript %}

class Point {
  construct(x, y) {
    this.x = x;
    this.y = y;
  }

  getX() {
  	return this.x;
  }

  static getDimension() {
    return 0;
  }
}

var p = new Point(10, 20);

{% endhighlight %}

It will generate this code: [http://goo.gl/W7jFRU](http://goo.gl/W7jFRU)

### References

* [[1]("http://www.amazon.com/dp/0596517742")] Javascript: The Good Parts - Douglas Crockford
* [[2]("http://yehudakatz.com/2011/08/12/understanding-prototypes-in-javascript/")] Yehuda Katz - Understanding Prototypes in Javascript
* [[3]("https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/")] MDN - JavaScript reference 
