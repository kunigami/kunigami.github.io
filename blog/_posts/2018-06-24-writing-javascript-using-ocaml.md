---
layout: post
title: "Writing JavaScript using OCaml"
tags: [javascript, ocaml]
---

<figure class="image_float_left">
    <a href="https://bucklescript.github.io/en/"><img src="{{site.url}}/resources/blog/2018-06-24-writing-javascript-using-ocaml/2018_06_output.png" alt="output" /></a>
</figure>

In this post we'll explore [BuckleScript](https://bucklescript.github.io/), a framework that enables developers to write JavaScript applications using OCaml syntax.

Note that BuckleScript is not a way to convert general OCaml code into JavaScript, but rather write JavaScript applications using OCaml's syntax and type system. The end runtime and libraries are still JavaScript.

For example, if you want to work with dates, you wouldn't look for the corresponding OCaml library, but instead include the JavaScript `Date` module in your OCaml code.

Let's first compare BuckleScript with similar frameworks to understand better why it exists and when to use it. Then we'll follow with several examples to get a better grasp of BuckleScript and how it translates to JavaScript.

It's better to have some knowledge of OCaml, and we have [written extensively](https://kunigami.blog/category/programming/ocaml/) about it, but it's also easy enough to get the gist of the benefits from our simple examples.


### Comparisons


**BuckleScript vs js_of_ocaml**
`js_of_ocaml` is another framework that connects the OCaml and JavaScript worlds.

According to [[4](https://github.com/BuckleScript/bucklescript/blob/master/site/docsource/Differences-from-js_of_ocaml.adoc)] both projects aim to compile OCaml code to JavaScript. The differences pointed are:

* `js_of_ocaml` takes low-level bytecode from OCaml compiler, BuckleScript takes the high-level rawlambda representation from OCaml compiler
* `js_of_ocaml` focuses more on existing OCaml ecosystem(opam) while BuckleScript’s major goal is to target npm

A simplistic way to see the differences is that BuckleScript is for JavaScript developers to start using better language features from OCaml, while `js_of_ocaml` is for OCaml developers to be able to run their code in the browser.


**BuckleScript vs ReasonML**
ReasonML is often mentioned together with BuckleScript, which makes it a bit confusing to understand their differences at first.

Compared do OCaml, [ReasonML](https://reasonml.github.io/) has a friendlier syntax (for people coming from JS) and better support for JSX (inline XML tags). The difference in syntax is significant that we are really talking about some dialect of OCaml.

Note that BuckleScript and ReasonML are complementary. BuckleScript can compile either OCaml or ReasonML to JavaScript. ReasonML in mostly about the syntax.


**BuckleScript vs TypeScript**
[Typescript](https://www.typescriptlang.org/) is a framework for adding types to a JavaScript codebase, similar to [Flow](https://flow.org/en/).

At first glance, TypeScript and BuckleScript seem to serve different purpose, but one of the main advantages of using OCaml to write JavaScript applications is to provide type safety.

In [2], Hongbo provides a comparison between the two systems. Some of the pros and cons raised are:

**TypeScript**

*  Pros:
  * Designed for JS, easy inter-operate with JS
* Cons:
  * Compiler slow (not scalable for FB/Google scale)
  * Verbose, very limited type inference
  * Start up time slow (very hard to build traditional build tools)
  * Types are only used for tooling - soundness is not the design goal, not very reliable
  * No code optimizations

**BuckleScript**

* Pros:
  * Compiles much faster, scales better
  * Sound type system, global type inference
  * Types are used in code optimization, optimizing compiler
  * Outputs to JavaScript but can also generate native backend for X86, ARM
* Cons:
  * Learning curve is higher compared with TypeScript
The author also suggests that OCaml is a better language than JavaScript, without providing more details.


### Setup
The easiest way to try out examples is through this [BuckleScript playground](https://bucklescript.github.io/bucklescript-playground/index.html#Event_Handler).

To try it locally, we can follow [5]. It generates an initial bsconfig.json file and the compilation process can be done via npm. The process involve converting a OCaml file (`.ml`) to a JavaScript file (`.bs.js`). The latter should be included in your application, but both should be considered part of your codebase and hence committed.


### Examples
Because most people learning BuckleScript are familiar with JavaScript and less familiar with OCaml, I'll provide the reverse examples: how to do implement a given JavaScript snippet of code in OCaml?

**console.log('Hello World')**

How to print to stdout using BucketScript? The most basic program is simply printing a Hello World. In OCaml we would do "Print.printf" to print to stdout, but OCaml modules are not readily available for BuckleScript. We can use the `Js.log()` function for this purpose:

{% highlight ocaml %}
let _ = Js.log ("Hello World")
{% endhighlight %}

This maps to:

{% highlight javascript %}
console.log("Hello World");
{% endhighlight %}

Note that Js log is a library provided by BuckleScript to make the integration with JavaScript more seamless.

**Looping over an Array**

We can use an imperative-style code in OCaml:

{% highlight ocaml %}
let sum arr =
    let v = ref 0 in
    for i = 0 to (Array.length arr) - 1 do
       v := !v + arr.(i)
    done;
    !v

let () = Js.log (sum [| 10; 20; 30 |])
{% endhighlight %}

Note that arrays have a different syntax than in JavaScript: `[| 10; 20; 30 |]`. If we generate this code we get:

{% highlight javascript %}
var Caml_array = require("./stdlib/caml_array.js");

function sum(arr) {
  var v = 0;
  for(var i = 0 ,i_finish = arr.length - 1 | 0; i <= i_finish; ++i){
    v = v + Caml_array.caml_array_get(arr, i) | 0;
  }
  return v;
}

console.log(sum(/* array */[
          10,
          20,
          30
        ]));
{% endhighlight %}

The interesting thing here is that we do have access to some basic OCaml libraries, for example `Array`.

Since we are dealing with a functional language, we might as well use some more idiomatic OCaml code. If we are to translate the code to native JavaScript structures, we have to use functions from the Js module:

{% highlight ocaml %}
let sum arr = Js.Array.reduce (+) 0 arr
let () = Js.log (sum [| 10; 20; 30 |])
{% endhighlight %}

Which maps to

{% highlight javascript %}
function sum(arr) {
  return arr.reduce((function (prim, prim$1) {
                return prim + prim$1 | 0;
              }), 0);
}

console.log(sum(/* array */[
          10,
          20,
          30
        ]));
{% endhighlight %}

**Looping over a List**

So `Array` is the basic structure to represent a sequence of items in JavaScript but in OCaml it's `List`. What happens if we use `List` instead?

{% highlight ocaml %}
let sum arr = List.fold_left (+) 0 arr
let () = Js.log (sum [10; 20; 30])
{% endhighlight %}

We simply dropped the \| to use a `List` instead of `Array` and now we can use the more standard fold_left instead of reduce. This translates to:

{% highlight javascript %}
var List = require("./stdlib/list.js");

function sum(arr) {
  return List.fold_left((function (prim, prim$1) {
                return prim + prim$1 | 0;
              }), 0, arr);
}

console.log(sum(/* :: */[
          10,
          /* :: */[
            20,
            /* :: */[
              30,
              /* [] */0
            ]
          ]
        ]));
{% endhighlight %}

This is very interesting! We studied [functional data structures OCaml](https://kunigami.blog/tag/purely-funcional-data-structures/) extensively in the past and the key concept is that by default data structures are [persistent]({{site.url}}/blog/2016/10/23/persistent-data-structures.html). If we look closely what is being passed to `sum()`, we see it's a linked-list like structure: `[10, [20, [30, 0]]]`, the 0 being used to indicate the end.

**Modules**

Another common pattern in JavaScript is to group a bunch of functions inside an Object. The Object name serves as namespace or a module name that can be referenced elsewhere. A natural mapping here are the OCaml modules:

{% highlight ocaml %}
module MyModule : sig
	val add: int -> int -> int
    val sub: int -> int -> int
end = struct
	let add x y = x + y
	let sub x y = x - y
end

{% endhighlight %}

which maps to:

{% highlight javascript %}
function add(x, y) {
  return x + y | 0;
}

function sub(x, y) {
  return x - y | 0;
}

var MyModule = /* module */[
  /* add */add,
  /* sub */sub
];

exports.MyModule = MyModule;
{% endhighlight %}

Note that the signature is only used for compilation/transpilation checks and is erased from the final JavaScript code. Another curious thing is that the functions are exported as arrays to `MyModule`. To me it would make more sense to export them as Object.

**Currying Functions**

A nice feature from functional languages is the concept of currying, which allow us to perform partial application of functions. For example, if we have a function that adds two numbers, we can derive an increment function that partially applies sum by binding the first value to 1:

{% highlight ocaml %}
module Curry = struct
 let add x y = x + y
 let inc = add 1
end
{% endhighlight %}

The resulting JavaScript code is:

{% highlight javascript %}
function add(x, y) {
  return x + y | 0;
}

function inc(param) {
  return 1 + param | 0;
}

var Curry = /* module */[
  /* add */add,
  /* inc */inc
];

{% endhighlight %}

Note that we can already perform partial application of function in vanilla JavaScript via `bind()`, but the syntax is not as neat:

{% highlight javascript %}
const sum = function(a, b) { return a + b; };
const inc = sum.bind(null, 1);

{% endhighlight %}

**Chaining Functions**

Another neat syntax from OCaml is the chaining operator. One can chain functions via the `|>` operator: the result of the lefthand function is passed as argument to the righthand function.

A common use for this is when we want to apply a series of function in sequence without assigning to a variable.

For example, if we have a function to normalize a given string by converting it to lower case, trimming the leading and trailing whitespaces and also converting intermediate spaces to underscores, we could write, in JavaScript:

{% highlight javascript %}
function normalize(s) {
  s = s.toLowerCase();
  s = s.trim();
  s = s.replace(/\s/g, "_");
  return s;
}
{% endhighlight %}

An alternative would be to nest the calls so we don't have to repeat the variable, but that would hurt legibility. In OCaml, we could chain these calls:

{% highlight ocaml %}
let normalize s =
    Js.String.toLowerCase s |>
    Js.String.trim |>
    Js.String.replaceByRe [%bs.re "/ /g"] "_"

{% endhighlight %}

Note the `[%bs.re]` tag. It is a macro that allows us to write regexes using JavaScript syntax. We can avoid repeating the module names if they are all the same:

{% highlight standard ml %}
let normalize s = Js.String.(
    toLowerCase s |>
    trim |>
    replaceByRe [%bs.re "/ /g"] "_"
)
{% endhighlight %}

**Using JavaScript libraries**

One of the main selling points of BuckleScript is that you can adopt it gradually, module by module. This is possible because we can require JavaScript modules inside our OCaml code. For example, if we were to convert the following code that reads a file asynchronously in JavaScript:

{% highlight javascript %}
fs = require("fs");
fs.readFile("data.json", "utf8", (err, data) => {
  console.log(data);
});

{% endhighlight %}

We could do:

{% highlight ocaml %}
external readFile :
  name:string ->
  ([ `utf8
   | `useAscii [@bs.as "ascii"]
   ] [@bs.string]) ->
   (string -> string -> unit) ->
   unit = ""
  [@@bs.module "fs"]

let callback error data = Js.log(data)

let _ = readFile ~name:"data.json" `utf8 callback
{% endhighlight %}

Here, the OCaml code is more verbose but we provided a stronger contract by typing the function `readFile()`. The syntax for importing modules is

{% highlight text %}
external <function name alias> :
  <function type interface> =
    <function name> [[@@bs.module "<module name>"]
{% endhighlight %}

Note: if `` is the same as ``, the latter can be omitted.

**Objects as Maps**

In JavaScript Objects are often used either as maps (key-value) or records (entries of distinct fields). In OCaml we can rely on types to enforce the specific use we want via types. In the example below, we declare a map with type string to int. If we try to set a value with a different type we get a compilation error:

{% highlight ocaml %}
let mapping: string Js.Dict.t = Js.Dict.empty ()
(* Error: This expression has type int but an expression was expected of type *)
let () = Js.Dict.set mapping "some_key" 10

{% endhighlight %}

**Objects as Records**

To represent an Object as a record, we can use a OCaml record type syntax:

{% highlight ocaml %}
type person = {
  name: string;
  age: int;
  job: string [@bs.optional];
} [@@bs.deriving abstract]
{% endhighlight %}

We added the `[@bs.optional]` to indicate that a field is optional. We also added the `[@@bs.deriving abstract]` attribute to indicate it should not be directly instantiated like

{% highlight ocaml %}
{name: "Joe", age: 20, job: "teacher"}
{% endhighlight %}

Instead, it generates a "constructor" function. In the same way, the properties of a record are not directly available. They also need to go through intermediate auto-generated accessors:

{% highlight ocaml %}
(* Creating record *)
let joe = person ~name:"Joe" ~age:20 ~job:"Programmer"

let joeNameVerbose = name joe
let joeName = joe |. name
{% endhighlight %}

The generated JavaScript code translates to an Object:

{% highlight javascript %}
var joe = {
  name: "Joe",
  age: 20,
  job: "carpenter"
};

{% endhighlight %}

The interesting thing is that the generated JavaScript Object is mutable, but within the OCaml code, the record cannot be modified. It's possible to mark it mutable, but the default immutability makes it easier to reason about code.


### Downsides
The benefits being stated, there are two main potential drawbacks of using BuckleScript.

**Mixed languages.** Adopting BuckleScript will cause the codebase to have a mix of different languages, which makes it harder for new developers to ramp up. Of course this can be mitigated by converting the entire codebase to use OCaml.

**Debugging.** We'll be writing code in on language but it's another language that will end up being executed. If a problem happens in the underlying JavaScript code, how to figure out which OCaml code is generating the faulty code?

BuckleScript tries to solve this issue by preserving the structure of the code as much as possible so that it's easier to understand what parts maps to what. This works well if we are using the Js wrappers that resembles the JavaScript code patterns, but it's unclear how easy the structure is preserved if we use more of OCaml persistent data structures or functional programming patterns like currying.

One possible improvement would be to add some traceability to the generated JavaScript code such that you won't need to look at the JavaScript code most of the time, in the same way that one doesn't usually need to inspect Assembly code when their C++ application crashes.


### Conclusion
In this post we did a comparison of BuckleScript with different frameworks and libraries to understand why and when to use it. Following that, we studied a few basic examples which one might encounter in a day-to-day JavaScript programming and how to express that in OCaml.

Through these examples, we saw the OCaml type system in use, as well as some neat syntax and immutable data structures, which can lead to more readable, succinct and reliable code.


### References
* [[1](https://github.com/bucklescript/bucklescript-addons)] BuckleScript Examples
* [[2](https://news.ycombinator.com/item?id=15111265)] BuckleScript: An OCaml to JavaScript compiler (Hacker News discussion)
* [[3](https://bucklescript.github.io/bucklescript-playground/index.html#Event_Handler)] BuckleScript playground
* [[4](https://github.com/BuckleScript/bucklescript/blob/master/site/docsource/Differences-from-js_of_ocaml.adoc)] BuckleScript/bucklescript - Comparisons
* [[5](https://bucklescript.github.io/docs/en/new-project.html)] BuckleScript - New Project
