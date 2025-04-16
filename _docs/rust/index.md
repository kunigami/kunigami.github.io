---
layout: doc
title: "Rust Cheatsheet"
---

# Index
{:.no_toc}

1. TOC
{:toc}

# Basic types

## Type names

* `char`
* `f32`, `f64` - float point type

## Arrays

Arrays are of fixed size. For variable size, see `Vec`.

### Initialize

{% highlight rust %}
// Initialize array
let arr: [i32; 3] = [1, 2, 3];
let arr: [i32; 10] = [0; 10];
{% endhighlight %}

### Iterate

{% highlight rust %}
let it:
let v: Option<Self::Item> = it.next()
{% endhighlight %}

## Bool

* Type: `bool`
* Examples: `true`, `false`

## Enum

{% highlight rust %}
enum Direction {
    N,
    E,
    S,
    W,
}
{% endhighlight %}

## Integers

* Types: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`

### Bit operations

<code>!</code> is the Rust version of <code>~</code>

## Iterator

Note that `Iterator` is not a class but rather a trait that classes implement.

Get the next element:

{% highlight rust %}
// returns Split class which implements Iterator
let mut parts = s.split(',');
let e: Option<&str> = parts.next();
{% endhighlight %}

NOTE: `next()` is not idempotent.


## Option

* Type: `option<T>`
{% highlight rust %}
// Handle option:
match my_option {
    Some(value) => value,
    None => 1, // handle null value
}
{% endhighlight %}

Create `Some`:

{% highlight rust %}
Some(1)
{% endhighlight %}

Throw if `None`:

{% highlight rust %}
my_option.unwrap()
{% endhighlight %}

Test if some:

{% highlight rust %}
my_option.is_some()
{% endhighlight %}

Test if none:

{% highlight rust %}
my_option.is_none()
{% endhighlight %}

Get a default value if none:

{% highlight rust %}
let v = my_option.unwrap_or(default_value)
{% endhighlight %}

or if the default is expensive to compute:

{% highlight rust %}
let v = my_option.unwrap_or_else(|| get_default_value())
{% endhighlight %}


## Pair

Done via tuples. See *Tuple*.

## String

### Types

* `String` - string object
* `&str` - string [slice](https://doc.rust-lang.org/book/ch04-03-slices.html)
* `&'static str` - string literal

### String (variable length)

{% highlight rust %}
// Empty string
let mut my_str: String = "".to_owned();

// Other form
let mut my_str = String::from("Hello");

// Length
my_str.len();

// Iterate over its characters
for c in my_str.chars() {
    // do something with c
}
{% endhighlight %}

### Operations

{% highlight rust %}
let concatenated = format!("{}{}", a, b);
{% endhighlight %}

From literal strings:

{% highlight rust %}
let s: String = "hello".to_owned();
{% endhighlight %}

Convert to int:

{% highlight rust %}
let i = s.parse::<i32>().expect("Should be numeric");
{% endhighlight %}

Convert from int:

{% highlight rust %}
let i: i32 = 10;
let s = i.to_string();
{% endhighlight %}

Convert to `Vec<char>`:

{% highlight rust %}
let cs: Vec<char> = s.chars().collect();
{% endhighlight %}

Split:

{% highlight rust %}
let parts: Vec<&str> = s.split(',').collect();
{% endhighlight %}

**Split into multiple lines.** There's a shortcut for splitting by the character `\n`:

{% highlight rust %}
let parts: Vec<&str> = s.lines().collect();
{% endhighlight %}

Substring:

{% highlight rust %}
s.contains(p);
{% endhighlight %}


Trim:

{% highlight rust %}
let trimmed = s.trim();
{% endhighlight %}

## Struct

keywords: record / object / shape

{% highlight rust %}
struct Tree {
    guess: Vec<i32>,
    children: Vec<Tree>,
}
{% endhighlight %}

### Destructuring ###

{% highlight rust %}
let foo = Foo { x: (1, 2), y: 3 };
let Foo { x: (a, b), y } = foo;
{% endhighlight %}

### Methods ###

{% highlight rust %}
struct Rectangle {
    length: u32,
    width: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.length * self.width
    }
}
{% endhighlight %}

## Tuple

Type: `(T1, T2)`, e.g. `(i32, bool)`

Create:

{% highlight rust %}
let tup: (i32, String) = (64, "hello")
{% endhighlight %}

Access:

{% highlight rust %}
tup.0 // 64
tup.1 // "hello"
{% endhighlight %}

or via destructuring:

{% highlight rust %}
let (n, s) = &tup;
{% endhighlight %}

## Void

"void" is an empty tuple an in rust is called the **unit** type. Example:

{% highlight rust %}
fn returns_void() -> () {
    return ();
}
{% endhighlight %}

`()` is also what gets returned if no return is provided:

{% highlight rust %}
fn returns_void() -> () {}
{% endhighlight %}

# Functions

{% highlight rust %}
fn myFun(arg1: i32, arg2: i32) -> i32 {
}
{% endhighlight %}

Rust doesn't support default arguments

## Closure

keywords: lambda

{% highlight rust %}
    let plus_one = |x: i32| x + 1;
{% endhighlight %}

### Multi-line

{% highlight rust %}
let multi_line = |x: i32| {
    let mut result: i32 = x;
    result += 1;
    result
}
{% endhighlight %}

# Flow Control

## Conditional

{% highlight rust %}
if n < 0 {
    print!("{} is negative", n);
} else if n > 0 {
    print!("{} is positive", n);
} else {
    print!("{} is zero", n);
}
{% endhighlight %}

Early-return/continue:

{% highlight rust %}
let Some(value) = maybe else {
    continue;
}
{% endhighlight %}


## Loops

{% highlight rust %}
for i in 0..3 {
    println!("{}", i);
}
{% endhighlight %}

See also "Iterating" on different data structures.

### iter() vs into_iter()

`iter()` borrows the collection, so it's still valid after the loop. However, each element is also a borrowed reference. There's a variant `iter_mut()`, in which the borrowed reference is mutable, so any changes are done inline on the collection.

{% highlight rust %}
for item in vec.iter() {
    // If vec is Vec<T>, item is a borrowed reference &T
}
{% endhighlight %}

`into_iter()` moves the collection into the loop, so it's not valid afterwards. Each element in the loop is not a reference.

{% highlight rust %}
for item in vec.into_iter() {
    // If vec is Vec<T>, item is T, and was moved from vec
}
// vec is not valid anymore
{% endhighlight %}

If you wish to mutate the element:

{% highlight rust %}
for mut item in vec.into_iter() {
    // item was moved from vec
}
{% endhighlight %}


## Result

Keyword: Exceptions

### API

{% highlight rust %}
let r: Result<()> = Ok(());
if r.is_ok() {
    println!("ok");
}
if r.is_err() {
    println!("not ok: {}", r.unwrap_err());
}
{% endhighlight %}

### Example

Result has type `Result<T, E>` and is union of `Ok<T>` and `Err<E>`. Example of a function that returns a `Result`:

{% highlight rust %}
fn maybe(v: i32) -> Result<i32, String> {
    if (v == 0) {
        return Err("Cannot be 0".to_owned());
    }
    Ok(v)
}
{% endhighlight %}

On the caller side:

{% highlight rust %}
let x = maybe(v);
match x {
    Ok(value) => println!("{}", value),
    Err(error) => println!("{}", error),
}
{% endhighlight %}

### Generic Error

To avoid specifying the error type, one can use the `Result` from the crate `anyhow`. Importing create in `Cargo.toml`:

{% highlight rust %}
[dependencies]
anyhow = "1.0"
{% endhighlight %}

Example from before:

{% highlight rust %}
use anyhow::{Result, anyhow};
fn maybe(v: i32) -> Result<i32> {
    if v == 0 {
        return Err(anyhow!("Cannot be 0"));
    }
    Ok(v)
}
{% endhighlight %}

The rest is the same. Noting that `anyhow::Result<T>` is an alias for `Result<T, anyhow::Error>`. The `anyhow:Error` is created using the macro `anyhow!`.

To map `Result<T, E>` into `Result<T>`:

{% highlight rust %}
use anyhow::{Result, anyhow};
fn my_func() -> Result<T, E>;

my_func()
    .map_err(|e| Error::new(e).context("new error"))?;
{% endhighlight %}


### Propagation

If a function calls functions returning `Result` it can propagate the results like so:

{% highlight rust %}
use anyhow::{Result, anyhow};
...
fn forward(v: i32) -> Result<i32> {
    let r = maybe(v);
    if r.is_err() {
        return r;
    }
    let x = r.unwrap();
    Ok(x + 1)
}
{% endhighlight %}

There's a very convenient operator, `?`, to avoid this boilerplate:

{% highlight rust %}
use anyhow::{Result, anyhow};
...
fn propagate(v: i32) -> Result<i32> {
    let x = maybe(v)?;
    Ok(x + 1)
}
{% endhighlight %}

Keyword: rethrow

If some context is needed, we can use `with_context()`:

{% highlight rust %}
use anyhow::{Result, anyhow};
...
fn propagate(v: i32) -> Result<i32> {
    let x = maybe(v).with_context(||
        format!("third_error")
    )?;
    Ok(x + 1)
}
{% endhighlight %}

In case of error

```
third_error

Caused by:
0: original_error
1: secondary_error

```

# Data structures

## BTreeMap

`BTreeMap` is another implementation of a efficient key-value (the other being `HashMap`). One analogy for C++ is that `BTreeMap` is `std::ordered_map` and `HashMap` is `std::unordered_map`.

{% highlight rust %}
use std::collections::BTreeMap;

// Create empty
let mut my_map = btreemap! {};

// Create initialized
use maplit::btreemap;
let my_map = btreemap! {
    "blue" => 1,
    "green" => 2,
    "red" => 3
};
{% endhighlight %}

## BTreeSet

There's an analogy between `BTreeSet` and `HashSet` with `BTreeMap` and `HashMap`.

{% highlight rust %}
// Create empty
let mut my_map = btreeset![];

// Create initialized
use maplit::btreeset;
let fruits = btreeset![
    "apple",
    "banana"
]
{% endhighlight %}

## Vector

### Initialize ###

{% highlight rust %}
let mut vec = Vec::new();
{% endhighlight %}

Fixed size, same value:

{% highlight rust %}
let mut vec = vec![0; 100];
{% endhighlight %}


###  Inserting ###

{% highlight rust %}
vec.push(1);
{% endhighlight %}

### Iterating ###

{% highlight rust %}
for item in vec.iter() {
    ...
}
{% endhighlight %}

### Mapping ###

{% highlight rust %}
let u = vec![1, 2, 3];
let v: Vec<_> = u.iter().map(f).collect();
{% endhighlight %}

### Filtering ###

{% highlight rust %}
let u = vec![1, 2, 3];
let v: Vec<_> = u.iter().filter(f).collect();
{% endhighlight %}

In-place

{% highlight rust %}
let mut u = vec![1, 2, 3];
u.retain(f);
{% endhighlight %}

### Length

{% highlight rust %}
vec.len()
{% endhighlight %}

### Sorting

{% highlight rust %}
vec.sort_by(|a, b| a.cmp(b))
{% endhighlight %}

### Destructured assignment

Like in Python, we can do destructured assignment by assuming a fixed length of a vector, but we have to handle the other cases:

{% highlight rust %}
let [a, b] = vec.as_slice() else {
    panic!();
}
{% endhighlight %}

## HashMap

Reference: [HashMap](https://doc.rust-lang.org/std/collections/struct.HashMap.html).

Import name:

{% highlight rust %}
use std::collections::HashMap;
{% endhighlight %}

Type definition:

{% highlight rust %}
HashMap<String, String>;
{% endhighlight %}

Create empty:

{% highlight rust %}
let mut my_map = HashMap::new()
{% endhighlight %}

Create initialized:

{% highlight rust %}
let my_map = HashMap::from([
    ("blue", 1),
    ("green", 2),
    ("red", 3),
]);
{% endhighlight %}

or

{% highlight rust %}
use maplit::hashmap;
let my_map = hashmap!{
    "blue" => 1,
    "green" => 2,
    "red" => 3
};
{% endhighlight %}

Insert:

{% highlight rust %}
my_map.insert(
    "key_a".to_string(),
    "value_a".to_string(),
);
{% endhighlight %}

Access:

{% highlight rust %}
match my_map.get("key_a") {
    Some(value) => println!("value={}", value),
    None => println!("not found")
}
{% endhighlight %}

Update

{% highlight rust %}
if let Some(value) = my_map.get_mut(key) {
    *curr_value = 1;
}
{% endhighlight %}

All values

{% highlight rust %}
for value in my_map.values() {
}
{% endhighlight %}

All keys

{% highlight rust %}
for key in my_map.keys() {
}
{% endhighlight %}

## HashSet

Use:

{% highlight rust %}
use std::collections::HashSet;
{% endhighlight %}

Create:

{% highlight rust %}
let s = HashSet::new();
{% endhighlight %}

Create initialized:

{% highlight rust %}
use maplit::hashset;
let my_map = hashset![1, 2, 3];
{% endhighlight %}

From vector:

{% highlight rust %}
let v = vec![1, 2, 3];
// cloned needed if borrowing v
let s: HashSet<_> = v.iter().cloned().collect();
{% endhighlight %}

Set intersection:

{% highlight rust %}
let i: HashSet<_> = s1.intersection(&s2).cloned().collect();
{% endhighlight %}

## Queue

{% highlight rust %}
use std::collections::VecDeque;

// Create
let mut queue = VecDeque::new();

// Enqueue
queue.push_back("a");

// Dequeue
if let Some(x) = queue.pop_front() {
    // use x
}

// Is empty?
if !queue.is_empty() {
    // use queue
}
{% endhighlight %}

# Object Oriented



## Structures

{% highlight rust %}
pub structure MyClass {
    my_field: bool,
}

impl MyClass {
    // Constructor-like
    pub fn new() -> MyClass {
        return MyClass {
        my_field: true,
        }
    }

    // Read Method
    pub fn set(&self, value: bool) {
        self.my_field = value;
    }

    // Write Method
    pub fn set(&mut self, value: bool) {
        self.my_field = value;
    }
}
{% endhighlight %}


## Traits

Traits are analogous to interfaces in other languages (though it can have default implementation).

{% highlight rust %}
pub trait Summary {
    fn summarize(&self) -> String;
}
{% endhighlight %}

Implement a trait for a structure:

{% highlight rust %}
pub struct Article {
    pub text: String,
}

impl Summary for Article {
    fn summarize(&self) -> String {
        self.text
    }
}
{% endhighlight %}

Note: we can't implement external traits on external types.

### Default implementation

{% highlight rust %}
pub trait Summary {
    fn summarize(&self) -> String {
        String::from("(Read more...)")
    }
}

impl Summary for Article {}
{% endhighlight %}

### Trait type

Traits can be used as type constraints:

{% highlight rust %}
pub fn notify(item: &impl Summary) {
    println!("Breaking news! {}", item.summarize());
}
{% endhighlight %}

Syntax sugar to:

{% highlight rust %}
pub fn notify<T: Summary>(item: &T) {
    println!("Breaking news! {}", item.summarize());
}
{% endhighlight %}

### Multiple trait types

Multiple traits requirement:

{% highlight rust %}
pub fn notify(item: &impl Summary + Display) {
    ...
}
{% endhighlight %}

or

{% highlight rust %}
pub fn notify<T: Summary + Display>(item: &T) {
    ...
}
{% endhighlight %}

### Return trait type

{% highlight rust %}
pub fn get() -> impl Summary {
    ...
}
{% endhighlight %}

Note however that `get()` cannot return different implementations of `Summary`.

### Conditional struct implementation

Below, the method `my_summary` is only implemented for template types implementing `Summary`, e.g. `C<Article>`.

{% highlight rust %}
struct C<T> {
    x: T,
}

impl<T: Display> C<T> {
    fn my_display(&self) {
        ...
    }
}
{% endhighlight %}

### Conditional trait implementation

A trait can be implemented for a struct conditioned on its type satisfying some other trait:

{% highlight rust %}
impl<T: Display> AnotherTrait for T {
    ...
}
{% endhighlight %}

### Dynamic dispatch

If we want to have a pointer to objects of a class implementing a trait `MyTrait`, we can use dynamic dispatch so that methods are resolved at runtime. Example:

{% highlight rust %}
struct A {}
struct B {}
trait MyTrait {
    fn method(&self) {}
}

impl MyTrait for A {
    fn method(&self) {}
}
impl MyTrait for B {
    fn method(&self) {}
}

fn f(ptr: Arc<dyn MyTrait>) {
    ptr.method()
}


f(Arc::new(A {}));
f(Arc::new(B {}));
{% endhighlight %}



# I/O

## Read from stdin

{% highlight rust %}
use std::io::{self, Read};

let mut input = String::new();
io::stdin().read_to_string(&mut input)
    .expect("Failed to read input");
{% endhighlight %}

## Read CLI arguments

{% highlight rust %}
use std::env;

// Skip program name
let args: Vec<String> = env::args().skip(1).collect();
{% endhighlight %}

## Printing to stdout

Vector:

{% highlight rust %}
let vec = vec![1, 2, 3, 4, 5];
println!("Vector:\n{:#?}", vec);
{% endhighlight %}

## Temporary File

{% highlight rust %}
use tempfile::NamedTempFile;
use std::io::{Write, Read};

let mut tmp_file = NamedTempFile::new()?;

// Write
writeln!(tmp_file, "Hello world!")?;

// Read
let mut contents = String::new();
tmp_file.reopen()?.read_to_string(&mut contents)?;
{% endhighlight %}


# Math

A lot of the math operations are methods on the numerical types.

## Exponentiation

{% highlight rust %}
let b: i32 = 10;
let p10: i32 = b.pow(2 as u32);
{% endhighlight %}

Note that the exponent has to be positive, since a negative one could change the type of the base to floating point.

## Square root

{% highlight rust %}
let x: f64 = 10.0;
let y: f64 = x.sqrt();
{% endhighlight %}

Not defined for integer types.

# Mutability

Variable doesn't need to be mutable if it's initialized only once:

{% highlight rust %}
let x: i32;

if check() {
    x = 1;
} else {
    x = 2;
}
{% endhighlight %}

# Attributes

Attributes are in the form `#[foo]` and placed on top of functions, structs and modules to add metadata to it, e.g.

{% highlight rust %}
#[test]
fn test_invalid_argument() {
    ...
}
{% endhighlight %}

or

{% highlight rust %}
#[derive(Clone, Debug, Eq)]
struct Tree {
    ...
}
{% endhighlight %}

# Modules

* Modules add namespace to functions
* Functions defined inside a module are private unless qualified with `pub`

Declaration:

{% highlight rust %}
mod math {
    fn add(a: i32, b: i32) -> i32 {
        a + b
    }

    pub fn sub(a: i32, b: i32) -> i32 {
        a - b
    }
}
{% endhighlight %}

Usage:

{% highlight rust %}
fn f() {
    math::sub(1, 2);
}
{% endhighlight %}

## Nested

Modules can be nested but because of visibility being private by default, inner modules must be made explicitly public:

{% highlight rust %}
mod outer {
    pub mod inner {
        pub fn f() {
            println!("Hello world");
        }
    }
}

fn f() {
    outer::inner(1, 2);
}
{% endhighlight %}

# Memory Management

## Smart Pointers

`Box` is analogous to `std::unique_ptr` in C++.

{% highlight rust %}
let x = Box::new(5);
{% endhighlight %}

Remembering that move happens by default on assignment in Rust:

{% highlight rust %}
let y = x; // x is now invalid
{% endhighlight %}

`std::sync::Arc` is analogous to `std::shared_ptr` in C++. It's a reference counted pointer to the heap. Creation:

{% highlight rust %}
let x = Arc::new(5);
{% endhighlight %}

Share ownership:

{% highlight rust %}
let y = Arc::clone(&x);
{% endhighlight %}



## Ownership

In Rust we have 3 ways of passing data from one variable to another:

* Copy (default C++ mode)
* Borrow (default Python mode, C++ references)
* Move (C++ std::move)

There's a convention for classes to implement these different modes.

| Mode / semantics | API |
| ---------------- | --- |
| Copy             | `to_` |
| Borrow           | `as_` |
| Move             | `into_` |

Reference: [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/naming.html?#ad-hoc-conversions-follow-as_-to_-into_-conventions-c-conv)
