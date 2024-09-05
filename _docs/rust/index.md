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
let e: Optional<&str> = parts.next();
{% endhighlight %}

NOTE: `next()` is not idempotent.


## Optional

* Type: `optional<T>`
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

## Loops

{% highlight rust %}
for i in 0..3 {
    println!("{}", i);
}
{% endhighlight %}

See also "Iterating" on different data structures.

# Data structures

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

## HashMap ##

Reference: [HashMap](https://doc.rust-lang.org/std/collections/struct.HashMap.html)

{% highlight rust %}
use std::collections::HashMap;

// Type definition
HashMap<String, String>;

// Create New
let mut my_map = HashMap::new()

// Create New Initialized
let my_map = HashMap::from([
    ("blue", 1),
    ("green", 2),
    ("red", 3),
]);

// Insert
my_map.insert(
    "key_a".to_string(),
    "value_a".to_string(),
);

// Access
match my_map.get("key_a") {
    Some(value) => println!("value={}", value),
    None => println!("not found")
}

// Update
if let Some(value) = my_map.get_mut(key) {
    *curr_value = 1;
}

// All values
for value in my_map.values() {
}

// All keys
for key in my_map.keys() {
}
{% endhighlight %}

## Set

Use:

{% highlight rust %}
use std::collections::HashSet;
{% endhighlight %}

Create:

{% highlight rust %}
let s = HashSet::new();
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

Attributes are in the form `#[foo]` and placed on top of functions and structs to add metadata to it, e.g.

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
