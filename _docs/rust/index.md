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

* `bool`
* `char`
* `i8`, `i16`, `i32`, `i64` - signed int
* `u8`, `u16`, `u32`, `u64` - unsigned int
* `f32`, `f64` - float point type

## Arrays ##

Arrays are of fixed size. For variable size, see `Vec`.

### Initialize ###

{% highlight rust %}
// Initialize array
let arr: [i32; 3] = [1, 2, 3];
let arr: [i32; 10] = [0; 10];
{% endhighlight %}

### Iterate ###

{% highlight rust %}
let arr: [i32; 3] = [1, 2, 3];
for x in &arr {
    print!("{} ", x);
}
{% endhighlight %}

## Bool ##

* Type: `bool`
* Examples: `true`, `false`


## Optional ##

* Type: `optional<T>`
{% highlight rust %}
// Handle option:
match my_option {
    Some(value) => value,
    None => 1, // handle null value
}

// Create Some
Some(1)

// Throw if None
my_option.unwrap()

// Test if some
my_option.is_some()

// Test if none
my_option.is_none()
{% endhighlight %}

## String ##

### Types ###

* `String` - string object
* `&str` - string [slice](https://doc.rust-lang.org/book/ch04-03-slices.html)
* `&'static str` - string literal

### Concatenation ###

{% highlight rust %}
let concatenated = format!("{}{}", a, b);
{% endhighlight %}

or

{% highlight rust %}
let s = "hello".to_owned();
let t = " world".to_owned();
s. push_str(&t);
{% endhighlight %}

## Struct ##

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

## Tuple ##

{% highlight rust %}
let tup: (i32, String) = (64, "hello")
// Can be accessed in the . notation
tup.0 // 64
tup.1 // "hello"
{% endhighlight %}

## Functions ##

{% highlight rust %}
fn myFun(arg1: i32, arg2: i32) -> i32 {
}
{% endhighlight %}

Rust doesn't support default arguments

## Closure ##

keywords: lambda

{% highlight rust %}
    let plus_one = |x: i32| x + 1;
{% endhighlight %}

### Multi-line ###

{% highlight rust %}
let multi_line = |x: i32| {
    let mut result: i32 = x;
    result += 1;
    result
}
{% endhighlight %}

## Conditional ##

{% highlight rust %}
if n < 0 {
    print!("{} is negative", n);
} else if n > 0 {
    print!("{} is positive", n);
} else {
    print!("{} is zero", n);
}
{% endhighlight %}

## Loops ##

{% highlight rust %}
for i in 0..3 {
    println!("{}", i);
}
{% endhighlight %}

See also "Iterating" on different data structures.

## Bit operations ##

<code>!</code> is the Rust version of <code>~</code>

# Data structures

## Vec - dynamic sized list ##

### Initialize ###

{% highlight rust %}
let mut vec = Vec::new();
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

### Length ###

{% highlight rust %}
vec.len()
{% endhighlight %}

## HashMap ##

Reference: [HashMap](https://doc.rust-lang.org/std/collections/struct.HashMap.html)

{% highlight rust %}
use std::collections::HashMap;

// Type definition
HashMap<String, String>;

// Create New
my_map = HashMap::new()

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

// All values
for value in my_map.values() {
}

// All keys
for key in my_map.keys() {
}
{% endhighlight %}

## String - variable length characters ##

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
