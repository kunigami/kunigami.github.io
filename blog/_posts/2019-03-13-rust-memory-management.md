---
layout: post
title: "Rust Memory Management"
tags: [rust]
---

Graydon Hoare is a Software Developer who created the Rust programming language while working at Mozilla Research [1]. He has an interesting presence on the internet, for example responding to this [question](https://www.reddit.com/r/rust/comments/7qels2/i_wonder_why_graydon_hoare_the_author_of_rust/) and on [Twitter](https://twitter.com/graydon_pub/status/958192076209897472).

In this post we'll talk about one of the key features of Rust, which I find the hardest to wrap my head around, which is its memory management.

### Motivation

In most programming languages we either have to manage memory allocation ourselves or rely on a complex garbage collector to which we have limited control and can lead to unpredictable performance bottlenecks.

Rust has a set of constraints around memory allocation that results in a deterministic automatic memory management.

We'll now delve into more details on what these constraints are, how they enforce the necessary guarantees to enable an efficient memory management by the runtime.

### Ownership Rules

Rust has the following basic rules around ownership:
* Each value in Rust has a variable that’s called its owner
* There can only be one owner at a time
* When the owner goes out of scope, the value will be dropped
They're very basic but have deep implications in how we think about programs. Let's analyze some of them.

### Assignment transfers ownership

To conform the second rule, whenever we assign a variable to another, we are transferring the ownership from one variable to another. For example:

{% highlight rust %}
let mut vec1 = vec![1, 2, 3];
// Performing changes is okay. vec1 is mutable
vec1.push(4);

// Assignment -> transferring ownership from vec1 to vec2
// This is the end of scope for vec1.
let mut vec2 = vec1;

// Error: vec1 cannot be read from/written to
println!("{}", vec1.len());

// vec2 can still perform further mutations
vec2.push(5);
{% endhighlight %}

In the example above, `vec1` transfer ownership of its data to `vec2`. This means that we can neither read nor write to `vec1` anymore. It's as if it was out of scope (unless it is assigned ownership to some other data later).

By having a single owner we don't have to worry about keeping track of references to a given object as garbage collectors do to know when we are allowed to free the memory. For example, if we had:

{% highlight rust %}
let mut vec1 = vec![1, 2, 3];
{
    let mut vec2 = vec1;
    vec2.push(5);
}
{% endhighlight %}

Because `vec2` owns the vector allocated initially and it goes out of scope after line 4, the runtime can free the memory safely, since we know `vec1` cannot access that data after line 3.

Similarly, when we use a variable as argument to a function, its data is transferred to the parameter. In the example below, `vec1`'s data is transferred to `vec` in `mutate_vec()`, so we cannot access it in line 9.

{% highlight rust %}
fn mutate_vec(mut vec: Vec<i32>) -> Vec<i32> {
    vec.push(4);
}

let mut vec1 = vec![1, 2, 3];
mutate_vec(vec1);

// Error: ownership transferred in function call
println!("{}", vec1.len());
{% endhighlight %}

One way to return the ownership back to `vec1` is for the function to return the argument.

{% highlight rust %}
fn mutate_vec(mut vec: Vec<i32>) -> Vec<i32> {
    vec.push(4);
    return vec;
}

let mut vec1 = vec![1, 2, 3];
// Transferring ownership to the function
vec1 = mutate_vec(vec1);
println!("{}", vec1.len());

{% endhighlight %}

### References &amp; Borrowing

To avoid transferring data to a variable on assignment, we can use references. In the example below, `vec2` "borrows" the data from `vec1` but there's no ownership transfer. We have read access to the data via `vec2`, which we can do by dereferencing `vec2` with &amp;`vec2`.

{% highlight rust %}
let vec1 = vec![1, 2, 3];
{
    // & indicates a reference, so we are not transferring 
    // ownership, just borrowing
    let vec2 = &vec1;
    println!("{}", &vec2);
}
// vec1 still has access to the data, since there was no
// ownership transfer
println!("{}", vec1.len());

{% endhighlight %}

If we want write access, we need to make `vec1` mutable and also obtain a mutable reference to `vec1` via the &amp;mut operator, like in the example below:

{% highlight rust %}
let mut vec1 = vec![1, 2, 3];
{
    let vec2 = &mut vec1;
    // We can mutate s1's data via a mutable reference!
    vec2.push(4);
    println!("{}", vec2.len());
}
println!("{}", vec1.len());

{% endhighlight %}

However, if we try to access the data via `vec1` while `vec2` has a mutable reference to it, we'll get an error.

{% highlight rust %}
let mut vec1 = vec![1, 2, 3];
{
    let vec2 = &mut vec1;
    // We can mutate s1's data via a mutable reference!
    vec2.push(4);
    // Error: cannot access data when a mutable 
    // reference is in place
    println!("{}", vec1.len());
}
println!("{}", vec1.len());
{% endhighlight %}

We can take as many (non-mutable) references as we want:

{% highlight rust %}
let mut vec1 = vec![1, 2, 3];
{
    let vec2 = &vec1;
    let vec3 = &vec1;
    println!("{} {}", vec2.len(), vec3.len());
}
println!("{}", vec1.len());
{% endhighlight %}

But once we try to obtain a mutable reference, we get an error:

{% highlight rust %}
let mut vec1 = vec![1, 2, 3];
{
    let vec2 = &vec1;
    let vec3 = &vec1;
    println!("{} {}", vec2.len(), vec3.len());

    // Error: cannot obtain mutable reference once
    // other references are in place
    let vec4 = &mut vec1;
}
println!("{}", vec1.len());

{% endhighlight %}

**Read and write locks.** Rust enforces these constraints to prevent race condition bugs in multi-thread applications. In fact the borrowing mechanism via references is very similar to *read locks* and *write locks*.

To recall, if some data has a **read lock**, we can acquire as many other reads locks as we want but we cannot acquire a write lock. This way we prevent data inconsistency between multiple reads since we prevent data mutations by not allowing writes. Conversely, if some data has a **write lock**, we cannot acquire neither read or write locks.

We can see that the regular borrowing implements a read lock while a mutable borrowing implements a write lock.

**Dangling references.** One potential issue with references is that if we return a reference to some variable that went out of scope.

{% highlight rust %}
fn dangle() -> &Vec<i32> {
    let v = vec![1, 2, 3];
    &v
}

{% endhighlight %}

Luckily, the compiler will prevent this case by making a compile error. It's now always the case that we cannot return a reference. If the reference we're returning was sent as argument, it's still a valid one, for example:

{% highlight rust %}
fn no_dangle(v: &Vec<i32>) -> &Vec<i32> {
    println!("{}", v.len());
    return &v;
}
{% endhighlight %}

However, if we try to pass two parameters, we'll run into a compile error:

{% highlight rust %}
fn get_largest(v1: &Vec<i32>, v2: &Vec<i32>) -> &Vec<i32> {
    if (v1.len() > v2.len()) {
        return v1;
    }
    return v2;
}

{% endhighlight %}

To see why it's not possible to guarantee this memory-safe, we can consider the following code calling get_largest:

{% highlight rust %}
let vec1 = vec![1, 2, 3];
let result;
{
    let vec2 = vec![];
    result = get_largest(&vec1, &vec2);
}
println!("{}", result.len());

{% endhighlight %}

Here we're sending references for both `vec1` and `vec2`, and returning back either of them. If `vec2` happened to be larger, we'd return it to result and try to access the data after `vec2` went out of scope.

However, if result is used while both `vec1` and `vec2` are in scope, it should be theoretically safe to allow calling get_largest:

{% highlight rust %}
let vec1 = vec![1, 2, 3];
{
    let vec2 = vec![];
    let result = get_largest_with_lifetime(&vec1, &vec2);
    println!("{}", result.len());
}
{% endhighlight %}

In fact, it's possible and we'll see how next, but we'll need to introduce a new terminology.

### Lifetimes

The lifetime of a variable represents the duration in which the variable is valid. In the example below, the lifetime of variable a is from line 2 to line 7. The lifetimes for b is from 4 to 5, for c is line 5 and for d is line 7. Note that a's lifetime contains all other lifetimes and b's lifetime contains c's lifetime.

{% highlight rust %}
{
    let a = 1;
    {
        let b = 2;
        let c = 3;
    }
    let d = 4;
}

{% endhighlight %}

We can annotate a function using a syntax similar to generics to parametrize the function arguments by their lifetimes. For example, if we want to include the lifetimes of the arguments in `get_largest()`, we can do:

{% highlight rust %}
fn get_largest_with_lifetime<'a>(
    v1: &'a Vec<i32>, 
    v2: &'a Vec<i32>
) -> &'a Vec<i32> {
    if (v1.len() > v2.len()) {
        return v1;
    }
    return v2;
}
{% endhighlight %}

This is essentially binding the each variable to the lifetime specified by `'a`. Note it's forcing that both variables have the same lifetime and that the return type has the same lifetime as the input parameters.

Now, if we replace `get_largest()` with `get_largest_with_lifetime()`, we won't get compiler errors. In the example below, result has the same lifetime was the common lifetimes of `vec1` and `vec2`, which is `vec2`'s lifetime. This means we're fine using result with the inner block.

{% highlight rust %}
let vec1 = vec![1, 2, 3];
{
    let vec2 = vec![];
    let result = get_largest_with_lifetime(&vec1, &vec2);
    println!("{}", result.len());
}
{% endhighlight %}

### Conclusion

Rust documentation is very detailed and well written. All the concepts presented in this post are explained in length there. In here I'm presenting them in my own words and different examples (`vec` instead of `string`).

I also tried to group topics around memory management, which is different from the documentation. In there "lifetimes" are not covered under ownership and I skipped the generics discussion.

### References

* [[1](https://en.wikipedia.org/wiki/Rust_(programming_language))] Wikipedia - Rust (programming language)
* [[2](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)] Rust Documentation: What is Ownership?
* [[3](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html)] Rust Documentation: References &amp; Borrowing
* [[4](https://doc.rust-lang.org/book/ch10-03-lifetime-syntax.html)] Validating References with Lifetimes
