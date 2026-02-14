---
layout: post
title: "Move Semantics in C++"
tags: [c++]
excerpt_separator: <!--more-->
vanity: "2022-03-01-moving-semantics-cpp"

---
{% include blog_vars.html %}

In this post we'll explore how to use modern C++ (C++11 onwards) to implement moving semantics, that is, how to move data between variables so as to avoid unnecessary copying.

We'll first understand rvalues references and see how it can be leveraged for implementing moving semantics.

<!--more-->

## lvalues and rvalues

In a simplistic way, **lvalues** are expressions that can be assigned to, i.e. they can appear on the *left* side of an assignment, while **rvalues** are the other type of expressions. Examples:

{% highlight c++ %}
int a = 42;
int b = f();
int* c = &a;
{% endhighlight %}

All of `a`, `b` and `c` are lvalues, while `42`, `f()` are rvalues, because they cannot be assigned to.

Another key property of rvalues is that they cannot be reused. In the code below, we seem to be apparently be reusing `f()` but the second call of `f()` is not the same as the first.

{% highlight c++ %}
int a = f();
int b = f();
{% endhighlight %}

In the next example, `x` is a lvalue that is reused.

{% highlight c++ %}
int x = f();
int a = x;
int b = x;
{% endhighlight %}

One way to put this is that rvalues cannot be *read* from again, analogous to how `const` values cannot be *written* to again.

## The rvalue Reference Syntax

A reference type is denoted by adding `&` to the type. In the example below `y` is a reference to `int`.

{% highlight c++ %}
int x = 42;
int &y = x;
{% endhighlight %}

Reference types can only refer to lvalues (like `x` above), not rvalues. In C++11, the *rvalue reference* was introduced, which is indicated by `&&`:

{% highlight c++ %}
int &&x = 42; // rvalue reference
int &y = x;   // lvalue reference
{% endhighlight %}

To disambiguate, the regular reference syntax can also be qualified as *lvalue reference*.

So far we only discussed the syntax. Let's now discuss the semantics of lvalues and rvalues.

## The Semantics of References

References are essentially aliases, so in the example below when we mutate `x` after initializing `y`, the latter will also be changed and vice-versa:

{% highlight c++ %}
int x = 42;
int &y = x;
x += 1;
print("%d\n", y); // 43
y += 1;
print("%d\n", x); // 44
{% endhighlight %}

We can use references to avoid copies, for example:

{% highlight c++ %}
struct A {
  A() { printf("new\n"); }
  A(A &x) { printf("copy\n"); }
};

A x = A(); // prints "new"
A y = x;   // prints "copy"
A &z = x;  // nothing is printed
{% endhighlight %}

When we do `A y = x` the copy constructor is invoked, while `A &z = x` doesn't invoke any constructors. We can think of `z` as an alias to `x`. The same thing applies to function calls:

{% highlight c++ %}
void f(A x) {}
void g(A& x) {}

A x = A();
f(x); // prints "copy"
g(x); // nothing is printed
{% endhighlight %}

The same behavior applies to rvalue references, except that now we can pass a rvalue `A()`:

{% highlight c++ %}
void h(A&& x) {}

h(A()); // prints "new"
{% endhighlight %}

Note that it prints `"new"` from the `A()` call, not from the `h()` call.

## Overload Resolution of References

As we know, C++ allows overloading, which means there can be multiple functions with the same name but different type signature. The function that ends up being called depends on the types of arguments.

Because `&` can only receive lvalues and `&&` only rvalues, having overloads for both `&` and `&&` is not ambigous:

{% highlight c++ %}
void f(A& x) { printf("lvalue\n"); }
void f(A&& x) { printf("rvalue\n"); }

A x = A();
f(x);   // prints "lvalue"
f(A()); // prints "rvalue"
{% endhighlight %}

## Copy and Move Constructors

A **copy constructor** of a class `A` is a constructor that takes a lvalue reference to `A`. For example:

{% highlight c++ %}
struct A {
  A(A& x) { printf("copy\n"); }
};
{% endhighlight %}

A **move constructor** of a class `A` is a constructor that takes a rvalue reference to `A`. For example:

{% highlight c++ %}
struct A {
  A(A&& x) { printf("move\n"); }
};
{% endhighlight %}

The idea behind the *move* terminology is that rvalues have the guarantee they won't be read again, so we can assume we can mess with the input as we see fit, including destroying/emptying it. One more realistic example is a class that holds onto some memory:

{% highlight c++ %}
int Block = sizeof(int)*100;
struct Block {
  int *mem_ = 0;
  Block() {  // new
    mem_ = (int*)malloc(N);
  }

  Block(Block const &x) { // copy
    mem_ = (int*)malloc(N);
    memcpy(mem_, x.mem_, N  );
  }

  Block(Block &&x) { // move
    mem_ = x.mem_;
    x.mem_ = 0;
  }
};
{% endhighlight %}

In a copy constructor we need to clone the input's memory because whatever `&x` is referring to might be used again, but in a move constructor we can simply *move* that memory since we know the reference it to a rvalue which won't be read from again.

## Casting lvalue References to rvalue References

We can cast a lvalue reference (`&T`) to a rvalue one (`&&T`) via `static_cast<T&&>()` or via `std::move()`. Example:

{% highlight c++ %}
void f(A& x) { printf("lvalue\n"); }
void f(A&& x) { printf("rvalue\n"); }

A x = A();
f(static_cast<A&&>(x)); // prints "rvalue"
A y = A();
f(std::move(y));        // prints "rvalue"
f(A());                 // prints "rvalue"
{% endhighlight %}

This is essentially telling the compiler: "I know this is a lvalue but trust me I won't try to read from it later, so treat it as a rvalue".

One example where it's useful is in defining `swap()`:

{% highlight c++ %}
void swap(const A &x, const A &y) {
  A t(x); // t = A(&x) -- copy
  x = y;  // x = A(&y) -- copy
  y = t;  // y = A(&t) -- copy
}
{% endhighlight %}

We can see the copy constructor is called 3 times. We can use `std::move()` to force the move constructor to be called:

{% highlight c++ %}
void swap(const A &x, const A &y) {
  A t(std::move(x)); // t = A(&&x) -- move
  x = std::move(y);  // x = A(&&x) -- move
  y = std::move(t);  // y = A(&&t) -- move
}
{% endhighlight %}

So here we're saying it's fine to treat `x` and `y` as rvalues since we'll overwrite them before we read from them again. For `t` we know it won't be read from since it's local.

Note how `std::move()` doesn't do anything special in regards to moving. It simply adds a semantic layer on top of this casting. Similarly, nothing in the syntax of rvalue references is specific to moving data, it's all about the meaning we add on top of it (a design pattern of sorts) hence the *semantic* bit in move semantics.

Since move semantics is not something the compiler understands, this "assume lvalue is rvalue" is a contract that must be honored by the code. The compiler won't prevent us from doing:

{% highlight c++ %}
Block x;
Block y = std::move(x);
// we lied to the compiler about x -> seg. fault
std::cout << x.mem_[0] << std::endl;
{% endhighlight %}

## Constructor Elision

Constructor elision is an optimization the compiler performs to avoid a copy when it knows lvalue won't be reused. For example:

{% highlight c++ %}
struct A {
  A() { printf("new\n"); }
  A(A &x) { printf("copy\n"); }
};

A f() {
  auto x = A();
  return x;
}

A y = f(); // prints "new"
{% endhighlight %}

In theory the variable `x` in `f()` would be copied to a temporary place before being assigned to `y`, but most compilers will special case this and skip the copy constructor, so we don't have to worry about moving here.

It's possible to turn off this behavior by compiling with the `-fno-elide-constructors` flag. Re-running the same code should now print `"new"` and `"copy"`.

## Conclusion

In this post we learned about rvalue references and move semantics and how they're connected. Thomas Becker's *C++ Rvalue References Explained* [1] is an excellent resource that goes into details while being very accessible through step-by-step progression.

One key observation that made me internalize why rvalue references are useful for move semantics is that *rvalues cannot be read from*. This constraint enables more efficient operations such as moving data instead of copying. I haven't seen it explicitly called out in the articles about move semantics I read.

## Related Posts

[Rust Memory Management]({{blog}}/2019/03/13/rust-memory-management.html). The move semantics is a first class citizen in Rust via the ownership model and can be enforced by the compiler. Here's an example from that post where the move is enforced by the compiler:

{% highlight rust %}
let mut vec1 = vec![1, 2, 3];

// Assignment -> transferring ownership from vec1 to vec2
// This is the end of scope for vec1.
let mut vec2 = vec1;

// Error: vec1 cannot be read from/written to
println!("{}", vec1.len());
{% endhighlight %}

Mentioned by [[Book] Effective Modern C++]({{blog}}/2022/10/25/review-effective-modern-cpp.html).


## References

* [[1](http://thbecker.net/articles/rvalue_references/section_01.html)] C++ Rvalue References Explained, T. Becker
* [[2](https://en.cppreference.com/w/cpp/language/move_constructor)] C++ reference: Move constructors
* [[3](https://en.cppreference.com/w/cpp/language/reference)] C++ reference: Rvalue references
