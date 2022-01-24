---
layout: post
title: "Haskell – Function Currying"
tags: [haskell]
excerpt_separator: <!--more-->
vanity: "2011-11-06-haskell-function-currying"

---
{% include blog_vars.html %}

<figure class="image_float_left">
    <img src="{{resources_path}}/haskell-curry.jpeg" alt="Haskell Curry"/>
</figure>


Function currying corresponds to the partial evaluation of a function's arguments. The technique is named after Haskell Curry, who according to Wikipedia [1] re-invented the method (Moses Schönfinkel had already developed the method previously). The Haskell language is also named after Curry.

In this post I will talk a little about currying functions in Haskell, based mainly on Chapter 4 of the book *Real World Haskell* [1].

<!--more-->

## Functions with multiple arguments in Haskell

The syntax for specifying the type of a function in Haskell with multiple arguments seems a bit confusing at first. For example, if we want to define a function that sums two integers, we must specify it as follows.

{% highlight haskell %}
sum:: Int -> Int -> Int
sum ab = a + b
{% endhighlight %}

From the header it doesn't give the impression that the function takes two integers as an argument and returns an integer. That's because functions in Haskell actually take only one argument. When there are multiple arguments, we call the `f` function with the first argument and return a `g` function with the occurrences of that first argument replaced by the value passed as the argument.

For example, consider a function `f(a, b, c)` over three integers `a`, `b` and `c`. The first call of the function `f(1,2,3)` returns a new function `g(b, c) = f(1, b, c)`, which in turn returns another function `h(c) = g(2, c)`, which takes only one argument.

To test this in practice, we can do in the terminal (ghci):

{% highlight text %}
> :type sum
sum :: Int -> Int -> Int
> let sumThree = sum 3
> :type sumThree
sum3 :: Int - > Int
> sumThree 7
10
{% endhighlight %}

In the example above, the parameter `a` of sum, was replaced by `3` and the resulting function assigned to sumThree, which now only takes one argument.

In a function with several parameters, we can work with intermediate functions resulting from the currying process by providing only some of the parameters. For example,

{% highlight haskell %}
-- Function that returns a triple from 3 arguments
foo abc = (a, b, c)
foo1 = foo 3
foo2 = foo 4 5
foo3 = foo 6 7 8
{% endhighlight %}

In this case, `foo1` receives two arguments, `foo2` just one and `foo3` is already the result of the function `foo`.

## Skipping parameters using currying

Suppose we want to select prime numbers from an input list. A very simple version, where we try to find a divisor of `p` going from `2` to $sqrt{p}$, can be given by:

{% highlight haskell %}
isPrime p
    | p < 2     = False
    | otherwise = isPrimeAux p 2

isPrimeAux pq
    | q*q > p        = True
    | p `mod` q == 0 = False
    | otherwise      = isPrimeAux p (q+1)
{% endhighlight %}

In Haskell's standard library, there is a function called `filter`, which takes an array and a predicate (i.e. function that takes an element of that array and returns true or false). We can pass our function `isPrime` to generate a list with prime elements from 1 to 50 for example.

{% highlight text %}
> :type filter
filter :: (a -> Bool) -> [a] -> [a]
> filter isPrime [1..50]
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
{% endhighlight %}

To make it easier, we can create a function that receives an array and returns the list of primes:

{% highlight haskell %}
primesList xs = filter isPrime xs
{% endhighlight %}

Using currying, it is possible to define the function above without parameter

{% highlight haskell %}
primesList = filter isPrime
{% endhighlight %}

In the first case, we are basically defining a new wrapper function that will call `filter isPrime` when invoked. In the second, we are more or less creating an alias to `filter isPrime`.

## Currying over the second parameter

By default, the substitution of arguments is done from the first argument, that is, from left to right. However, for functions with two (binary) parameters, we can use infix notation to replace the second argument first.

The infix notation consists of calling the function between(backticksbacktick). For example, the sum function could be called like this:

{% highlight haskell %}
1 `sum` 2 -- 3
{% endhighlight %}

This syntax is interesting for code readability purposes. When calling the function in infix form passing only the second argument, the substitution is done only for it. As an example, let's define the power function that takes two integers a and b and raises a to b (the sum function isn't funny because it's commutative).

{% highlight haskell %}
let power a b = a ^ b
{% endhighlight %}

Now we can curry and apply the function to just one argument

{% highlight haskell %}
-- Substitute b instead of a
let square = (`power` 2)
square 9 -- 81
{% endhighlight %}

## Currying in C++

The STL provides some tools for working with the functional paradigm, through the library `functional`. Through it we can reproduce our first example in Haskell:

{% highlight c++ %}
#include <functional>
#include <iostream>
using namespace std;

struct adder : public binary_function<int, int, int> {
    int operator() (int a, int b) const {
        return a + b;
    }
};

// currying over the first function argument
int main(){
    adder sum;
    binder1st<adder> sumThree = bind1st(sum, 3);
    cout << sumThree(7) << endl; // 10
    cout << sumThree(-1) << endl; // 2
}
{% endhighlight %}

In this case we work with a functor, which is an object representing a function. The class `adder` must derive from the class `binary_function`. The function `bind1st` makes a bind of the first argument of the function, and returns an object of type `binder1st`, which derives from the class `unary_function` which takes a single argument.

It is possible to bind the second argument using the function `bind2nd`.

## References

* [[1](http://book.realworldhaskell.org/)] Real World Haskell - [Chapter 4](http://book.realworldhaskell.org/read/functional-programming.html), by Bryan O'Sullivan, Don Stewart, and John Goerzen.
* [[2](https://en.wikipedia.org/wiki/Haskell_Curry)] Wikipedia: Haskell Curry
