---
layout: post
title: "Combinators in Haskell"
tags: [haskell]
excerpt_separator: <!--more-->
vanity: "2012-08-05-combinators-in-haskell"

---
{% include blog_vars.html %}

<figure class="image_float_left">
    <a href="https://en.wikipedia.org/wiki/Paul_Graham_(programmer)#/media/File:Paulgraham_240x320.jpg">
        <img src="{{resources_path}}/paul-graham.jpeg" alt="Paul Graham Profile" />
    </a>
</figure>

Paul Graham is an essayist, programmer and investor. He holds a degree from Cornell and a PhD from Harvard [1].

As an essayist, he has written several articles, the most famous of which is [A plan for spam](http://paulgraham.com/spam.html) on the use of a Bayesian filters to combat spam.

As a programmer, Graham is known for his work with Lisp, having written books on the language and developing a dialect called [Arc](https://en.wikipedia.org/wiki/Arc_(programming_language)). He also developed the first web application called Viaweb, later acquired by Yahoo!

As an investor, he founded the company Y Combinator in 2005, which invests in start-ups. Some of the companies funded by Y-Combinator include Dropbox, Airbnb and reddit.

According to their [FAQ](https://www.ycombinator.com/faq.html#q41), the motivation behind the company name is:

> The Y combinator is one of the coolest ideas in computer science. It's also a metaphor for what we do. It's a program that runs programs; we're a company that helps start companies.

In this post I would like to present some notes from my studies on Haskell combinators, describing some of the main types including the Y combinator.

This post originated from my initial studies of [Chapter 9](http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html) of Real World Haskell. At a certain point in the chapter the authors mention the term combinators, not making it very clear what they are and what they are for. I did some research on the subject and decided to write a post.

## Combinators in Haskell

A **combinator** can be defined as a lambda function that has no free variables. A free variable is a variable that has not been passed as a parameter in the lambda function. Some examples of lambda functions in Haskell:

1) The function below is a combinator because the variable `x` is in the parameter:

{% highlight haskell %}
\x -> x
{% endhighlight %}

2) In the following example `y` is a free variable and therefore not a combinator.

{% highlight haskell %}
\x -> x y
{% endhighlight %}

3) Here both `x` and `y` were passed by parameter, so we have a combinator.

{% highlight haskell %}
\xy -> xy
{% endhighlight %}

4) The example below is not a combinator because `f` was not passed as a parameter

{% highlight haskell %}
\x y -> f (x y)
{% endhighlight %}

5) Here's a tricky one: Note that the `+` operator is actually a function that takes two arguments, as in example (4). But since the operator was not passed as a parameter, it is also *not* a combinator.

{% highlight haskell %}
\x y -> x + y
{% endhighlight %}

We can think of a combinator as a function that takes in functions and returns another function that results from combining the input functions.

## Types of Combinators

In the next sections, we will introduce some of the most popular combinators. These combinators are named with a single capital letter. However, in the book To Mock a Mocking Bird [2], Raymond Smullyman named the different types of combinators after birds. Therefore, each section will have the name of the combinator and, in parentheses, the nickname given by Smullyman.

### I Combinator (Identity Bird or Idiot Bird)

This is the simplest combinator, the identity. It is a function that returns its own parameter. In Haskell we can write:

{% highlight haskell %}
i x = x
{% endhighlight %}

This is precisely the definition of the `id` function in the Prelude library.

### K Combinator (Kestrel)

This combinator is the constant combinator, taking two arguments and ignoring the second. In Haskell we can write:

{% highlight haskell %}
k x y = x
{% endhighlight %}

This is precisely the definition of the `const` function in the Prelude library.

### S Combinator (Starling)

The `S` combinator is a function that can be defined as follows in Haskell:

{% highlight haskell %}
s x y z = x z (y z)
{% endhighlight %}

We can write the `I` combinator using just the `S` and `K` combinators as

{% highlight haskell %}
i = s k k
{% endhighlight %}

The proof is as follows:

Applying the function `(s k k)` on an argument `x` we ​​get

{% highlight haskell %}
(s k k) x = s k k x
{% endhighlight %}

Substituting `s` for its definition,

{% highlight haskell %}
(s k k x) = k x (k x)
{% endhighlight %}

Now let `f = (k x)` be any function. We have `k x f = x` by definition, so

{% highlight haskell %}
k x (k x) = k x f = x = i x
{% endhighlight %}

It is possible to show that any combinator can be written as a function of the combinators `S` and `K`!

### B Combinator (Bluebird)

The `B` combinator is defined as

{% highlight haskell %}
b x y z = x (y z)
{% endhighlight %}

Which can be seen as doing the composition of functions `x` and `y` (when curried). For example:

{% highlight haskell %}
f = b (\x -> 2*x) (\x -> x + 1)
print (f 2) -- 6 = (2 + 1) * 2
{% endhighlight %}

This is precisely the definition of Haskell's `(.)` operator. `B`  can be written in terms of `S` and `K` as:

{% highlight haskell %}
b = s (k s) k
{% endhighlight %}

Let's see what this function looks like applied to some arguments. Let `f1 = k s`. Using currying we see that this is a function that takes an argument but always returns `s`. Applying `b` over an argument `x`, we get

{% highlight haskell %}
b x = s f1 k x = f1 x (k x) = s (k x)
{% endhighlight %}

Let `f2 = (k x)`. We have that `f2` is a function that takes any argument and always returns `x`. So we have `b x = s f2`. Let's apply it over two more parameters `y` and `z`:

{% highlight haskell %}
b x y z = (s f2) y z = s f2 y z = f2 z (y z) = x (y z)
{% endhighlight %}

### C Combinator (Cardinal)

This combinator can be written as a function of the combinators `K`, `S` and `B` as

{% highlight haskell %}
c = s (b b s) (k k)
{% endhighlight %}

It can be shown that this combinator is equivalent to

{% highlight haskell %}
c f x y = f y x
{% endhighlight %}

This is precisely the definition of the operator `flip` from Prelude, which returns a function with the order of the parameters changed.

### Y combinator (Sage Bird)

Simply put, the `Y` combinator is a combinator that transforms a non-recursive function into a recursive function!

Before describing it, let's analyze how we would do it without the Y-combinator. For that, let's use as an example a function that calculates the factorial [8]. Using recursion, we can write:

{% highlight haskell %}
fact = \n -> if n == 0 then 1 else n * fact (n - 1)
fact 5 -- 120
{% endhighlight %}

The above lambda expression is not a combinator because it depends on `fact`, which was not passed as a parameter.

Let's try to pass the function by parameter so

{% highlight haskell %}
fact' = \f n -> if n == 0 then 1 else n * f (n - 1)
{% endhighlight %}

Note that `fact'` is not a recursive function. Now we do the following:

{% highlight haskell %}
fact = fact' fact
fact 5 -- prints 120
{% endhighlight %}

We are defining `fact` as the result of `fact'` when we pass `fact` as a parameter. Sounds like magic, but in languages ​​with lazy evaluation this will work. Why?

Let's unwrap the definition of `fact` one level:

{% highlight haskell %}
fact =
  fact' fact =
      \n -> if n == 0 then 1 else return n * fact (n - 1)
{% endhighlight %}

For `n = 0`, the function will return 1 and will not go to the trouble of calculating `fact(n - 1)`.

Let's now unwrap the function for 2 steps:

{% highlight haskell %}
fact = fact' fact = fact' (fact' fact)
  = if n == 0 then 1 else return n *
    (if (n - 1) == 0 then 1 then return (n - 1) * fact (n - 2))
{% endhighlight %}

For `n = 1`, you can see that a lazy evaluation needs no further evaluations than that.

Okay, we've managed to turn a non-recursive `fact'` function into a recursive function. Let's create a helper function so that the `fact'` function is passed as a parameter

{% highlight haskell %}
aux f = f (aux f)
fact = aux fact'
fact 5 -- prints 120
{% endhighlight %}

The `aux` function does what we want the Y combinator to do: takes a non-recursive function (e.g. `fact'`) and returns a recursive one (in this case `fact`). However, `aux` is not a combinator because the `aux` that is called recursively is a free variable.

So let's introduce the Y combinator. Its theoretical definition is given by

{% highlight haskell %}
y = \f -> (\x -> f (xx)) (\x -> f (xx))
{% endhighlight %}

I still haven't been able to understand this definition clearly enough to venture an explanation here. My suggestion is to read [8] where Mike Vaniel derives the above function in Scheme.

Anyway, if we were to implement it in Haskell, we would get a compilation error due to infinite type [3]. To solve this problem, [4] presents the following solution:

{% highlight haskell %}
newtype Rec a = In { out :: Rec a -> a }

y :: (a -> a) -> a
y = \f -> (\x -> f (out xx)) (In (\x -> f (out xx)))
{% endhighlight %}

To avoid problems with infinite type, we encapsulate the second term `(\x -> f (x x))` in a new type, `Rec a`, through the `In` constructor. Lambda functions take `x` which is of type `Rec a` and so we cannot apply `x` to `x` directly. Before we must "unwrap" the function contained in it using out.

Remembering, the `newtype` defines a structure similar to an algebraic data type, with the difference that it only allows a constructor, in the case defined by `In`, and a "getter", specified by `out`.

For example, let's define a type called `NewType` that encapsulates a function that takes a generic type and returns that same generic type, that is, with type `a -> a`.

{% highlight haskell %}
newtype NewType a = Constructor { getter :: a -> a }
{% endhighlight %}

We can wrap a function that takes and returns an integer. For example:

{% highlight haskell %}
sumOne::Int -> Int
sumOne v = v + 1
let x = Constructor sumOne
{% endhighlight %}

We can check the type of this variable in ghci:

{% highlight haskell %}
> :type x
x :: NewType Int
{% endhighlight %}

The getter extracts the function that was encapsulated, that is, we can do:

{% highlight haskell %}
> (getter x) 9
{% endhighlight %}

## Other combinators

This site [5] has a more extensive list of combinators. There is a Haskell package called data-aviary [6] containing the implementation of several of these combinators.

Reg Braithwaite discusses some combinators in practice using Ruby in his homoiconic blog/project [7].

## Conclusion

I was glad to take a break from reading the Real World Haskell book to learn more about combinators. Thanks to that I ended up discovering interesting things like the origin of Graham's company name and that some commonly used functions in Haskell are combinators.

Also, while the Y combinator isn't that useful in practice, the ideas behind it are very interesting (and, in my opinion, complicated!).

## References

* [[1](http://www.paulgraham.com/bio.html)] Paul Graham - Biography
* [2] To Mock a Mockingbird - Raymond Smullyman
* [[3](http://stackoverflow.com/questions/4273413/y-combinator-in-haskell)] StackOverflow - Y Combinator in Haskell
* [[4](https://mail.haskell.org/pipermail/haskell/2006-September/018497.html)] [Haskell] How to define Y combinator in Haskell
* [[5](http://www.angelfire.com/tx4/cus/combinator/birds.html)] Combinator Birds
* [[6](http://hackage.haskell.org/package/data-aviary/)] Hackage – The data-aviary package
* [[7](https://github.com/raganwald/homoiconic)] Github – Homoiconic
* [[8](http://mvanier.livejournal.com/2897.html)] Mike's World-O-Programming – The Y Combinator
* [[9](http://www.mail-archive.com/boston-pm@mail.pm.org/msg02716.html)] Y-Combinator in Javascript
