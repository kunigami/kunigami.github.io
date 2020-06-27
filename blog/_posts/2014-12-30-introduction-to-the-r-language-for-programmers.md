---
layout: post
title: "Introduction to the R language for programmers"
tags: [r]
---

<figure class="image_float_left">
    <a href="https://kunigami.files.wordpress.com/3014/11/r-logo.png"><img src="{{site.url}}/resources/blog/2014-12-30-introduction-to-the-r-language-for-programmers/3014_11_r-logo.png" alt="r-logo" /></a>
</figure>


In this post we will cover the basics of the R programming language. We'll work with the assumption that the reader has experience with imperative programming languages.

In particular, we're often going to make references to similar features from C++, Python, Matlab/Octave and Javascript. Knowing them will be useful for make associations with R, but it's not necessary.

### History

R is a programming language and software environment for statistical computing. It is an alternative implementation of the S language, combined with lexical scoping (we'll talk about it later). It was developed by Ross Ihaka and Robert Gentleman.

The name of the language comes from the first letter of the authors' names and also because it's right before S.

### Features

R is an interpreted and dynamically typed language. As we shall see, it is centred around vectors, so that even scalars are considered unit vectors in R. It also has built-in matrix operations like Matlab.

R also offers a package manager, which makes it easy to extend the basic package. Common libraries include **ggplot** and **dplyr**, but we won't talk about them here.

Although R has a CLI version, there's a really nice free IDE called [RStudio](http://www.rstudio.com/).

### Syntax

First let's discuss some general R syntax.

**Statements.** Semi-colons in R are optional. R also parse new line as end of statements, much like Javascript.

**Comments.** The `#` character starts a comment for the current line, like in Python.

**Variable Names.** R has a very unique variable name syntax. In C++ and other languages, a variable name can contain basically all letters from the alphabet, numbers and underscore, except that it can't start with a number. In R, the `.` character is also a valid character in variable names, but variables starting with `.` are treated differently, since they are hidden from the default variable listing (`ls()`) [3].

There are some gotchas though: variables can't start with underscore and a number cannot be used in the second position if it started with a dot. Examples of invalid variable names: `_hello`, `.1ello`. Valid variable names: `._`, `..1hello`.

R also has a set of one-line variables that should be reserved but can be overridden in practice. For example, `c` is a function used to create vectors, but it can be easily overridden and cause head scratching.

{% highlight r %}

> c(1, 3)
[1] 1 3
> c <- function (x) {x}
> c(1, 3)
Error in c(1, 3) : unused argument (3)

{% endhighlight %}

Similarly, `T` and `F` are aliases for `TRUE` and `FALSE`, but they're just variables that can also be overridden. Thus, we can mess up and do

{% highlight r %}

> T <- FALSE
> F <- TRUE

{% endhighlight %}

The command `rm(list=ls())` is very useful to clean up a messed up environment.

**Assignment.** In R assignment is often denoted by the `<-` symbol. It also allows left to right assignment by using the symbol `->`, for example:

{% highlight r %}

2 -> x;

{% endhighlight %}

R also accepts the usual `=` sign for assignment. The reason for the `<-` is merely historical and a more context can be found [here](http://blog.revolutionanalytics.com/2008/12/use-equals-or-arrow-for-assignment.html).

**Blocks of code.** Like C++, R has the concept of blocks of code delimited by curly braces: `{}`. One important observation is that it returns the result of the last expression. Example:

{% highlight r %}

> c <- {
+   a = 3 + 7;
+   b = a + 1;
+ }
> c
[1] 11

{% endhighlight %}



## Data Types

We'll now discuss the main built-in data types in R.

### Vectors

Vectors in R represent an ordered list of elements of the same (atomic) type. The main types are `integer`, `numeric` (integers and floats), `character` (strings), `logical` (booleans) and `complex`. Vectors cannot contain vectors or functions.

As we mentioned before, R is a vector-centred language, so, in the command line, if we type:

{% highlight r %}

> 2
[1] 2  # Unit vector

{% endhighlight %}

To create a vector with more elements, we use the `c()` function:

{% highlight r %}

> x <- c(1, 2, 3)
> x
[1] 1 2 3

{% endhighlight %}

R also offers helper functions for generating vectors, like `seq()` and `rep()`.

{% highlight r %}

> seq(1, 10, 3)
[1]  1  4  7 10

> rep(c(1, 2), 5)
[1] 1 2 1 2 1 2 1 2 1 2

{% endhighlight %}

We can also generate empty vectors. Since vectors must have types, we need to inform which type we're intending to have (since it can't figure out from the element):

{% highlight r %}

> numeric()
numeric(0)
> character()
character(0)
> logical()
logical(0)

{% endhighlight %}

**Vector operations.** R has built-in operators and functions that work with vector out of the box. This include things like vector addition, scalar multiplication and element-by-element multiplication:

{% highlight r %}

> c(3, 4) + c(7, 6)
[1] 10 10

> c(1, 2, 3) * 2
[1] 2 4 6

> c(1, 2, 3) * c(1, 2, 3)
[1] 1 4 9

{% endhighlight %}

R is able to perform scalar and also element-by-element multiplication in a quite peculiar way: by repeating the smaller vector until it becomes the size of the larger vector. To illustrate, consider the following example:

{% highlight r %}

> c(2, 2, 2, 2, 2, 2) * c(2, 3)
[1] 4 6 4 6 4 6

{% endhighlight %}

It repeated c(2, 3) 3 times, so we basically did:

{% highlight r %}

> c(2, 2, 2, 2, 2, 2) * c(2, 3, 2, 3, 2, 3)
[1] 4 6 4 6 4 6

{% endhighlight %}

The same behaviour happens with a scalar, since it's an unit vector repeated many times.

If the length of the larger one is not a multiple of the length of the smaller one, it used only part of the vector in the last repetition, and displays a warning.

{% highlight r %}

> c(1, 2, 3) + c(10, 20, 30, 40)
Warning: longer object length is not a multiple of shorter object length
[1] 11 22 33 41

{% endhighlight %}

**Notable arithmetic operators.** Most operators work as we expect, but the ones I found worth noting were: `^` for exponentiation, `/` is float division, `%%` is the module function and `%/%` is integer division.

**Vector access.** Indexes in R start from 1, like Matlab. [Programmers are used to it starting from 0](https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html).

We can use numeric vectors to index another vector:

{% highlight r %}

> c(1, 2, 3, 5, 8, 13)[c(1, 4)]
[1] 1 5

{% endhighlight %}

Negative numbers is used for exclusion (can't be mixed with positive numbers):

{% highlight r %}

> c(1,2,3,4,5)[c(-1, -3)]
[1] 2 4 5

{% endhighlight %}

Logical vectors are used for filtering:

{% highlight r %}

> x <- 1:10
> x %% 2 == 0
[1] FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE
> x[x %% 2 == 0]
[1]  2  4  6  8 10

{% endhighlight %}

Character vectors can be used to index vectors by name. We can set names to vectors so they work like an associative array:

{% highlight r %}

> fruits <- c(1, 2, 3)
> names(fruits) <- c("apple", "banana", "coconut")
> fruit[c("apple", "coconut")]
[1] 1 3

{% endhighlight %}

We can also use indexing for assignment:

{% highlight r %}

> v <- c(10, 11, 12)
> v[c(1, 3)] = 20
> v
[1] 20 11 20

{% endhighlight %}

It's worth noting that numeric indexing can cause resizing if it's greater than the current vector length:

{% highlight r %}

v <- numeric()
v[10] <- 1
v
[1] NA NA NA NA NA NA NA NA NA  1

{% endhighlight %}

Because of vector indexing, we can easily perform permutations of a vector just by indexing a permutation of its indexes:

{% highlight r %}

> c(10, 20, 30, 40, 50)[c(4, 2, 3, 1, 5)]
[1] 40 20 30 10 50

{% endhighlight %}

The `order()` function makes use of this idea. Instead of returning a copy of the sorted vector, it returns a permutation of the original index representing a natural ordering:

{% highlight r %}

> v <- c("a", "c", "d", "b")
> order(v)
[1] 1 4 2 3
> v[order(v)]
> [1] "a" "b" "c" "d"

{% endhighlight %}

### Lists

As we saw, vectors require all elements to be of the same atomic type. Lists are an ordered list of elements of any type, including lists, vectors and any other types.

{% highlight r %}

>list(c(1,2), "hello", list(TRUE, FALSE))
[[1]]
[1] 1 2

[[2]]
[1] "hello"

[[3]]
[[3]][[1]]
[1] TRUE

[[3]][[2]]
[1] FALSE

{% endhighlight %}

As suggested in the output, list access can be done by double brackets:

{% highlight r %}

> x <- list(c(1,2), "hello", list(TRUE, FALSE))
> x[[1]]
[1] 1 2

{% endhighlight %}

One key difference between the single bracket and double brackets is that single brackets always return the same type of the indexed element, making it especially suitable for subsetting. For example:

{% highlight r %}

> x[1]
[[1]]
[1] 1 2

{% endhighlight %}

Returns a list with one element, whereas the double brackets return the element itself. Lists can have names like vectors do:

{% highlight r %}

y <- list(a=c(1,2), b="hello", c=list(TRUE, FALSE))

{% endhighlight %}

And we can use the double bracket notation for access as well:

{% highlight r %}

> y[["b"]]
[1] "hello"

{% endhighlight %}

There's a common alternative to this syntax which consists of using `$` and the name without quotes:

{% highlight r %}

> y$b
[1] "hello"

{% endhighlight %}

It's similar to javascript where you can also access an object obj with key `"foo"` either as `obj["foo"]` or `obj.foo`. This pattern is useful when the value to that key is a function, so instead of calling `obj["myFunction"](baz)` we can do `obj.myFunction(baz)`. The same idea applies to R.

### Matrices

Matrices are two-dimensional vectors. In most languages, matrices are just vectors of vectors, but since in R vectors cannot contain vectors, they cannot represent matrices.

The advantage of having a new type for matrices is that we can guarantee they always have consistent sizes (that is, all rows have the same number of columns) and also that we can have built-in operations for them, like matrix multiplication.

Since matrices are just 2 dimensional vectors, we also have the restriction over its elements. We can construct a matrix out of a vector:

{% highlight r %}

> matrix(c(1, 2, 3, 4, 5, 6), nrow=2, ncol=3)
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6

{% endhighlight %}

Note how it generates the matrix column-first by default. We can change it to be row-first by setting the `byrow` flag:

{% highlight r %}

> matrix(c(1, 2, 3, 4, 5, 6), nrow=2, ncol=3, byrow=TRUE)
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    4    5    6

{% endhighlight %}

Indexing is straightforward, we just provide a pair of values representing the row index and the column index:

{% highlight r %}

> x <- matrix(c(1, 2, 3, 4, 5, 6), nrow=2, ncol=3, byrow=FALSE)
> x[1, 2]
[1] 3

{% endhighlight %}

We can easily index entire rows or columns and also subsets:

{% highlight r %}

> x[1,] # first row
[1] 1 3 5

> x[,2] # second column
[1] 3 4

> x[, c(1, 3)] # first and third column
     [,1] [,2]
[1,]    1    5
[2,]    2    6

{% endhighlight %}

### Data Frames

Simply put, data Frames are to matrices what lists are to vectors. They usually represent tables, where each column from the table may have different types. We can construct a data frame from a set of vectors. They have to have the same number of elements, but if not, R applies the repetition principle:

{% highlight r %}

> x <- data.frame(c(1, 2, 3, 4), c(1, 2), 1);
> x
  c.1..2..3..4. c.1..2. X3
1             1       1  3
2             2       2  3
3             3       1  3
4             4       2  3

{% endhighlight %}

We can give names to the columns, similar how we named lists and vectors:

{% highlight r %}

colnames(x) <- c("a", "b", "c");

{% endhighlight %}

Data frames can be indexed pretty much like lists, but in this case we are always guaranteed to get a vector of the same length.

Useful functions to explore data frames are `head()` and `summary()`:

{% highlight r %}

> head(x, 3) # Displays the top 3 rows
  a b c
1 1 1 3
2 2 2 3
3 3 1 3

{% endhighlight %}



## Control Structures

Now that we've covered the basics of data types, let's discuss some control structures, mainly conditionals and loops.

### Conditionals

The boolean values are `TRUE` and `FALSE`. We also can use the `T` and `F` variables as alias.

Logical composition is done by `&`, `&&`, `|` and `||`. The difference is that the shorter versions (`&`, `|`) are vectorized (ie, element-by-element), while the other ones expect scalars, and will only look at the first element of the vectors.

Also, the double version is "lazy" in a sense that it doesn't compute the second argument if the first is enough.

Some examples:

{% highlight r %}

>TRUE | c(FALSE, TRUE)
# same as c(TRUE, TRUE) | c(FALSE, TRUE)
TRUE TRUE
>TRUE || c(FALSE, TRUE)
# same as TRUE || FALSE
TRUE
> TRUE || someFunction()
# someFunction is never called
TRUE

{% endhighlight %}

**If clause.** The if clause is similar to many languages, but since R blocks return the value of last expression and one-line blocks don't require braces, this allow us to define a ternary operator without a different syntax:

{% highlight r %}

> y <- if (4 > 3) 10 else 0
> y
[1] 10

{% endhighlight %}

**For loop.** It always iterates over elements of a vector:

{% highlight r %}

for (x in c("a", "b")) {
  ...
}

{% endhighlight %}

If we're interested in the index of the elements, one idea is to use `seq_along(v)`, which returns a vector with the indices from a vector `v`:

{% highlight r %}

xs <- c("a", "b")
for (i in seq_along(xs)) {
  x <- xs[i];
  ...
}

{% endhighlight %}

The while construct is nothing special, but one interesting related construct is the repeat. It is equivalent `while(TRUE)`, but since it's a common programming practice, it was aliased as a keyword.

Also, instead of the usual `continue` to skip to the next iteration of the loop, R uses `next`.

R has a switch construct, but it works very differently from the C++ style. For example, the following C++ pseudo-code:

{% highlight r %}

switch(variable) {
  "value_1": expression_1;
  break;
  "value_2": expression_2;
  break;
}

{% endhighlight %}

Would be translated to:

{% highlight r %}

switch(variable, value_1=expression_1, value_2=expresion_2).

{% endhighlight %}

It's shorter, but it doesn't allow fallthroughs for example.



## Functions

Functions are defined anonymously and can be assigned to like in Javascript. The key difference is that R returns the last statement (the `return` keyword is optional).

{% highlight r %}

> f <- function (x) {
  x + 1;
}
> f(1)
[1] 2

{% endhighlight %}

Functions in R can take default values and named calls like in Python. For example:

{% highlight r %}

> f <- function (a, b=2) {
  a + b;
}
> f(3)
[1] 5
> f(b=3, a=3)
[1] 6

{% endhighlight %}

Differently from Python, R doesn't require arguments with default values to come after the arguments without. For example, the following construct is valid in R but not in Python:

{% highlight r %}

> f <- function (a=1, b) {
  a + b;
}
> f(3)
Error in f(2) : argument "b" is missing, with no default
> f(b=3)
[1] 4

{% endhighlight %}

Varargs. R lets us work with a variable number of arguments by using the ellipsis variable. It works the same as the rest parameters from the Javascript ES6 version. For example, the following JS code:

{% highlight r %}

x = function(y, ...other) {
  return sum(other);
}
x(2, 3, 4, 5); //  11

{% endhighlight %}

Can be translated to this R snippet:

{% highlight r %}

> x <- function(y, ...) {
+   sum(...)
+ }
> x(2, 3, 4, 5);
[1] 11

{% endhighlight %}

**Scoping.** When reading R material, we often see it has lexical scoping as opposed to dynamic scoping. It wouldn't be worth mentioning in basic introduction since that's how most languages work, but it's one of the main difference from the original S language.

The difference between lexical scoping and dynamic scoping relies on which variable a free variable refers to in a function (a free variable is one that hasn't been created or passed as argument). In lexical scoping it's determined in compile time (static), while in dynamic scoping it's determined in runtime (dynamic). A good explanation with examples is given in [this post](http://stackoverflow.com/questions/22394089/static-lexical-scoping-vs-dynamic-scoping-pseudocode).

An example in R would be the following:

{% highlight r %}

> f <- function() {
  x <- 1;
  a <- function() {
    x;
  }
  b <- function() {
    x <- 2;
    a();
  }
  b();
}
> f()
[1] 1

{% endhighlight %}

The `x` in the a function is a free variable. In lexical scoping it's bound to the variable of the ancestor where it is defined, in this case, `f()`. In dynamic scoping, it's bound to the variable to the ancestor where it is called, thus `b()`, so in this case the code above would return 2.

**Lazy evaluation.** Arguments in a function are only evaluated if they are used. A good example is given [here](http://adv-r.had.co.nz/Functions.html#function-arguments).

### Conclusion

In this post we present a simplified introduction to the R language and compared it with languages like C++, Javascript and Python. We covered data types, control structures and functions.

R has a very unique syntax and much of it is due to legacy reasons. It has been increasing in popularity in recent years. R also seems more error-tolerant than other languages and this can cause unexpected behaviours. [1] offers some criticism about the language.

I was planning to learn the Julia programming language, but I had opportunities to use R this year and also there as a quick course on [Coursera](https://www.coursera.org/course/rprog). The R course is easy for programmers and only lasts for 4 weeks.

### References

* [[1](http://cran.r-project.org/doc/manuals/r-release/R-intro.html)] 
 CRAN: An Introduction to R
* [[2](http://www.johndcook.com/blog/r_language_for_programmers/)] 
 R Language for Programmers - John D. Cook
* [[3](http://stackoverflow.com/questions/7526467/what-does-the-dot-mean-in-r-personal-preference-naming-convention-or-more)] 
 Stack Overflow: What does the dot mean in R – personal preference, naming convention or more?
* [[4](http://blog.revolutionanalytics.com/2008/12/use-equals-or-arrow-for-assignment.html)] 
 Revolutions: Use = or &lt;- for assignment?
* [[5](http://ariya.ofilabs.com/2013/03/es6-and-rest-parameter.html)] 
 ECMAScript 6 and Rest Parameter - Ariya Hidayat
* [[6](http://adv-r.had.co.nz/Functions.html#function-arguments)] 
 Advanced R: Functions - by Hadley Wickham
* [[7](http://www.burns-stat.com/pages/Tutor/R_inferno.pdf)] 
 The R Inferno - Patrick Burns

