---
layout: post
title: "Notes on Coursera's Functional Programming Principles in Scala"
tags: [scala]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2017-03-01-notes-on-coursera's-functional-programming-principles-in-scala/2017_02_martin.png" alt="martin" />
</figure>

I've just concluded the *[Functional Programming Principles in Scala](https://www.coursera.org/learn/progfun1/)* class offered by École Polytechnique Fédérale de Lausanne through Coursera. The lecturer is Martin Odersky, the creator of the language. The course is very easy and lasts 4 weeks.

In this post I'll write down my notes from a perspective of someone who knows basic Java and functional programming, that is, this will cover mostly syntax.

The first thing we need to do is install the Java compiler version 1.8.*, by downloading the latest SDK. Then we download `sbt`, the *Scala Build Tool*, which is a framework that handles compiling, testing (similar to Java's *Ant* and *Maven*) and also offers a console (REPL). On mac we can get `sbt` via brew (0.13.8)

{% highlight text %}

brew install sbt

{% endhighlight %}

The recommended project organization is similar to Maven's but having separate directories for Java and Scala:

{% highlight text %}

<project>
  src/
    main/
      resources/
      java/
      scala/
    test/
      resources/
      java/
      scala/
  target/
  build.sbt

{% endhighlight %}

The `target` subdirectory is for the output of generated files.

The build definition file is named `build.sbt` and is located at the top level directory. For the sake of this introduction, we'll use two tools to test and run the code: `sbt`'s console and a basic skeleton for running code in a file (for larger programs). The skeleton is a scala file located in `/src/main/scala/Example.scala`:

{% highlight text %}

object Example extends App {
  println("hello world");

  // We'll add example code down here
}

{% endhighlight %}

and in sbt we can just do:

{% highlight text %}

> run
hello world

{% endhighlight %}

Don't worry about `object` and `App`. We'll cover them later. Like in JavaScript, semi-colons to end expressions are optional and there's no consensus on whether to always use them or only in the rare cases they're actually needed. In this post, I'll omit them for simplicity though in practice my personal preference is to always use semi-colons.

### Variables

There are 3 ways to define a variable, `def`, `val` and `var`. The difference between `val` and `var` is simple: a variable declared using `val` cannot be reassigned whereas if it is declared with `var` it can (note that if a `val` variables points to a mutable object, the object itself can be modified). We should use `val` whenever possible because it makes it easier to reason about code.

For `val`/`var` vs `def`, the difference is more subtle. Let's first try these examples using `sbt`'s console tool:

{% highlight scala %}

// Using def
scala> def xdef = util.Random.nextInt
scala> xdef
Int = 387633735
scala> xdef
Int = 305498057

// Using val
val xval = util.Random.nextInt
scala> xval
Int = 1203502093
scala> xval
Int = 1203502093

{% endhighlight %}

We can see that for `xdef`, the value returned by `xdef` is different every time, while for `xval` it's the same. For `val`, the code in the assignment is executed only once, while for `def`, the code is executed every time it's called. We can think of `def` as an alias to the RHS. `def` can also be used to define functions as we'll see next.

### Functions

Functions can be defined using `def` and the parameter type is written after the param itself and the return type is written after the parameters block. A simple example:

{% highlight scala %}

def add(a: Int, b: Int): Int = a + b

{% endhighlight %}

We can invoke the function as:

{% highlight scala %}

val result = add(3, 4)
println(result)

{% endhighlight %}

Curly braces are optional for one-liners, but we need them for multi-line functions:

{% highlight scala %}

def add(a: Int, b: Int): Int = {
  val a2 = a
  val b2 = b
  a2 + b2
}

{% endhighlight %}

The last line is always returned as value, so we don't use a `return` keyword even for multi-line functions. We can also define nested functions, which is convenient when we don't want to define auxiliary functions. For example, if we want to implement a tail-recursive factorial function:

{% highlight scala %}

def factorial(n: Int) = {
  @tailrec def factorialAux(n: Int, f: Int): Int = {
    if (n == 0) f
    else factorialAux(n - 1, f * n)
  }
  factorialAux(n, 1)
}

{% endhighlight %}

In the code above we see `factorialAux()` defines inside factorial so it's not visible to other functions. We also made use of the `if/else` construct to handle the base case and because the auxiliary function is tail recursive, we can annotate it with `@tailrec` to hint to the compiler it can be optimized (and avoid using a stack for the recursion).


**Lambda**



We can also have lambda/anonymous functions. In the code below we define `squarer`:

{% highlight scala %}

def squarer = (x: Int) => x * x
println(List(1, 2, 3) map squarer)

{% endhighlight %}

In the code above we're also creating a List and using its `map()` method. In Scala methods can be written without the . and if there's only one argument, parenthesis can be omitted.

There's an even shorter version in which underscores are used instead of variables. The parameters are assigned to the underscore in order. For example:

{% highlight scala %}

(a, b) => a + b

{% endhighlight %}

Can be written as

{% highlight scala %}

_ + _

{% endhighlight %}

This form is only valid when we're passing it directly as argument, like below, but it cannot be assigned to a variable like regular lambdas:

{% highlight scala %}

println(List(1, 2, 3).reduce(_ + _))

{% endhighlight %}


**Partial application**



Scala supports partial application but, differently from languages like OCaml and Haskell, this is something the function definition must be explicit about. We do that by grouping arguments in parenthesis, for example:

{% highlight scala %}

def sum(a: Int)(b: Int): Int = a + b
def increment = sum(1) _
println(increment(4))

{% endhighlight %}

In the code above we can do a partial application on the first argument of `sum()` but we need to make use of the _ on the remaining arguments.


**Var arg**



Functions support a variable length list of arguments:

{% highlight scala %}

def varArgFun(args: (Int)*) = println(args)
varArgFun(1, 3, 4)

{% endhighlight %}

### Generics

**Fun fact:** Odersky was one of the responsible for adding generics to Java.

Before describing generics, let's discuss type hierarchy in Scala. The main types are depicted in the image below. In particular we have `Any` which is the super type of all types and `Nothing` which is a subtype of all types. We also have the division between native types and object types. In the diagram we can also see dashed arrows, which means the types are not strictly subtypes but they can be coerced to the other (for example `Int`s can be converted to `Long`s)

<figure class="center_children">
    <a href="https://kunigami.files.wordpress.com/2018/02/scala-class-hierarchy.png"><img src="{{site.url}}/resources/blog/2017-03-01-notes-on-coursera's-functional-programming-principles-in-scala/2018_02_scala-class-hierarchy.png" alt="Type Hierarchy (click for the full-size version)" /></a>
    <figcaption> Class Hierarchy (click for the full-size version)</figcaption>
</figure>

In Scala generics are represented between `[]` (as opposed to the `<>` in Java). We can define functions that take a generic parameter as follows:

{% highlight scala %}

def singleton[T](a: T): T = a

{% endhighlight %}

From the invocation side, we can be explicit about which type we're binding the generic `T` to, or let the compiler figure it out.

{% highlight scala %}

// Explicit Int type
println(singleton[Int](3)) // 3
// Explicit double type - casts 3 to double
println(singleton[Double](3)) // 3.0
// Implicit
println(singleton("str")) // str

{% endhighlight %}

We can also use constraints to limit which types we can provide. We can either constrain the types to be subtypes of a particular type (`[T <: SuperType]`), a supertype of a particular type (`[T >: Subtype]`) or both (`[T >: Subtype <: Supertype]`).

For example:

{% highlight scala %}

def identityIterable[T <: Iterable[Int]](a: T): T = a

{% endhighlight %}

This is slightly different from

{% highlight scala %}

def identityIterable(a: Iterable[Int]): Iterable[Int] = a

{% endhighlight %}

We can pass a subtype of `Iterable`, such as `List[Int]`, to both versions but the first returning type is `List[Int]` while the second return's type is `Iterable[Int]`.


**Covariance and contra-variance**



This question arises often when talking about generics. If `Derived` is a subclass of `Base`, is  `List[Derived]` a subclass of `List[Base]`? Is it different for `Arrays`? We can try both examples:

{% highlight scala %}

def takesBaseList(a: List[Base]) = Nil
def takesDerivedList(b: List[Derived]) = takesBaseList(b)

// Compile error
def takesBaseArray(a: Array[Base]) = Nil
def takesDerivedArray(b: Array[Derived]) = takesBaseArray(b)

{% endhighlight %}

We'll see the code compiles for Lists but not for Arrays. This means that `List[Derived]` is a subclass of `List[Base]` while `Array[Derived]` is **not** a subclass of `Array[Base]`. Because of this property, we say that `List[T]` are covariant on the type T, while Arrays are not. The key difference is that Lists are immutable. The problem arises if we `takesBaseArray` does the following:

{% highlight scala %}

class Base {}
class Derived extends Base {}
class AnotherDerived extends Base {}

def takesBaseArray(a: Array[Base]) = {
    a :+ new AnotherDerived()
}

{% endhighlight %}

Now, if we pass a `Array[Derived]` as argument to `takesBaseArray`, because it's mutable, the array passed as argument would have an element of `AnotherDerived`, even though its type is `Array[Derived]`.

We can specify whether a class is covariant on type T, by adding a plus sign, for example,

{% highlight scala %}

class List[+T] ...

{% endhighlight %}

In general terms, a class `C` is **covariant** on a generic type `T` if, given a type `Td` which is a subtype of type `Tb`, then `C[Td]` is a subtype of `C[Tb]`. The **contravariant** property is when the implication is reversed, that is, if `Td` is a subtype of type `Tb`, then `C[Tb]` is a subtype of `C[Td]`.

It's less common for a class to be contravariant on a type. An interesting example presented in the videos is if we model a function that takes a single argument of type `Tk` and returns a value of type `Tk`. We can model this as a class:

{% highlight scala %}

class MyFunction[Tk, Tv](f: (Tk) => Tv) {
  def apply(x: Tk): Tv = f(x)
}

{% endhighlight %}

Now say that we define a higher order function that takes our function type and simply calls apply. Say it expects a function type that takes a `Derived` and returns `Base`. Could we pass a function with different type to `higherOrderFunction`?

{% highlight scala %}

def higherOrderFunction(f: MyFunction[Derived, Base]) = {
  val x = f(new Derived()).apply
  x.methodDefinedInBase
}

{% endhighlight %}

Because of the parameter type, we know `higherOrderFunction` will only provide values of type `Derived` (or subtypes) to f, so we can pass any function that takes a super type of `Derived`. On the other hand, we know that `higherOrderFunction()` will only use methods from x (i.e. the return value of `f`) defined in `Base`, so we can pass any function that returns any subtype of `Base`. In other words, we could pass a function of type `MyFunction[Base, Derived]` to `higherOrderFunction()`. In general case, `MyFunction` is *covariant* on its return type but *contravariant* on its input type, so we can modify our definition to:

{% highlight scala %}

class MyFunction[-Tk, +Tv](f: (Tk) => Tv) {
  def apply(x: Tk): Tv = f(x)
}

{% endhighlight %}

This part is a bit confusing to understand, but having a concrete example helped me.


**Implicit parameters**



Now that we've seen generics, we can go back to functions and talk about implicit parameters. If we have

{% highlight scala %}

def foo(e: T, implicit ord: Ordering) {...}

{% endhighlight %}

A natural example for using implicit arguments is for sorting. We'll use wrap List's `sortWith` in a function, listSort, so that we can test passing an implicit comparator. See the example below with comments:

{% highlight scala %}

// First, we define the interface for the comparator
abstract class Comparator[T]{
  def compare(v1: T, v2: T): Boolean
}

// Because implicit cannot be applied to top-level objects, we'll
// define them within our sample App
object Example extends App {

 // Now we implement the comparator for Int and String. The
 // have to contain the 'implicit' modifier
 implicit object IntComparator extends Comparator[Int] {
   def compare(v1: Int, v2: Int): Boolean = v1 < v2;
 }
 implicit object StringComparator extends Comparator[String] {
   def compare(v1: String, v2: String): Boolean = v1 < v2;
 }

 // We have to define the implicit parameter as partial argument
 // so the caller can omit it.
 def listSort[T](l: List[T])(implicit cmp: Comparator[T]) = l.sortWith(cmp.compare)

 // Testing with a list of Ints
 println(listSort(List(3, 2, 1)))
 // Testing with a list of Strings
 println(listSort(List("a", "c", "b")))
 // Error: implicit parameter not implemented.
 println(listSort(List(List(1), List(1, 3), List(1, 2, 3))))
}

{% endhighlight %}

A few things on the code to highlight:

* We have to define the implicit parameter as partial argument
* We have to implement the implicit objects within another class/object
* If a type doesn't have an implicit implementation, compilation fails.
* If a type has two or more implicit implementation, compilation fails due to ambiguity. We can see this by implementing another comparator for strings:

{% highlight scala %}

object Example extends App {
 ...
 implicit object StringComparator extends Comparator[String] {
   def compare(v1: String, v2: String): Boolean = v1 < v2;
 }
}

{% endhighlight %}

[[2](http://docs.scala-lang.org/tutorials/tour/implicit-parameters.html)] has more information about `implicit`.

### Classes

At this point we've seen classes already, but in this section we'll talk about their specific features and syntax. Below is a minimal example of a class representing an integer. It covers constructors, public/private methods, overriding methods from ancestors and defining operators.

{% highlight scala %}

class MyNumber(x: Int) { // <- implicit constructor
  private val privateMember = x;

  // Explicit constructor
  def this() = this(0)

  // Methods are public by default
  def publicMethod() = 1

  // Private method
  private def privateMethod() = 1

  // Overriding methods
  override def toString = privateMember.toString

  // Handy operator (e.g. instead of sub)
  def - (that: MyNumber) =
    new MyNumber(privateMember - that.privateMember)

  // Unary operators: Convention
  // 'this' is optional but can be used when
  // accessing a member/method
  def unary_- = new MyNumber(this.privateMember)
}

{% endhighlight %}


**Abstract classes**



Abstract classes are defined by adding the `abstract` modifier. We don't need to add these modifiers to abstract methods, just leave them "un-implemented". Abstract classes can provide method's implementation too. Here's an example:

{% highlight scala %}

abstract class Base {
  def toBeImplemented(v: Int): Int;

  def implemented(v: Int): Int = v + 1;
}

{% endhighlight %}


**Traits**



Scala doesn't have interfaces, but it has traits. It looks very similar to abstract classes in that they cannot be instantiated and can have abstract methods. The main difference is that a class can "extend" or use multiple traits. For example, say we have two traits:

{% highlight scala %}

trait TraitA {
  def abstractMethod(x: Int): Int
  def methodA(): String = "a";
}

trait TraitB {
  def methodB(): String = "a";
}

trait TraitC {
  def methodC(): String = "c";
}

{% endhighlight %}

A class can extend a trait as it would extend an abstract class, but additional traits have to use the `with` construct:

{% highlight scala %}

class UseTraits extends TraitA with TraitB with TraitC {
  // Needs to be implemented
  def abstractMethod(x: Int): Int = x + 1
}

{% endhighlight %}

If any of the traits have methods with colliding signatures, a compilation error happens. Note that traits are basically [multiple-inheritance](https://en.wikipedia.org/wiki/Multiple_inheritance), something that is not allowed in Java. It's something that can be abused, but I do like to use in rare some cases.


**Objects and static methods**



Objects are basically a singleton (or a utils/helper class). They cannot be instantiated but its methods are equivalent to static methods in Java:

{% highlight scala %}

object MyObj {
  def get(): String = "value";
}

println(MyObj.get());

{% endhighlight %}

Interestingly, in Scala regular classes cannot have static methods. Either all methods are static (`object`) or none are (`class`). The workaround is that objects and classes can have the same name, so in practice we can keep static methods in the object and non-static in the class:

{% highlight scala %}

class MyEntity {
  def nonStatic(): String = "non-static";
}
object MyEntity {
  def static(): String = "static"
}

{% endhighlight %}

I actually like this separation. Public static methods often don't belong to a class, but are rather helper or utils. By forcing them out of the class, it encourages placing them in an object which might have a more general scope. For example, we could have a class User and we have a function called `titleize()`, which would return the name with the first capitalized.

{% highlight scala %}

class User {
  def getName(): String = User.titleize(this.name)
  // NOTE: This is invalid syntax. It's just for
  // demonstration purposes.
  def [static] titleize(name: String): String = ...
}

{% endhighlight %}

If we're forced to move it to a different class, we could take a step up and put it into a help class called `StringUtils` or `NameUtils`, which is a higher level of abstraction and hence more re-usable.

### Misc


**Pattern Matching**



In Scala we can to type matching, similar to [OCaml]({{site.url}}/blog/2016/06/27/exploring-ocaml.html)'s. The only caveat is that we need to explicitly add the `case` modifier to a class in order for them to be matched against.

{% highlight scala %}

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

{% endhighlight %}

Now we can match a generic `Expr` to either `Number` of `Sum`, including destructuring the constructor arguments:

{% highlight scala %}

def show(e: Expr) = e match {
  case Number(x) => x.toString
  case Sum(x, y) => show(x) + " + " + show(y)
}

{% endhighlight %}

We can also have destructuring assignments for some structures such as tuples and lists:

{% highlight scala %}

val (first, second) = (10, 11);
println(first, second);
val (x::xs) = List(1, 2, 3)
println(x, xs)

{% endhighlight %}

### Conclusion

In this post we covered several aspects of the Scala language, which were introduced during the Coursera class, with some extra research here and there to understand the features better. We ended up not covering the data structures, because the post was getting too long.

This course is very easy for people with prior experience in Java and functional programming. It's interesting to learn about specific language designs and the associated tradeoffs.

I'm looking forward to completing the other Scala classes in the series.

### Further Reading

* Code Commit - Scala for Java Refugees ([part 1](http://www.codecommit.com/blog/scala/scala-for-java-refugees-part-1) \| [part 2](http://www.codecommit.com/blog/scala/scala-for-java-refugees-part-2) \| [part 3](http://www.codecommit.com/blog/scala/scala-for-java-refugees-part-3))

### References

* [[1](http://stackoverflow.com/questions/18887264/what-is-the-difference-between-def-and-val-to-define-a-function)] Stack Overflow - What is the difference between “def” and “val” to define a function
* [[2](http://docs.scala-lang.org/tutorials/tour/implicit-parameters.html)] Scala Documentation - Implicit Parameters



