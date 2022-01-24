---
layout: post
title: "Haskell – Algebraic Data Types"
tags: [haskell]
excerpt_separator: <!--more-->
vanity: "2011-09-25-haskell-algebraic-data-types"

---
{% include blog_vars.html %}

Haskell uses an elegant syntax to represent a type of data structure, analogous to many other structures present in languages ​​such as C and C++. These structures are called *algebraic data types*.

This is the second post about my studies in Haskell. The first can be found [here](({{blog}}/2011/08/07/haskell.html)).

This post makes analogies with some basic C++ features but prior knowledge is not required.

## Struct

Consider the following C++ `struct` representing a book through its code, title and authors:

{% highlight c++ %}
#include <string>
#include <vector>
struct {
    int isbn;
    std::string title;
    std::vector < std::string > authors;
};
{% endhighlight %}

In haskell we can write:

{% highlight haskell %}
data BookInfo = Book Int String [String]
                deriving (Show)
{% endhighlight %}

To define this structure, start with the keyword `data` and give a name to this new type, in this case `BookInfo`, called the **type constructor**.

In addition, we define a constructor of this type, which in the example is named `Book`, called a **value constructor** or **data constructor**, but could have the same name as the type. Both the constructor name and the type name must be capitalized.

The `deriving (Show)` line is being used in this and other examples in this post so that an instance of this structure can be printed to the terminal. We can instantiate the structure `BookInfo` as follows:

{% highlight haskell %}
myBook = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]
{% endhighlight %}

Note that the input parameters must be in the same order as they are defined in the structure.
Also note that the "members" of this structure are implicit, being represented only by their type.

To improve readability a bit, it is possible to use type synonyms, through the keyword `type` (similar to the `typedef` C++).

{% highlight haskell %}
type ISBN = Int
type Title = String
type Author = String

data BookInfo = Book ISBN Title [Author]
                deriving (Show)
{% endhighlight %}

## Enum

Another C++ structure that can be represented succinctly in Haskell is the enumation structure. Consider an `enum` representing RGB colors:

{% highlight c++ %}
enum Color { Red, Green, Blue };
{% endhighlight %}

An algebraic data type admits more than one constructor, as long as they have different names. Also, constructors don't need to have parameters, which allows you to write the enum abovelike this:

{% highlight haskell %}
data Color = Red | Green | Blue
             deriving (Show)
{% endhighlight %}

## Union

Still taking advantage of the fact that we can have several constructors with different parameters, we can also represent a union. Consider the following C++ example of a union representing two geometric shapes – a circle or a polygon:

{% highlight c++ %}
struct Point {
    double x, y;
};
struct Circle {
    Point center;
    double radius;
};
struct Polygon {
    int nvertices;
    Point *vertices;
};
union Shape {
    Circle circle;
    Polygon polygon;
};
{% endhighlight %}

In Haskell, you can do the following:

{% highlight haskell %}
type Point = (Double, Double)
type Center = Point
type Radius = Double

data Shape = Circle Center Radius
           | Polygon [ Point ]
             deriving (Show)
{% endhighlight %}

## Getters and Setters

When working with classes, we occasionally want to protect read/write access to their members using getters and setters, for example:

{% highlight c++ %}
#include <string>
class Person {
    int id;
    std::string name;

public:

    int get_id(){ return id; }
    void set_id(int _id){ id = _id; };
    std::string get_name(){ return name; }
    void set_name(std::string &_name) { name = _name; }
};
{% endhighlight %}

There is a similar mechanism in Haskell, implemented by the so-called **record syntax**. In an analogous example to the previous one:

{% highlight haskell %}
type ID = Int
type Name = String

data Person = Person {
      personID  	:: ID,
      personName	:: Name
} deriving (Show)
{% endhighlight %}

This more verbose syntax allows you to instantiate the structure by assigning the parameters to the members explicitly like:

{% highlight haskell %}
you = Person {personName = "YourName", personID = 2}
{% endhighlight %}

The getter member is given by:

{% highlight haskell %}
personName you
{% endhighlight %}

While setter would correspond to:

{% highlight haskell %}
you {personID 3}
{% endhighlight %}

## Parameterized Types

Types of algebraic data also represent other functionality present in C ++: templates. Let's take as an example a class representing a generic pointer to any type:

{% highlight c++ %}
template <typename T>
struct GenericPointer {
    T* ptr;
};
{% endhighlight %}

By instantiating this class we can define the types we want to represent:

{% highlight c++ %}
int i = 2;
GenericPointer<int> p1;
p1.ptr = &i;
GenericPointer<double> p2;
p2.ptr = NULL;
{% endhighlight %}

Here we have pointers for types int and double, where `NULL` is a generic value to say that the pointer doesn't point anywhere.

A (forced) analogy in Haskell would be as follows:

{% highlight haskell %}
data GenericPointer a = Ptr a |
                        Null
                        deriving(Show)
{% endhighlight %}

Although in Haskell we don't have the value `NULL`, we can simulate it through a constructor as in the example above. This encapsulation is particularly interesting for providing an exception condition. For example, consider a function that returns the first element of a list:

{% highlight haskell %}
first_elem (x:xs) = x
{% endhighlight %}

What to return if the list is empty? One solution would be to use encapsulation:

{% highlight haskell %}
first_elem (x:xs) = Ptr x
first_elem _  	= Null
{% endhighlight %}

In the example, the character `_` works as a wildcard, that is, any parameter matches it. As it was put after the other pattern, it behaves like an `else` and will only be called when the list has no elements.

## Recursive Types

Another application of an algebraic data types is to define recursive types, that is, members of the structure are of the type of that structure. In C and C++ it is possible to do this using pointers. For example, we can implement a linked list of generic elements as follows:

{% highlight c++ %}
template <typename T>
struct LinkedList {
    LinkedList<T>* next;
};
{% endhighlight %}

In Haskell we have something like:

{% highlight haskell %}
data LinkedList a = Node a (LinkedList a)
            | Null
            deriving(Show)
{% endhighlight %}

To represent a linked list containing 3, 1 and 2, in that order, we use the following syntax:

{% highlight haskell %}
Node 3 (Node 1 (Node 2 Nil))
{% endhighlight %}

A binary tree is also quite simple to represent:

{% highlight haskell %}
data Tree a = Node a (Tree a) (Tree a)
            | Null
              deriving (Show)
{% endhighlight %}

## Conclusion

Haskell has a succinct syntax for describing algebraic data types, which in turn are quite versatile, being able to represent both structures and unions, enumerations, classes, etc.

I still know very little about the language, but for now I'm finding it quite interesting.

## References

* [[1](http://book.realworldhaskell.org/)] Real World Haskell - [Chapter 3](http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html), by Bryan O'Sullivan, Don Stewart, and John Goerzen.
