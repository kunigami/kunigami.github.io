---
layout: post
title: "Revisiting Python: Object Oriented Programming"
tags: [python, software engineering]
---

This is the third post in the series on revisiting Python. In the [previous post]({{site.url}}/blog/2015/02/23/revisiting-python:-functions.html) we talked about Python functions.

<figure class="center_children">
    <a href="https://kunigami.files.wordpress.com/2016/02/python-logo.png"><img src="{{site.url}}/resources/blog/2015-03-08-revisiting-python:-object-oriented-programming/2016_02_python-logo.png" alt="python-logo" /></a>
</figure>

We’ll talk about [classes](https://docs.python.org/2/tutorial/classes.html) in Python 3.7. Following the premisses of previous posts, this is not intended to be an introduction to Object Oriented Programming, but rather how we can use this paradigm in Python.

We'll first cover basic concepts like classes and inheritance. Later we'll discuss some more advanced features from the Python language that allows us extending the set of built-in patterns provided by the language.

This post has been revisited in 2021/11/06 to update the examples for Python 3.7.

## Classes

Classes are the building blocks of object oriented programming. Python classes look similar to those in languages like C++ and Java, but there are some important differences, which we'll comment on this section.

**Classes are also objects.** When writing a class definition the code is executed and a class object is created and assigned to a name corresponding to the class name. For example, in the code below, an object representing the class is assigned to a variable in the current scope `MyClass`.

{% highlight python %}

class MyClass:
    print('hi')

print(MyClass) # __main__.MyClass
x = MyClass
print(x)       # __main__.MyClass

{% endhighlight %}

If we run the code above it will print 'hi'. We can manipulate `MyClass` as a regular variable, assigning it to other variables, passing it as parameter to functions, etc.

**Instantiating.** We can create an instance of a given class by using a function call notation, that is, `MyClass()`.

This will also call the method `__init()__` which can be used to initialize the instance, much like a constructor. Functions defined within a class become methods to the class instances. Methods can be called using the syntax `instance.method()`. For example:

{% highlight python %}

class MyClass:
    def myMethod(self):
        print(self)

instance = MyClass()

instance.myMethod() # Prints <__main__.MyClass instance at 0x10891f290>;

{% endhighlight %}

When invoking a function using `instance.method()`, `instance()` is bound to the first argument to `method()` in this case. We usually name this first parameter `self`, but it's just a convention.

**Class vs. instance members.** Note that local variables defined at the class level belong to the class object, not to a specific instance object. Making an analogy to C++, this is a static member variable.

{% highlight python %}

class MyClass:
    x = 10

inst = MyClass()
inst.x = 20
inst2 = MyClass()
print(inst2.x) # 20

{% endhighlight %}

To make a variable instance specific, we need to make use of the instance passed as the first parameter to the methods.

{% highlight python %}

class MyClass:
    def __init__(self):
        self.x = 10

inst = MyClass()
inst.x = 20
inst2 = MyClass()
print(inst2.x) # 10

{% endhighlight %}

All methods and variables defined at the class level are stored internally in the `__dict__` variable.

{% highlight python %}

class MyClass:
    x = 10
    def f():
        pass

print(MyClass.__dict__)
# 'x': 1, '__module__': '__main__', '__doc__': None, 'f': &amp;lt;function f at 0x109f32d70&amp;gt;}

{% endhighlight %}

**Methods.** In Python, methods are "static", but by default it requires an instance of the class to be bound to the first parameter. As we saw above, this happens automatically when we use the `instance.method()` syntax. Alternatively, we could explicitly pass the instance to the static method, using the `SomeClass.method(instance)` syntax.

To illustrate that, imagine we have a method in our class a method `printX()`. We can invoke it either by a method from the instance object or from the class object passing the instance:

{% highlight python %}

class MyClass:
    def __init__(self):
        self.x = 10

    def printX(self):
        print(self.x)

inst = MyClass()

# Both are equivalent:
inst.printX() # 10
MyClass.printX(inst) # 10

{% endhighlight %}

Methods are essentially functions assigned to class member variables. To make the point clear, we can rewrite the previous snippet replacing the inline definition of `printX()` with an external function:

{% highlight python %}

def externalPrintX(self):
    print(self.x)

class MyClass:
    def __init__(self):
        self.x = 10

    # Assigning external definition to a member variable
    printX = externalPrint

{% endhighlight %}

Note that the external function still needs to have as the first arguments an instance of `MyClass`.

Given that all methods are public and data variables take precedence over static ones, it can cause some confusion if we assign functions to *instance* members:

{% highlight python %}

def externalPrintX():
    print('external')

class MyClass:
    def __init__(self):
        self.x = 10
        # This is overridden during instantiation
        self.printX = externalPrintX

    def printX(self):
        print('internal')

inst = MyClass()
inst.printX() # external

{% endhighlight %}

Here, the method `printX()` method got replaced by another function, `externalPrintX()`. Note that, differently from regular methods, the instance is not bound to the first argument when invoking `inst.printX()`.

**Static Methods.** Since we're required to provide an instance to the first argument, it's not truly static in the C++ class terminology. It's possible to override this requirement by using the `staticmethod()` function. To illustrate this, here's an example:

{% highlight python %}

class ClassWithStaticMethod():
    def regularMethod(x):
        print 'This is not bound to any instance:', x

    staticMethod = staticmethod(regularMethod)

x = ClassWithStaticMethod()
x.staticMethod(10) # This is not bound to any instance: 10

{% endhighlight %}

**Class Methods.** are different from regular methods because they always receive the *class object* instead of the *instance object* as the first argument. More specifically, when calling from a class object, it is bound automatically as the first parameter and when called from the instance object, its corresponding class object is bound instead. For example,

{% highlight python %}
class ClassWithClassMethod():
    def regularMethod(type):
        print(type)

    classMethod = classmethod(regularMethod)


ClassWithClassMethod.classMethod() # __main__.ClassWithClassMethod

# binds to the class object even when called via an instance object
x = ClassWithClassMethod()
x.classMethod() # __main__.ClassWithClassMethod

{% endhighlight %}

## Inheritance

The syntax for inheritance is `class Derived(Base)`. Methods defined in the base classes can be overridden by derived classes. If a name (variable or method) is referenced from an instance, it's searched from the current class, going up on the inheritance chain, until the definition is first found.

{% highlight python %}

# animal.py
class Animal:
    def breath(self):
        return 'breathing air'
    def eat(self):
        return 'eating'

class Dog(Animal):
    def eat(self):
        return Animal.eat(self) + ' meat'

dog = Dog()
print(dog.breath())  # breathing air
print(dog.eat())     # eating meat

{% endhighlight %}

In this example, `eat()` is first found at `Dog`, while `breath()` is only found at `Animal`. Note that we can refer to a specific implementation by providing the class explicitly, like `Animal.eat(self)`.

**Old-style vs. new style classes**. In a nutshell, the difference between old-style and new-style classes is that the latter is a descendant of `object`. Thus, by default user defined classes are old-style by default (though old-style classes are removed from Python 3).

According to the docs, the motivation for creating the new-style classes is to unify Python types and user defined classes. In this model, classes are able to derive from built-in types [[2](https://www.python.org/doc/newstyle)].

**Multiple-inheritance.** is supported in Python. The syntax is a comma separated list of base classes:

{% highlight python %}

class DerivedClassName(Base1, Base2, Base3):
   ...

{% endhighlight %}

**Method resolution order (MRO).** We just saw that for simple inheritance chains, the natural lookup order for methods is straightforward. For multiple-inheritance it can be complicated mainly because of the [diamond pattern](http://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem).

For old-style classes, the method lookup order, MRO, is defined by a "depth-first search". It first looks in the entire inheritance chain of `Base1`, then `Base2`, and so on. The MRO is always well defined as long as the inheritance graph is a DAG (direct acyclic graph).

The problem is that it's not intuitive. Depending on the order a given class appears in a descendant, its own MRO can vary. For example, consider the following complex dependency:

{% highlight python %}

class A:
    def name(self):
        return 'a'

class B:
    def name(self):
        return 'b'

class C(B, A):
    def composed_name(self):
        return 'c ' + self.name()

class D(A, C): pass

d = D()
print(d.composed_name())

{% endhighlight %}

When we call `composed_name()` on the instance of class `D`, the name resolution will only find it in class `C`. This class on its turn needs to resolve the `name()` method. Should we get it from class `B`, since it's listed first on `C`'s inheritance?

That's not what happens. `name()` is resolved from the class `D`, because there, `A` appears before. This is a very confusing behavior and error-prone.

In new-style classes, the diamond pattern exists by default when multiple inheritance is used, since everything ends up deriving from `object`. But in the new style, the type of inheritance that can be created is more strict. It uses an interesting algorithm, C3, described in [[3](https://www.python.org/download/releases/2.3/mro/)] to create a consistent ordering or throw an exception in case such ordering doesn't exist.

**super.** In the `animal.py` example, we had to hard code `Animal.eat()` to be able to call `Dog`'s parent class. The `super()` function takes a class object and an instance, and it returns a proxy object on which we can call methods and it will resolve them based on the MRO the object class. More specifically, consider the following example:

{% highlight python %}

class A(object):
    def method(self):
        print('calling method from a')

class B(object):
    def method(self):
        print('calling method from b')
    def anotherMethod(self):
        print('calling another method from b')

class C(A, B):
   def method(self):
        print('calling method from c')
    def anotherMethod(self):
        print('calling another method from c')

    def run(self):
        proxy = super(C, self)
        proxy.method()          # calling method from a
        proxy.anotherMethod()   # calling another method from b

C().run()

{% endhighlight %}

In this example, `super()` returned an object, proxy, which can resolve `method()` and `anotherMethod()` properly. Since `C` derives from `A` first, it resolves it to `A.method()`. On the other hand, since `anotherMethod()` is only defined in `B`, it will resolve to `B.anotherMethod()`.

## Descriptors

So far, we discussed the basic components of object oriented programming. Now we'll describe some advanced concepts that will help us better understand how some functions like `staticmethod()` work.

A class is said to be a **descriptor** if it implements any of `__get__()`, `__set__()` or `__delete__()`. In particular, if it doesn't implement `__set__()`, it's called **non-data descriptor**. If it implements both `__get__()` and `__set__()`, it's a **data descriptor** [[4](http://nbviewer.ipython.org/gist/ChrisBeaumont/5758381/descriptor_writeup.ipynb), [5](https://docs.python.org/2/howto/descriptor.html)].

If another class contains **class members** that are descriptors, the `__get__()` and `__set()__` will be executed when do reads and assignments respectively. For a concrete example, considering the following descriptor:

{% highlight python %}

class Descriptor(object):
    def __get__(self, obj, objtype):
        print('getting x')
        return self.x

    def __set__(self, obj, val):
        print('setting x to ' + str(val))
        self.x = val

{% endhighlight %}

It stores a value internally at the variable `x`, but includes some logging when setting or getting this value. Now, suppose we have a class that has a member that is a descriptor:

{% highlight python %}

class ClassWithDescriptors(object):
    member = Descriptor()

x = ClassWithDescriptors()
x.member = 20
print x.member

{% endhighlight %}

When we try to assign a value to `x.member`, the method `__set__()` from `Descriptor` is called instead. The same applies to when we try to read from `x.member`. It's important that member is a class member, not an instance member. For example, if we have the instance member variable, it would not print the logging messages:

{% highlight python %}

class ClassWithDescriptors(object):
    def __init__(self):
        self.instance_member = Descriptor()

x = ClassWithDescriptors()

# This is just overriding instance_member with an integer
x.instance_member = 20 # Prints setting x to 20
print(x.instance_member) # Prints getting x and then 20

{% endhighlight %}

**staticmethod() and classmethod().** One interesting use case for descriptors to implement the `staticmethod()` and `classmethod()` "functions". We could write the following descriptor for each of these, respectively:

{% highlight python %}

class StaticMethod(object):
    def __init__(self, f):
        self.f = f

    def __get__(self, obj, objtype=None):
      return self.f

class ClassMethod(object):
     def __init__(self, f):
          self.f = f

     def __get__(self, obj, objtype=None):
          if objtype is None:
               objtype = type(obj)
          def newfunc(*args):
               return self.f(objtype, *args)
          return newfunc

{% endhighlight %}

## Functor

Python has the concept of function objects or functors. It's an object that can be invoked as a function so long its class defines the `__call__()` method. A simple example is:

{% highlight python %}

class Multiplier:
    def __init__(self, factor):
        self._factor = factor
    def __call__(self, a):
        return self._factor * a

double = Multiplier(2)
triple = Multiplier(3)
print(double(5)) # Prints 10
print(triple(7)) # Prints 21

{% endhighlight %}

One advantage of functors over regular functions is the capability of carrying context.

### Decorators

*Decorator* is a [design pattern](http://en.wikipedia.org/wiki/Decorator_pattern) in which we can modify the behavior of an object without modifying all the objects from that particular class. It's also known as a wrapper, because we wrap the object inside another object that adds the desired behavior.

We'll now describe a motivation for using decorators and how Python offers a syntax sugar for this pattern using annotations. For our example, imagine we have a function that takes a three variables and returns the sum of them:

{% highlight python %}

def add(a, b, c):
    return a + b + c

{% endhighlight %}

Now imagine we want to validate the parameters passed to `add()`. We could add that to the beginning of the function, but maybe we already have a function to do the validation for us. In this case, we could wrap the function in a another one, which would first perform the parameters validation and then call the function. The code below does that:

{% highlight python %}

def validate(f):
    def closure(*args):
        for arg in args:
            assert isinstance(arg, int)
        return f(*args)
    return closure

validated_add = validate(add)

print(validated_add(5, 2, 3))
# Throws an exception
print(validated_add(5, 2, 3.0))

{% endhighlight %}

`validate()` takes a function `f()` and wraps it inside `closure()`, which verifies all arguments are integers before invoking `f()`. So by passing `add()` to `validate()`, we're getting a decorated function `validated_add()` which performs validation on the arguments.

Python offers a syntax sugar to do exactly that. By annotating a function with `@something`, it passes the function to a function named `something()`, that should act as a function transformation, and uses the transformed function instead.

{% highlight python %}

@validate
def add(a, b, c):
    return a + b + c

{% endhighlight %}

Imagine we want to customize the validate function to enable us to define the type of each element passed to the decorated function.

**Annotations with parameters.** We can do that by passing parameters to the decorator, using this syntax `@something(arg1, arg2, ...)`. In this form, we actually construct a decorator builder, so we need to wrap our validate() function inside another one:

{% highlight python %}

def validate(*arg_types):
    def validate_impl(f):
        def closure(*args):
            assert len(arg_types) == len(args), "Arguments and types don't match"
            for i in range(len(args)):
                assert isinstance(args[i], arg_types[i])
            return f(*args)
        return closure
    return validate_impl

{% endhighlight %}

With this new validate() function, we can update the annotation to our simple add() function:

{% highlight python %}

@validate(int, int, int)
def add(a, b, c):
    return a + b + c

{% endhighlight %}

We can now use this as a very simple type validation. Imagine we have a function that takes a string and repeats it N times. We can enforce the types using the same annotation:

{% highlight python %}

@validate(str, int)
def rep(s, n):
    return &amp;quot;&amp;quot;.join([s]*n)

{% endhighlight %}

Decorators can also be function objects, so we can define a class to do a similar work the `validate()` function does:

{% highlight python %}

class Validator:
    def __init__(self, *arg_types):
        self._arg_types = arg_types

    def __call__(self, f):
        arg_types = self._arg_types
        def closure(*args):
            assert len(arg_types) == len(args), "Arguments and types don't match"
            for i in range(len(args)):
                assert isinstance(args[i], arg_types[i])
            return f(*args)
        return closure

{% endhighlight %}

In this case, the parameters defined in the annotation are passed to the constructor of the class.

Finally, two very common annotations are **staticmethod** and **classmethod**. There is no magic here. These cause the `staticmethod()` and `classmethod()` instances to be instantiated, and we saw how these work in previous sections.

### Conclusion

In this post we covered object oriented programming in Python. We learned details about classes, especially how they compare to classes in other languages. We then discussed inheritance, including multiple inheritance and old and new-style classes. We then delved into more advanced concepts related to object oriented programming. This included descriptors, functors and decorators.

Python requires relatively few special constructs to accomplish features from other object-oriented languages. Examples include the concept of super and static methods, which are implemented using general purpose features from Python like descriptors and decorators.

### References

* [[1](https://docs.python.org/2/tutorial/classes.html)]  The Python Tutorial - Classes
* [[2](https://www.python.org/doc/newstyle)] Python.org Docs - New-style Classes
* [[3](https://www.python.org/download/releases/2.3/mro/)] The Python 2.3 Method Resolution Order
* [[4](http://nbviewer.ipython.org/gist/ChrisBeaumont/5758381/descriptor_writeup.ipynb)] Python Descriptors Demystified
* [[5](https://docs.python.org/2/howto/descriptor.html)]  Python HOWTOs - Descriptor HowTo Guide
