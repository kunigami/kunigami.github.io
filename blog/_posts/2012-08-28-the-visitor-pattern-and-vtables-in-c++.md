---
layout: post
title: "The Visitor pattern and Vtables in C++"
tags: [c++, compilers, java, software engineering]
---

The other day I was developing a Java code and I needed to use `instanceof`, but since I have read that this is a sign of bad design, I asked for help and was recommended the [Visitor](http://en.wikipedia.org/wiki/Visitor_pattern) pattern.

In this post we’ll talk about this design pattern in Java, that is where my need began, but also in C++ because the implementation is related to [vtables](http://en.wikipedia.org/wiki/Virtual_method_table), which I’ve been wanting to read about.

### Visitors in Java

Let us consider the following example, where we have the classes `Dog` and `Cat`, which are specializations of `Animal` and have methods to emit sound, respectively `bark()` and `meow()`. We want to implement a method `emitSound()` which receives an instance of `Animal` and emits the sound according to the actual class of the instance. A first alternative using `instanceof` is the following:

{% highlight java %}

// Listing 1
interface Animal {
}
public class Dog implements Animal {
	public void bark(){
		System.out.println("Woof");
	}
}
public class Cat implements Animal {
	public void meow(){
		System.out.println("Meow");
	}
}
...
public static void emitSound(Animal animal){
    if(animal instanceof Dog){
        Dog dog = (Dog) animal;
        dog.bark();
    }
    else if(animal instanceof Cat){
        Cat cat = (Cat) animal;
        cat.meow();
    }
}

{% endhighlight %}

Here we can quote Scott Meyers, from *Effective C++* book:

> Anytime you find yourself writing code of the form “if the object is of type T1, then do something, but if it’s of type T2, then do something else,” slap yourself.


To get rid of `instanceof`, we can add the method `emitSound()` to `Animal` interface and replace `bark()/meow()` for it. In our method `emitSound()`, we let polymorphism choose the correct implementation.

{% highlight java %}

// Listing 2
interface Animal {
    void emitSound();
}
public class Dog implements Animal {
    @Override
    public void emitSound(){
        System.out.println("Woof");
    }
}
public class Cat implements Animal {
    @Override
    public void emitSound(){
        System.out.println("Meow");
    }
}
...
public static void emitSound(Animal animal){
    animal.emitSound();
}

{% endhighlight %}

Another possibility is to delegate the implementation of `emitSound()` to another class through the use of the Visitor pattern. This pattern considers two types of classes: an **element** and a **visitor**. The element implements a method usually named `accept()` while the visitor implements the method `visit()`. The `accept()` method receives a visitor as a parameter and calls the method `visit()` from this visitor with itself as parameter. This way, the visitor can implement the `visit()` method for each possible element.

We can implement like this:

{% highlight java %}

// Listing 3
interface Animal {
    void accept(AnimalVisitor visitor);
}
public class Dog implements Animal {
    @Override
    public void accept(AnimalVisitor visitor){
        visitor.visit(this);
    }
}
public class Cat implements Animal {
    @Override
    public void accept(AnimalVisitor visitor){
        visitor.visit(this);
    }
}
interface AnimalVisitor {
    void visit(Dog dog);
    void visit(Cat cat);
}
public class SoundEmissor implements AnimalVisitor {
    @Override
    public void visit(Cat cat){
        System.out.println("Meow");
    }
    @Override
    public void visit(Dog dog){
        System.out.println("Woof");
    }
}
...
public static void emitSound(Animal animal){
    SoundEmissor soundEmissor = new SoundEmissor();
    animal.accept(soundEmissor);
}

{% endhighlight %}

The advantage of this approach is that `SoundEmissor` may contain members and methods related to the way animals emit sounds. Otherwise, these methods would ended up being implemented in `Animal`.

Another advantage is that the visitor implementation can be chosen at runtime and, being a dependency injection, simplifies testing.

### Visitors in C++

In C++ we don't have `instanceof`, but we can use the `typeid()` operator instead. According to [1], if the argument to this operator is a reference or a dereferenced pointer to a polymorphic class, `typeid()` will return a `type_info` corresponding to the runtime object type.

{% highlight cpp %}

// Listing 4
struct Animal {
};
struct Dog : public Animal {
    void bark(){
        cout << "Woof\n";
    }
};
struct Cat : public Animal {
    void meow(){
        cout << "Meow\n";
    }
};
void emitSound(Animal *animal){
    if(typeid(*animal) == typeid(Dog)){
        Dog *dog = dynamic_cast<Dog *>(animal);
        dog->bark();
    }
    else if(typeid(*animal) == typeid(Cat)){
        Cat *cat = dynamic_cast<Cat *>(animal);
        cat->meow();
    }
}

{% endhighlight %}

Again, we can create `emitSound()` in a interface and make use of polymorphism. In C++ we can implement an interface through a class containing only purely virtual function and no visible constructor like `Animal` in the code below:

{% highlight cpp %}

// Listing  5
struct Animal {
    virtual void emitSound() = 0;
};
struct Dog : public Animal {
    void emitSound(){
        cout << "Woof\n";
    }
};
struct Cat : public Animal {
    void emitSound(){
        cout << "Meow\n";
    }
};
void emitSound(Animal *animal){
    animal->emitSound();
}

{% endhighlight %}

The Visitor pattern can be similarly implemented in the following way:

{% highlight cpp %}

// Listing 6
struct Cat;
struct Dog;

struct AnimalVisitor {
    virtual void visit(Cat *cat) = 0;
    virtual void visit(Dog *dog) = 0;
};
struct Animal {
    virtual void accept(AnimalVisitor *visitor) = 0;
};
struct SoundEmissor : public AnimalVisitor {
    void visit(Cat *cat){
        cout << "Meow\n";
    }
    void visit(Dog *dog){
        cout << "Woof\n";
    }
};
struct Dog : public Animal {
    void accept(AnimalVisitor *visitor){
        visitor->visit(this);
    }
};
struct Cat : public Animal {
    void accept(AnimalVisitor *visitor){
        visitor->visit(this);
    }
};
void emitSound(Animal *animal){
    AnimalVisitor *visitor = new SoundEmissor();
    animal->accept(visitor);
}

{% endhighlight %}

### Single and multiple dispatch

We just saw that the Visitor pattern relies on the use of virtual functions. So now, let's analyse how C++ implements these kind of functions. Before that, we need to understand the concept of dynamic dispatch.

**Dynamic dispatch** is a technique used in cases where different classes (with a common ancestor) implement the same methods, but we can’t tell on compile time what is the type of a given instance, as in the cases above.

In C++ and Java, this type of dispatch is also known as single dispatch since it only considers the type of the object calling the method. In Lisp and other few languages, there is the multiple dispatch that also takes into account the type of the parameters.

As an example, take a look at the following code:

{% highlight cpp %}

// Listing 7
void visitor(Dog *dog){
    cout << "Woof\n";
}
void visitor(Cat *cat){
    cout << "Meow!\n";
}
void emitSound(Animal *animal){
    visitor(animal);
}

{% endhighlight %}

We'll get a compile error since method/function matching for parameters is made at compile time. In this case we need to have an implementation for the Animal type.

Now, we'll see how C++ implements dynamic dispatch.

### C++ Vtables

Although the C++ standard does not specify how dynamic dispatch should be implemented, compilers in general use the a structure called the virtual method table, known for other names such as vtable, which is the one we'll use henceforth. Actually, this table is an array of pointers to virtual methods.

Each class that defines or inherits at least one virtual method has its own vtable. This table points to the virtual methods defined by the nearest ancestral (including the class itself) that are visible for that class. Besides, each instance of these classes will have an additional member, a pointer that points to the table corresponding to the class used to instantiate that object.

As an example, if we were to instantiate a `Dog` from **Listing 5**:

{% highlight cpp %}

Animal *animal = new Dog();

{% endhighlight %}

Here, the `animal's` pointer points to `Dog`'s and not `Animal`'s. Thus, when we do

{% highlight cpp %}

animal->emitSound();

{% endhighlight %}

the corresponding table is looked up to find exactly which `emitSound()` to call. Note that virtual methods requires an extra indirection that may affect performance. In C++ dynamic dispatch is not performed by default unless some class defines a virtual method. On the other hand, in Java this is done by default.

Let's see a final example,

{% highlight cpp %}

struct A {
    virtual void foo(){}
    virtual void bar(){}
    void baz(){}
};
struct B : public A {
    void bar(){}
    virtual void baz(){}
};

{% endhighlight %}

We can analyse the vtables from class A and B compiling the code above with `gcc` using the `-fdump-class-hierarch` flag. A text file with extension .class will be generated like this:

{% highlight text %}

Vtable for A
A::_ZTV1A: 4u entries
0 (int (*)(...))0
4 (int (*)(...))(&amp; _ZTI1A)
8 (int (*)(...))A::foo
12 (int (*)(...))A::bar

Class A
size=4 align=4
base size=4 base align=4
A (0xb71f6038) 0 nearly-empty
vptr=((&amp; A::_ZTV1A) + 8u)

Vtable for B
B::_ZTV1B: 5u entries
0 (int (*)(...))0
4 (int (*)(...))(&amp; _ZTI1B)
8 (int (*)(...))A::foo
12 (int (*)(...))B::bar
16 (int (*)(...))B::baz

Class B
size=4 align=4
base size=4 base align=4
B (0xb7141618) 0 nearly-empty
vptr=((&amp; B::_ZTV1B) + 8u)
A (0xb71f61f8) 0 nearly-empty
primary-for B (0xb7141618)

{% endhighlight %}

Here we can see that function `A::foo` and `A::bar` appear in `A`'s vtable, but `A::baz` doesn't, since it was not declared virtual. On `B`'s vtable we have `A::foo`, because it was not overridden in `B`. We also have `B::bar`, although it has not been declared virtual in `B`, this property was inherited from `A::bar`. Finally, `B::baz` appears on the vtable because it was declared virtual in `B`.

We can also see the value that the instances of such classes will have: `vptr=((& A::_ZTV1A) + 8u)` and `vptr=((& B::_ZTV1B) + 8u)`, which is the offset to the functions' pointers on the respective tables. A nice reference for a understanding of vtables can be seen in [2].

### Referências

* [1] [The typeid operator](http://publib.boulder.ibm.com/infocenter/comphelp/v8v101/index.jsp?topic=%2Fcom.ibm.xlcpp8a.doc%2Flanguage%2Fref%2Fthe_typeid_operator.htm) (C++ only)
* [2] [LearnCpp.com – The Virtual Table](http://www.learncpp.com/cpp-tutorial/125-the-virtual-table/)
