---
layout: post
title: "Function Objects in C++"
tags: [c++, compilers]
vanity: "2023-06-01-functions-in-cpp"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/cpp-logo.png" alt="C++ logo" />
</figure>

In this post we'll spend some time exploring function objects in C++, in particular `std::function`. There are a lot of subtleties and unique syntax that is worth understanding.

<!--more-->

First we'll cover different ways of working with functions as objects such as function pointers, functors and lambdas. We'll then discuss `std::function` and provide a possible way to implement it.

## Function Objects

### Function Pointer

When we compile a C++ code into assembly instructions, they will be loaded in memory, so every instruction can be associated with a memory address, including that of a function. During execution, the processor keeps a program counter (PC) in a register pointing to the address of the instruction being executed.

So when we call a function like:

{% highlight c++ %}
f(1);
{% endhighlight %}

The processor will change the program counter to the address of `f`'s first instruction. I did a test with Apple's M1 processor, which uses ARM's instruction set and the code looks like:

{% highlight asm %}
f:
  ; f's instructions
  ; ...
main:
  bl	f
{% endhighlight %}

In here, `bl` is a mnemonic for "branch with link", meaning it will change the program counter to the address indicated by the label `f` but before that will save the current program counter to a register (called link register) which will be used to restore the program counter once `f` is finished.

We see that it abstracts away the address of the instruction by using labels, but we could in theory use an actual address with `bl`. A similar idea can be applied at the C++ level. Instead of using the function name, we can call a function via its address:

{% highlight c++ %}
int (*fptr)() = &f;
(*fptr)(1);
{% endhighlight %}

The syntax is a bit convoluted with the required parenthesis but these are needed for disambiguation, since `int *fptr()` would be interpreted as a function definition returning `int *`, and `*fptr(1)` would look like a dereferencing of the pointer returned by `fptr(1)`.

To make the point clear that we're invoking functions out of a memory address, we can convert to an intermediate 64-bit number:

{% highlight c++ %}
typedef int (*func_ptr)(int);

int64_t* x = (int64_t*)&f;
(*(func_ptr)x)();
{% endhighlight %}

Worth mentioning this code is not portable because it assumes a 64-bit instruction addressing. Also notice the use of `typedef` for the cast - I don't know if there's a syntax that allows casting to function pointer inline.

In C++, we can actually write:

{% highlight c++ %}
int (*fptr)() = f;
fptr(1);
{% endhighlight %}

It will implicitly convert `f` into `&f` in the first expression since when a function `f` is on the right hand side and called without parenthesis, getting its address is the only operation one could intend. Similarly, called a function pointer with parenthesis can only mean invoking the underlying function it is pointing to.

It's a bit surprising that low-level languages like C have function objects that can be manipulated as regular data, but this stems from how computers run code, treating code and data the same way.

Enough with function pointers. Let's move to object oriented programming.

### Functors

In C++ another way to obtain a function object is by working with classes. We define a method `f()` in the class and at some point create an instance for that class, which can be passed around and eventually invoked, for example:

{% highlight c++ %}
struct MyClass {
    int f(int x) {
      return 1;
    }
};
MyClass c;
cout << c.f(1) << endl;
{% endhighlight %}

C++ provides a syntax sugar for objects that are meant to be called as a function, by enabling the overloading of the operator `()`. Classes overloading the operator `()` are known as *functors*. The example above can be written as:

{% highlight c++ %}
struct Functor {
    int operator() (int x) {
        return x + 1;
    }
};
Functor f;
cout << f(1) << endl;
{% endhighlight %}

The major advantage over plain function pointers is that class-based functions can have state. So if we'd like to create a family of functions that increment by different amounts we could do:

{% highlight c++ %}
struct Functor {
    Function(int amount) : amount_(amount) {};
    int operator() (int x) {
        return x + amount_;
    }
    int amount_;
};
Functor f1(1);
cout << f1(1) << endl; // 2
Functor f2(2);
cout << f2(1) << endl; // 3
{% endhighlight %}

Let's now move to C++11.

### Lambda expressions

Lambda expressions were introduced in C++11 and are a further syntax sugar over functors:

{% highlight c++ %}
auto f = [](int x) {
  return x + 1;
};
cout << f(1) << endl;
{% endhighlight %}

Lambda expressions get converted to functors during compilation. We can notice above the use of `auto`, another of C++11 feature without which we can't use lambdas because their type is unspecified.

We discussed lambdas in the review of Scott Meyers's [Effective Modern C++]({{blog}}/2022/10/25/review-effective-modern-cpp.html).

## std::function

Now, with at least 3 ways to work with function objects which are not convertible to each other, how to make a general API that doesn't require a different overload for each type?

`std::function` was added to the STL in C++11 and it can "hold" any of a function pointer, a functor or a lambda, for example:

{% highlight c++ %}
// lambda
std::function<int(int)> f = [](int x) {
    return x + 1;
};

int inc(int x) {
    return x + 1;
}
// function pointer
std::function<int(int)> g = inc;

// functor
C c;
std::function<int(int)> h = c;
{% endhighlight %}

### Implementation

I was wondering how `std::function` is implemented and found it very hard to understand from looking at GCC's source. A simpler approach is suggested by `neuront` in [2], in which they define a custom constructor, destructor and the call/invoke operator. We can focus on the constructor since the others follow a very similar pattern:

{% highlight c++ %}
template <typename R, typename... Args>
class function<R(Args...)> {

  typedef char* (*construct_fn_t)(char*);

  template <typename Functor>
  static char* construct_fn(Functor* f) {
    return (char*) new Functor(*f);
  }

  construct_fn_t construct_f_ptr;
  char* buffer;

public:
  template <typename Functor>
  function(Functor f) :
    construct_f_ptr(reinterpret_cast<construct_fn_t>(construct_fn<Functor>)) {
      buffer = construct_f_ptr(reinterpret_cast<char*>(&f));
  }
};
{% endhighlight %}

There are a few things to dissect here. First, the template type of `function` uses a special syntax:

{% highlight c++ %}
template <typename R, typename... Args>
class function<R(Args...)> {};
{% endhighlight %}

Which is different from a simpler template type like:

{% highlight c++ %}
template <typename T>
struct Wrapper {};
{% endhighlight %}

When we define the type `function<int(int, float)>`, the return type is matched to `R` and the varargs are matched to `Args...`.

In the snippet below, `construct_fn_t` is just a type alias, a `typedef` trick we saw above in *Function Pointer*, and it's basically the untyped version of the constructor.

{% highlight c++ %}
typedef char* (*construct_fn_t)(char*);
{% endhighlight %}

Below, `construct_fn` is the typed constructor. It allocates a copy of `f` on the heap and we erase the type by casting the pointer to `char*`:

{% highlight c++ %}
template <typename Functor>
static char* construct_fn(Functor* f) {
  return reinterpret_cast<char*>(new Functor(*f));
}
{% endhighlight %}

Finally in the constructor of `function` we first erase the type of `construct_fn<Functor>` by casting it to `construct_fn_t` and then store it as a function pointer `construct_f_ptr`. Then we call the constructor via this "untyped" function pointer. We also need to cast `f` to `char*` because this is the type of argument `construct_f_ptr` expects.

{% highlight c++ %}
function(Functor f) :
  construct_f_ptr(reinterpret_cast<construct_fn_t>(construct_fn<Functor>)) {
    buffer = construct_f_ptr(reinterpret_cast<char*>(&f));
}
{% endhighlight %}

We might ask why do we need to define `construct_fn` as a static function and store it as a pointer, instead of simply doing:

{% highlight c++ %}
function(Functor f) {
  buffer = reinterpret_cast<char*>(new Functor(*f));
}
{% endhighlight %}

The key is that we might need to call `new Functor` again, but in a context where we might not have a handle to the type `Functor`. This is basically the role `construct_fn` plays here: keep a handle to the type `Functor`.

One place where we need this for example is for the copy-constructor:

{% highlight c++ %}
function(function const& rhs) : construct_f_ptr(rhs.construct_f_ptr) {
  if (construct_f_ptr) {
    buffer = construct_f_ptr(rhs.buffer);
  }
}
{% endhighlight %}

Here we don't have the `Functor` type but it's enclosed in `rhs.construct_f_ptr` "untyped" pointer. That's basically the whole idea and we can use the same principle to implement `operator()` and `~function`.


### Application

The technique for capturing a type in a closure without explicitly referencing the type anywhere can be useful, for example, to store a list of distinct template specialized types. Let's consider a relatively involved example, but one I've ran into before. Suppose we have a widget orchestrator class which we use as follows:

1. Register widgets
2. Set params
3. Execute widgets

To execute the widgets we need the params set in *Step 2*, so we can't execute them during registration (*Step 1*). If the widgets have a common base class, say `Widget`, we could keep them in a list internally and on *Step 3* we call their one of their virtual methods, say `run()`. A possible implementation is as follows:

{% highlight c++ %}
struct Widget {
  virtual void run(int param) = 0;
};

struct Orchestrator {
  void registerWidget(Widget& w) {
    _widgets.push_back(w);
  }
  void setParam(int param) {
    _param = param;
  }
  void run() {
    for (auto& widget : _widgets) {
      widget.run(_param);
    }
  }

  std::vector<Widget> _widgets;
  int _param;
};

// Example call
Orchestrator orch;
Widget a, b;
orch.registerWidget(a);
orch.registerWidget(b);
orch.setParam(1);
orch.run();
{% endhighlight %}

Now suppose the `Widget` is a template class and its children implementation are also doing some template specialization, for example:

{% highlight c++ %}
template <typename T>
struct Widget {
  Widget(T& impl) : _impl(impl) {}
  virtual void run(int param) = 0;

  T _impl;
};

struct WidgetWriter : Widget<Writer> {
  void run(int param) {
    _impl.write(param);
  }
};

struct WidgetReader : Widget<Reader> {
  void run(int param) {
    _impl.read(param);
  }
};
{% endhighlight %}

We can't store `std::vector<Widget<T>>` in `Orchestrator` because `T` doesn't have any meaning there. A solution is to store `std::function` which captures `Widget<Writer>` or `Widget<Reader>` respectively:

{% highlight c++ %}
struct Orchestrator {
  template<typename T>
  void registerWidget(Widget<T>& w) {
    widgetRunners_.push_back([&](int param) {
      w.run(param);
    });
  }
  void setParam(int param) {
    _param = param;
  }
  void run() {
    for (auto& widgetRunner : _widgetRunners) {
      widgetRunner(_param);
    }
  }

  std::vector<std::function<void(int)>> _widgetRunners;
  int _param;
};

// Example call
Orchestrator orch;
WidgetWriter a;
orch.registerWidget(a);
WidgetReader b;
orch.registerWidget(b);
orch.setParam(1);
orch.run();
{% endhighlight %}

Notice how `_widgetRunners` has no type information about `Widget<T>` but the lambdas it stores do. Also notice how this pattern is very similar on how `std::function` itself is implemented!

Malte Skarupke's [blog post](https://probablydance.com/2012/12/16/the-importance-of-stdfunction/) [3] also makes the observation `std::function` can be useful to replace the need for virtual functions altogether. In our example, instead of having `registerWidget()` we could have `registerWriter` and `registerReader` and have the lambda also do the work that `Widget::run()` is doing:

{% highlight c++ %}
struct Orchestrator {
  void registerWriter(Writer& w) {
    widgetRunners_.push_back([&](int param) {
      w.write(param);
    });
  }
  void registerReader(Reader& r) {
    widgetRunners_.push_back([&](int param) {
      r.write(param);
    });
  }
  ...
};
{% endhighlight %}

## Conclusion

I was trying to understand how the template syntax - `<int(int, bool, std::string)>` - got passed to a template and the one place I recalled seen this was with `std::function`, so I decided to try to create my own version.

It turned out to be really hard and looking at the source code for the GCC implementation didn't help since it's very hard to understand. It wasn't until a ran into a StackOverflow [post](https://stackoverflow.com/questions/18453145/how-is-stdfunction-implemented) [2] that I found an implementation I could understand.


## Related Posts

[Von Neumann Architecture](https://www.kuniga.me/blog/2019/06/10/von-neumann-architecture.html) - In that post we touched on the idea of storing data and instructions in the same place.

Mentioned by [Shared Libraries]({{blog}}/2025/04/25/shared-libraries.html).

## References

* [[1](https://en.cppreference.com/w/cpp/utility/functional/function)] cppreference.com - std::function
* [[2](https://stackoverflow.com/questions/18453145/how-is-stdfunction-implemented)] StackOverflow - How is std::function implemented?
* [[3](https://probablydance.com/2012/12/16/the-importance-of-stdfunction)] Probably Dance - The importance of std::function
