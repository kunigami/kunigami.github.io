---
layout: post
title: "Memory Management Choices in C++"
tags: [c++]
excerpt_separator: <!--more-->
vanity: "2022-04-07-memory-management-choices-cpp"

---
{% include blog_vars.html %}

In this post we'll explore the different mechanisms for passing data around in C++ and be build a set of heuristics to help us choosing between them for different scenarios.

A lot of these will boil down to personal taste, so keep the salt shaker handy.

<!--more-->


<figure class="center_children">
  <img src="{{resources_path}}/tomatoes.jpg" alt="Table with the results of this experiment" />
  <figcaption>By Chantal Garnier (<a href="https://unsplash.com/photos/oiIJ5jGijdQ">Source</a>)</figcaption>
</figure>

## Pass by Value vs by Reference

In high-level languages like JavaScript and Python, variables containing primitive types are passed by value while other types are passed by reference. In C++ variables are always passed by value unless we pass pointers or references:

{% highlight c++ %}
struct C {}

void by_value(C x) {}
C c;
by_value(c);

void by_reference(C& x) {}
by_reference(c);

void by_pointer(C* x) {}
by_pointer(&c);
{% endhighlight %}

Implicit in the pass-by-value is a call to the copy constructor. We can see it being called by defining `C(const C&)`:

{% highlight c++ %}
struct C {
  C(const C&c) {
    cout << "Making a copy" << endl;
  }
}

C c;
by_value(c); // prints "Making a copy"
{% endhighlight %}

If we don't provide one, there's usually a default copy constructor that is used, which recursively calls the copy constructor of its internal variables. If `C` is a complex class it can be a very expensive operation.

Do we ever want to pass data by value? Meyers [1] (Item 20) suggests that we should always pass by reference, except for built-in types (note this does not include `std::string`) and STL iterators (e.g. like `vector::begin()`).

**Heuristic 1.** *Always pass by reference, except for built-in types and STL iterators.*

## Pointers vs. References

When should we use pointers vs references? There's no general consensus [2] but I personally am on the camp of avoiding pointers as much as possible. Let's consider a few scenarios.

### Optional Values

Pointers can be null, so it might be desirable to use them to encode optional values:

{% highlight c++ %}
void optional_value(std::shared_ptr<C> maybe) {
  if (maybe != nullptr) {
    maybe->print();
  }
}

std::shared_ptr<C> none(nullptr);
optional_value(none);
std::shared_ptr<C> some;
optional_value(some);
{% endhighlight %}

The alternative is to use `std::optional`. The major problem for this contrived example is that `std::optional` cannot store a reference [3], so to avoid copies when wrapping it in an `std::optional` we need to use `std::optional<std::reference_wrapper>`:

{% highlight c++ %}
void optional_value(std::optional<std::reference_wrapper<C>>& maybe) {
  if (maybe != std::nullopt) {
    maybe.value().get().print();
  }
}

std::optional<std::reference_wrapper<C>> none;
optional_value(none);
std::optional<std::reference_wrapper<C>> some(std::ref(obj));
optional_value(some);
{% endhighlight %}

It unfortunately looks quite verbose. Interestingly `boost::optional` does allow optional references. [4] provides some insight on why optional references did not make into the standard.

### Received Pointers

Another scenario is when we receive a pointer from a function and might want to pass it to an internal function. In this case de-referencing the pointer and passing as reference does not involve copies, so it's fine.

{% highlight c++ %}
std::shared_ptr<C> get_pointer() {
  return std::make_unique<C>();
}

void print(C& c) {
  c.print();
}

auto res = get_pointer();
print(*res);
{% endhighlight %}

### Setters

How about a setter method? We likely will assign the argument to an member variable which possibly outlive the object. Example:

{% highlight c++ %}
struct B {};

struct C {
  B b_;
  void set_b(B& b) { b_ = b; }
  B get_b() { return b_; }
};

C c;
{
  B b;
  c.set_b(b);
}
c.get_b(); // okay
{% endhighlight %}

In this case a copy will be triggered on `b_ = b` so it's ok. We cannot store a reference since its referred object could fall out of scope, so if we do want to avoid a copy we then need to use a pointer:

{% highlight c++ %}
struct B {};

struct C {
  std::shared_ptr<B> b_;
  // B copy constructor not triggered
  void set_b(std::shared_ptr<B> b) { b_ = b; }
  std::shared_ptr<B> get_b() { return b_; }
};
{% endhighlight %}

Alternatively we could transfer the ownership to the class by expecting a rvalue reference so the caller is forced to use `std::move()`:

{% highlight c++ %}
struct B {};

struct C {
  B b_;
  // Move happens
  void set_b(B&& b) { b_ = std::move(b); }
  // Copy happens
  B get_b() { return b_; }
};

C c;
{
  B b;
  c.set_b(std::move(b));
}
c.get_b();
{% endhighlight %}

The problem with this approach is that `get_b()` needs to make a copy.

With the caveat of potential boilerplate and some care to avoid copying when modeling optionals, we can state the following:

**Heuristic 2.** *Always prefer references over pointers for input arguments. Except if it will be stored internally and we want to avoid copies.*

## Smart Pointers vs. Raw Pointers

One other scenario where we can't use references is when returning a object on the heap (created via `new`), and we have to return a pointer (Item 21 [1]). The problem with returning a pointer is that someone needs to `delete` the object in the heap, otherwise we incur in memory leak.

{% highlight c++ %}
C* get_ptr() {
  return new C();
}
{
  C* c = get_ptr();
  c->print();
}
// memory leak, we didn't delete the object in the heap
{% endhighlight %}

Smart pointers aim to solve this sort of pitfalls. It knows when a object cannot be referenced anymore, and thus handles the `delete` automatically:

{% highlight c++ %}
std::shared_ptr<C> get_ptr() {
  return std::make_unique<C>();
}
{
  auto c = get_ptr();
  c->print();
}
// deleted
{% endhighlight %}

For this case it seems strictly better to return smart pointers. For function arguments, [6] argues we should use raw pointers if we don't care about the ownership model of the pointer.

However, in light of *Heuristic 2*, we either will not use pointers or we should care about ownership (i.e. when storing it internally) and thus can define another heuristic:

**Heuristic 3.** *Always prefer returning a smart pointer over a raw pointer.*

Another thing to keep in mind is the constructor elision we mentioned in [6]. The compiler usually optimizes cases where we return objects by value to avoid a copy constructor.

{% highlight c++ %}
C get() {
  return C();
}
// no copy performed
C c = get();
c.print();
{% endhighlight %}

So we don't need to use pointers in this case.

## Shared vs. Unique Pointers

Both `shared_ptr` vs `unique_ptr` are smart pointers in the sense that they know when to delete the object when it knows such object cannot be referenced anymore.

The difference between them is that we can assign a `shared_ptr` to multiple variables whereas a `unique_ptr` can only belong to one, and re-assignments must "move" the data. Example for `shared_ptr`:

{% highlight c++ %}
std::shared_ptr<C> get_shared() {
  return std::make_shared<C>();
}

auto c1 = get_shared();
auto c2 = c1; // ok
{% endhighlight %}

Example for `unique_ptr`:

{% highlight c++ %}
std::unique_ptr<C> get_unique() {
  return std::make_unique<C>();
}

auto c1 = get_shared();
// ok, changing ownership
auto c2 = std::move(c1);
// error: call to implicitly-deleted copy constructor
// of 'std::unique_ptr<C>'
auto c3 = c2;
{% endhighlight %}

### unique_ptr and Destructors

One potential pitfall of `unique_ptr` over `shared_ptr` is that the former doesn't keep information about the type used to create it. This means when the pointer gets out of scope, the *current* type's destructor is used [7].

For example, suppose we have class `D` deriving from `C`, when when we create a smart pointer we return it as a pointer to `C`:

{% highlight c++ %}
struct C {
  ~C() { std::cout << "destroy C" << std::endl; }
};
struct D : C {
  ~D() { std::cout << "destroy D" << std::endl; }
};

std::shared_ptr<C> get_shared_d() {
  return std::make_shared<D>();
}
std::unique_ptr<C> get_unique_d() {
  return std::make_unique<D>();
}

{
  auto d1 = get_shared_d();
} // calls D + C destructors
{
  auto d2 = get_unique_d();
} // only calls C destructor
{% endhighlight %}

We can see that when the `std::unique_ptr<C>` falls out of scope, only `C`'s destructor was called even though we constructed an instance of `D`. The solution is to make `C`'s destructor virtual:

{% highlight c++ %}
struct C {
  virtual ~C() { std::cout << "destroy C" << std::endl; }
};
struct D : C {
  ~D() { std::cout << "destroy D" << std::endl; }
};

std::shared_ptr<C> get_shared_d() {
  return std::make_shared<D>();
}
std::unique_ptr<C> get_unique_d() {
  return std::make_unique<D>();
}

{
  auto d1 = get_shared_d();
} // calls D + C destructors
{
  auto d2 = get_unique_d();
} // calls D + C destructors
{% endhighlight %}

In general I prefer to start with the most restrictive mode possible (e.g. `const` modifier), since it simplifies reasoning about the code. When the need arises, it's possible to relax the constraints.

Thus, unless we explicitly expect our pointer to be shared by multiple owners, I'd default to `unique_ptr`.

**Heuristic 4.** *Everything being equal, prefer unique_ptr over shared_ptr.*

## Conclusion

In this post we came up with 4 heuristics to help deciding between different memory syntax and semantics.

The idea is that these heuristics are very general with small exceptions, so they can be remembered more easily. The problem with them is that they leave out a lot of nuance and can be overly prescriptive.

As we gain more experience with C++ we get a better "feel" for when to use what. As with a lot of subjective recommendation I value consistency more than strong opinions so I'd rather stick to existing patterns than follow my own heuristics.

## References

* [1] Effective C++ (3rd ed.), S. Meyers
* [[2](https://stackoverflow.com/questions/7058339/when-to-use-references-vs-pointers )] StackOverflow: When to use references vs. pointers
* [[3](https://stackoverflow.com/questions/45264601/does-an-stdoptional-parameter-create-a-copy)] StackOverflow: Does an std::optional parameter create a copy?
* [[4](https://www.fluentcpp.com/2018/10/05/pros-cons-optional-references/)] Fluent C++: Why Optional References Didn’t Make It In C++17
* [[5](https://stackoverflow.com/questions/6675651/when-should-i-use-raw-pointers-over-smart-pointers)] When should I use raw pointers over smart pointers?
* [[6]({{blog}}/2022/03/01/moving-semantics-cpp.html)] NP-Incompleteness: Move Semantics in C++
* [[7](https://stackoverflow.com/questions/6876751/differences-between-unique-ptr-and-shared-ptr)] Differences between unique_ptr and shared_ptr [duplicate]
