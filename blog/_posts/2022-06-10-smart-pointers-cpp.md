---
layout: post
title: "Smart Pointers in C++"
tags: [c++]
excerpt_separator: <!--more-->
vanity: "2022-06-10-smart-pointers-cpp"

---
{% include blog_vars.html %}

In this post we'll implement our own version of `std::unique_ptr` and `std::shared_ptr` as an educational exercise.

These are wrappers around raw pointers and are known as smart pointers because they can handle the automatic deletion of the underlying raw pointer.

<!--more-->


## Unique Pointer

The semantics of unique pointer is that only one variable can reference it at a given time. When that variable falls out of scope the underlying raw pointer is freed.

We start off by defining the class skeleton:

{% highlight c++ %}
template <typename T>
class unique_ptr {
  public:
    ...

  private:
    T* raw_ptr_ = nullptr;
};
{% endhighlight %}


### Constructors / Destructors

The main constructor takes a raw pointer to some type `T`:

{% highlight c++ %}
explicit unique_ptr(T* raw_ptr) :
  raw_ptr_(raw_ptr) {}
{% endhighlight %}

Another useful constructor is one with argument is provided, which implicitly sets `raw_ptr` to `nullptr`:

{% highlight c++ %}
unique_ptr() {}
{% endhighlight %}

We implement the move constructor which enables transferring ownership. Since `this->raw_ptr_` is initialized with `nullptr` we are effectively setting `ptr.raw_ptr_` to null when we perform a swap.

{% highlight c++ %}
unique_ptr(unique_ptr<T> &&ptr) {
  std::swap(ptr.raw_ptr_, raw_ptr_);
}
{% endhighlight %}

We delete the copy constructor since a copy would violate that only one variable references the raw pointer.

{% highlight c++ %}
unique_ptr(unique_ptr<T> &ptr) = delete;
{% endhighlight %}

This also gives us compile-time checks that prevents unintended copies:

{% highlight c++ %}
unique_ptr<C> p(new C("test"));
// error: call to deleted constructor of 'unique_ptr<C>'
unique_ptr<C> p2 = p;
{% endhighlight %}

We can rely on the invariant that any particular instance of `unique_ptr` is the only reference to the underlying raw pointer, so when it fall out of scope, at the destructor, we can free the raw pointer.

We define the helper function `reset()` which is useful for other cases too. Calling reset frees the existing raw pointer and assigns a new one, `nullptr` in this case.

{% highlight c++ %}
~unique_ptr() {
  reset();
}

void reset(T* raw_ptr = nullptr) {
  if (raw_ptr_) {
    delete raw_ptr_;
  }
  raw_ptr_ = raw_ptr;
}
{% endhighlight %}

### Accessing Raw Pointer

We can provide a read-only access to the internal raw pointer:

{% highlight c++ %}
const T* get() { return raw_ptr_; }
{% endhighlight %}

or transfer the ownership away from `unique_ptr` via `release()`:

{% highlight c++ %}
T* release() {
  auto raw_ptr = raw_ptr_;
  raw_ptr_ = nullptr;
  return raw_ptr;
}
{% endhighlight %}

### Assignment Operators

As with the copy constructor, we don't want two references to the raw pointer, so we delete the copy assignment.

{% highlight c++ %}
unique_ptr<T>& operator= (const unique_ptr<T> &other) = delete;
{% endhighlight %}

We can have the move operator. But we need to make sure to clear `other`'s raw pointer (via `release()`) and also to free the existing raw pointer of `this` (via `reset()`).

{% highlight c++ %}
unique_ptr<T>& operator= (unique_ptr<T> &&other) {
  reset(other.release());
  return *this;
}
{% endhighlight %}

### Pointer Syntax

We can add pointer syntax to our `unique_ptr` class by overloading the operators `->` and `*`:

{% highlight c++ %}
T* operator->() const { return raw_ptr_; }
T& operator*() const { return *raw_ptr_; }
{% endhighlight %}

Now we can use them as we do with raw pointers:

{% highlight c++ %}
struct C {
  C(int x): x(x) {}
  int x;
}
unique_ptr<C> p(new C(1));
cout << p->x << endl;
// De-reference
C c = *p;
cout << c.x << endl;
{% endhighlight %}

### Asymmetrical Lifecycle Management

Despite the restrictions imposed by `unique_ptr` it's possible to violate the constraints, for example:

{% highlight c++ %}
C* c = new C(1);
unique_ptr<C> p1(c);
{
  unique_ptr<C> p2(c);
}
cout << p1.x << endl;
{% endhighlight %}

The unique pointer `p1` thinks it's the only reference to `c` and so does `p2`, so when `p2` fall out of scope, it will free `c`. When `p1` tries to access its internal raw pointer, it will have a null pointer.

One way to avoid this is to let `unique_ptr` handle the entire lifecycle of the pointer. We can change the constructor to take vararg set of params and forward to a `new` call internally:

{% highlight c++ %}
template<typename... Args>
unique_ptr(Args&&... args) {
  raw_ptr_ = new T(std::forward<Args>(args)...);
}
{% endhighlight %}

However, what if `T`'s constructor could take `T*` as parameter? Which constructor of `unique_ptr` would be used?

{% highlight c++ %}
struct X {
  X() {}
  X(X* x) {}
};
unique_ptr<X> px(new X());
{% endhighlight %}

`unique_ptr(T* raw_ptr)` seems to take precedence here, but it could be a confusing behavior. Similarly if `T`'s constructor could take `nullptr` as parameter we'd occlude it via `unique_ptr`'s "forward" constructor.

The way STL solves this is by introducing a helper function `std::make_unique()` which is exactly our vararg constructor:

{% highlight c++ %}
template<typename T, typename... Args>
unique_ptr<T> make_unique(Args&&... args) {
    return unique_ptr<T>(new T(std::forward<Args>(args)...));
}
{% endhighlight %}

Alternatively we could make this a static method in `unique_ptr`:

{% highlight c++ %}
template<typename... Args>
unique_ptr<T> static make(Args&&... args) {
  return unique_ptr<T>(new T(std::forward<Args>(args)...));
}
{% endhighlight %}

we could then make `explicit unique_ptr(T* raw_ptr)` private to avoid the pitfall of the lifecycle being managed by two different entities. Perhaps static methods for constructing objects is not very idiomatic in C++ given it usually does via signature overloading.

### Memory Leak During Exceptions

Delegating a `new` call to a function makes it exception safe. Herb Sutter [5] argues that:

{% highlight c++ %}
f (unique_ptr<C> { new C(1) }, unique_ptr<C> { new C(2) });
{% endhighlight %}

Can leak memory if the expressions happen to be evaluated in interleaved order:

1. Allocate memory (first argument)
1. Allocate memory (second argument)
1. Invoke constructor `C(1)`
1. Invoke constructor `C(2)`
1. Call `unique_ptr<T>()` (first argument)
1. Call `unique_ptr<T>()` (second argument)
1. Call `f()`

If 3. throws, the memory allocated by `new` (2) is not freed. If the `new` is called inside a function, then we guarantee the interleaving doesn't happen:

{% highlight c++ %}
f (std::make_unique<C>(1), std::make_unique<C>(2));
{% endhighlight %}

Note we left out many functionalities available from `std::unique_ptr`, including the assignment operator.

## Shared Pointer

The semantics of shared pointer is that it allows one or more references to it and it deletes the underlying raw pointer when the last reference falls out of scope.

### Constructors / Destructors

It's very similar to `unique_ptr` in many aspects but for `shared_ptr` we also need to keep a reference count.

We keep the reference as a pointer so that it can be shared by multiple instances of `shared_ptr`.

{% highlight c++ %}
template <typename T>
class shared_ptr {
  public:

  explicit shared_ptr(T* raw_ptr) : raw_ptr_(raw_ptr) {
    ref_count_ = new int(0);
    ++(*ref_count_);
  }

  private:
    T* raw_ptr_;
    int* ref_count_;
};
{% endhighlight %}

The copy constructor can be defined for shared pointers. We just need to increment the reference count and share it with the destination.

{% highlight c++ %}
shared_ptr(const shared_ptr<T> &ptr) {
  ref_count_ = ptr.ref_count_;
  raw_ptr_ = ptr.raw_ptr_;
  ++(*ref_count_);
}
{% endhighlight %}

The move constructor just transfers the contents from source to destination without changing the reference count.

{% highlight c++ %}
shared_ptr(const shared_ptr<T> &&ptr) {
  ref_count_ = ptr.ref_count_;
  raw_ptr_ = ptr.raw_ptr_;
  ptr.raw_ptr_ = nullptr;
}
{% endhighlight %}

The destructor decrements the reference count and if gets to 0, it can free both pointers. Note this implementation is not thread safe in regards to reference count, but [the STL one is](https://stackoverflow.com/questions/9127816/stdshared-ptr-thread-safety-explained).

We implement the logic in its own function, `clear()`, since it is useful for other cases.

{% highlight c++ %}
template <typename T>
~shared_ptr() {
  clear();
}

void clear() {
  if (ref_count_ == nullptr && raw_ptr_ == nullptr) {
    return;
  }

  --(*ref_count_);
  if (*ref_count_ == 0) {
    delete raw_ptr_;
    delete ref_count_;
  }
}
{% endhighlight %}

Note that `release()` doesn't make sense in a `shared_ptr`.

### Assignment Operators

The assignment operators have the same spirit as the constructors. The copy assignment incurs increase of the reference count and the clearing of the prior raw pointer:

{% highlight c++ %}
shared_ptr<T>& operator= (const shared_ptr<T> &other) {
  clear();
  raw_ptr_ = other.raw_ptr_;
  ref_count_ = other.ref_count_;
  ++(*ref_count_);
  return *this;
}
{% endhighlight %}

The move assignment keeps the `other` reference count unchanged, but it clears its prior raw pointer:

{% highlight c++ %}
shared_ptr<T>& operator= (const shared_ptr<T> &&other) {
  clear();
  raw_ptr_ = other.raw_ptr_;
  ref_count_ = other.ref_count_;
  other.raw_ptr_ = null;
  other.ref_count = null;
  return *this;
}
{% endhighlight %}

### Cyclical Reference

One problem with reference count is that it can lead to memory leaks if cycles are created:

{% highlight c++ %}
struct Node {
  static int destroyed_cnt;
  shared_ptr<Node> next;

  ~Node() {
    destroyed_cnt++;
  }
};

int Node::destroyed_cnt{0};

{
  auto n1 = shared_ptr<Node>::make();
  auto n2 = shared_ptr<Node>::make();
  auto n3 = shared_ptr<Node>::make();
  n1->next = n2;
  n2->next = n3;
  n3->next = n1;
}
// Prints 0, none of the destructors were called
std::cout << Node::destroyed_cnt << std::endl;
{% endhighlight %}

In the example above, none of the allocated raw pointers are freed because upon destruction, each `shared_ptr` has a reference count of 1. It's possible to prevent this by using `std::weak_ptr` [6].

The complete implementation including tests is available on [Github]({{github}}/).

## Related Posts

Mentioned by [[Book] Effective Modern C++]({{blog}}/2022/10/25/review-effective-modern-cpp.html).

## Conclusion

From [4] it seems like a lot of people attempt to write their own implementation. I found it an useful exercise and learned about a few things:

* Smart pointers are wraps over raw pointers. It's obvious in hindsight but I hadn't stopped to think about it.
* The direct call of multiple `new`s in the same expression can lead to memory leak.
* We cannot forward arguments in `unique_ptr()`/`shared_ptr()` constructor since it occludes other constructors.

## References

* [[1](https://gcc.gnu.org/onlinedocs/libstdc++/libstdc++-html-USERS-4.4/a01404.html)] GCC Source code: unique_ptr.h
* [[2](https://gcc.gnu.org/onlinedocs/gcc-4.6.0/libstdc++/api/a01033_source.html)] GCC Source code: shared_ptr.h
* [[3](https://stackoverflow.com/questions/9127816/stdshared-ptr-thread-safety-explained)] Stack Overflow: std::shared_ptr thread safety explained
* [[4](https://lokiastari.com/blog/2014/12/30/c-plus-plus-by-example-smart-pointer/)] Loki Astari - Smart-Pointer - Unique Pointer
* [[5](https://herbsutter.com/gotw/_102/)] GotW #102: Exception-Safe Function Calls (Difficulty: 7/10)
* [[6](https://en.cppreference.com/w/cpp/memory/weak_ptr/~weak_ptr)] std::weak_ptr<T>::~weak_ptr
