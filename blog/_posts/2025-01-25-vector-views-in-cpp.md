---
layout: post
title: "Vector Views in C++"
tags: [c++]
vanity: "2025-01-25-vector-views-in-cpp"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/logo.svg" alt="C++ logo" />
</figure>

A view can be thought of as an object that is derived from another without representing it explicitly.

In programming, a classic example is a string view: it can be used to represent a substring of a string without actually storing the whole substring: it only needs two indexes representing the start and end of the interval.

Another classic example are views in SQL. A view represents a table but it doesn't actually store the rows explicitly. It's a query to another table and it can be materialized on demand.

In this post I'd like to explore views but for `std::vector`.

<!--more-->

## Context

Recently at work I wrote a function that takes in a vector of a given type, then groups the entries by some key. For example, suppose we have a class `Person`:

{% highlight c++ %}
struct Person {
  std::string name;
  int age;
};
{% endhighlight %}

and that we want to group them by age first:

{% highlight c++ %}
std::unordered_map<int, std::vector<Person>> peopleByAge;
for (const auto &person : people) {
    peopleByAge[person.age].emplace_back(person);
}
{% endhighlight %}

so that we can process them grouped by age:

{% highlight c++ %}
void processPeopleForAge(const std::vector<Person>& people, int age);

for (const auto& [age, people] : peopleByAge) {
    processPeopleForAge(people, age);
}
{% endhighlight %}

It was then pointed out to me that this code is innefficient, because `Person` is copied when assining to the `std::vector` inside `std::unordered_map`.

## Vector of references

My first attempt was to turn `std::unordered_map<int, std::vector<Person>>` into `std::unordered_map<int, std::vector<Person&>>`, i.e. have the `Person` inside the inner vector be a reference.

It turns out `std::vector` does not allow references. The reason being is that it requires its elements to be copiable and assignable.

A vector stores its data in a contiguous segment of memory. It starts with a pre-allocated segment, but if it grows beyond this initial size, it must move to a bigger segment of memory.

When it does so, it actually needs to move or copy the elements over and it will do so using the copy/move constructors. So adding elements to a vector or resizing it can actually cause the constructor of its existing elements to be called! I've been working with C++ for many years and never realized that! In my mind I thought `std::vector` would simply do memcpy to a new destination, to copy the bytes as is.

To reduce these extraneous copies, we can explicitly reserve a size if we have a good estimate of the vector size via `.reserve()`.

The need for it to be assignable is because when we do `vec[i] = x`, the existing object at index `i` would be re-assigned a different value.

### Using `std::reference_wrapper<T>`

A way to make a reference into an actual copiable and assignable object is to wrap it into one. This is essentially what `std::reference_wrapper` does. The key to turn a reference into something we can manipulate is by converting it into a pointer!

A pointer is just a integer, so it can be easily copied and assigned to. Here's a simple version of `std::reference_wrapper`:

{% highlight c++ %}
template <typename T>
struct reference_wrapper {
  reference_wrapper(T& t) {
    value_ = std::addressof(t);
  }

  T& get() { return *value_; }

  T* value_;
};
{% endhighlight %}

The expression `std::addressof()` is the one "converting" from a reference to a raw pointer. To get a reference back to the original variable we can first dereference the pointer and then return a reference.

This enables us to have a vector of references in our original example:

{% highlight c++ %}
std::unordered_map<
  int,
  std::vector<reference_wrapper<Person>>
> peopleByAge;

for (const auto &person : people) {
  peopleByAge[person.age].emplace_back(person);
}
{% endhighlight %}

With the caveat that now `processPeopleForAge()` will need to take in these references and call `.get()` on them.

{% highlight c++ %}
void processPeopleForAge(
  const std::vector<reference_wrapper<Person>>& people,
  int age
);
{% endhighlight %}

The STL implementation of `reference_wrapper` also has this interesting operator:

{% highlight c++ %}
operator T&() {
  return *value_;
}
{% endhighlight %}

Which is invoked when we assign that class to a reference type, so it's just a syntax sugar for calling `.get()`:

{% highlight c++ %}
reference_wrapper<Person> p;

Person& pRef = p.get();
Person& pRef = p; // calls that operator
{% endhighlight %}

### Using Raw Pointers

Another option is to use raw points instead of `reference_wrapper`, since that's what it does under the hoods anyway. One advantage of `reference_wrapper` besides being more readable is that it cannot be null (even when it's moved away from).

## Vector View

Instead of having each element be a reference, I was wondering if it would be helpful to have the concept of a vector view, that is, a data structure that can represent a subset of a vector without incurring in copies.

### Span

A special case of this already exists via the C++20 structure `std::span<T>`. It represents a contiguous range and can be used as a view for a vector, for example:

{% highlight c++ %}
#include <span>

std::vector<int> vec = {1, 2, 3, 4, 5};
std::span<int> subVec(vec.begin() + 1, vec.begin() + 3);
for (auto& x : subVec) {
  std::cout << x << std::endl;
}
{% endhighlight %}

The span doesn't copy the elements, but only works with contiguous intervals, so it wouldn't be useful for the case I had.

### Custom Class

We can implement our own version of a vector view by storing a reference to the original object and another vector of indices:

{% highlight c++ %}
template <typename T>
class vector_view {
public:
    using TData = typename std::vector<T>::iterator;

    vector_view() = default;

    vector_view(TData data) : data_(data) {}

    void push_back(size_t index) {
      indices_.push_back(index);
    }

    T& operator[](size_t index) {
      return data_[indices_[index]];
    }

    size_t size() {
      return indices_.size();
    }

private:
  std::vector<size_t> indices_;
  TData data_;
};
{% endhighlight %}

Then our code could be changed to:

{% highlight c++ %}
std::unordered_map<int, vector_view<Person>> peopleByAge;

for (int = 0; i < people.size(); i++) {
  Person& person = people[i];
  if (!peopleByAge.contains(person.age)) {
    peopleByAge[person.age] = people.begin();
  }
  peopleByAge[person.age].emplace_back(i);
}
{% endhighlight %}

It feels pretty clunky that the view starts unitialized and that we to check for it inside the loop. Another undesired semantics is that the elements can appear in a different order inside the `vector_view` which seems wrong: vector is an ordered list of elements, so we'd expect a subset to preserve the relative order.

Another implementation could be to use a bit vector to indicate the presence of the element in the view, but this could be pretty wasteful and innefficient if the vector view was very sparse. Yet another approach would be to keep the indices sorted, which has its own set of downsides.

For this particular case we might encapsulate this use into its own structure, a map of vector views. It takes the original vector and a function to compute the key:

{% highlight c++ %}
template <typename T, typename K>
class vector_partition {
public:
  vector_partition(std::vector<T>& data, std::function<K(T&)>) {
    for (int i = 0; i < data.size(); ++i) {
      K key = getKey(data[i]);
      if (!dataByKey_.contains(key)) {
        dataByKey_[key] = vector_view<T>(data.begin());
      }
      dataByKey_[key].push_back(i);
    }
  }

  const vector_view<T>& operator[] (K key) {
    return dataByKey_[key];
  }

private:
  std::unordered_map<K, vector_view<T>> dataByKey_;

};
{% endhighlight %}

This avoids the issues with using `vector_view` directly since we can guarantee internally that indices preserve relative order and he hide the ugly initialization inside the loop. It has the obvious downside of having a much narrower application. The code using it would be very simple however:

{% highlight c++ %}
vector_partition<Person> peopleByAge(
  people,
  [](Person& p) {
    return p.age;
  }
);
{% endhighlight %}

## Conclusion

When I started looking into how to solve my original problem, i.e. avoid copies when groupping by a vector by a key, I imagined there would be an existing data structure that would model this use case neatly.

Turns out it doesn't and after trying my hand on coming up with one, I arrived at implementations that are too clunky or too narrow to be useful. In the end, I went with `std::vector<std::reference_wrapper<T>>`.

It was a fun exercise though, and I learned things I feel like I should have known! One lesson learned for me is that `.reserve()` is much more important than I thought!
