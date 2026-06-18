---
layout: post
title: "Velox: The Vector"
tags: [distributed systems, databases]
excerpt_separator: <!--more-->
vanity: "2026-06-01-velox-vectors"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/velox-logo.svg" alt="Velox Logo" />
</figure>


[Velox](https://github.com/facebookincubator/velox) is an open source C++ library by Meta that can be used to perform computation common to distributed engines like Presto.

Its offerings include columnar operations, a rich type system, an expression parser and a smart resource management such as memory [1]. In this series of posts we'll go over different components of Velox. In this inaugural post we cover the vector data structure which is used to store the columnar data.

<!--more-->

One of the main data structures in Velox is the "vector" (more specifically subclasses of `BaseVector`). We'll see how this structure can be used to represent columnar data later, but first, let's build our understanding from familiar concepts. We'll start with one of the simplest vector types, the `FlatVector`, which is analogous to `std::vector`.

## Flat Vector

We'll start by defining a function, `makeFlatVector()`, to create a `FlatVector` from a `std::vector`, both to cover the API but also to reuse this function for later examples, given flat vectors are the base for more complex vectors:

{% highlight c++ %}
#include "velox/buffer/Buffer.h"
#include "velox/common/memory/Memory.h"
#include "velox/vector/FlatVector.h"

using namespace facebook::velox;

// (3)
template <typename T>
VectorPtr makeFlatVector(
    memory::MemoryPool* pool,
    const std::vector<T>& values) {
    // (1)
    using NativeType = typename CppToType<T>::NativeType;
    auto vector = BaseVector::create<FlatVector<NativeType>>(
        CppToType<T>::create(), values.size(), pool);
    for (vector_size_t i = 0; i < values.size(); ++i) {
        vector->set(i, NativeType(values[i]));
    }
    return vector;
}

// (2)
memory::MemoryManager::initialize(memory::MemoryManager::Options{});
auto pool = memory::memoryManager()->addLeafPool("demo");

std::vector<int> integers {10, 20, 30};
auto vector = makeFlatVector(pool.get(), integers);
{% endhighlight %}

We'll cover Velox types in more details later, but we note here that Velox has dedicated types for some of them, for example `std::string` is mapped to `velox::StringView`, which can be used to avoid copies when manipulating vectors. The template `CppToType<T>::NativeType` can be used to map from C++/STL types to Velox, which is what we use in (1).

We'll also cover this later, but for now we note that Velox manages its own memory [1] so we need to work with a memory pool and pass it around APIs (2). Part of the reasons include being optimized for data analytics use cases and it also supports disk spill out of the box.

The return type is `VectorPtr` (3), which is an alias for `std::shared_ptr<BaseVector>` and it's one of the most common way to represent Velox vectors in code.

## Encoding

**Dictionary.** As we mentioned, `FlatVector` is semantically similar to `std::vector` including in how is stores the data. But one big reason to use columnar-oriented data structures is to be able to store them more efficiently. A classic example is that if you underlying table has some columns of low cardinality. For example, suppose your data is:

{% highlight text %}
| Species         | Class     |
| --------------- | --------- |
| Axolotl         | Amphibian |
| Fennec Fox      | Mammal    |
| Aardvark        | Mammal    |
| Shoebill        | Bird      |
| Leafy Seadragon | Fish      |
| Quokka          | Mammal    |
{% endhighlight %}

Here the column `Species` has high cardinality (unique), whereas `Class` is bounded to a dozen. If we were to represent this efficiently in memory we could use indirection (normalization) and store either indices to a different vector or shared pointers. This is what the class `DictionaryVector` accomplishes.

Let's define a utility `makeDictionaryVector()`:

{% highlight cpp %}
// for copyToBuffer()
#include "velox/vector/VectorUtil.h"

template <typename T>
VectorPtr makeDictionaryVector(
    memory::MemoryPool* pool,
    const std::vector<T>& baseValues,
    const std::vector<vector_size_t>& indices) {
  auto base = makeFlatVector<T>(pool, baseValues); // (1)
  auto indicesBuf = copyToBuffer(indices, pool); // (2)
  return BaseVector::wrapInDictionary(
      nullptr, // (3) no nulls
      indicesBuf,
      indices.size(), // (4) output size
      base); // (5)
}

std::vector<std::string> values {"Amphibian", "Mammal", "Bird", "Fish"};
auto d = makeDictionaryVector<std::string>(
    pool.get(),
    values,
    {1, 2, 2, 0, 1}); // (6)
{% endhighlight %}

The key here is that in (6) we only store the indices of the values array. So instead of repeating the same strings multiple times, we only repeat the indices. In (4), we provide the size of the vector indices explicitly, because we're storing the entries on a generic buffer (which might be over-allocated for reuse) so it doesn't store the used size.

In (5) the base vector doesn't need to be a flat vector. It could be a dictionary-encoded vector. So it's worth to keep in mind: encodings are recursive!

In (3) note that nulls buffer argument: in Velox every type is implicitly nullable. However, since we cannot represent null values with STL types, the API takes an explicit list of positions that are null. In our case, since our data does not have null values we use `nullptr`.

The calls (1) and (2) are only for converting STL data structures to the right type, `BufferPtr` and `FlatVector`, respectively.

We can see the efficiency of this encoding by estimating how much memory a vector takes for an example of `10,000` with random values:

{% highlight cpp %}
std::vector<vector_size_t> indices;
for (int i = 0; i < N; i++) {
    int ri = randomInt(4);
    indices.push_back(ri);
}
auto d2 = makeDictionaryVector<std::string>(
    pool.get(),
    values,
    indices);
std::cout << d2->size() << "\n"; // 10000
std::cout << d2->estimateFlatSize() << "\n"; // 240000
std::cout << d2->retainedSize() << "\n"; // 49152
{% endhighlight %}

As we can see, it's about 5x smaller to store it using dictionary encoding. Dictionary encoding can also be used to represent a *subset* of the base data. We'll see it in practice in *Selectivity*.

**RLE.** Even if we're storing integers for low-cardinality columns, we're still storing `N * sizeof(vector_size_t)`. If the data in column repeats multiple times in a row for example `[true, true, true, false, false, true, true]` then it can be compactly represented as, conceptually, `[(true, 3), (false, 2), (true, 2)]`. This is know as *run length encoding* or RLE [1], however support to it in the codebase seems limited.

One problem is that for this encoding to be useful, the data has to have a very specific pattern and it's hard to know for a general purpose data processing system.

**Constant.** An easier to reason about case is when all the data in the column is the same, in which case we can use `ConstantVector`.

{% highlight cpp %}
template <typename T>
VectorPtr makeConstantVector(memory::MemoryPool* pool, T value, vector_size_t size) {
  auto base = makeFlatVector<T>(pool, {value});
  auto constant = BaseVector::wrapInConstant(
      size,
      /*index=*/ 0,     // (1) base's offset
      base
  );
  return constant;
}

auto c = makeConstantVector<std::string>(pool.get(), "hello", 1000);
std::cout << c->estimateFlatSize() << "\n"; // 16 (2)
std::cout << c->retainedSize() << "\n"; // 16
{% endhighlight %}

The code is easy enough to understand. The parameter `index` in `wrapInConstant` corresponds to the offset of where the constant element is in `base` (in case it's not a single-element vector). Note that we often work with `VeloxPtr` not specific implementations, but we can obtain the underlying encoding from it:

{% highlight cpp %}
VectorEncoding::mapSimpleToName(c); // VectorEncoding::Simple::CONSTANT
{% endhighlight %}

Other enums in this encoding include `FLAT` and `DICTIONARY`.

One downside with encoding is that now the *physical* layout of the data can be very different from the *logical* layout of the data. Most of the times Velox is able to abstract this away, but sometimes we need to keep them in mind. More to be said in *Manipulaton*.

## Complex Vectors

So now we know how to represent a single column's data either as a STL-like vector or a more efficient representation. We can now use complex vectors to represent tables, i.e. a group of columns. Velox has 3 complex types: array (`ArrayVector`), map (`MapVector`) and row (`RowVector`). The latter corresponds to named tuples or method-less `struct`s in C++.

The way to interpret them is not "Velox vector of std::vector", but rather "std::vector of Velox vectors". Or in other words it's not a column where the elements are lists, but a column where the elements are other columns. Maps and structs are similar to arrays, they're list of columns with extra metadata, differing only on which types they can represent, so we'll focus on the most general of them, `RowVector`.

We start by defining a utility to create row vectors:

{% highlight cpp %}
VectorPtr makeRowVector(
    memory::MemoryPool* pool,
    // (1)
    std::vector<std::pair<std::string, VectorPtr>> columns
) {
    VELOX_CHECK_GT(columns.size(), 0, "must be non-empty");

    names.reserve(columns.size());
    types.reserve(columns.size());
    children.reserve(columns.size());

    for (auto& [name, child] : columns) {
        types.push_back(child->type());
        names.push_back(std::move(name));
        children.push_back(std::move(child));
    }

    const vector_size_t numRows = children[0]->size();
    return std::make_shared<RowVector>(
        pool,
        ROW(std::move(names), std::move(types)), // (2)
        nullptr, // no row-level nulls
        numRows,
        std::move(children));
}
{% endhighlight %}

Like in a table, column order matters, so in (1) we take a vector or pairs (we could take 2 separate vectors like `ROW()` does, but then we need to enforce they're the same size).

Notice we take `VectorPtr` instead of `std::vector` as we did for our other utilities. There are two reasons: first, `VectorPtr` is untemplated. Accepting vectors of different template types would require some ugly metaprogramming. Second, we accept different encodings instead of assuming a flat vector. We'll see this in our next example.

The `RowVector` takes a plain list of columns, `children`, without the name information. This is passed via (2). This signals the intent: column names and types are metadata not part of the data. The utility `ROW()` return `RowTypePtr` and we can interpret this as the schema of the table.

We can represent the table from the beginning of this section using a row vector:

{% raw %}
```cpp
auto species = makeFlatVector<std::string>(
    pool,
    {
        "Axolotl",
        "Fennec Fox",
        "Aardvark",
        "Shoebill",
        "Leafy Seadragon",
        "Quokka",
    }
);

auto klass = makeDictionaryVector<std::string>(
    pool,
    {"Amphibian", "Mammal", "Bird", "Fish"},
    {0, 1, 1, 2, 3, 1}
);

auto animals = makeRowVector(
    pool,
    {{"Species", species}, {"Class", klass}}
);
```
{% endraw %}

A row vector can be dictionary-encoded too. For example:

{% highlight cpp %}
// 0 -> 1, 0 -> 2, 0 -> 5
std::vector<vector_size_t> indices {1, 2, 5};
auto indicesBuf = copyToBuffer(indices, pool);

VectorPtr animalsDict = BaseVector::wrapInDictionary(
    nullptr,
    indicesBuf,
    indices.size(),
    base);
{% endhighlight %}

Note that the index is with respect to the rows of the child columns, not to the children themselves. It's an important distinction because one mental model is to see the "rows" of a row vector as columns. Also note that only it's the top-level row vector that is dictionary encoded, not its children.

## Types

One of the key feature of Velox is that it carries type as metadata but at compile type it performs type erasure. This allows for generic compute engines to be built on top of `VectorPtr` while still preserving type information in runtime if needed.

When we do need the real type information we can use explicit casts via `->as<T>()`:

{% highlight cpp %}
auto* classes = animals->childAt(1)->as<SimpleVector<StringView>>();
{% endhighlight %}

The class `SimpleVector` is the parent of `FlatVector`, `DictionaryVector` and `ConstantVector`. This is so we don't have to know the encoding, just that it represents a column conceptually. If you try to cast a non-flat vector to `->as<FlatVector<T>>()` (or the syntax sugar `->asFlatVector<T>()`) it returns null.

To get a flat vector from a non-flat vector we can use the static `flattenVector()`:

{% highlight cpp %}
BaseVector::flattenVector(animals);
{% endhighlight %}

This flattens vectors recursively. So for our example, it's the children of `animals`, in this case the column `classes` that will be flattened (this operation is a no-op for already flat vectors). This also mutates the vector in place.

We can obtain the element type of a `VectorPtr` via `type()`. The type in Velox is a high-level entity (i.e. each type as a templated class associated with it). For example, a flat vector of `velox::StringView` would return

{% highlight cpp %}
vec->type();             // VARCHAR() (subclass of a TypePtr)
vec->type()->kind();     // TypeKind::VARCHAR
vec->type()->toString(); // "VARCHAR"
{% endhighlight %}

For our row vector `animals`, the last line would print something like `ROW<Species:VARCHAR, Class:VARCHAR>`.

## Selectivity

As a data processing engine, one of the main things it does is transform vectors. For me it's useful to think in terms of relational tables and SQL. So for example, if our table is called `animals` we could filter for mammal species:

{% highlight sql %}
SELECT species
FROM animals
WHERE class = 'Mammal'
{% endhighlight %}

Column selection is pretty efficient in Velox vectors because it's column oriented. Filtering is row-based, so one way to do this efficiently is to keep an external vector representing which rows are actuall valid, i.e. selected. This vector is called `SelectivityVector`. This avoids having to make copies whenever performing filters.

{% highlight cpp %}
auto* classes = animals->childAt(1)->as<SimpleVector<StringView>>();
// (1)
SelectivityVector selectedAnimals(animals->size());

for (vector_size_t i = 0; i < row->size(); ++i) {
    if (classes->valueAt(i) != StringView("Mammal")) {
        selectedAnimals.setValid(i, false);
    }
}
// (2)
selectedAnimals.updateBounds();
{% endhighlight %}

In (1) `selectedAnimals` acts as a [view](https://www.kuniga.me/blog/2025/01/25/vector-views-in-cpp.html) without touching the underlying vectors.

It's imperative to call `SelectivityVector.updateBounds()` before reading from it again. There are derived (cached) computation that are not computed on every `setValid()` call, for performance reasons.

Having an external representation does create difficulties though, when traversing the rows of the vector, we need to take into account which rows are selected, we can't do a simple "for-loop".

{% highlight cpp %}
auto* species = animals->childAt(0)->as<SimpleVector<StringView>>();
selectedAnimals.applyToSelected([&](vector_size_t i) {
    std::cout << animals->valueAt(i) << "\n";
});
{% endhighlight %}

We can "absorb" a selectivity vector into a vector via the dictionary encoding, by only keeping indices that are selected. There are two ways to do this: (A) we either dictionary encode each child of the row vector or (B) we dictionary encode the row vector itself.

There are some tradeoffs here. With (A) we need to traverse each column of a row vector and re-encode them as dictionary. (B) we do it once for the top vector but we have possibly nested encoding, because if one of the children of the row vector was dictionary encoded, we need to go through multiple hops to access a row on it.

We show an example of option (B):

{% highlight cpp %}
std::vector<vector_size_t> indices;
indices.reserve(selectedAnimals.countSelected());
// (1)
selectedAnimals.applyToSelected([&](vector_size_t i) { ridx[k++] = i; });
auto indicesBuf = copyToBuffer(indices, pool);

auto mammals = BaseVector::wrapInDictionary(
    nullptr, indicesBuf, mammals.countSelected(), animals);
{% endhighlight %}

We still need to do `applyToSelected()` once (1), but if you're a library owner and operate with selectivity vectors internally but want to hide this from users, this can be useful.

As far as I understand `SelectivityVector` is a relatively advanced optimization mechanism used internaly inside the Velox expression evaluator. Even if we could keep updating dictionary indices and not touch the base data, it wouldn't be as efficient as using the selectivty vector's bitmasks.

## Nullability

A very related concept to selectivity is nullability. We've already mentioned that types in Velox are nullable, and that when creating a Velox vector we can pass an explicit list of indices for which the entries are null.

We can also set entries in a vector to null, after the fact, via the `setNull(index, null=true)` API. The `index` here is the logical one, so even for a dictionary encoded vector it would do the right thing.

{% highlight cpp %}
auto* species = row->childAt(0)->as<SimpleVector<StringView>>();
species->setNull(2, true);

for (vector_size_t i = 0; i < species->size(); ++i) {
    if (table->isNullAt(i)) {
        std::cout << "  <null>\n";
    } else {
        std::cout << "  " << species->valueAt(i) << "\n";
    }
}
{% endhighlight %}

We might ask ourselves why nullability is a property of the vector but selectivity is not. The observation is that null is data. If you set an entry to null, you cannot later retrieve the "original" data, you overwrote the value with a new one.

Selection on the other hand is a reversible mask since it doesn't mutate the data. It's more like an optimization aid rather than part of the content.

## Operations

### Reads

As we've seen in *Nullability*, if not using selectivity vectors, accessing the `i`-th position of a vector can be done via `valueAt(i)`:

{% highlight cpp %}
auto* species = animals->childAt(0)->as<SimpleVector<StringView>>();
for (vector_size_t i = 0; i < species->size(); ++i) {
    std::cout << species->valueAt(i) << "\n";
}
{% endhighlight %}

Here `i` is the logical index, which means that if the vector is dictionary-encoded, it will resolve indirection of dictionary encodings properly. However depending on the nesting level of encoding and how often we access the data (in this example we do a "full scan"), it can be rather inefficient. In this case we can obtain a view that "flattens" the indices in one go. It's called `DecodedVector`:

{% highlight cpp %}
DecodedVector decoded(*animals->childAt(0));
for (vector_size_t i = 0; i < decoded.size(); ++i) {
    std::cout << decoded.valueAt<StringView>(i) << "\n";
}
{% endhighlight %}

If the vector is a row vector and we wish to access the `i`-th row of the underlying children we can do:

{% highlight cpp %}
DecodedVector rowDecoded(*animalsDict);
auto* animals = rowDecoded.base()->as<RowVector>();

DecodedVector species(*rows->childAt(0));
DecodedVector classes(*rows->childAt(1));

for (vector_size_t i = 0; i < table->size(); ++i) {
    auto baseRow = rowDecoded.index(i);
    std::cout << species.valueAt<StringView>(baseRow) << " | "
            << classes.valueAt<StringView>(baseRow) << "\n";
}
{% endhighlight %}

Here `rowDecoded.index` corresponds to the `indices` we used to create `animalsDict` in *Complex Vectors*.

### Writes

Mutating data is when the abstraction break a bit, because it's encoding dependent. For example, suppose you were to mutate the column `klass` by doing:

{% highlight cpp %}
klass->set(1, "Amoeba");
{% endhighlight %}

The value `"Amoeba"` is not in the base vector we passed when constructing the dictionary. We can mutate the dictionary, but we'd either mutate the indices or the underlying vector that is the base. For example, to change `Bird` to `Avian` on the base vector:

{% highlight cpp %}
auto base = dict->valueVector();
base->asFlatVector<StringView>()->set(2, StringView("Avian"));
{% endhighlight %}

Note here that we're assuming the base vector is a flat vector, but that's not necessarily true.

Next post on the series: [Velox Execution]({{blog}}/2026/06/04/velox-execution.html)


## Conclusion

I used Claude Code to help me with this post, by having it read the Velox codebase (yay open source!) and test assumptions by writing and running small C++ examples. I've been a user of Velox on my day-to-day work for a few years now but still had a lot of "a-ha" moments while researching for this post. I should really take the time to study things I work with more often.

I particularly liked the process of asking questions: why is selectivity external while nullability is internal to the vector? It led to a deeper understanding of the high level design. That's one of the main reasons I like writing posts, even if no one ever reads them: it forces me to be more curious.

I'm looking forward to covering other topics on Velox, in particular the execution engine and memory management.


## References

* [[1](https://vldb.org/pvldb/vol15/p3372-pedreira.pdf)] Velox: Meta's Unified Execution Engine, Pereira, Pedro et al.
* [[2](https://github.com/facebookincubator/velox)] Github - Velox
