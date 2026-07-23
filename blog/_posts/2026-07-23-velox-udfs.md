---
layout: post
title: "Velox: UDFs"
tags: [distributed systems, databases]
excerpt_separator: <!--more-->
vanity: "2026-07-23-velox-udfs"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_shared}}/velox-logo.svg" alt="Velox Logo" />
</figure>


[Velox](https://github.com/facebookincubator/velox) is an open source C++ library by Meta that can be used to perform computation common to distributed engines like Presto.

Its offerings include columnar operations, a rich type system, an expression parser and a smart resource management such as memory [1]. In this series of posts we'll go over different components of Velox.

In this post we'll study user defined functions, UDFs, including the aggregate ones, UDAFs.

<!--more-->

Previous posts on the series:

* [Vectors]({{blog}}/2026/06/01/velox-vectors.html)
* [A Simple Application]({{blog}}/2026/06/18/velox-application.html)

We'll start by providing a high-level example of a custom UDF and then dive deeper in how it fits in the overall Velox machinery. Then we'll cover UDAFs and its more complex lifecycle.


## Implementation

One of the simplest UDFs we can define is a function that multiplies a number by 2:

{% highlight c++ %}
// (1)
template <typename TExec>
struct TimesTwoFunction {
  // (2)
  VELOX_DEFINE_FUNCTION_TYPES(TExec);

  void call(int64_t& result, const int64_t& value) {
    result = value * 2;
  }
};
{% endhighlight %}

The overall idea is to implement a [functor](https://www.kuniga.me/blog/2012/09/09/functors.html), a C++ class/struct that implement a method corresponding to a function call (`call()` in this case).

The template in (1) is implementing *Policy-based design*, a mechanism that is used for dependency injection. In production, it mostly binds to `velox::exec::VectorExec`, but this allows tests and benchmarks to "mock" it, so we shall not pay much attention to it.

The macro in (2) is syntax sugar that is also pegged on the template. One of the things it defines is this type alias:

{% highlight c++ %}
template <typename TArgs>
using arg_type = typename TExec::template resolver<TArgs>::in_type;
...
template <typename TArgs>
using out_type = typename TExec::template resolver<TArgs>::out_type;
{% endhighlight %}

So we could write our signature as:

{% highlight c++ %}
void call(out_type<int64_t>& result, const arg_type<int64_t>& value);
{% endhighlight %}

But for primitives we have `arg_type<int64_t>` becomes `int64_t` and `out_type<int64_t>` becomes `int64_t`. In this example it wouldn't make a difference to use `arg_type<int64_t>` instead of `int64_t`. But for non-primitives we might change: `arg_type<Varchar>` becomes `StringView` and `out_type<Varchar>` becomes `StringWriter`.


### Nullability

Recall that Velox types are always nullable. For return type for example, `call()` can return false/true to indicate if the return value is null.

{% highlight c++ %}
template <typename TExec>
struct SafeDivideFunction {
  VELOX_DEFINE_FUNCTION_TYPES(TExec);

  // (1)
  bool call(double& result, const double& num, const double& den) {
    if (den == 0) {
      return false; // (2) NULL
    }
    result = num / den;
    return true;
  }
};
{% endhighlight %}

If we implement the `bool call()` instead of `void call()` and return false, then we're indicating `result` is null and will be ignored.

For the arguments, Velox **short-circuits** it: if any of the arguments is null, it doesn't invoke `call()` and immediately assumes `result` is null. To prevent this behavior, one can implement the `callNullable()` overload:

{% highlight c++ %}
template <typename TExec>
struct ArrayMinOrDefaultFunction {
  VELOX_DEFINE_FUNCTION_TYPES(TExec);

  // (1)
  bool callNullable(int64_t& result, const arg_type<Array<int64_t>>* arr) {
    if (arr == nullptr) {
      result = 0;
      return true;
    }

    bool found = false;
    // (2)
    for (const auto& element : arr->skipNulls()) {
      result = found ? std::min(result, element) : element;
      found = true;
    }
    return found;
  }
};
{% endhighlight %}

The major difference is that the input argument is a raw pointer now, which can represent null when it is `nullptr` (1).

A nested type might be null, for example in `array<int>`, the element type `int` can be null. The resolved array type exposes a method `skipNulls()` (2) that filters out null values.

The same is true for `call()` overloads: even though Velox short-circuits top-level null values, it does not do it for nested null types. To have the short-circuit behavior for the whole nested structure, we can use the overload `callNullFree()`.

## Registration

The registration is straightforward, we just need to call:

{% highlight c++ %}
void registerUdfs() {
  registerFunction<TimesTwoFunction, int64_t, int64_t>({"times2"});
}
{% endhighlight %}

The string `times2` is the identifier which we'd call inside an expression. We also need to call this once:

{% highlight c++ %}
void registerUdfs() {
  ...
  parse::registerTypeResolver();
}
{% endhighlight %}

This explicit step is a bit confusing, but it's there to avoid circular dependencies. We only really need this when parsing expressions from strings, which is typically not needed in practice.

## Evaluation

To test the example, we can add some helpers. First to execute an expression against vector data by running a [task](https://www.kuniga.me/blog/2026/06/18/velox-application.html):

{% highlight c++ %}
RowVectorPtr evaluate(
    const RowVectorPtr& input,
    const std::string& expr) {
  auto plan = exec::test::PlanBuilder()
                  .values({input})
                  .project({expr})
                  .planFragment();
  auto task = makeSerialTask("udf-demo", plan);
  return task->next();
}
{% endhighlight %}

We can create some hard-coded [Velox vector](https://www.kuniga.me/blog/2026/06/01/velox-vectors.html):

{% highlight c++ %}
auto pool = memory::memoryManager()->addLeafPool();
test::VectorMaker maker(pool.get());
auto c0 = maker.flatVectorNullable<int64_t>({1, std::nullopt, -3, 4});
auto input = maker.rowVector({"c0"}, {c0});

registerUdfs();

evaluate(
    input,
    "times2(c0) AS doubled"
);
{% endhighlight %}

## Determinism

UDFs in Velox are deterministic by default. This allows some optimizations when evaluating expressions. In the example above, if `c0` were to have constant encoding (see [Velox Vectors]({{blog}}/2026/06/01/velox-vectors.html)), we would just need to evaluate the expression `times2(c0)` once.

It's possible to mark a function as non-deterministic:

{% highlight c++ %}
template <typename TExec>
struct AddNoiseFunction {
  VELOX_DEFINE_FUNCTION_TYPES(TExec);

  // Marks UDF as non-deterministic.
  static constexpr bool is_deterministic = false;

  void call(int64_t& result, const int64_t& value) {
    result = value + folly::Random::rand32(100);
  }
};

void registerUdfs() {
  ...
  registerFunction<AddNoiseFunction, int64_t, int64_t>({"add_noise"});
  ...
}
{% endhighlight %}

A non-deterministic UDF causes the whole expression to be non-deterministic, which then makes the evaluation not leverage the encoding. In the following example, we would have to run the expression for each row:

{% highlight c++ %}
evaluate(input, "times2(c0) + add_noise(c0)")
{% endhighlight %}

## Vector Function

So far we've only seen functions that take a single "row" from a vector, which for most cases is the right thing to do and keeps the code simple. Other times it's preferable to process the functions in batch.

In this case we can define a function that inherits from `exec::VectorFunction`. Here's an example that implements a function that returns the values of a map. By processing the whole vector at once, it bypasses the decoding that needs to happen which incurs copies.

{% highlight c++ %}
class MapValuesFunction : public exec::VectorFunction {
 public:
  void apply(
      const SelectivityVector& rows,
      std::vector<VectorPtr>& args,
      const TypePtr& /*outputType*/,
      exec::EvalCtx& context,
      VectorPtr& result) const override {
    auto* mapVector = args[0]->as<MapVector>();
    VELOX_CHECK_NOT_NULL(mapVector, "map_values expects a flat MapVector");

    auto mapValues = mapVector->mapValues();
    auto localResult = std::make_shared<ArrayVector>(
        context.pool(),
        ARRAY(mapValues->type()),
        mapVector->nulls(),
        rows.end(),
        mapVector->offsets(),
        mapVector->sizes(),
        mapValues,
        mapVector->getNullCount());

    context.moveOrCopyResult(localResult, rows, result);
  }
};
{% endhighlight %}

Registering Vector functions needs a special builder because the argument now is an untyped vector.

{% highlight c++ %}
exec::registerVectorFunction(
    "map_values",
    {exec::FunctionSignatureBuilder()
          .typeVariable("K")
          .typeVariable("V")
          .returnType("array(V)")
          .argumentType("map(K,V)")
          .build()},
    std::make_unique<MapValuesFunction>());
{% endhighlight %}

## Conclusion

In this post we explored how to extend Velox by adding new UDFs. We covered a few properties of them such as nullability and determinism and saw the difference between "scalar" and "vector" UDFs.

## Related Posts

As we mentioned Velox UDFs are [functors](https://www.kuniga.me/blog/2012/09/09/functors.html). We covered [Function Objects in C++]({{blog}}/2023/06/01/functions-in-cpp.html) more broadly too.

The policy-based design is similar in spirit to [Customization Point Objects in C++]({{blog}}/2025/09/10/cpo-in-cpp.html) in that they're compile time (via template) dispatches, instead of [Vtables]({{blog}}/2012/08/28/the-visitor-pattern-and-vtables-in-c++.html).

We already associated Google's [FlumeJava]({{blog}}/2022/05/18/flumejava.html) to Velox in [A Simple Application]({{blog}}/2026/06/18/velox-application.html). It also supports extensions via UDFs.

## References

* [[1](https://vldb.org/pvldb/vol15/p3372-pedreira.pdf)] Velox: Meta’s Unified Execution Engine, Pereira, Pedro et al.
