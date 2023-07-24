---
layout: post
title: "Big Floats"
tags: [c++]
vanity: "2023-07-24-big-floats"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/floating.png" alt="Floating lillies on a lake impressionist painting" />
</figure>

At work, I recently spent quite some time scratching my head trying to make sense of a behavior that turned out to be due to large integers being converted to floats.

In this post we'll delve a bit into floating points, using C++ for the examples, then explain the problem with big floats.

<!--more-->

## Terminology and Assumptions

There are many ways we can approximate real numbers in a digital system. Floating-point numbers is one of the most common ways. The term *floating-point* refers to the fact that the decimal point is not fixed, or that the number of digits to the right of the decimal point is not fixed, so it can represent both `1.23` and `12.3` without having to pad with zeros.

In C++ the most common floating-point numerical types are `float` and `double`, which use 32-bits and 64-bits respectively. For simplicity, we'll focus on `float`s in this post since the ideas are largely the same for `double`s.

There are many ways in which a floating-point number can be represented, but we'll focus on a particular implementation, the IEEE 754 standard. The C++ standard doesn't dictate which format to use but we can check whether if a compiler utilizes IEEE 754 via this code snippet:

{% highlight c++ %}
#include <limits>

if (std::numeric_limits<float>::is_iec559) {
    std::cout << "This compiler uses IEEE-754" << std::endl;
}
{% endhighlight %}

## IEEE-754

### Mapping the Real Line

In general terms, the IEEE-754 standard divides the real line (i.e. $x \in \mathbb{R}$) into positives ($\gt 0$) and negatives ($\lt 0$). We'll consider 0 and numbers with very large absolute values as special cases.

It keeps a flag to determine whether the number is positive (false) or negative (true) and then it works with its absolute value. So for now, assume we have $x \in \mathbb{R}^{+}$.

The standard then splits this interval into $k$ sub-intervals of the form $[2^{i}, 2^{i + 1})$ (notation: the interval includes the left term but not the right term). Each sub-interval contains the same number $m$ of real values, equally spaced.

Since numbers in a given sub-interval are equidistant, we can easily compute the *gap* between two numbers in it. The interval length is $2^{i+1} - 2^{i} = 2^{i}$, so the gap is given by $2^{i} / m$. The smaller the $i$, the smaller the interval and the smaller the gap between numbers.

We'll cover this idea in more detail on the next section.

### Encoding Floats To Binary

The standard encodes a floating point number into bits with 3 parts: the **sign**, the **exponent** and the **significand**. For a 32-bit float point number we use 1 bit for the sign, 8 bits for the exponent and 23 bits for the offset.

**Sign.** The sign is the easiet part to determine. It corresponds to the flag mentioned in the previous section: If the float number is positive, the sign bit is 0, otherwise it's 1.

**Exponent.** The exponent is the "$i$" from the previous section and determines the sub-interval a number is mapped to. It's encoded to binary as follows: find the exponent of the largest power-of-two that is smaller or equal to the float number. For example, if we have $3.1415$ ($\pi$), the exponent we're looking for is $1$, since $2^1 \lt 3.14 \lt 2^2$. For $0.3678$ ($e^{-1}$) this would be $-2$ since $2^{-2} \lt 0.3678 \lt 2^{-1}$. A simpler way to define the exponent is $\lfloor \log_2 \abs{x} \rfloor$.

The maximum exponent supported is $2^7 - 1$ and the minimum is $-(2^7 - 2)$. Anything outside this range is considered infinity. We add $2^7 - 1 = 127$ to $i$ before converting to binary, so that the result is always positive.

Since the minimum exponent is $-(2^7 - 2)$ and we add $2^7 - 1$, the final result is always greater than 0. The exponent `00000000` is reserved for representing `0.0f` and the so-called *subnormals* (which we'll cover in *Extremes*).

Coversely since the maximum exponent is $2^7 - 1$, after adding $2^7 - 1$ we get at most $2^8 - 2$, that is `11111110`. The exponent $2^8 - 1$, `11111111`, is reserved for representing inifity and NaN (which we'll also cover in *Extremes*).

We then write the result as a 8-bit number. In our examples, for $3.1415$, we'd get $1 + 127$ or `10000000` and for $0.3678$, we'd write $-2 + 127$ or `01111101`.

**Significand.** Recall from the previous section that the exponent $i$ defines the sub-interval $[2^{i}, 2^{i + 1})$ and that within that interval we have $m$ numbers equally spaced from each other. Since we have 23 bits for the significand we can map $m = 2^{23}$ numbers to any given sub-interval and the gap between each number would be $2^{i} / m = 2^{i - 23}$.

Note that we don't store the number itself, but its relative position $d$ within the sub-interval, because given the start of the interval and the fixed gap between elements, we can recover the number (with some loss of information, which we'll discuss in the section *Precision*). Maybe instead of significant we could call this *relative position* or *index*.

To map a number to an interval, we first determine its exponent $i$ and then compute its offset in relation to the beginning of the interval, by subtracing $2^{i}$. For $3.1415$ we do $3.1415 - 2^{1}$ to get $1.1415$ and for $0.3678$ we do $0.3678 - 2^{-2} = 0.1178$.

Now we need to "normalize" so that it maps to an integer on the interval $[0, 2^{23} - 1]$. We do this by dividing the offset by the gap $2^{i - 23}$. For $3.1415$ we do $1.1415 / 2^{1 - 23} = 1.1415 \cdot 2^{22} = 4787798.016$. We round to the closest integer, $4787798$, and encode this as the 23-bit number `10010010000111001010110`. For $0.3678$ we do $0.1178 / 2^{-2 - 23} = 0.1178 \cdot 2^{25} = 3952712.0896$, which we round to $3952712$ and encode to `01111000101000001001000`.

So now we have the algorithm to encode a float number to a binary.

### Binary To Float

The conversion back to float is just the reverse process. Let $s$ be $-1$ if the sign bit is `1` or $1$ if it's $0$. Let $e$ be the exponent in decimal and let $d$ be the significand in decimal. We have $i = e - 127$.

$$s \cdot d \cdot 2^{i - 23} + 2^{i}$$

Let's try recovering $3.1415$ first. We have $s = 1$, $d = 4787798$ and $e = 128$, so $i = 1$. Plugging those in the formula above we have:

$$1 \cdot 4787798 / 2^{22} + 2^{1} = 3.1414999961853027$$

Quite close! Let's do $0.3678$ now. We have $s = 1$, $d = 3952712$ and $e = 125$, so $i = -2$:

$$1 \cdot 3952712 / 2^{25} + 2^{-2} = 0.3677999973297119$$

Not bad either.

### Extremes

**Maximum.** What is the maximum absolute number that a float can represent? The maximum non-reserved exponent value is $e = 2^8 - 2$, $i = e - (2^7 - 1) = 2^7 - 1$ so the corresponding interval is $(2^{2^7 - 1}, 2^{2^7})$. We can than set the significand to all 1s or $2^23 - 1$. Plugging in the formula with $s = 1$ (since absolute):

$$(2^{23} - 1) \cdot 2^{127 - 23} + 2^{127} = 2^{128} - 2^{104}$$

Which is about `340282346638528859811704183484516925440` or $3.4028235 \times 10^{38}$. Worth noting that this looks very similar to $2^{128}$, `340282366920938463463374607431768211456`, especially when written in scientific notation, so I mistakenly thought was the maximum float number before writing this post.

**Minimum.** How about the minimum absolute number that a float can represent? The minimum non-reserved exponent value is $e = 1$, so $i = e - (2^7 - 1) = 2 - 2^7$ and the corresponding interval is $[2^{2 - 2^7}, 2^{3 - 2^7})$. We can then set the significand to all 0s. Plugging in the formula, since the significand is 0, we're only left with the last summand:

$$2^{2 - 2^7} = 2^{-126} = 1.1754943508222875 \cdot 10^{-38}$$

This is what's returned by the STL when we do `std::numeric_limits<float>::min()`. However this is not the actual minimum possible representation as we'll see next in *Exceptions > Subnormals*.

### Exceptions

As we mentioned, the exponent `11111111` is reserved for infinity and NaN. More precisely, if the exponent binary is `11111111`, then it represents infinity if the significand bits are all `0`, otherwise it represents NaN.

**Subnormals.** In theory, any number smaller than $1.1754 \cdot 10^{-38}$ gets truncated to $0$, which causes problems because once you have a zero, you lose all information, so this is called *underflow*.

To reduce this issue, the standard also defines a special sub-interval, for when the exponent is `00000000`, that is $e = 0$ or $i = -127$. If we were to follow the pattern, this would represent the sub-interval $[2^{-127}, 2^{-126})$, but we need to include the 0, so instead it represents $[0, 2^{-126})$.

For this sub-interval the size is $2^{-126}$ and the gap is $2^{-126} / 2^{23} = 2^{-149}$. The smallest number greater than zero that can be represented is when the significand is $1$ which in this case coincides with the gap size $2^{-149} = 1.401298464324817 \cdot 10^{-45}$. This is what's returned by `std::numeric_limits<float>::denorm_min()`.

Subnormals are usually not handled efficiently by hardware and can lead to low-performance code [2].

## Floats in C++

Now that we understand the theory, we can write some C++ code to put it into practice.

### Endianness

Before we do that though, we need to determine whether our system is [little-endian or big-endian](https://en.wikipedia.org/wiki/Endianness). We can use this code snippet:

{% highlight c++ %}
bool is_little_endian() {
    int num = 1;
    return *(char *) &num;
}
{% endhighlight %}

To see what's happening, let's consider how the 32-bit signed integer 1 is represented in memory in a big-endian and little endian system:

{% highlight c++ %}
// address ->
00000000000000000000000000000001 // big endian
10000000000000000000000000000000 // little endian
{% endhighlight %}

Big-endian systems store the most significant bit in lower memory addresses, while little-endian systems do the opposite. We take address of `num` variable which points to the lowest memory position:

{% highlight c++ %}
// address ->
00000000000000000000000000000001 // big endian
10000000000000000000000000000000 // little endian
^ &num
{% endhighlight %}

And then cast it to a pointer to `char`, pretending we're pointing to a 8-bit variable, instead of a 32-bit one, so it now looks like:

{% highlight c++ %}
// address ->
00000000 // big endian
10000000 // little endian
^ (char*) &num
{% endhighlight %}

So if dereference it, we'll get 0 in a big endian system and 1 in a little endian system, then casting to boolean will tell us the endianness. Intel's x86 and Apple's M1 chips both use little endian addressing, while PowerPC chips use big endian. I only work with the first two, so let's focus on little endian on the rest of this post.

### Bit representation

We're now ready to represent the bits of a `float` in C++. First we define a struct `FloatBits` using bitsets:

{% highlight c++ %}
struct FloatBits {
    unsigned int d : 23;
    unsigned int e : 8;
    unsigned int s : 1;
};
{% endhighlight %}

This splits a 32-bit unsigned int into 3 sets, corresponding to the digits (first 23 bits), the exponent (next 8 bits) and the sign (last 1 bit). The fields order is important and depends on the endianess of the memory address system. Here we assume little-endian.

We can define a union struct to represent either `float` or `FloatBits`.

{% highlight c++ %}
union FloatIntUnion {
    float f;
    FloatBits b;
};
{% endhighlight %}

Both have 32-bits so they can be reprented using the same amount of memory, which means we can write to the union using `float` but read using `FloatIntUnion` which implicitly maps a float to a `FloatBits` with the same internal bit representation!

In this case it lets us inspect those bits:

{% highlight c++ %}
#include <iostream>
#include <bitset>
#include <iomanip>

void print_float_bits(float f) {
    FloatIntUnion u;
    u.f = f; // write as float
    FloatBits b = u.b; // read as bitset
    std::bitset<1> sign(b.s);
    std::bitset<8> exp(b.e);
    std::bitset<23> digits(b.d);
    std::cout << std::setw(13) << std::setfill(' ')  << f;
    std::cout << ":  " << sign << "|" << exp << "|" << digits << '\n';
}
{% endhighlight %}

We can try some of the numbers we discussed before: `0`, `-0`, `3.1415`, `0.3678`, `std::numeric_limits<float>::max()`, `std::numeric_limits<float>::min()` and `std::numeric_limits<float>::denorm_min()`:

{% highlight text %}
            0:  0|00000000|00000000000000000000000
           -0:  1|00000000|00000000000000000000000
       3.1415:  0|10000000|10010010000111001010110
       0.3678:  0|01111101|01111000101000001001000
  1.17549e-38:  0|00000001|00000000000000000000000
  3.40282e+38:  0|11111110|11111111111111111111111
   1.4013e-45:  0|00000000|00000000000000000000001
{% endhighlight %}

## Large Integers

We know now that `floats` and `int32` use 32-bits to represent numbers. There are some numbers such as decimals that can be represented in float, that of course cannot be represented in `int32`. Thus it must be that there are some numbers `int32` can represent that `float` can't.

One question we might ask: what is the range of integers that can be represented losslessly with floats? We can run a simple experiment to find out:

{% highlight cpp %}
int n = 0;
while (n == (int)((float)n)) {
    n += 1;
}
std::cout << n << "\n";
{% endhighlight %}

It prints `16777217` which is $2^{24} + 1$, meaning we can represent the integers from $0$ to $2^{24}$ without loss of information.

To see why, notice that for $n \lt 2^{24}$, its exponent will be $i \le 23$. Consider first the the case where $i = 23$ with the corresponding sub-range $[2^{23}, 2^{24})$. The gap is exactly $1$ (i.e. $2^{i - 23}$). So the significand is $n - 2^{23}$. Note that there are no divisions, and hence no truncation, meaning that integers are represented perfectly.

For $i \lt 23$, the gap is $\le 2^{-1}$, so an integer is stored as a decimal number in the range $(n - 0.5, n + 0.5)$ and when it gets rounded back to integer, it goes back to $n$.

For $i = 24$, there are truncation, but powers of 2 are represented losslessly, because they're disible by the gap size, so $n = 2^{24}$ is also preserved.

Let's estimate the magnitude of gap for powers of $10$. He have that the gap is $2^{i - 23}$. Our number is given as $n = 10^k$. which we can convert to a power of 2 as $10^k = 2^{(\log_{2} 10) k}$, so the gap is $2^{(\log_{2} 10) k - 23}$.

To compute the number of digits in this number we apply $log_{10}$, giving us:

$$log_{10}(2) * (\log_{2} 10) k - 23) = k - 23 log_{10}(2) \approx k - 7$$

So an integer with $k$ digits will have a gap of about $k - 7$. For example, consider the number $1073631824$ with $10$ digits. We'd expect some error around $3$ digits, so giving a safe margin, we should expect this number to be within $1073631824 \pm 1000$.

Putting it more simply, only the first 6 most significant digits of a large integer are guaranteed to be preserved.

### Mystery Solved

The weird behavior that triggered diving deeper into floats was that we were seeing some corruption on the data when running through a system that was supposed to be a no-op.

We had some data like:

{% highlight text %}
| category | timestamp  |
| -------- | ---------- |
| foo      | 1690058495 |
| bar      | 1690058496 |
{% endhighlight %}

But the system was spitting out

{% highlight text %}
| category | timestamp  |
| -------- | ---------- |
| foo      | 1690058496 |
| bar      | 1690058496 |
{% endhighlight %}

We thought where was some race condition or memory corruption causing the timestamp of the first row to be overwritten, but turned out `timestamp` was declared as `float` and it happened that `1690058495` gets rounded to `1690058496` which happened to be the exact timestamp of the other row.

## Conclusion

The conclusion is that storing unix timestamps as 32-bit floats is a bad idea. Storing them in 32-bit integers is also [not a great idea](https://en.wikipedia.org/wiki/Year_2038_problem).

During my coding competition days, we're always on alert to never compare float/double on equality, always adding some margin $\epsilon = 10^{-6}$ (e.g. `abs(x - y) < eps` vs. `x == y`). However, I don't recall ever running into precision issues with *large* integers.

I now realize why: we're also always instructed to use 64-bit floating point numbers (double), which has 52-bits for the significand, so using the rationale we did for floats, we can see it can represent $0$ to $2^{53}$ losslessly, more than enough for the 32-bit integers.

I love writing posts about basic subjects I think I know about because I always end up learning something new. I don't recall if I ever learned the details of float point numbers, but I'm pretty sure I never heard of subnormals before.

## References

* [[1](https://wizardzines.com/zines/integers-floats/)] How Integers and Floats Work - Wizard Zines, Julia Evans.
* [[2](https://stackoverflow.com/questions/9314534/why-does-changing-0-1f-to-0-slow-down-performance-by-10x)] StackOverflow - Why does changing 0.1f to 0 slow down performance by 10x?
