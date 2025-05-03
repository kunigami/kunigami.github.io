---
layout: post
title: "Shared Libraries"
tags: [compilers, c++, operating systems]
excerpt_separator: <!--more-->
vanity: "2025-04-25-shared-libraries"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/thumbnail.png" alt="Image of an library, AI-generated using anime style" />
</figure>


[Previously]({{blog}}/2025/04/12/elf.html) we learned about the ELF file used by Linux operating systems. There, we briefly covered the dynamic linker which is responsible for loading shared libraries and that they're also encoded as ELF files.

In this post we'll study shared libraries in more details. Knowing about ELF is not strictly necessary for this, but I would recommended reading the [ELF post]({{blog}}/2025/04/12/elf.html) to fully follow some of the discussions.

<!--more-->

Shared libraries can be thought as code that was previously compiled separately. It can be either because system code that we don't want included by default (e.g. math) or a third party library (e.g. boost).

They also help with compile time: since the code from the library is already compiled into object code, we don't need to compile it again when including in our binary. We just need to *link* it. There are two ways of linking a shared library with our binary.

## Static vs Dynamic Linking

**Static linking** is when the shared library is linked during the overall compilation process. It's no different from other object code. For example, if we have some code like:

{% highlight cpp %}
// main.cpp
#include "my_file_1.h"
#include "my_file_2.h"

int main() {
    f();
    g();
    return 0;
}
{% endhighlight %}

And a set of `.cpp`/`.h` as:

{% highlight cpp %}
// my_file_1.h
void f();

// my_file_1.cpp
# include <stdio.h>
void f() {
    printf("hello\n");
}

// my_file_2.h
void g();

// my_file_2.cpp
# include <stdio.h>
void g() {
    printf("world\n");
}
{% endhighlight %}

We can compile it as

{% highlight text %}
clang main.cpp my_file_1.cpp my_file_2.cpp
{% endhighlight %}

In the process, `clang` will compile each `.cpp` into a corresponding object code `.o`. Then the static linker will combine the object files into one, the final binary (see *Figure 1*).

<figure class="center_children">
  <img src="{{resources_path}}/normal-compilation.svg" alt="See caption." width="450" />
  <figcaption>Figure 1: Compilation in which all `.cpp` files are provided. Blue boxes indicate the source code. Green boxes are ELF files.</figcaption>
</figure>

A similar process happens with a statically linked library: we package multiple object code files into a `.a` archive. When we do static linking, the linker will unarchive those `.o` files and link them as if they were compiled just now.

We can actually do this with our example above. First we compile `my_file.cpp` without running the linker:

{% highlight text %}
clang -c my_file_1.cpp my_file_2.cpp
{% endhighlight %}

This produces `my_file_1.o` and `my_file_2.o`. We can package it into a `.a` archive called `libmy.a`:

{% highlight text %}
arc rcs libmy.a my_file_1.o my_file_2.o
{% endhighlight %}

Linux systems use the convention that shared libraries are prefixed with `lib`, but when referring to it to the compiler/driver we can omit it. For example, we can statically link our shared library with `main.cpp` via:

{% highlight text %}
$ clang main.cpp -L. -lmy
$ a./out
hello
world
{% endhighlight %}

The `L.` is telling the linker to search for libraries in the current directory (`.`). The `-lmy` is telling the linker to link a library called `libmy.a`.

<figure class="center_children">
  <img src="{{resources_path}}/static-linking.svg" alt="See caption." width="500" />
  <figcaption>Figure 2: Compilation in which dependencies are specificed via a statically linked shared library, which is just a wrapper around multiple `.o` files.</figcaption>
</figure>

**Dynamic linking** happens at runtime. We can think of it as a deferred or lazy linking. When compiling with dynamic linking, the static linker won't include the code from the library in the binary. It will just note down which symbols are to be resolved at runtime (see *Figure 3*).

<figure class="center_children">
  <img src="{{resources_path}}/dynamic-linking-compile.svg" alt="See caption." max-width="400" />
  <figcaption>Figure 3: Compilation in which dependencies are specificed via a dynamically linked shared library. The object code are generated differently from the static case. Nothing is added to the main binary at this point</figcaption>
</figure>


During program startup, as we saw in our last post [1], the dynamic linker, or interpreter, will do the actualy linking. We can do dynamic linking with our previous example. First we compile it into object code:

{% highlight text %}
clang -fPIC -c my_file_1.cpp my_file_2.cpp
{% endhighlight %}


The main difference with static linking is the flag `-fPIC`. This tells the compiler to generate the object code as *Position-Independent Code*. Recall from our ELF post [1] that the ELF file for the binary can specify the exact virtual memory address where code and data are to be loaded. But because dynamic libraries can be linked against any binary, we don't know a proper address to use upfront, so a position-independent code lets the dynamic linker decide.

We can then generate the shared library file:

{% highlight text %}
clang -shared -o libmy.so my_file_1.o my_file_2.o
{% endhighlight %}

This combines multiple ELF files (`my_file_1.o` and `my_file_2.o`) into another one, `libmy.so`. Note: `.so` stands for **s**hared **o**bject, MacOS uses `.dylib` which stands for **dy**namic **lib**rary. The corresponding ELF file has the type  `ET_DYN` and contains sections related to dynamic linking such as `.dynsym`, `.dynamic`, `.got`, and `.plt`, which we'll explore later.

<figure class="center_children">
  <img src="{{resources_path}}/dynamic-library.svg" alt="See caption." />
  <figcaption>Figure 4: Object code gets compiled into a single ELF file, corresponding to a shared library.</figcaption>
</figure>

Finally, we can compile the main binary with dynamic linking:

{% highlight text %}
$ LD_LIBRARY_PATH=. clang main.cpp -L. -lmy
$ LD_LIBRARY_PATH=. ./a.out
hello
world
{% endhighlight %}

Note: The environment variable `LD_LIBRARY_PATH` is the list of directories the dynamic linker looks for when searching for shared libraries. We need to tell it to also look in the current directory.

<figure class="center_children">
  <img src="{{resources_path}}/dynamic-linking-runtime.svg" alt="See caption." max-width="400" />
  <figcaption>Figure 5: Execution of a binary with dynamic linking. The dynamic linker will load the dependencies at the start up of the program.</figcaption>
</figure>

If we compile with dynamic linking, it won't include the code from `my_file.cpp` into the compiled binary, but will add metadata for the dynamic linker to know what to link. This also tells the compiler which symbols are ok to not have definitions for because they will be provided during runtime.

To exemplify the fact that the `a.out` binary does not have the code from `my_file.cpp`, we can actually change the code of the latter to print "dynamic":

{% highlight cpp %}
// my_file_1.cpp
# include <stdio.h>
void f() {
    printf("dynamic\n");
}
{% endhighlight %}

We then recompile the dynamic library:

{% highlight text %}
clang -fPIC -c my_file_1.cpp my_file_2.cpp
clang -shared -o libmy.so my_file_1.o my_file_2.o
{% endhighlight %}

and re-run our binary *without re-compiling* it:

{% highlight text %}
$ LD_LIBRARY_PATH=. ./a.out
dynamic
world
{% endhighlight %}

Some advantages of dynamic linking over static ones:

* The binary is smaller because it does not include the dependencies code.
* The kernel can share the code from the shared library across processes by loading it into memory once and mapping virtual addresses from different processes to the same physical one.
* You can update the library without recompiling all the dependents.

Some downsides:

* Overhead in program start-up to perform the dynamic linking.
* Dependencies must be properly installed and paths setup correctly when running a binary with dynamic linking.
* If a back-incompatible change is made to the shared library, it will crash during runtime.

For the rest of this post we'll assume dynamic linking. The shared libraries that are dynamically linked are also called *Dynamic Shared Objects* or DSOs.

## Recursive Dependencies

What if our DSO depends on other DSOs? How do we encode this dependency? For example, let's suppose our `my_file_2.cpp` requires some other external dependency, say `my_file_3.h`:

{% highlight cpp %}
// my_file_2.cpp (MODIFIED)
# include "my_file_3.h"
void f() {
    h();
}

// my_file_3.h
void h();

// my_file_3.cpp
# include <stdio.h>
void h() {
    printf("indirect\n");
}
{% endhighlight %}

And suppose we compile them into separate DSOs (`libmy.so` and `libmy2.so`):

{% highlight text %}
clang -fPIC -c my_file_1.cpp my_file_2.cpp my_file_3.cpp
clang -shared -o libmy.so my_file_1.o my_file_2.o

# my_file_3 is compiled into its own library
clang -shared -o libmy2.so my_file_3.o
{% endhighlight %}

When we try to compile `main.cpp` as before:

{% highlight text %}
$ LD_LIBRARY_PATH=. clang main.cpp -L. -lmy
/usr/bin/ld: ./libmy.so: undefined reference to `h()'
{% endhighlight %}

We need to provide the dependencies of `libmy` as well, not just of `main.cpp`:

{% highlight text %}
$ LD_LIBRARY_PATH=. clang main.cpp -L. -lmy -lmy2
{% endhighlight %}

Ideally `libmy` should declare its own dependencies so we don't need to know about them ourselves. We can do this via:

{% highlight text %}
# compile libmy2 before hand:
clang -shared -o libmy2.so my_file_3.o

# link libmy with libmy2
clang -shared -o libmy.so my_file_1.o my_file_2.o -L. -lmy2
{% endhighlight %}

Now we can compile the binary as before, only providing direct dependencies:

{% highlight text %}
$ LD_LIBRARY_PATH=. clang main.cpp -L. -lmy
{% endhighlight %}

## Dynamic Linking Process

As we discussed in the [ELF post]({{blog}}/2025/04/12/elf.html), the dynamic linker is a binary that is loaded in memory before our main binary starts and its main function is to load the DSO our main binary depends on. We discuss a few steps the linker perform both before yielding control to the main binary and afterwards.

It first loads the dependencies (DSOs) in memory. Depending on whether symbol lookup is configured to be done eagerly or lazily, the dynamic linker will perform it before the binary starts or on demand, when the program is run. We'll assume lazy evaluation.

The symbol lookup has two parts to it: from the main binary perspective and from the perspective of each of the DSOs. Let's go over each steps in detail.

### Loading Dependencies

It knows which libraries to load based on the `.dynamic` section of our binary, which can be obtained via:

{% highlight text %}
$ readelf -d ./a.out | grep NEEDED
 0x0000000000000001 (NEEDED)             Shared library: [libmy.so]
 0x0000000000000001 (NEEDED)             Shared library: [libstdc++.so.6]
 0x0000000000000001 (NEEDED)             Shared library: [libm.so.6]
 0x0000000000000001 (NEEDED)             Shared library: [libgcc_s.so.1]
 0x0000000000000001 (NEEDED)             Shared library: [libc.so.6]
{% endhighlight %}

Here we can see our custom library `libmy.so` and `libstdc++` which implements the STL. It also includes `libm.so` (for floating point functions), `libgcc_s.so.1` (for stack unwinding, used by exception handling) and finally `libc.so.6` (for functions like `printf()`).

The linker will go over these dependencies in this order and process them recursively, in breadth-first search fashion. We can check the dependencies of our DSO:

{% highlight text %}
readelf -d libmy.so | grep NEEDED
 0x0000000000000001 (NEEDED)             Shared library: [libmy2.so]
 0x0000000000000001 (NEEDED)             Shared library: [libstdc++.so.6]
 0x0000000000000001 (NEEDED)             Shared library: [libm.so.6]
 0x0000000000000001 (NEEDED)             Shared library: [libgcc_s.so.1]
 0x0000000000000001 (NEEDED)             Shared library: [libc.so.6]
{% endhighlight %}

We can see our other DSO there `libmy2.so` and the other same dependencies. Of course it will not re-load a shared dependency like `libm.so.6` multiple times, only the first one.

We can check all recursive dependencies of a binary via `ldd`:

{% highlight text %}
$ LD_LIBRARY_PATH=. ldd ./a.out
{% endhighlight %}

Which also includes the path of the library the dynamic linker found (this is very useful when there are multiple possible copies of the same library available in different directories).

### Symbol Lookup: Main Binary

When the binary is loaded into memory by the kernel, it also loads sections used by the dynamic linker: `rel.plt`, `.got`, `.got.plt` and `.plt`. The GOT stands for *Global Offset Table* and PLT for *Procedure Linkage Table*. `.got` is used for global varibles and `.got.plt` is used for function calls.

We can think the GOT as a memoization for the symbol lookup. Once a symbol lookup is performed, it's added to the GOT so it doesn't need to be done again. The `.rel.plt` table lists the symbols that are to be looked up and also the index of the GOT where they should be written to.

The PLT serves as a indirection layer. The `.plt` section is loaded into a read-only segment of memory and we can inspect it via:

{% highlight text %}
$ objdump -d -C --section=.plt ./a.out

0000000000401020 <.plt>:
  401020:       ff 35 e2 2f 00 00       push   0x2fe2(%rip)
  401026:       ff 25 e4 2f 00 00       jmp    *0x2fe4(%rip)
  40102c:       0f 1f 40 00             nopl   0x0(%rax)

0000000000401030 <f()@plt>:
  401030:       ff 25 e2 2f 00 00       jmp    *0x2fe2(%rip)
  401036:       68 00 00 00 00          push   $0x0
  40103b:       e9 e0 ff ff ff          jmp    401020 <.plt>

0000000000401040 <g()@plt>:
  401040:       ff 25 da 2f 00 00       jmp    *0x2fda(%rip)
  401046:       68 01 00 00 00          push   $0x1
  40104b:       e9 d0 ff ff ff          jmp    401020 <.plt>
...
{% endhighlight %}

The interesting thing is that this is actual machine code that is executed by the CPU, not some array-like data structure like the GOT is. In the binary, when we call `f()`, instead of jumping to a label `f():` as it would for a statically linked symbol would, it actually jumps to the label `f()@plt:` shown above (address `401030`).

The first instruction then jumps to the address stored in `GOT + 0x2fe2` (`GOT` is stored in the register `%rip`). Initially that entry holds the address of the second instruction of `f()@plt`, i.e. `401036`. We can check that by first grabbing the address of `f()` in the GOT:

{% highlight text %}
objdump -R -C  ./a.out
...
0000000000404018 R_X86_64_JUMP_SLOT  f()
0000000000404020 R_X86_64_JUMP_SLOT  g()
{% endhighlight %}

So the entry for `f()` is at address `0x404018`. Then we peek at that location using a debugger:

{% highlight text %}
LD_LIBRARY_PATH=. gdb ./a.out
(gdb) start
(gdb) x/a 0x0000000000404018
0x404018 <f()@got.plt>: 0x401036 <f()@plt+6>
{% endhighlight %}

So the effect is that it moves on to the next instruction (as if it was a no-op). The next instruction is `push $0x0`, pushing the index `0` to the stack (this will be used for the linker to write back at position `0` of the GOT). It then jumps to `.plt` (`401020`) which pushes the address of the `GOT` to the stack and then jumps to the address held by `GOT + 0x2fe4` which is the linker code responsible for doing the actual symbol lookup. This example is depicted in *Figure 6*.

<figure class="center_children">
  <img src="{{resources_path}}/lookup-cold.svg" alt="See caption." width="450" />
  <figcaption>Figure 6: "Cold" lookup of `f()`'s address. It has to go through many hoops and eventually invoke the dynamic linker code to search for `f()` among the DSOs.</figcaption>
</figure>

Once it finds the address of `f()` in the corresponding DSO, it will write to `GOT + 0x2fe2`, so that next time we jump into `f()@plt`, we'll jump directly to the real address of `f()`! *Figure 7* illustrates this flow.

<figure class="center_children">
  <img src="{{resources_path}}/lookup-warm.svg" alt="See caption." width="450" />
  <figcaption>Figure 6: "Warmed up" lookup of `f()`'s address. When the PLT looks up on the GOT and jumps, it goes straight to `f()`'s address.</figcaption>
</figure>


We note a few things from this clever process:

* Neither the original binary code nor the PLT code are modified when the linker resolves the address of `f()`. Only the GOT is.
* The address resolution is lazy, so for functions that are never invoked we need pay the cost of resolving their addresses.
* There's always a level indirection when we invoke `f()@PLT`: it first needs to jump to the PLT and then to the actual address of `f()`.
* There are no conditionals in the lookup process aside from the linker code. That is, we don't check if the entry is set in the GOT, we always unconditionally jump to the address stored in there.

### Symbol Lookup: DSO

The linker loads a few sections from a DSO's ELF including `.dynsym`, `.dynstr` and `.gnu.hash`. The `.dynsym` contains the symbols that can be used by dependents. This table is normalized: it doesn't include the text of the symbol. They're listed in `.dynstr` and the `.dynsym` only includes an offset to that table.

The `.gnu.hash` is a hashtable for looking up symbols in the DSO. It is meant to replace an old, less efficient hashtable under the `.hash` section. The details on the differences and optimizations are well described in [2]. This hash table contains references to entries in `.dynsym`.

So the process of looking-up a symbol (on demand or otherwise) is: the linker visits each DSO in order and does a lookup in its hash table. If it finds a matching hash, it might have to check a list of entries (due to hash collision), and for each entry it involves doing string comparison for the symbol name.

If it finds the symbol, it returns the absolute address of the symbol, which is the relative address within the DSO + the address of the DSO itself. If it doesn't find, then the next DSO is looked up.

Let $D$ be the number of DSOs, $R$ the number of "external" symbols to be looked up, $m$ the number of entries for a given hash due to collisions and $s$ the length of a symbol name. The complexity of the lookup process is $O(DRms)$.

So a high number of external symbols and shared libraries, the quality of the hash function and the length of a function name can all affect the performance of the lookup process. In C++ in particular the length of a symbol makes things worse because the namespace is part of the mangled name. So if there are a lot of nested namespaces shared across symbols, they become share prefixes, and the string comparison will take longer in most cases.

We can get a sense of how many symbol lookups ($R$) are taking place via:

{% highlight text %}
LD_LIBRARY_PATH=. LD_DEBUG=symbols ./a.out 2>&1 | grep -o "symbol=" | wc -l
{% endhighlight %}

For the simple example above, this returns around 10,000 instances! For a bloated binary I work with on a regular basis it showed 8,000,000 lookups.

## ABI

Another aspect to consider for shared libraries is ABI-compatibility. ABI stands for *Application Binary Interface*.

The most obvious compatibility issue is regarding symbol names and signature: if during compilation time we rely on a given DSO but then at link time we provide a DSO that has changed the signature of one of its exported functions, we'll get a runtime error.

There are more subtle ways to make the binary and DSOs incompatible: for example, if the way structs are organized in memory changes. The binary code might be expecting a specific order of member variables. For example, suppose we have a point struct defined as:

{% highlight cpp %}
// point.h
struct Point {
  int x;
  int y;

  void print();
};

// point.cpp
void Point::print() {
  printf("(%d, %d)", x, y);
}
{% endhighlight %}

And we use it in a main function:

{% highlight cpp %}
int main() {

  Point p {
    .x = 1,
    .y = 2
  };
  p.print();

  return 0;
}
{% endhighlight %}

We can make `point.cpp` into a DSO and link dynamically with `main.cpp`. It will work just fine and print `(1, 2)`. Now suppose during a refactoring we swapped the order of `x` and `y` in `point.h`:

{% highlight cpp %}
// point.h
struct Point {
  int y;
  int x;

  void print();
};
{% endhighlight %}

If we re-compile the DSO (but not the binary), it will print `(2, 1)`! This is because the binary and the DSO are working with different `Point` structs. So one must be very careful in changing `.h` files in DSOs.

To avoid such problems, if the DSO author needs to make a back-incompatible change, they can append a version number at the end of the library name, e.g. `libc.so.6` we saw above. Neither the compiler nor the linker has the notion of versioning though. It's up to the user or the OS packaging system to symkink `libc.so` to the appropriate version.

## Conclusion

The post ended up becoming much longer than I initially envisioned, but that's because there was so much I learned about shared libraries.

I had this misconception for a long while: I thought statically linked libraries were those that are self-contained, meaning they do not depend on dynamic libraries. As we learned, it's just a list of `.o` objects that are linked with the main binary at compile time. In fact, statically linked libraries can themselves depend on dynamic libraries which then become dependencies of the main binary.

It was also great to dig into the symbol lookup process of the dynamic linker and learn about many of the different sections I had seen while studying a sample ELF file in my previous post.

I had no idea that that calling functions from DSOs involved a level of indirection and that the symbol process can be so expensive! Finally, trying to come up with examples for ABI incompatibility gave a much more solid grasp of it.

## Related Posts

In [Local Inter-Process Communication]({{blog}}/2024/12/07/local-ipc.html), we covered shared memory segments as a way to communicate between processes, and we also raised the issue related to ABI compatibility if the processes are using incompatible binaries.

In [Function Objects in C++]({{blog}}/2023/06/01/functions-in-cpp.html), we briefly discussed how function calls happen at the machine code level, in that case for ARM intruction set.

## References

* [[1]({{blog}}/2025/04/12/elf.html)] NP-Incompleteness: ELF: Executable and Linkable Format
* [[2](https://www.akkadia.org/drepper/dsohowto.pdf)] How To Write Shared Libraryes - Ulrich Drepper
* [3] ChatGPT
