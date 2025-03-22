---
layout: post
title: "Review: Getting Started with LLVM Core Libraries"
tags: [review, compilers, cpp]
excerpt_separator: <!--more-->
vanity: "2025-03-13-review-llvm-core-libs"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/llvm.jpg" alt="Book Cover" />
</figure>


In this post I’ll share my notes on the book *Getting Started with LLVM Core Libraries* by Bruno Cardoso Lopes and Rafael Auler.

The book is aimed at people interested in developing code using LLVM internal libraries. It's very practical in the sense that it provides lots of code examples, but installation steps and command lines.

My main objective on reading this book was to get a better understand on how LLVM worked and so I skimmed most of the parts around how to setup and run things. Hence I'll focus more on the high level parts of the book in my summary.

<!--more-->

## Book Organization
{:.no_toc}

This book has about 285 pages and 11 chapters. *Chapters 1*, *2* and *3* explain how to set up the project and getting a sense of the codebase. I'll skip these.

*Chapters 4*, *5* and *6* cover the main architecture of LLVM (Frontend, IR, Backend) and the ones I was mostly interested in.

*Chapter 7* is about the JIT (Just-in-time compilation) feature LLVM provides. I'm not particularly interested in JIT as of now so I'll just provide a summary.

*Chapter 8* is about cross-platform compilation. This means compiling code in an architecture/system that is different from where it will be executed. I'm not interested in this at the moment, so I'll skip.

*Chapter 9* and *10* cover non-compilation features LLVM provides: static analysis and code transformation. I'm actually very interested in these, but until I get to try it out myself I don't feel like I can provide a good summary, so I'll try to provide some high-level overview instead.

## Table of Contents

1. TOC
{:toc}


## Overview

For the sake of simplicity and self-interest, I'll throughout assume the language being compiled is C++, even though LLVM is able to compile other languages. The C++ compiler is known as *Clang* which I had trouble differentiating from LLVM, but reading this book made things clearer.

LLVM stands for *Low Level Virtual Machine*. It started as a virtual machine rivaling the Java VM but used a lower level intermediate representation, which is now the LLVM IR. The Java IR is called Java bytecode, and as a jab at how verbose that format is, LLVM's IR is sometimes called bitcode. LLVM today is not used as a virtual machine but the name stuck.

In very high-level and simplistic terms, LLVM is composed of two parts: the frontend and the backend. The **frontend** is coupled with a specific programming language (e.g. C++, Rust), while the **backend** is coupled with the target where the code is run (e.g. Linux or Windows, x86 or ARM64).

To avoid having to handle for every single combination of language and target the frontend writes to an **intermediate representation** (IR) which in theory is agnostic to both programming language and target (in practice I learned from the book this is not true).

<figure class="center_children">
  <img src="{{resources_path}}/overall.svg" alt="See caption." width="500" />
  <figcaption>Figure 1: Relationship between frontend, backend, IR and linker.</figcaption>
</figure>


This modularity is useful not only from the maintainability perspective of the code, but it also allows partial use of LLVM. For example, the Rust compiler (`rustc`) has its own frontend component that is capable of compiling to LLVM's IR and then only uses LLVM's backend component for general optimizations.

Modularity is a key design principle of LLVM and it goes beyond just the frontend and backend. Each of these components are composed of substeps which are implemented by libraries with public interfaces, so it's possible to only use a subset of steps from either of these components.

The process of converting a source file into an object code is called compilation. This process generates one such artifact for each `.cpp` file, so it is still necessary to combine them into a single binary. This is done by the linker. At the time the book was written, LLVM relied on the GNU linker (`ld`) because its own linker was not mature yet.

## The Frontend

The frontend component that handles C++ (and also C and Objective-C) is called **Clang**. The terminology is confusing because *Clang* can also refer to the full suite of compilation and linking, called *compiler driver* (via the `clang` command) or just the compiler (via the `clang --cc1` option).

In this post when we mention Clang we'll be referring to the frontend of LLVM for C++.

As we mentioned in *Overview*, the frontend is composed of a few steps, show in *Figure 2*:

<figure class="center_children">
  <img src="{{resources_path}}/frontend.svg" alt="See caption." width="500" />
  <figcaption>Figure 2: Steps of the frontent. The blue boxes and blue text represent artifacts.</figcaption>
</figure>


We'll cover each of the steps briefly:

### Lexical Analysis

This step is also known as **lexing**, and is responsible for splitting the source code (text) into tokens, which have an associated type.

There's also the *Preprocessing* step which runs interleaved with the *Lexical Analysis* and hence is not depicted in the diagram of *Figure 2*. This step is responsible for replacing C macros.

This part was a learning for me: I always assumed the preprocessing happened before lexing.

### Syntactic Analysis

This step is also known as **parsing**, and it structures the tokens into a tree, the Abstract Syntax Tree or AST.

### Semantic Analysis

This step is essentially a type checker. It traverses the AST and keeps type information about variables to detect type inconsistencies.

Clang runs the semantic analysis while constructing the AST.

### LLVM IR Generator

This step consists in transforming the C++-specific AST into the generic AST of LLVM, called LLVM IR.

Following the principle of modularity, we can run inspect the result of each of these steps individually with the invokation of `clang`. For example, if we want to see the output of the parser, we can do

{% highlight text %}
clang -fsyntax-only -Xclang -ast-view min.c
{% endhighlight %}


## The LLVM IR

The intermediate representation has to stike a fine balance to avoid coupling too much with specific languages or specific targets. In practice there is not a single format of the LLVM IR due to being unable to account for all possible languages and targets.

Target-independence is challenging to achieve, since even the source code might be target-dependent (e.g. C++ can make Linux syscalls).

The LLVM IR can be represented in disk either as bitcode (extension `.bc`) or LLVM assembly (extension `.ll`). These are parallel analogous to object code (extension `.o`) and LLVM assembly (extension `.asm`).

The bitcode is a binary format, while the LLVM assembly is human readable. Let's explore an example for the code `sum.cpp`:

{% highlight cpp %}
int sum(int a, int b) {
  return a + b;
}
{% endhighlight %}

We can generate the LLVM assembly via:

{% highlight text %}
clang sum.cpp -emit-llvm -S -c -o sum.ll
{% endhighlight %}

and obtain:

{% highlight text %}
; ModuleID = 'sum.cpp'
source_filename = "sum.cpp"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i32 @_Z3sumii(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %3, align 4
  %6 = load i32, ptr %4, align 4
  %7 = add nsw i32 %5, %6
  ret i32 %7
}
{% endhighlight %}

Some observations about the format:

* It uses virtual registers, e.g. `%3`, which are mapped into physical ones by the backend step.
* It uses *Static Single Assignment* (SSA) which means a variable is never reassigned. SSA requires phi instructions to handle merging flow paths, for example, if the original code was:

{% highlight text %}
int x;
if (condition) {
    x = 1;
} else {
    x = 2;
}
{% endhighlight %}

In SSA form it becomes:

{% highlight text %}
if (condition) {
    x1 = 1;
} else {
    x2 = 2;
}
x3 = φ(x1, x2)
{% endhighlight %}

The special function φ that tells us only one of `x1` and `x2` is valid.

* It uses [three-address instructions](https://en.wikipedia.org/wiki/Three-address_code), meaning an instruction uses at most 3 addresses, so the expression such as `x = (-b + sqrt(b^2 - 4*a*c)) / (2*a)` has to be broken down in multiple ones using intermediate variables.

The lines

{% highlight text %}
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"
{% endhighlight %}

Specify information about the target architecture. In this example the arm64 architecture of the MasOS operating system.

In `datalayout` the `-` separates attributes, so we have `e`, `m:o`, `i64:64`, etc.

The `e` indicates a little endian addressing (big endian would be `E`. Neat!). The `m:o` entry indicates the binary format. The entry `i64:64` indicates the alignment of 64-bit integers (it could be more). `S128` means the stack address is aligned is at 128 bits.

## The Backend

Like the frontent, the backend is composed of multiple steps. Its aim is to convert the LLVM IR into either object code or assembly.

Figure 3 depicts the main steps, which are called passes.

<figure class="center_children">
  <img src="{{resources_path}}/backend.svg" alt="See caption." width="500" />
  <figcaption>Figure 3: Steps of the backend. The gray boxes are mostly optimizations and not critical to the correctness of the process. The blue boxes and blue text represent artifacts.</figcaption>
</figure>

We'll go over briefly over the green boxes of *Figure 3*.


### Instruction Selection

The instruction selection converts the LLVM IR into a structure called *Selection DAG*, a DAG where each node is an operand or instruction of the target architecture. The process of converting generic instructions into target-specific ones is called **lowering**.

This pass is also responsible for **legalization**, making sure that the instructions only use existing types of the target architecture. For example, if the target architecture only supported 32-bit integers, operations involving 64-bit integers need to be split.

The instruction selection is the most expensive step of the backend, taking up to half of the time of the entire process. There are less optimized versions of this step that runs faster and can be used with different compiler optimization levels such as `-O0`.

### Pre Instruction Scheduling

At this point we still have a DAG which doesn't fully specify the order in which we execute instructions. This is that the instruction scheduling does.

This step uses prior target-specific information to better guide the decision of how to order instructions. This information is called **instruction itineraries**. One example is data about how many CPU cycles a given instruction takes.

This step also compute **hazards**, situations which might lead to poor performance, for example an instruction that might depend on the result of some slow computation (data hazard). In this case the compiler might opt to insert other instructions in between to avoid idleness.

### Register Allocation

The overall idea of register allocation is to assign virtual registers (unbounded) to physical ones (constrained by the CPU architecture).

This process can be modeled as the graph coloring problem, which is NP-Complete. The graph coloring problem consists in assigning a color to each node such that adjacent vertices don't share the same color.

We can reduce the register allocation to graph coloring as follows: each node is a virtual register and there's an edge between two nodes if their lifetime overlap. Registers with the same color can use the same physical register.

During register allocation, phi-expressions are "expanded" so they're become valid:

{% highlight text %}
L1:
  x1 = 1
  goto L3
L2:
  x2 = 2
  goto L3
L3:
  x3 = φ(x1, x2)
{% endhighlight %}

Becomes

{% highlight assembly %}
L1:
  x1 = 1
  mov x3, x1
  goto L3
L2:
  x2 = 2
  mov x3, x2
  goto L3
L3:
{% endhighlight %}

Noting that this requires "pushing" the `mov` instructions into the respective branches. (I wonder how this is actually implemented in practice).

Before the register allocation step is run, it does a **register coalescing**, a process in which unecessary register copies are removed. For example:

{% highlight text %}
mov r1, r2
add r3, r1, r4
{% endhighlight %}

If `r1` is never used again after the `add` instruction we can directly use `r2` there and avoid a copy:

{% highlight text %}
add r3, r2, r4
{% endhighlight %}

The register allocation process relabels the virtual registers, but the actual assignment to physical ones is its own step, the **virtual register rewrite**. In the coloring process it's possible to end up with identity copying (e.g. `mov x1 x1`) which could not have been coalesced beforehand. This last step removes them as well.

### Post Instruction Scheduling

The book doesn't say much about this step except that it takes in a `MachineInstr` as input (the pre instruction scheduling takes in a `SelectionDAG`).

I'm guessing that after registers are allocated and instructions are added and deleted there's a need to re-schedule these instructions.

### Code Emission

This last step consists of converting an instance of `MachineInstr` into either assembly instruction or object code. One interesting detail is that this step uses streaming, meaning each instruction is processed at a time, suggesting this step is a simple 1:1 mapping.

## The Just-in-Time Compiler

LLVM offers a JIT compiler that operates at a function granularity, i.e. if a function is invoked, the compiler will process the entire function body. An alternative is trace granularity: only processing specific code paths of a function (e.g. a branch).

Function-based granularity is slower but supports better optimizations.

## The Clang Static Analyzer

The static analyzer is essentially a linter. It relies on a **symbolic execution engine** which in theory computes all possible paths a program can take, but it might run forever due to combinatorial explosion. In practice it employs heuristics and limits how many paths it actually searches.

The following example is provided by the book:

{% highlight c %}
#include <stdio.h>
void f(int x) {
  int y;
  if (x) {
    y = 5;
  }
  if (!x) {
    printf("%d\n", y);
  }
}
{% endhighlight %}

Here `y` might be uninitialized depending on the value of `x`. It compiles without warnings but we can run the analyzer:

{% highlight text %}
$ clang --analyze -Xanalyzer -analyzer-checker=core bug.cpp
bug.cpp:8:5: warning: 2nd function call argument is an uninitialized value [core.CallAndMessage]
    8 |     printf("%d\n", y);
      |     ^~~~~~~~~~~~~~~~~
1 warning generated.
{% endhighlight %}

The static analyzer can be extended with custom checkers. It works via a visitor pattern: by subclassing a checker class and implementing visitor for specific AST nodes.

## Clang Tools

The other non-compiling feature LLVM/Clang provides are code-related tools. One of them is a formatter called `clang-tidy`. It also provides a tool for modernizing C++ code (for example convering)

A particularly relevant tool is `clang-query`, a CLI which allows testing AST matchers which is useful for writing code refactoring tools. The book explains how to write one such tool.

## Conclusion

Overall the book is very well written and detailed. As I mentioned at the start, my main goal was to get a better understand on how LLVM worked so I didn't find the code details very useful.

I did learn a few things about LLVM:

* The frontend and the backend architecture.
* LLVM IR can be represented in disk as bitcode or LLVM assembly.
* LLVM relies on modular libraries which can be used by applications, and they can serializing their outputs to disk for debugging.
* LLVM started as a virtual machine but it's not used as such anymore.
* LLVM has a JIT compiler.
* Clang is the frontend for C++, but can be used as a driver to not only carry out the full compilation (i.e invoke the backend) but also the linking step.
* `rustc` uses the LLVM backend but not the frontend.

It was also a good refresher on compilers, which I studied in college a while back.
