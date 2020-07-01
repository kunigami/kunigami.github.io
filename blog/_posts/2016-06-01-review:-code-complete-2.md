---
layout: post
title: "Review: Code Complete 2"
tags: [review, software engineering]
---

<figure>
    <img src="{{site.url}}/resources/blog/2016-06-01-review:-code-complete-2/2019_05_code-complete.jpg" alt="code-complete" />
</figure>

In this post I'll share my notes about the book: Code Complete 2, by Steve McConnell. The book has great information about several aspects of Software Development, but it's quite long: 862 pages.

This is not a summary of the book by any means, but rather points that I found interesting, novel or useful. I hope the reader find it useful or at least that it inspires you to go after the book for more details on a specific topic.

I've written down bullet points about each chapter, some of which I added my own thoughts/comments.

### Chapter 1 - Introduction

* Motivation: What is Software construction and why it's important
* Explains the overall format of the rest of the book

### Chapter 2 - Metaphors

* Metaphors help to understand problems better and use solutions developed for the analogous problems and apply them to the original problem.
* Common analogy to software development is civil construction.
* Bad metaphors, from forced analogies, which can be misleading.

### Chapter 3 - Prerequisites (gathering requirements)

* Errors caught in the specification phase are the cheapest to fix (10x if the error is caught in construction).
* Different types of software require different degrees of investment in prerequisites. Three categories of softwares:
-- business (e.g. website),
-- mission-critical (e.g. packaged software) and
-- embedded/life critical (e.g. avionics or medical devices).
* Requirements changes over time, so your design has to be flexible enough to allow changes.

### Chapter 4 - Construction Planning

* Important decisions that have to be taken before the construction phase: programming language, code conventions, development practices (e.g. pair-programming), etc.
* Technology wave and the tradeoffs of choosing technology in different stages of this wave. For example, late-wave technology is more mature, has better documentation and user-friendly error messages. On the other hand early-wave environments are usually created to address problems with existing solutions.

*Comments:* Adopting early technology also helps with recruiting. Programmers like new technology.

### Chapter 5 - Design

* The main goal of the design should be to keep software complexity low.
* Different levels of design: software-level, packages, classes, routines and internal routines -- different types of software require different amounts of design details.
* Two main flavors to carry over the design: bottom-up or top-down approaches.
* Information hiding is key in a good design: It helps lowering the complexity by not requiring the person reading the code abstract details and reduce decoupling.

*Comments:* Hiding information properly is an art. It doesn't help to stick as much code as possible into private methods if the public methods are not intuitive and require diving into implementation details.

### Chapter 6 - Classes

* Consistent abstractions - different methods in the class should model the problem at the same level. Example: a class representing an employees record which inherits from a `List` with two methods:

{% highlight text %}

addEmployee()
firstItem()

{% endhighlight %}

The second one is closer to implementation detail. In general, the closest to the business level the abstraction is, the better.

* Inheritance vs. composition: Inheritance if often abused and long chains of inheritance is often hard to read. Arthur Riel suggests no more than 6 levels, author says it should be limited to 3 levels.
* Be careful with excess of attribution to a single class. Heuristic that a class should have at most 7 members.

### Chapter 7 - Routines

* Naming: should be verb + noun and should describe the value it returns (if any).
* The purpose of a routine is to reduce complexity.
* The heuristic to the maximum number of parameters is 7.
* Routines should follow the linux philosophy: it should do one thing and do it well.

*Comments:* in the past I used to think of routines as ways to share code. This sometimes conflicts with readability and the linux principle. This is especially true when you group several routines calls into one because it's used in two places, but they're not cohesive enough to name it the routine clearly, so we end up using vague terms such as init, prepare, preprocess, cleanup, etc. Nowadays I prefer being verbose (i.e. repeating the lines in both places) in favor of readable code.

### Chapter 8 - Defensive Programming

* When to use assertions: error handling for things you expect to occur and assertion for the ones you don't expect.
* When to use exceptions: should be defined a convention. The name of the exceptions should match the level of abstraction of the current code (e.g. no RuntimeException where business logic is defined) this also means catch/re-throwing if the exception crosses the boundary of two different abstraction layers.
* Barricades: a specific layer that deals with bad input so that the core code doesn't have to deal with them.

*Comments:* the barricade is pretty nice, it helps reducing the complexity in the main code by not having to handle too many corner cases and also centralizes where bad data is handled, so you don't risk double or triple checking the same conditions in several places.

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2016-06-01-review:-code-complete-2/2019_05_defense.jpg" alt="defense" />
</figure>

### Chapter 9 - Pseudocode Programming Process (PPP)

* Describe the routine first in pseudo-code and then add the actual implementation but leaving the pseudo-code as comment.

### Chapter 10 - Variables

* The **span** of a variable is defined as the number of lines between where a variable is declared to where it's used. The average span of a variable is a indicator of complexity. High span variables means that the variable is spread out along the code. To reduce this number one can try to re-order statements in such a way that variables are declared close to where it's used and all its uses are grouped in closer places.

### Chapter 11 - Variable naming

* Optimal **length** is 10 to 16 characters.
* Computed qualities such as total, max, should be suffix, not prefix.
* Use conventions to indicate special types of variables such as loop indexes, temporary, boolean, enums, etc.
* Define a document for consistent variable naming conventions, including abbreviations.

### Chapter 12 - Fundamental Data Types

* Consider using special purposed containers instead of plain arrays. Most of the cases we need sequential access, so queue, stack or sets can be more appropriate.
* Use intermediated boolean variables for the sole purpose of making complex predicates (in if clauses) simpler.

### Chapter 13 - Unusual Data Types

* Organize related set of variables into a structure so they become more readable/easier to copy.
* Global variables are evil. If you need them, at least out them behind access routines (e.g. static member variables in a class).

### Chapter 14 - Organizing code within a routine

* Make dependencies between 2 pieces of code obvious: via parameters, comments or flags + invariants.
* To break dependencies chunks of code, initializing variables in the beginning of the routine might help.

*Comments:* I also like memoized functions to break dependencies. If B depends on A being run, I create A as a memoized function that B can call no matter if it had been called already.

### Chapter 15 - Conditionals

* When doing if/else conditionals, test the "normal" case first and the exception in the "else".

*Comments:* I tend to do the opposite in favor of the early returns: this helps reducing nesting and clear up corner cases first - this works well when the handling of the exception case is simple.

### Chapter 16 - Loops

* Keep as much code outside of the loop as possible, and treat it as a black box, if possible.
* Perform only one function in the loop, prefer using multiple loops with simple functions than one loop with many functions - unless you can prove that using multiple loops is the bottleneck of the code.
* The loop body should be short (&lt;20 lines) and should be visible entirely in the screen.
* Loop nesting should be limited to 3 levels.

### Chapter 17 - Unusual Control Structures

* Multiple returns within the same routine: use only if this improves readability.
* Avoid recursion if possible. Restrict it to a single routine (no chains like `A -> B -> A -> B...`).

### Chapter 18 - Table-driven methods

* Simplify complicated conditional logic by pre-computing the values and hard-coding them in a lookup table (works if inputs are discrete).

### Chapter 19 - General control issues

* When comparing numbers, use the number-line order, in other words, always use the ``. For example, instead of

`a < 0 || a > 10` do `a < 0 || 10 < a`

* Do not put side effects on conditionals.
* Switch/Case statements indicates poorly factored code in OO programming.
* Measuring control complexity in a routine: count the number of if, while, for, and, or and case. It should be less than 10.

### Chapter 20 - Quality assurance

* Testing and code reviews are not enough by themselves. A combination of different techniques yields the lowest number of bugs in general.
* In studies, code review by 2 people found twice more errors as code reviews by 1 person - this is surprising because one would think a lot of the errors found by each reviewer would overlap. The book doesn't mention the optimal number of reviewers.
* There are many qualities in a software including: correctness, usability, efficiency, reliability, flexibility and robustness, and some of them are conflicting (e.g. improving robustness decreases efficiency). Decide upfront which characteristic to optimize and keep the tradeoffs in mind.

### Chapter 21 - Collaborative Development

* Formal inspections of design: the author of the design creates a document and other people in the team have to review it independently. This not only forces the designer to think it thoroughly, but also makes sure other people in the team will be familiar with the architecture.

### Chapter 22 - Testing

* Exercise **control flows**. Instead of testing all conditionals combinations (which would be exponential, and prohibitive), add 2 tests for each conditional (one for the true and another for the false cases).
* Test **data flow**. The suggestion is to test all pairs of (definition, usage) of all variables. For example, if we have

{% highlight text %}

  int a = 10; // 1
  ...
  if (x < a) { // 2
     ...
  }
  int b = a + 1; // 3

{% endhighlight %}

In this case we would add two tests: one that exercises lines (1) and (2) and another that exercises (1) and (3).

* Bug distribution is not uniform across the code. It's more likely that 20% of the code contains 80% of the bugs. It's important to identify these areas and avoid over-testing, especially if using TDD.
* Keep **records of bugs** and fixes: where the bugs were found, severity of the code, etc. This will help to identify the critical paths.

### Chapter 23 - Debugging

* A bug in your code means you don't fully understand your code.
* Reproduce the error in different ways, to make sure you understand what is really causing the problem.
* Rewriting code might be a better alternative if debugging is taking too long. The idea is set a maximum time dedicated to debugging, after which one is allowed more drastic solutions such as rewrites.

### Chapter 24 - Refactoring

* Make refactorings safe. In order to accomplish that, they should be small, self-contained, easy to review, documented, and tested.
* Different refactorings have different risks degrees and the planned accordingly.
* Book recommendation: *Refactoring: Improving the Design of Existing Code* by Martin Fowler.

### Chapter 25 - Code Tuning

* Code tuning is overrated.
* Performance is often not the best feature to optimize: throughput and correctness are more important.
* 4% of the code accounts for 50% of the performance - and programmers are bad at guessing code bottlenecks. Finish the product first, and profiling to find the bottlenecks.
* Common sources of performance bottlenecks are system calls, I/O and pagination.

### Chapter 26 - Code Tuning Techniques

* Specific techniques to improve runtime of code.
* Experimental results for each technique shows that in some environments the optimizations provide great performance gains, but in other cases, no significant improvements are obtained (sometimes degrading performance).
* The key takeaway is: profile! Compilers are very smart nowadays, so it's hard to predict what roles an optimization will be converted to final machine code.
* Downside of optimizations is loss of readability:

> "The impact of unmeasured code tuning on performance is speculative at best, whereas the impact on readability is as certain as it is detrimental."

### Chapter 27 - Program Size Affect Construction

* As the code size grows,
-- More people are necessary, increasing communication overhead
-- Productivity is lower
-- Error density increases
-- More time has to be proportionally spent on non-construction phases (planning and design)

### Chapter 28 - Managing Construction

This chapter seems to be more targeted to managers, but also useful to developers to understand the "other side".

* On encouraging good coding:

> "If someone on a project is going to define standards, have a respected architect define the standards rather than the manager. Software projects operate as much on an expertise hierarchy as on an authority hierarchy."

* **Configuration management**: practices to deal with changes, either in requirements, design or source code.
-- Discuss planned changes in group, no matter how easy they are to implement, so people keep track of .
* **Estimating construction time**:
-- Use estimating software,
-- Treat estimation seriously
-- Adjust estimates periodically and
-- If initial estimations were off, learn why.

### Chapter 29 - Integration

* Different strategies for doing code integration, mainly top-down (start with skeleton and fill in the blanks) and bottom-up (starts with individual pieces and glue them together).
* Strategies to make the integration smoother, such as automated builds and smoke tests.

### Chapter 30 - Programming Tools

* Covers: IDE's, tools for compiling, refactoring, debugging and testing
* Large projects require special purpose tools
* Programmers overlook some powerful tools for years before discovering them

<figure class="center_children">
    <a href="https://www.flickr.com/photos/19779889@N00/11054398564"><img src="{{site.url}}/resources/blog/2016-06-01-review:-code-complete-2/2019_05_11054398564_58a9322fa1_o.jpg" alt="11054398564_58a9322fa1_o" /></a>
</figure>

### Chapter 31 - Layout and Style

* Covers indentation, line breaks
* Do not align assignment statements on the '=' sign. The idea is noble and it improves readability, but it's a standard hard to maintain. Not everyone will have the same care and also automated refactors will likely miss it.
* Add at least one blank line before each comment

### Chapter 32 - Self-documenting code

* Don't comment too much or too little :)
* The author admits there's not a lot of hard-data regarding usefulness of comments and what the "right" amount is
* Comment while or, better yet, before coding
* Especially in bug fixes, the comment should explain why the code works now, not why it didn't work in the past.
* Comments styles have to be easy to maintain (avoid end-of-line comments, because if the variable gets renamed, it will misalign the comment)

### Chapter 33 - Personal Character

Traits that the author considers important in a good programmer:

* Humility - admits their limitation, open to learn new things, change their minds
* Curiosity

Analyze and plan before you act: dichotomy between analysis and action. Programmers tend to err of the side of acting.

* Intellectual Honesty - admit mistakes, be honest/realistic about estimates and delays,
* Communication
* Creativity
* Discipline
* Laziness

> The most important work in effective programming is thinking, and people tend not to look busy when they're thinking. If I worked with a programmer who looked busy all the time, I'd assume he was not a good programmer because he wasn't using his most valuable tool, his brain.

Traits that the author thinks are overrated:

* Persistence
* Experience
* Gonzo programming - programming like crazy, non-stop

### Chapter 34 - Themes in Software Craftsmanship

Conclusion and review of thee book

* The primary goal of software design and construction is conquering complexity
* Process matters.

> My message to the serious programmer is: spend a part of your working day examining and refining your own methods. Even though programmers are always struggling to meet some future or past deadline, methodological abstraction is a wise long-term investment - Robert W. Floyd.

* Write programs for people first, computers second
* Watch for warning signs. Examples: a high number of bugs in a particular class may indicate the class is poorly design. A lot of bugs in the project overall might indicate a flawed development process. Difficulty to reuse in another place, indicates it's too coupled, etc.

### Chapter 35 - Where to find more information

Books recommendation:

* Pragmatic Programmer - Hunt and Thomas.
* Programming Pearls - Bentley, J.
* Extreme Programming Explained: Embrace Change - Beck, K.
* The Psychology of Computer Programming - Weinberg.
* The Mythical Man-Month - Brooks
* Software Creativity - Glass, R.
* Software Engineering: A Practitioner's Approach - Pressman, R.
* Facts and Fallacies of Software Engineering - Glass, R.
* UML Distilled - Fowler, M.
* Refactoring: Improving the Design of Existing Code - Fowler, M.
* Design Patterns - Gamma et al.
* Writing Solid Code - Maguire, S.

## Conclusion

This book contains a lot of valuable information and I've incorporated several of his ideas in my day-to-day work, especially regarding making code easier to read.

The suggestions in the book are often backed by hard data, making them more credible. Sometimes the advice is subjective, even contradicting, but he often provides several points of view or alternatives, so that the reader can make their best judgement of when to use them.
