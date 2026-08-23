---
layout: books
title: "C++ Concurrency in Action"
vanity: cpp-concurrency-in-action
rating: 4
image: "concurrency.png"
author: "Anthony  Williams"
category: "Technology"
---

{% include blog_vars.html %}

{{ book_cover }}

<b>C++ Concurrency in Action</b> by Anthony Williams. This book covers many aspects of concurrency in C++ including threads, mutexes, locks, condition variables and atomic variables.
<br /><br />
One part I found lacking details was the <i>Release-Acquire</i> memory model. I tried to complement by reading a paper and found it a lot more complicated than the book described. I intend to write a post about it some day.
<br /><br />

After discussing primitives, the book presents some concurrent data structures, which was interesting and useful. In later chapters the book describes how to turn them into lock-free structures and the level of complexity to write a stack is so great that I question how useful this is to most people.
<br /><br />

One part that bothered me is how big the Appendix is. It's about 1/3 of the book and includes references that can be looked up on the internet if needed. Rating: 4/5

---

*This entry was parsed from an older [retrospective post]({{blog}}/tags/#retrospective).*
