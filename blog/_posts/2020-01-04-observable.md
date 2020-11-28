---
layout: post
title: "Observable"
description: "First impressions on the Observable platform"
tags: [data visualization, javascript, observable]
---

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2020-01-04-observable/2020_01_logo.png" alt="logo" />
</figure>

Observable is a web-first interactive notebook for data analysis, visualization, and exploration [1].

It was created by Mike Bostock, the creator of d3.js, which we [discussed previously]({{site.url}}/blog/2014/08/25/introduction-to-d3.js.html) way back in 2014 [2]. At some point, Mike stepped down from d3.js to focus on a new library called *d3.express*, which he presented in 2017 during the [OpenVis conference]({{site.url}}/blog/2017/05/01/openvis-conf-2017.html) [3] and was still in the makes. d3.express eventually got renamed to Observable.

I've been experimenting with Observable and in this post I'll cover some of my learnings while trying to build two visualizations.

One of the benefits of notebooks like Observable is to co-locate code with its output. This makes it much easier to explain code because in addition to markdown comments, we can modify the code and see the results in real-time. This means that a lot of the documentation of APIs available in Observable are notebooks themselves. For these reasons, this post consists of a series of links to notebooks with high-level comments.

Before we start, some assumptions I'm making are familiarity with imperative programming and the basics of JavaScript.
# What is Observable?
[Why Observable](https://observablehq.com/@observablehq/why-observable) - this notebook explains the motivation behind Observable and how it works at a high level:
> An Observable notebook consists of reactive cells, each defining a piece of state. Rather than track dependencies between mutable values in your head, the runtime automatically extracts dependencies via static analysis and efficiently re-evaluates cells whenever things change, **like a spreadsheet**

[Five-Minute Introduction](https://observablehq.com/@observablehq/five-minute-introduction) - provides a hands-on approach of the many topics, some of which we'll cover in more detail in this post:
* Real-time evaluation of JavaScript snippets
* Cell references
* Markdown
* HTML/DOM display
* Promises
* Generators
* Views
* Importing from other notebooks
Let's start by delving into the details of JavaScript snippets and cell references.
# Observable vs. JavaScript
[Observable’s not JavaScript](https://observablehq.com/@observablehq/observables-not-javascript) - in this notebook Bostock explains the differences between JavaScript and Observable. Some notes I found interesting from that notebook:
* A cell is composed by a cell name and an expression:
  * `[cell name] = [expression]`
It can be simple statements like 1 + 2, or multi-line statements like

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2020-01-04-observable/2020_01_screenshot-from-2020-01-04-09-56-25.png" alt="Screenshot from 2020-01-04 09-56-25" />
</figure>
* The value of ` cell_name` can be used in other cells, but by default is read-only.
* Each cell can only have one ` cell_name`  assignment. In other words, it can only "export" one variable. It's possible to cheat by exporting an object. Note that because curly brackets are used to denote code blocks, we need to wrap an object literal with parenthesis:
<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2020-01-04-observable/2020_01_screenshot-from-2020-01-04-10-14-58.png" alt="Screenshot from 2020-01-04 10-14-58" />
</figure>
* Cells can refer to other cells in any part of the code - Observable builds a dependency DAG to figure out the right order. This also means dependencies cannot be circular. [How Observable Runs](https://observablehq.com/@observablehq/how-observable-runs) explains this in more details.
* Constructs like async functions (await) and generators (yield) have special behavior in a cell. We'll expand in the *Generators* section below.
* Cells can be mutated if declared with a qualifier (mutable). We'll expand in the *Mutability* section below.
# Mutability
[Introduction to Mutable State](https://observablehq.com/@observablehq/introduction-to-mutable-state) - cells are read-only by default but Observable allows changing the value of a cell by declaring it `mutable`. When changing the value of the cell elsewhere, the `mutable` keyword also must be used.

It's important to note that the cell is immutable, but if the cell is a reference to a value, say an `Object`, then the value can be mutated. This can lead to unexpected behavior, so  I created a notebook, [Mutating references](https://observablehq.com/@kunigami/mutating-references), to highlight this issue.
# Markdown
[Markdown summary](https://observablehq.com/@jaynel/markdown-summary) - is a great cheat sheet of Markdown syntax in Observable. I find the Markdown syntax in Observable verbose which is not ideal given text is so prevalent in notebooks:

`md`Hello World`
`
More cumbersome still is typing inline code. Because it uses backticks, It has to be escaped:

`md`Hello \`code\```

To be fair, most code will be typed as cells, so this shouldn't be too common. It supports LaTeX via KaTeX which is awesome. [KaTeX on Observable](https://observablehq.com/@kunigami/tex-katex-on-observable) contains a bunch of examples.
# Generators
Generators are a JavaScript construct that allows a function to yield the execution back to the caller and resume from where it stopped when it’s called again. We talked about this in the [Async Functions in JavaScript]({{site.url}}/blog/2019/07/01/async-functions-in-javascript.html) post.

[Introduction to Generators](https://observablehq.com/@observablehq/introduction-to-generators) explains how generators can be used in Observable. The combination of generators and delays (via promises) is a great mechanism to implement a ticker, which is the base of animation. Here's a very simple way to define a ticker in Observable (remember that Promises are awaited without extra syntax):

{% highlight javascript %}
tick = {
  while (true) {
    yield Promises.delay(1000);
  }
}
{% endhighlight %}
# Views
[Introduction to Views](https://observablehq.com/@observablehq/introduction-to-views) explains what views are and how to construct one from scratch. I like this recommendation:
> If there is some value that you would like the user to control in your notebook, then usually the right way to represent that value is as a view.

Views are thus convenient ways to expose parameters via buttons and knobs such as text inputs, *dropdowns* and *checkboxes*.

I ran into a problem when using checkbox with labels in it, like the cell below:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2020-01-04-observable/2020_01_screenshot-from-2020-01-03-23-14-08.png" alt="Screenshot from 2020-01-03 23-14-08" />
</figure>

It does not yield true/false as I wanted. This notebook, [Checkbox](https://observablehq.com/@nitaku/checkbox), discusses the problem and offers a solution. This is an example where great but imperfect abstractions make it hard to understand when something doesn't work. It's worth learning about how `viewof` is implemented behind the scenes.

In one of my experiments I needed to synchronize a view and a ticker. Bostock provides a neat abstraction in his [Synchronized Views](https://observablehq.com/@mbostock/synchronized-views) notebook.
# Importing from other notebooks
[Introduction to Imports](https://observablehq.com/@observablehq/introduction-to-imports) shows how easy it is to import cells from other notebooks.

It also shows that it's possible to override the value of some of the cells in that notebook:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2020-01-04-observable/2020_01_screenshot-from-2020-01-03-23-28-09.png" alt="Screenshot from 2020-01-03 23-28-09.png" />
</figure>

This is neat, because the `chart` cell depends on the `data`, and by allowing overriding data, it allows parameterizing `chart`. In other words, we import `chart` as a function as opposed to a value (the result of `chart(data)`).

**Versioning.** When importing a notebook we always import the most recent version, which means it could potentially break if the notebook's contract changes. The introduction above mentions the possibility of specifying the version when importing but didn't provide an example on how to do so.

My strategy so far has been to clone a notebook if I'm really concerned about it changing, but that means one has to manually update / re-fork if they want to sync with the upstream changes.
# React Hooks
I'm used to write UI components with React and luckily Jed Fox created a notebook which I forked into this [React Notebook](https://observablehq.com/@kunigami/react) which allowed me to use [React Hooks](https://reactjs.org/docs/hooks-intro.html) to write a [view](https://observablehq.com/@kunigami/animation-utils).
# Conclusion
I really like Observable and am looking forward to add implement more data visualizations in it. I experimented with 2 animations so far: a simplistic [Solar System model](https://observablehq.com/@kunigami/solar-system-model-i) and a spinning [Globe](https://observablehq.com/@kunigami/globe).

One natural question that crossed my mind is whether I should have written this whole post as an Observable notebook. In the end I decided to stick with an old-school static post for two reasons:
* Consistency: Observable only supports JavaScript, so if I'm writing a post about Python I'd need to fallback to a post, and I wouldn't want to have a mix of both.
* Durability: Observable has a lot of potential but it's still mostly experimental and I'm not sure I can rely on it sticking around for a long time.
# References
### Notebooks mentioned in the post
* Introduction
  * [Why Observable](https://observablehq.com/@observablehq/why-observable)
  * [Five-Minute Introduction](https://observablehq.com/@observablehq/five-minute-introduction)
  * [Observable’s not JavaScript](https://observablehq.com/@observablehq/observables-not-javascript)
  * [How Observable Runs](https://observablehq.com/@observablehq/how-observable-runs)
* Mutability
  * [Introduction to Mutable State](https://observablehq.com/@observablehq/introduction-to-mutable-state)
  * [Mutating references](https://observablehq.com/@kunigami/mutating-references)
* Markdown
  * [Markdown summary](https://observablehq.com/@jaynel/markdown-summary)
  * [KaTeX on Observable](https://observablehq.com/@kunigami/tex-katex-on-observable)
* Views
  * [Introduction to Generators](https://observablehq.com/@observablehq/introduction-to-generators)
  * [Introduction to Views](https://observablehq.com/@observablehq/introduction-to-views)
  * [Synchronized Views](https://observablehq.com/@mbostock/synchronized-views)
*  Importing
  * [Introduction to Imports](https://observablehq.com/@observablehq/introduction-to-imports)
* Integration
  * [React Notebook](https://observablehq.com/@kunigami/react)
### Other
[[1](https://observablehq.com/@observablehq/why-observable)] Observable - Why Observable?
[[2]({{site.url}}/blog/2014/08/25/introduction-to-d3.js.html)] Introduction to d3.js
[[3]({{site.url}}/blog/2017/05/01/openvis-conf-2017.html)] OpenViz Conf - 2017
[[4]({{site.url}}/blog/2019/07/01/async-functions-in-javascript.html)] Async Functions in JavaScript
