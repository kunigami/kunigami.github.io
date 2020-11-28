---
layout: post
title: "Review: Working Effectively With Legacy Code"
description: "Review of Michael C. Feathers' Working Effectively With Legacy Code"
tags: [review, software engineering]
---

In this post we will review the book *Working Effectively with Legacy Code* by Michael C. Feathers.

The basic premise of the book is that we want to make a change in an existing (legacy) code base and we want to make it safely. The flowchart below describes in broad strokes the content of the book:

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2020-05-22-review:-working-effectively-with-legacy-code/2020_05_flowchart.png" alt="" />
</figure>

&nbsp;

The core of the book focuses on breaking dependencies of legacy code so that we can add tests more easily. How can we break the vicious cycle? We need to make changes to add tests, but we need to have tests to make changes safely. The idea is to break the dependencies in safe ways even if it temporarily leads to a worse design until we can add tests and refactor the code.

## Organization of the book

The book is divided into 3 parts of a total of 25 chapters. In the first part the author describes concepts and introduce tools that are used in the rest of the book. In part II, he describes several different situations and problems and goes on to provide a solution to them. Part 3 contains a single chapter and describes several dependency breaking techniques. As the author suggests, they are not required to be read in order.

## Selected Notes

Here I’m going to list some of the techniques the book provided that I found interesting and novel.

### How to break dependencies

* *Seam model* - the idea is to make behavior changes while minimizing code changes. One example is to add a new subclass in order to remove/mock dependencies for tests.
* *Sprouting* - move blocks of code to new methods and classes which can be easily tested.
* *Wrapper class* - wrap code inside a new class which we have more control of, and can be mocked. This is a variant of the seam model, but instead of relying on inheritance it uses composition. As the author notes, this is the decorator design pattern.
A common pattern between them is keeping code changes to a minimum. The bigger the changes that more likely subtle behavior changes are introduced.

### Where to test

* *Effect analysis* - draw an informal DAG, where nodes represent functions or classes and a directed edge implies that changing one node will affect the other. This will help understanding which code is subject to be affected and must be tested.
* *Pinch points* - The effect analysis might yield too many affected nodes - pinch points nodes that cover a large portion of affected code but has relatively few dependencies so it’s easier to test.

### Interpreted Languages

The book has the strongest assumption that we are working with a compiled object-oriented language, namely C++, Java or C#. He dedicates about a page or two to dynamic languages, in this case Ruby.

## Quotes

The author shares several design principles throughout the chapters. Here are some quotes describing some of them.

Temporal coupling:
> When you group things together just because they have to happen at the same time, the relationship between them isn't very strong.
Refactoring:
> Rename class is a powerful refactoring. It changes the way people see code and lets them notice possibilities that they might not have considered before.
Command and Query Separation:
> A method should be a command or a query but not both. A command is a method that can modify the state of the object but that doesn't return a value. A query is a method that returns a value but that does not modify the object.
Psychology of working with legacy code:
> If morale is low on your team and it's slow because of code quality, here's something that you can try: pick the ugliest most obnoxious set of classes in the project and get them under test. When you’ve tackled the worst problem as a team you will feel in control of your situation.

## Conclusion

Overall there are many interesting techniques for breaking dependencies and making the code more testable. I’ve been using some of them in a less methodic way. I like the pragmatism of the techniques (done is better than perfect), which can be seen when good practices and designs are temporarily violated to write tests.

I’ve also enjoyed the code design tips interspersed throughout the chapters, but I disagree with some of the suggestions, for example converting static utils to methods in class instances. In my opinion static classes are a good forcing function to keep implementation modular and stateless. This is especially the case if there’s a good mocking framework available which can mock out static functions.

I found that the set of tips are less useful for dynamic language or just in time compilers which allows runtime modifications of the objects, because it allows much more powerful testing frameworks which for example can easily mock dependencies is for us. Nowadays I work mostly with Hack and JavaScript, which both provide a lot of runtime flexibility and support for mocking dependencies. The challenge I often face is having to mock too many dependencies which is tedious.

The major negative of the book is that it contains a lot of obvious typos and typographic error such as duplicated listings.

## Related Posts

* [Review: Code Complete 2]({{site.url}}/blog/2016/06/01/review:-code-complete-2.html) - This is another review of a highly practical and pragmatic book that I believe made me a better programmer. The overlap in content is minimal, so they're a nice complement.
