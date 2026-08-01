---
layout: post
title: "On Doing Things Manually"
tags: [opinion]
vanity: "2026-08-01-on-doing-things-manually"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/human-coder.png" alt="Human writing code via a computer. Generated with Nano Banana. I don't mind using AI to generate thumbnails for me." />
</figure>

In my last post about [KD-tree](https://www.kuniga.me/blog/2026/07/31/kd-tree.html), I vibe-coded a [JavaScript application](https://www.kuniga.me/resources/blog/2026-07-31-kd-tree/build.html) to demonstrate visually, step-by-step, how to construct and query a KD-tree. Codex one-shotted it in a few minutes; I spent a few minutes linking it to the blog post but did not even take a look at the code.

I've been using AI more and more both at work and for personal stuff but I also deliberately avoid using AI for specific tasks. I wanted to spend some time reflecting on this and document it.

<!--more-->

## Writing

### For Work

Since the beginning of the year, I write effectively [0% of my code by hand](https://www.kuniga.me/blog/2026/02/14/on-ai.html) at work. I don't consider it vibe-coding because I spend a large amount of time *reading* the code and making sure I understand what it does. I do skim over details and most of the generated unit tests.

AI sometimes writes lengthy comments with too much context which make it cumbersome to understand. If it's too much I manually erase chunks and sometimes rewrite it myself. I'm very close to the point of also not reading existing code directly so in theory I should not worry about walls of text which could be useful for AI.

I also write PR summaries manually, mostly to force myself to understand what the PR does. I found many times I had a gap in understanding of a change while drafting the PR summary.

### For Personal

For learning algorithms or solving [programming puzzles](https://github.com/kunigami/programming-contests), I write 100% of the code. I even turn off AI auto-complete. One reason is to keep my programming skills sharp (it's like hiking instead of driving), even if I might not need them in the future. The more important reason is that I find I learn better when I do things myself.

That's also the reason why I write 100% of my blog posts manually. I use AI to [correct typos and grammatical errors](https://github.com/kunigami/kunigami.github.io/blob/master/CLAUDE.md) but explicitly ask it not to rephrase things, even if it improves clarity. I want my blog to reflect my way of thinking, even if it's gradually but increasingly being influenced by AI.

A few times I did ask for feedback from AI on how to convey my ideas better, especially when [reviewing books](https://www.kuniga.me/books/), which helped clarify my understanding.

## Reading

I've been relying more and more on AI for learning, instead of reading textbooks or Wikipedia. I still love [reading books](https://www.kuniga.me/books/) for fun, but for difficult subjects I find it more effective to do everything through AI. I have specific learning patterns and some textbooks are so dense I waste time trying to decipher missing information (I spent [2 years](https://www.kuniga.me/blog/2026/04/05/book-complex-analysis.html) reading a book on Complex Analysis).

AI is very useful for digging into details on demand too. I might not care about details of a specific topic but want to keep asking for details on others. Printed books often cannot provide such a tailored experience. AI makes it easier to follow my curiosity. If I had to buy textbooks or spend hours researching, I'd just not do it.

The one concern with using AI is that some say that true learning only comes with struggle. If AI makes it too easy to digest things, am I learning optimally? Writing posts explaining stuff (even if no one reads it!) is even more important now, because it tests my understanding and keeps me honest, but I don't know if it's enough.

## Principles

My guiding principle for when to use AI vs. not: what's the most effective way to learn and understand? I don't have qualms about relying on AI to do 100% of my grunt work, but I also don't aim to blindly automate my work without learning anything in the process.

I'm fine to blindly let AI build tools to advance my projects but I'd like to be on top of the architecture and the algorithms that it uses, trade-offs taken, how the system deals with corner cases, etc.

I still think it's valuable to know and understand things instead of being an [AI puppet](https://www.kuniga.me/books/manna), but I also recognize that the value of knowledge might shrink the more AI can expand the scope of what it can do. I'd still want to learn for fun, even if it doesn't matter, the same way I enjoy reading [biology books](https://www.kuniga.me/books/the-body) even though I likely won't make use of it. I have more thoughts on this but I'll leave it for its own post.

## Conclusion

To circle back to the start of the post, learning as a goal makes it easy to justify vibe-coding the JavaScript app to better understand Delaunay triangulation and KD-trees. My goal is not to learn how to write JavaScript apps, it's to learn the algorithms themselves.
