---
layout: post
title: "Horseshoe Theory"
tags: [opinion]
excerpt_separator: <!--more-->
vanity: "2025-08-24-horseshoe-theory"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/horseshoe.png" alt="A horseshoe" />
</figure>

According to Wikipedia [1], the horseshoe theory asserts that:

*"The extreme left and the extreme right, rather than being at opposite and opposing ends of a linear political continuum, closely resemble each other, analogous to the way that the opposite ends of a horseshoe are close together."*

In this post I'd like to share some anectodes and [thoughts]({{blog}}/2024/12/02/centralizing-thoughts.html) on it.

<!--more-->

## Midwit Meme

I learned about the horseshoe theory after reading the [IQ Bell Curve / Midwit](https://knowyourmeme.com/memes/iq-bell-curve-midwit) meme description [2], which also includes:

> ... groups with low intelligence and high intelligence often chose to follow the same goals while being guided by different reasoning.


## Examples

### Physics

The first time I recall noticing this phenomenon was while studying for college admissions. In the study book there was this exercise asking roughly:

*Suppose we have an extremely precise scale. We first weigh 1kg of feathers and then 1kg of iron. Which one will be heavier according to the scale?*

A noob / uneducated person might answer that the iron will be heavier since they imagine lifing some iron and lifting the same "amount" of feathers and the former definitely feels heavier.

Most people (myself included) will notice that both objects have the same mass of 1kg and think that the scale will report the same weight for both.

Someone more experient / attentive might realize that because the iron is denser than the feathers, 1kg of feathers occupy a much larger volume than 1kg of iron. Then, assuming this experiment wasn't done in vacuum, there's buoyance, a force acting on an immersed object that is proportional to its volume. So there's a stronger force acting opposite to gravity on the feathers than on the iron, which would make the resulting force measured by the scale lower and it would report it being less heavy.

The noob and expert would give the same answer but with different reasons! Needless to say this would be a terrible multiple choice question.

### Programming

Programming has lots of options and opinions, so it's a breeding ground for this meme. These are some I think might be relevant.

<figure class="center_children">
  <img src="{{resources_path}}/meme.jpg" alt="See caption." width="450" />
  <figcaption>Figure 1: The midwit meme, include the character names for reference in the table below.</figcaption>
</figure>

<br /><br />

| Simpleton | Midwit | Genius |
| - | - | - |
| No need for comments | Comment every function | Comments gets stale, comment sparingly |
| What's O-notation? | Let's write assembly! | Profile first |
| Monolith | Microservices | Monolith, but extract parts that need scaling or independent deployment. |

### The Golden Mean

I think for programming a higher level pattern is: we start with some bad practices as a noob. We then overcorrect and become zealots. Later we develop a more balanced and nuanced view.

The take from the "Genius" is actually quite different from the "Simpleton", but it is closer to it than the "Midwit", so in exaggerating the differences the meme buckets the "Genius" and "Simpleton" together.

This reminds me of the [Golden mean](https://en.wikipedia.org/wiki/Golden_mean_(philosophy)) in Philosophy.

### Illusory Superiority

Statistically speaking, most people are "midwits". However, those creating or sharing versions of this meme probably see themselves as "genius". Given it is a meme (hence popular), one might think not everyone who shares this meme is a "genius".

This makes me think of [Illusory Superiority](https://en.wikipedia.org/wiki/Illusory_superiority) bias (a classic example is that the majority of people think they're above average drivers).

Another possibility is that the distribution we have in mind is wrong. For example, we may think most people do premature optimization (the midwits) but in reality the number of programmers that don't know O-notation or practice premature optimization is very small, and the majority of the curve have the more balanced view. There might be simply no "genius" take on this.
