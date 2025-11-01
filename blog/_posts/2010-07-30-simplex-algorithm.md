---
layout: post
title: "The Simplex Algorithm"
tags: [combinatorics]
vanity: "2010-07-30-simplex-algorithm"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/dantzig.jpg" alt="Portait of George Dantzig" />
</figure>

[George Dantzig](https://en.wikipedia.org/wiki/George_Dantzig) (1914-2005) can be considered the father of linear programming, since he invented the Simplex method, the first algorithm for solving linear programs. In its worst case, the simplex algorithm has exponential complexity as a function of the input size, and polynomial algorithms have been developed to solve linear programs, such as the interior-point method.

In practice, however, the simplex algorithm performs very well. Furthermore, it possesses characteristics that make it ideal for branch-and-bound algorithms, which are used to solve integer linear programs.

## Solving open problems

Legend has it that Dantzig once arrived late for his first day of statistics class at Berkeley University. When he arrived, he saw two problems written on the board and, thinking they were homework assignments, wrote them down in his notebook.

After a few days, he handed the exercises to the professor and apologized for taking so long to solve them, as they were more difficult than usual. The professor didn't seem to care much at the time, but later revealed to Dantzig that he had solved two open-ended problems in statistics.

## Good Will Hunting

The movie [Good Will Hunting](https://www.imdb.com/title/tt0119217/) was inspired by this story. For those who haven't seen it or don't remember, Will Hunting was a university janitor, a troublemaker with a criminal record, and a mathematical genius. One day, professors were discussing a theorem on a blackboard, and after leaving, they left the theorem's statement written down.

The next day, the theorem is solved by an anonymous author. Later, they discover it was the janitor, and the film unfolds with the professors trying to convince Hunting to study mathematics and seek a psychologist to curb his impulses. It's a movie I really enjoyed and recommend.

<figure class="center_children">
  <img src="{{resources_path}}/PBF237-Lyles_Constant.gif" alt="See caption." width="450" />
  <figcaption>Figure 1: A parody of the movie, by the <a href="https://pbfcomics.com/comics/lyles-constant/">Perry Bible Fellowship</a>.</figcaption>
</figure>

## Nobel Prize

In 1975, economists Tjalling Koopmans and Leonid Kantorovich were awarded the Nobel Prize for their theory of [optimal allocation of resources](https://en.wikipedia.org/wiki/List_of_Nobel_Memorial_Prize_laureates_in_Economic_Sciences), which was essentially linear programming. Unjustly, Dantzig was not awarded the prize.

## Notes

This is a review and translation done in 2025 of my original post in Portuguese: [George Dantzig e o Algoritmo Simplex
](https://kunigami.blogspot.com/2010/07/george-dantzig-e-o-algoritmo-simplex.html) from 2010. I omitted the part about Integer Linear Programming constraints as it didn't fit the title.
