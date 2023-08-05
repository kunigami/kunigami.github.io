---
layout: post
title: "Review: Thinking In Systems"
tags: [review]
excerpt_separator: <!--more-->
vanity: "2023-02-11-review-thinking-in-systems"
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/tis.jpg" alt="Thinking In Systems book cover" />
</figure>

In this post I'll share my notes on the book *Thinking in Systems: A Primer* by Donella H. Meadows.

Meadows starts by defining a framework with which to model many real-world events as general systems. She then includes pitfalls with these systems and how we can leverage them to make effective changes.

In this post we provide a summary of each of the chapters with varying degrees of detail. My impression of the book is included in the *Conclusion*.

<!--more-->

## Chapter 1 - The Basics

This chapter defines a system and its components.

In a system we have the **stocks**, representing the quantity of something (material or otherwise), the **flows**, representing a component that can change the stock's quantity. For example, we can model the room temperature as a stock and two flows: the heater flow, one that increases the stock and poor insulation flow, one that decreases the stock.

A third component of a system is the **purpose** or goal. In the above example we could aim to keep the temperature at a specific value and design the system to achieve that.

The model doesn't have to be complete or formal. For example, one of the endpoints of the flow edge might not be included in the system and the stocks on different endpoints of a flow might not be of the same nature, there being implicit conversions between them.

It's possible to change the flow rate in the model. For example in the bathtub case, opening or closing down the faucet can increase or reduce the inflow of water. If the flow rate depends on the stock's quantity itself, we have a loop, which can be either stabilizing loops (**balancing feedback**) or runaway loop (**reinforcing feedback**).

An example of *balancing feedback* is modeling the temperature of a cup of coffee in a room. The coffee temperature is the stock and the flow is the change in temperature. The hotter the coffee is, the higher the rate of its temperature drop. At some point the coffee will cool down to room temperature, achieving equilibrium and the change in temperature (flow) will go to 0.

An example of *reinforcing feedback* is an investment. The more money one invests, the more interest you get and can re-invest, leading to an exponential growth.

Stocks also serve as buffers. This property enables the inflow and outflow to be (temporarily) independent. This is basically the why we use a queue in a (computer) system to communicate between a producer and consumer, since they might have different throughputs at any given time.

One difficulty with systems is that usually only the stock is measurable, while the flow and all its causal agents are hidden. We have even less visibility on the underlying purpose of systems.

## Chapter 2 - A Brief Visit to the Systems Zoo

Provides examples of simple systems.

**Population.** (*Figure 1*) A system with two feedback loops, one reinforcing, the other balancing.

<figure class="center_children">
  <img src="{{resources_path}}/population.png" alt="See caption." />
  <figcaption>Figure 1: Population modeled with two flows and feedback loops. The more people there are, the higher the number of births and the higher the population, so it's a reinforcing loop (R). The more people there are, the higher the number of deaths and that reduces the population, hence a balancing loop (B). Fertility affects the number of births independently in this model.</figcaption>
</figure>

**Car Inventory.** (*Figure 2*) A more complex system modeling the stock of car inventory. There is an additional feedback loop representing human intervention resulting from a forecast. In order to keep up with changes in sales, the agent might over or under order the delivery of cars so that it doesn't end up with too much or to little inventory.

<figure class="center_children">
  <img src="{{resources_path}}/car.png" alt="See caption." />
  <figcaption>Figure 2: Car inventory with two flows and three feedback loops. The more sales there are, the lower the inventory will be. The lower the inventory, the more deliveries from the factory happen. The "Orders" loop indicate a forecast based on sales trends.
</figcaption>
</figure>

One interesting factor is delays. There's an inherent delay on feedback loops but the amount of delay can be further tweaked. In the example above, the forecaster might choose different window lengths to compute the current trends and this will affect the delivery.

Delays can introduce oscilations that increase over time (much like in [resonance](https://en.wikipedia.org/wiki/Tacoma_Narrows_Bridge_(1940))). The idea is that with delays the feedback loop might be acting upon outdated data and correction might be happen in the wrong direction, adding ever more to the value, which then incurs into a even bigger correction.

The intuitive action of reducing the delay in the forecasting can actually make the oscilations worse, depending on how it plays with the other delays in the system.

## Chapter 3 - Why systems work so well

Basically due to three properties:

* *Resilience* - It contains set of feedback loops that restore the system to some equilibrium. This does not imply that resilent system are static, quite on the contrary, resilient systems are dynamic (since they can respond to changing conditions).
* *Self-organization* - The capacity of a system to make its own structure more complex. It produces heterogeneity and unpredictability.
* *Hierarchy* - A hierarchy reduces the amount of information needed to be passed between every pair of notes (in *Graph Theory* terminology, hierarchy is a tree which has fewer edges than a complete graph).

## Chapter 4 - Why Systems Surprise Us

Some reasons include:

* The models are too simplistic and don't capture real world behavior well.
* Our brain is too limited to grasp complexity or non-intuitive behavior.
* The bottleneck of a system (its limiting factor) might change as the system grows or evolves.

## Chapter 5 - Systems Trap

This chapter describes how systems can have perverse outcomes, even when designed with good intentions in mind. Examples:

* **Policy resistance.** When a policy is introduced to change the system towards a direction but it reacts to resist it. One example provided is the war on drugs: when more money is poured in to make illegal drugs harder to enter the country, the result is not necessarily a reduction in traffic but more costly means to get the drugs into the country, higher drug prices and then higher crime rates due to the addicts unable to afford the more expensive drugs.
* **Shifting the burden to the intervenor.** This happens when a solution is provided that doesn't fix the underlying problem. Drug use might provide short term numbing of some undesired state, but dependency on it grows over time. Government subsidizing or over dependency on modern medicine can have this effect too.
* **Rule beating.** "Abiding by the letter but not of the spirit of the law". There seems to be so many examples of this. One recent example I can think of is [p-hacking](https://en.wikipedia.org/wiki/Data_dredging) mentioned in *How Not To Be Wrong*.
* **Seeking the wrong goal.** This is somewhat the opposite of *Rule beating* in which people follow the rule properly but the rule itself is bad. For example, measuring gross national product (GNP) as a positive metric - it only measures throughput, not capital stocks and doesn't necessarily correspond to a good life.

## Chapter 6 - Leverage Points

This chapter discusses what are the knobs we can tweak on a system in order to control it. It goes from the least to the most effective according to the author's perspective.

The gist is that the effectiveness of a lever is proportional to the difficulty of changing it. Changing stocks and flows is relatively easy but not very effective in making lasting changes. Changing the connections between stocks or the goals is a much larger leverage but pretty much impossible to do.

## Chapter 7 - Living in a World of Systems

This chapter makes the case that our world is very complex and hard to understand. It gets a bit vague on how to deal with it (the author uses the term "dance"). Some of the more relatable advice:

* Don't mistake growth for progress - related to *Seeking the wrong goal* from *Chapter 5*.
* *Listen to the Wisdom of the System* - Before trying to change the system, learn what is already in there. This seems to resonate with Jacobs' *The Death and Life of Great American Cities*, which criticizes affordable housing projects that are large new artificial dwellings instead of aiming to revitalize existing communities [2]. I can also resonate with our tendency as programmers to want to re-implement some system from scratch because they think the existing one is bad before spending enough time trying to understand it.


## Conclusion

I liked this book because it's full of examples but at the same time it doesn't feel repetitive. I learned some interesting bits, especially on the effects of delays in the systems and the system traps. I liked the humanistic perspective on system in the last chapter.

I didn't feel like this book changed the way I see things much though. Perhaps I haven't tried to apply these ideas on my daily life. I think the most actionable one could be identifying leverage points that one can only see via the systems framework.

I had trouble summarizing the book by articulating these concepts in my own words, so I have extra appreciation for Meadows' clarity in writing.

### The Realm of Patterns

One thing that this book got me thinking is identifying general patterns or high-level concepts that apply to many day-to-day situations or problems in different domains. To given an example, right on page 3 the author maps some common sayings to a system description, for example:

> A stitch in time saves nine.

Can be described in systems parlance as:

> Because of feedback delays within complex systems, by the time a problem becomes apparent it may be unnecessarily difficult to solve.

I think jargons and sayings encode a lot of higher-level concepts that appear in disparate contexts. For example, if I hear someone say *"I procrastinated buying tickets early and now they're twice the price"*, I can make an association to the saying above and my comprehension of the information contained in that statement is much faster.

I notice this a lot when reading and find that having this collection of concepts and ideas in my mind makes reading faster and easier. And the more knowledge one accumulates of these concepts the better it gets. I started collecting those in a private document but would like to write a post or keep a wiki for my own collection of concepts.

Associations also help strengthening memories and is something [I actively try to do]({{blog}}/2022/12/27/on-memory.html).

## References

* [1] Thinking In Systems,  Donella H. Meadows.
* [2] The Death and Life of Great American Cities, Jane Jacobs.
