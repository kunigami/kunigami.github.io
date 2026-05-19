---
layout: books
title: "Elbow Room"
vanity: elbow-room
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{site.url}}/resources/books/elbow-room.jpg" alt="Book cover" />
</figure>

*Elbow Room: The varieties of free will worth wanting* is a book by the philosopher Daniel C. Dennett which investigates the age-old question of whether free will truly exists, in light of scientific discoveries and the assumption of determinism. The term "elbow room" means "wiggle room" to convey some degree of freedom.

He claims that free will exists, irrespective of whether determinism is true or false. However, he uses a specific definition of free will, but he argues that it is the one people care about (hence the subtitle "the variety worth wanting").

## Topics

### Why Do We Care?

Normal people typically don't spend time philosophizing but many seem to care about the question of free will. He argues that's because people associate free will with control, and people fear being controlled.

### Determinism

The *Laplace's Demon* is a thought experiment, where an all-knowing being knows the exact position and velocity of every particle in the universe at any given time. He knows all the laws of physics. This demon can predict with certainty the future. Now this demon doesn't need to exist. But it implies that the world is fully determined.

Quantum mechanics changes the picture because of the Heisenberg uncertainty principle in which a particle's position is actually in a superposition state (this is irrespective of observation), so because positions are not fully determined, determinism doesn't exist. There are interpretations that preserve determinism such as the many-worlds interpretation in which all possible paths are taken, so for any path taken the world is fully determined (see *Pseudo-Randomness* and *Past Determinism*).

### Compatibilism

Is the idea that free will can exist even if determinism is true.

### Nihilism

One worry about lack of free will is that people will assume a nihilist attitude: "nothing matters", so they won't try to make an effort or make decisions, because after all, everything is determined anyway, so what's the point?

In practice however, we're biologically wired to take action. So even if we believe free will doesn't exist, we'll not act on this belief for too long.

### Automata: Sphex

The [sphex](https://en.wikipedia.org/wiki/Sphex) is a type of wasp, which looks like a free-willed animal. However, once its environment is disturbed scientists realize it operates in a very deterministic way, almost like an algorithm or an automaton. Its behavior was shaped via natural selection over many generations to adapt to its environment. Most people would agree that the sphex does not have free will.

We're clearly smarter than a wasp, we can definitely react to changes in our environment within one generation, but are we of a different *quality* such that we are also not completely determined by our environment?

### Control

Having control is the type of "free will" that Dennett thinks is worth having. A system has control if it can represent alternatives, perform simulations (evaluate consequences) and adjust behavior based on feedback.

Humans have higher-order levels of control: we can make decisions that will give us more options in the future. For example, an airplane pilot might make maneuvers to avoid turbulence, since once inside one he has a lot less freedom.

I think this version of free will hinges on our higher-order capabilities. We run software inside our brain (computer), whereas a sphex is more like running on rigid hardware (calculator).

### Amplifiers

He proposes a fascinating characterization that information processing systems are amplifiers: small energy input signals (triggers) get amplified. One example is a photon entering the retina of our eyes causing us to perform a much more complex (informationally denser) action.

### Randomness and Pseudo-Randomness

If determinism is true, then there's no true randomness, because every process can be, in theory, "reverse engineered" in a deterministic way. So only pseudo-random processes exist, much like in programming / algorithms.

Even if the quantum mechanics interpretation that there are truly random processes (which cannot be determined upfront) is right, we don't have control over the outcome of these probabilistic states, so it's largely irrelevant whether true randomness exists.

This argument made me change my view about determinism and free will. If we are to assume the world is subject to the laws of physics as we know them, then whether the world is determined reduces to the question of whether true probability exists. So practically determinism is orthogonal to free will.

One interesting characterization of random vs pseudo-random processes is that the latter is informationally compressible: you can just store the (finite) code for the pseudo-random generator. A true random generator cannot be stored in any way.

A fascinating argument for why we use randomness against nature is to avoid using some pattern that happens to coincide with the thing you're trying to measure. I have never heard this argument before but it helps make intuitive sense of why [probabilistic algorithms](https://www.kuniga.me/blog/tags/) work well in practice.

### Past Determinism

Even if we believe determinism is not true, the path taken that led us to the present is (in hindsight) fully determined; you cannot change the past. So a pre-determined world is one that is determined (at least) up to the next instant of time $t + 1$, but irrespective of that, the world is fully determined up to the instant of time $t - 1$.

I found this framing very interesting: the difference between a pre-determined world and a non-determined world is quite small!

### Blindspots

One interesting point raised by the author is about blindspots when we do introspection. At some point we can't see further and start to make things up. That's why we cannot answer the question of free will just by introspection.

> We have to wait to see how we are going to decide something, and when we do decide, our decision bubbles up to consciousness from we know not where.


## Conclusion

As I mentioned, the book changed how I view determinism and free will: whether determinism is true has little practical importance on whether we have free will, and the sharpest argument for me, as a computer science person, is the one between true random generators vs. pseudo-random generators.

I still think that metaphysically free will doesn't exist. Even if determinism is false, the source of randomness is so *random* that we cannot attribute any will or agency to it. Also, the causation chain is so unpredictable and subject to the laws of physics that even if an agent had control over them, it would have little control over the outcome. So for practical purposes, we can consider the probability associated with quantum uncertainty as a neutral source, and from there the causal chain is fully determined.

I also believe that in practice my beliefs don't matter and I will continue to act as if I had a choice, possibly because I'm biologically wired to do so.

Dennett argues that free will exists by using a specific definition of free will, one in which agents operate on a high level of thought. They can consider options, learn, make simulations about the future, be flexible, etc. This seems to operate on a different level of abstraction than the physical world, much like software operating on top of hardware. With this definition too, many AI agents would be considered to have free will, and perhaps this is on purpose. The term *agent* implies *agency*.

There are many interesting discussions in the book and I really value that he takes into account modern science, including theoretical computer science, information theory and artificial intelligence, which makes the arguments a lot more grounded.
