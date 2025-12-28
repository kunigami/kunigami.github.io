---
layout: books
title: "The Music of the Primes"
vanity: the-music-of-the-primes
---

{% include blog_vars.html %}


<figure class="image_float_left">
  <img src="{{site.url}}/resources/books/the-music-of-the-primes.jpg" alt="Book cover" />
</figure>

## Summary

This book covers the history of the quest to understand prime numbers, in particular the Riemann hypothesis. This hypothesis is connected to the prime number theorem which states how many primes there are up to $n$, commonly denoted by $\pi(n)$.

If this number could be computed efficiently we would also have an efficient way to determine where a number is prime by determining if the difference $\pi(n) - \pi(n - 1)$ is 0 or 1.

Du Sautoy uses the Riemann hypothesis as a backbone to cover the history of modern mathematics and computers, because many of the towering figures on these fields at some point tried to attack this problem.

## General Timeline

While generally following events chronologically, the book sometimes cover topics out of order or some unrelated ones, usually to provide some background to people mentioned in the book.

I wanted a list of major events around the understanding of prime numbers to have a better overview of these events. The following is an attempt at that:

| Date Publicized | Who | What
| -    | -   | -
| 300 BC | Euclid | Proved that there is an infinite number of primes. The proof consists in multiplying all primes up to `N` and add one, and show that this number is not divisible by any of these numbers. This method is not the most efficient for generating large primes because you need to know all primes up to `N`.
| 1640 | Pierre de Fermat | Fermat's Little Theorem: $2^{n-1} \equiv 1 (\mbox{mod} n)$ is prime if $n$ is prime. Gauss later found that if $n$ is not prime the converse does not always hold by finding a counter example, 341.
| 1644 | Marin Mersenne | Conjectured that $2^n - 1$ is a prime number for $n = 2, 3, 5, 7, 13, 17, 19, 31, 67, 127, 257$ and there are no prime numbers of this form for other $n \le 257$. This conjecture has been proven wrong rather recently. It was found that $2^n - 1$ is prime for $n = 61$ in 1883, $n = 89$ in 1911 and $n = 107$ in 1914. It was shown that $2^n - 1$ is *not* prime for $n = 257$ in 1927.
| 1797 | Adrien-Marie Legendre | Based on experimental data, conjectured that $\pi(n)$ is approximated by $n / (a \log a + b)$ for some $a$ and $b$, later in 1808 refined it to $a = 1$ and $b \approx 1.08366$. This is the early version of the **Prime Number Theorem**.
| 1849 | Carl Friedrich Gauss | In a 1849 correspondence, Gauss claimed he defined $Li(n) = \int_2^n dt / \log t$ as an approximation for $\pi(n)$. The question is how accurate is the approximation, i.e. what's the error function $\epsilon(n) = \abs{Li(n) - \pi(n)}$.
| 1850 | Pafnuty Chebyshev | Showed that if $\lim_{n \rightarrow \infty} \pi(n) \log n / n$ exists, it must be equal to 1. This is known as the **Asymptotic Prime Number Theorem**.
| 1859 | Bernhard Riemann | Conjectured that the zeros of the zeta function all have $1/2$ as the real part. This is the **Riemman Hypothesis** (RH). The line $x = 1/2$ is known as the **critical line**. He also found Gauss' approximation is much tigher if we assume the RH is true: $\pi(n) = Li(n) - \sum_{\rho} Li(\rho) + \delta(n)$. Here $\rho$ are the non-trivial zeros of the zeta function and $\delta(n)$ is a tiny complicated correction factor, but a known one.
| 1896 | Hadamard and de la Vallée Poussin | Proved the asymptotic prime number theorem.
| 1900 | David Hilbert | Posed his list of 23 unsolved problems in mathematics in a lecture at the International Congress of Mathematicians. The lecture broke with tradition: he was expected to discuss some results rather than open problems.
| 1910 | Srinivasa Ramanujan | Devised formulas around primes that reassembled the zeta function. Sent letters to Hardy and Littlewood and was invited to Trinity College.
| 1914 | G. H. Hardy | Showed that there are an infinite number of zeros in the critical line.
| 1939-1953 | Alan Turing | Devised algorithms to calculate zeros of the zeta function.
| 1940 | André Weil | While in prison he proved an analogous version of the RH, for curves over finite fields. Developing the new field of algebraic geometry in the process.
| 1948 | Atle Selberg and Paul Erdős | Found a more elementary proof of the asymptotic prime number theorem. There was some drama because Selberg didn't like to publish join papers which was the diametrical opposite of Erdős' practice.
| 1951 | Kurt Godel | Didn't contribute directly to the RH but his [Incompleteness theorems](https://en.wikipedia.org/wiki/G%C3%B6del%27s_incompleteness_theorems) cast doubt on whether RH can be proved. A interesting observation is that if RH is unprovable it must be true, because for it to be false we just need one counter-example, which would constitute an easy proof.
| 1960 | Alexander Grothendieck | Developed the theory of schemes, to address some conjectures made by Weil around generalizations of the analogous version of the RH for curves over finite fields. It was used by Andrew Wiles to prove Fermat's Last Theorem. It's also hoped that it might provide insights to solving the RH.
| 1973 | Hugh Montgomery | After a serendipitous discussion with Dyson, posed the [Montgomery's pair correlation conjecture](https://en.wikipedia.org/wiki/Montgomery%27s_pair_correlation_conjecture), connecting the distribution of the distance between consecutive zeros in the critical line to some distribution found in quantum mechanics.
| 1976 | Miller and Rabin | Miller came up with a deterministic primality test that depends on the RH. Rabin later modified it to remove the dependency, but making it probabilistic. This is known as the **Miller-Rabin primality test**.
| 1977 | Rivest, Shamir and Adleman | Came up with the RSA encryption protocol which relies on the difficulty of factorizing primes, taking the primes from the realm of pure mathematics to become the foundation of internet security. Fun fact: Alderman was a mathematician who didn't think he had contributed enough, so initially didn't want his name in, but finally agreed to have his initial be put last.
| 2002 | Agrawal, Kayal and Saxena | Came up with the first deterministic primality test not relying on the RH.
| 2008 | Berry and Keating | Stated the Berry–Keating conjecture which connects the RH with quantum chaos.

## Themes

### Machines replacing Mathematicians

When the possibility of a general purpose computing machine was being discussed, some mathematicians were worried that:

> They would effectively put the mathematician out of business. No longer would we need to rely on the imagination, on the cunning, intuition of the human mind to produce clever arguments.

Hardy was skeptical it was possible to mechanize the proof of theorems:

> There is, of course, no such theorem, and this is very unfortunate since if there were, we should have a mechanical set of rules for the solution of all mathematical problems, and our activities as mathematicians would come to an end. It is only the very unsophisticated outsider who imagines that mathematicians make discoveries by turning the handle of some miraculous machine.

The author, also a mathematician, also shows his skepticism towards machines:

> The computer is certainly a powerful new ally in mathematicians' attempts to navigate the world in a sturdy sherpa in our scent of Mount Reiman, but it can never replace the mathematician.

But concedes (my emphasis):

> Although the computer can outstrip the mathematician in any finite computation, it lacks the imagination (**as yet**) to embrace an infinite picture and unmask the structure and patterns underlying mathematics.


Working at a level of abstraction up
191 - instead of machine specialized for computing logarithm write one for general purpose
Same today: software vs AI

### Bodies as Machines

In the chapter discussing computers, this quote from Edwin Tennery Brewster in this book *Natural Wonders Every Child Should Know* is used:

> For of course the body is a machine. It is a vastly complex machine, many, many times more complicated than any machine ever made with hands;

I used this quote in my post about [Complex Systems]({{blog}}/2025/12/20/complex-systems.html).

### Limits of the Human Mind

In one of Selberg's quotes he says:

> (...) Maybe the proof will be so involved that the human brain will not catch up with it.

I often wonder about this. When automated theorem provers like Lean get more powerful, is it possible that it generate a proof that no one can understand? Or is the mathematics required for the proof so advanced that no one can understand?

Will we get to a point that the amount of knowledge a human needs to make any progress would exceed their lifespan? Today a PhD might require 25-30 years to become an expert enough to make a significant contribution. Unless we can build proper abstractions that wouldn't require knowing every single detail, would it perhaps take 50 years to get to the frontier of human knowledge?


### Platonism

Earlier in the book the author discusses the beauty of mathematics, citing the mathematician Alain Connes:

> There exists, independently of the human mind, a raw and immutable mathematical reality.

and that mathematics is:

> unquestionably the only universal language.

The bit about an independent mathematical reality reminds me of *Our Mathematical Universe* by Max Tegmark. In there the author goes even further to claim that our reality (including our main) *is* mathematics.

### Center of learning

The University of Göttingen in the small town of Göttingen was the center of the mathematical universe with professors such as Carl Friedrich Gauss, Peter Dirichlet, Bernhard Riemann, Felix Klein and David Hilbert. It also included the physicists Max Born and Heisenberg.

During his reign, Hitler anti-semitic policies expelled many academics from Germany and destroyed the reputation of German universities, including Göttingen, never to be restored.

The focus point switched to the Institute for Advanced Studies in Princeton, which hosted André Weil, Kurt Godel, Paul Erdős and Atle Selberg.


### Language and Thought

One interesting bit I learned was that André Weil was an ancient language enthusiast. He knew Sanskrit and read the Bhagavad-Gita in the original language.

> Weil believed that the only way to see the full beauty of any text, not just epic poetry, was to read the original. Weil believed that in mathematics too, one should go back and read the regional papers of the masters, not rely on secondary accounts of their work.

Weil is credited to creating the field of algebraic geometry, which provided a new language to express new subtle ideas:

> He believed that the development of new mathematical ideas went hand-in-hand with the development of sophisticated forms of language.

### Layman Language

Speaking of language, one aspect that often got in the way of understanding things more precisely is the language used: many terms I knew were referred to by some more generic terms to make it accessible to a more general audience and I had to keep "translating" what he meant.

For example, instead of saying the complex plane the author uses the analogy of a topographical map and call it landscape. He uses terms such as east-west for the x-axis and north-south for the y-axis and mountains for the absolute value of the zeta function, with zeros called sea-level points.

I understand this cannot be otherwise if this is to reach as many people as possible, but I wished there was some book that assume basic high-school level math such as complex numbers. This was also an issue I found with the book *General Relativity from A to B* by Robert Geroch.


## Conclusion

I had read this book during college, sometime between 2005 and 2010. I remember liking it a lot and vaguely remembered the Riemman Hypothesis and the critical line, enough to want to go back to it after studying [The Riemann Zeta Function]({{blog}}/2025/10/25/riemann-zeta-function.html).

I enjoyed the second read tremendously, and it was a different experience reading it now that I have deeper background in the related mathematical concepts.
