---
layout: post
title: "On Lifetime"
tags: [opinion]
vanity: "2023-07-10-on-lifetime"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/lifetime.png" alt="AI Generated image depicting Lifetime" />
</figure>

There were several topics I ran into recently around the theme of *Lifetime* but in different contexts. I myself had thoughts on this regarding this blog, so this post is a collection of commentaries on these topics.

<!--more-->

## Lifetime of Technologies

Recently, Matt Watson's *My 20 Year Career is Technical Debt or Deprecated* [1] was featured in the front page of HackerNews. In the post, Matt makes the case that everything will be eventually technical debt and cites several examples of technologies that fell out of fashion, including Visual Basic, Fortran and Perl.

I spent some time thinking about why some languages and technologies last longer than others and came up with some traits that might have an influence.

### Frameworks

This includes frameworks that were mentioned in his post such as Java Applets, Angular JS, Ruby on Rails and Flash, but also currently popular ones like Django and React.

Frameworks solve a narrower use case than general purpose programming languages, which means that the use case can outgrow the scope of the framework or even cease to exist. In a similar vein, frameworks are also less evolvable, because they make a lot more assumptions about the world.

Finally frameworks also bring a lot of baggage and the lack of flexibility to optimize them, could make them less scalable and force big players to ditch them.

### Age

In the book *Algorithms to Live By* [2], the authors suggest that a good guess for the lifespan of something we have no information about is twice its current age. This is also the idea behind *exponential backoff*.

A lot of the deprecated technologies mentioned in the article are web technologies which are relatively new. Contrast that with older ones like C++ (1985), Java (1995) and Python (1991) which are still going strong.

Mobile technologies are even newer, so we could expect many of existing ones to disappear in the near future.

### Powerful Sponsors

Big companies like Google, Meta and Microsoft have a lot of resources to spend in improving and evolving languages they use, which is why C++, C#, Java and Python are likely to stick around as long as there are companies investing on them. Companies also sponsor conferences, which helps building community.

### Evolvability

We touched upon lack of evolvability in frameworks, but a language might fail to catch up with new paradigms and user needs by inertia, lack of investment, difficulty in maintaining back-compatibility, achieving consensus amongst its committee, etc.

C++ had a pretty long hiatus to get to C++11 but it has had more frequent updates since then. This trend of reduced updating cycles is also being followed by Java and Python.

Cobol is a language that doesn't get updated very often and it regarded as a non-popular language. However, C is also not updated that often but is still considered popular by some measurements, especially due to Linux and embedded systems.

### User Friendliness

I find that interpreted languages like JavaScript and Python are more approachable than system languages like C++ and Rust, and that languages with different paradigms such as Haskell and Prolog are very difficult to pick up. That might contribute to language popularity.

If it wasn't for performance and safety constraints, such that there would be less tradeoffs to be made, I'd imagine we would have even fewer dominant languages today.

### Historical Contingency

In *Why Nations Fail* [3], Acemoglu and Robinson make the case that rich nations today are so because they got lucky early on and ended up "on the right side of history" and that once you start ahead, the advantage compounds. They call this phenomenon **historical contingency**. So perhaps we should not make any assumptions on specific language features to explain their success or lack thereof.

A lot of the points around *Powerful Sponsors* and *Evolvability* are a chicken-and-egg problem: the reason there are sponsors and people willing to spend resources in evolving a language is because they are widely used.

I wonder what caused these languages to start ahead in the first place. It might have been something small like a pioneer college offering programming classes choosing a specific programming language to teach undergrads, the founder of a now big company having more experience with this or that language or maybe Sun Microsystem (Java) having a stronger sales team than Borland (Delphi).

## The Lifetime of this Blog

When I first started writing web pages back in college, I had this plan of writing interactive apps for algorithms. Computational Geometry algorithms are very suitable for this purpose since they're discrete and often in 2D, so I did it for the *Ear clipping* polygon triangulation algorithm and the *Jarvis march* convex hull algorithm.

I wrote them in Java Applets, which is now a defunct technology, so these applets are no longer accessible. I have a strong preference for building things that last, so I'm now very deliberate about choosing technologies that I think have a good chance to last a long time.

Doing some analysis on my blog, these are the following dependencies in rough order of my assessment of their lifetime:

* External Links
* GitHub Pages
* Jekyll
* JavaScript Libraries
* JavaScript
* Browser
* Image formats
* Markdown

**External links** are most likely to be broken since people change domains, restructure their URL or simply delete the page without putting any migration in place. I mostly use external links as reference and they should not impact the content of the post, since they are in theory self-sufficient. We can think of dead links as reference to books that went out of print.

**GitHub Pages** hosts my site and integrates with *Jekyll* by running it on markdown sources in my repository. Even if GitHub itself lasts a long time, it could decide to shut down pages at any moment. My hope is that porting static pages to a self-hosted environment wouldn't be too hard, but it would sure make deployment more cumbersome.

The main dependency on **Jekyll** is its DSL, including variables, loops, special tags, and its engine that converts markdown to actual HTML pages. I use it on pretty much every page but in a very limited and consistent way, so it would be not impossibly hard to migrate. The fact it's open-source makes me trust it will last even in some maintenance mode form.

For **JavaScript Libraries** I mostly rely on *React* and *Babel* in some of my custom pages, such as in [A Puzzling Calendar
](https://www.kuniga.me/blog/2022/05/28/puzzling-calendar.html) post and the [Bulls and Cows Solver
](https://www.kuniga.me/bulls_and_cows/) page. I use fixed versions from ages ago, so back-compatibility doesn't seem a risk, but they could end up dropping these projects from the CDN. Fixing them to use plain JavaScript would be some work.

**JavaScript** to me is like the assembly of the web. Every framework currently has to "transpile" to it and even WebAssembly requires the loading to be done via JavaScript. The cost of back incompatible changes seems too high to me so I would bet on it being stable for a long time.

I'd be surprised if **browsers** went away any time soon. It has stood the test of time even against the app-centric mobile paradigm. I don't know if **Image formats** comes before of after the browser. On one hand, image formats like PNG and JPEG are used outside the web, but on the other, browser vendors might decide to move on from these formats towards more modern ones such WebP, but I doubt they would stop displaying the legacy formats.

**Markdown** as a format is very simple and self-contained, so the chances of it requiring changes seem very remote to me. There's a chance *Jekyll* moves away from *Markdown* but I'd count that as a *Jekyll* dependency.

## Lifetimes beyond

I read two book recently, *Lifespan* by David Sinclair [3] and *Brief Answers to the Big Questions* [4] by the late Stephen Hawking. Sinclair's book focuses on extending our lifespan beyond our ~80 years old and eventually allowing humans to live theoretically forever by not aging. Hawking's book on its turn touches on the lifetime of our solar system and the Universe.

What would be my assessment of the lifetime of these things?

* Me
* Blog
* Humanity
* Solar System
* Universe

I put my blog after my own lifetime because as of now I intend to keep its lights on for as long as I can. I wonder how long until I'm gone would the blog stay up. The first thing that would fail is the domain name after I stop paying for the registration, but I could switch back to the default Github pages domain which is free. So in theory my blog could keep working until one of the dependencies mentioned in *The Blog's Lifetime* breaks it. My blog is entirely open source so a crazy person could keep it alive for a little longer.

By definition I cannot outlive humanity, but these days I keep wondering if we'll go away together. Mass extinction risks include nuclear war, irreversible damage from climate change and AI [6].

The next question is whether humanity should come before or after our solar system in the list above. Hawking says that the lifetime of our sun is 9 billion years and he estimates it already lived for 3.5 billon years, so we have about 5.5 billion years to find another home. He suggests that the closest planet in the habitable zone is *Proxima b* which is 4.2 light-years away. With the fastet manned rocket (Apollo 10) clocking 40,000 km/h, it would take a humans about 27,000 years to get there, assuming we figured out the fuel part.

A more sci-fi alternative would be to upload our brains to computers and make sure they can keep running. This could expand the "habitable" zone and transporting "humans" would just involve sending data over.

The current theories about the future of the Universe is that will not support any possible organized information, including any form of life or computers as we know it. As they say it, it's not a matter of "if" but "when". Egan's Permutation City [7] envisions a way out of it, basically having computation happen without computers. It's a pill that is hard to swallow but the idea is intriguing and I'd recommend the book.

And yet, in face of the apparent pointlessness of all this, I still find myself motivated to build things, leaving things better than I found and cheering for this miracle that is humanity to extend itself as far as possible into the future.

## Conclusion

In this post we started reflecting on the lifetime of technologies and then applied it to the context of this very blog. The third part is quite gloomy but I hope my own perspective on the last paragraph offers some solace.

While studying science and learning about things such as the end Universe, I remember being disappointed about the fact that nothing lasts forever. It didn't affect me negatively much more than that though and introspecting on my own reactions, I was surprised about it. I think our lizard brains aren't equipped to process timescales larger than a couple of generations and cannot comprehend or accept these facts.

## References

* [[1](https://blog.visionarycto.com/p/my-20-year-career-is-technical-debt)] My 20 Year Career is Technical Debt or Deprecated, Matt Watson.
* [2] Algorithms to Live By, Brian Christian and Tom Griffiths
* [3] Why Nations Fail, Daron Acemoglu and James A. Robinson
* [4] Lifespan, David Sinclair
* [5] Brief Answers to the Big Questions, Stephen Hawking
* [6] Superintelligence: Paths, Dangers, Strategies, Nick Bostrom
* [7] Permutation City, Greg Egan
* [8] Letters from a Stoic, Seneca
