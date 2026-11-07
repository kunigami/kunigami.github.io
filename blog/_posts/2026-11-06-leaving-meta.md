---
layout: post
title: "Leaving Meta"
tags: [job]
vanity: "2026-11-06-leaving-meta"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/farewell.png" alt="AI Generated image of a person waving farewell to a company's office" />
</figure>

A few weeks ago I left Meta (prev. Facebook). In this post I reflect on my time there, especially projects and lessons learned. I'll bias towards more recent projects because I remember them in more detail.

I always refrained from talking about my work projects on my blog to reduce the risk of any accidental leak, even though I always worked on internal tools and infrastructure, so nothing particularly confidential. Now I can be more open about the projects I worked on and link back to posts I wrote related to them during the time. This is a long post, detailing 14 years of work.

<!--more-->

## Interview

I did 3 rounds of interviews remotely from Brazil, then Facebook flew me to California for an onsite round. I assumed I had already passed, and that they just wanted to make sure I was the same person that did the online interviews. My rationale was that no company would spend money on an international flight ticket if they weren't sure the candidate was going to join.

As a digression, one nice perk that Meta had was that for long distance flights employees could book business class, no matter your role or level. I flew maybe a handful of times to the offices in London and Tel Aviv.

Anyway, I later learned that the onsite interview wasn't just formalities. I just did not know how much money Silicon Valley was willing to spend on candidates. This misunderstanding turned out to be really good because I was so relaxed that I did the interviews at my best capacity. It would be almost a year before I actually joined because of immigration cycles.

## Data Tools (2012-2022)

### Bootcamp

When I joined Facebook, it had this program called bootcamp. By default, when you started you were not pre-assigned to a team. You had about 4-6 weeks to go through training, try tasks from a few different teams and by the end you were supposed to choose a team.

I think it was a good idea for the newcomers, even more so for new grads, because they might not have any idea of what kind of stuff was interesting to them before they actually try some tasks out. It was not as good for some teams because they might not look as interesting, so they really struggled to fill headcounts.

There were a few defining moments during bootcamp that shaped how I operated during my tenure there. The first was that my bootcamp manager told me on week 2 or 3 that I was not going very fast, and that I should be submitting more PRs.

One of the reasons was that I spent a lot of time debugging things on my own. That was one of the first lessons I learned from coming from University / R&D: In a big company that owns a lot of the stack, you are supposed to ask for a lot more help and sooner.

**Lesson 1.** Ask for help early.

There was a series of posters in the campus and one I quickly saw in practice said "Move fast and break things". This used to be a motto at Facebook before "break" started to have a negative reputation.

But the bootcamp manager's feedback also instilled in me an urgency / anxiety to always be demonstrating productivity and performing well. Meta is a metrics driven company with a high-bar performance review, so sometimes I felt like I ended up not taking as many risks for fear of doing badly.

More consequentially, this ended up influencing the team I chose. I ended up not working on any major backend task because the setup was so cumbersome and slow to ramp up on that I was afraid I was not going to be productive there, and this made me choose a frontend team, even though I initially wanted a backend team. It would be almost 10 years before I gave backend a try again.

Finally, one negative aspect of bootcamp was that a lot of engineers used it to delegate tasks they wanted to get done with minimum effort, so they spent as little time as possible scoping it and were stingy with their help. I recall asking an engineer for help on their task and they replied something along the lines that I needed to figure it out on my own. I think this was eventually fixed by holding task owners more accountable for them. 

The bootcamp process ended a few years before I left.

### Communication

I ended up joining the data tools team, which built internal tools for analytics and observability. One of my first projects was to work on a reporting tool.

I was working closely and being mentored by a really good engineer, who one day told me I should communicate more with my manager about my project. He said that my manager wasn't aware that I was doing hard work and making progress because I was not being very chatty about it.

This was also a lesson I learned early on and carried with me: broadcast partial work, broadcast often. It's not about bragging, it's about keeping others up-to-date. Don't expect anyone to proactively ask what's on your mind.

**Lesson 2.** Share progress often. Advertise your work.

I also got feedback from my manager that I should also look into helping with projects that were not my main project. This influenced me to a point that towards the end of my tenure I got opposite feedback, that I worked on too many projects at a time.

### Digression: The blog

There is an interesting reflection on the blog during this time! I like to say I've been blogging once a month on average since 2010, which is statistically true, but it also makes it sound like I blog *regularly* every month. This is true nowadays, but it wasn't the case in 2012-13: I didn't blog between October 2012 and April 2013, which coincides with my first 6 months at Facebook!

I was so busy trying to keep my head above (the perceived at least) water and figuring life out in a new country that I didn't have time to do anything else.

### Dashboards

My first large project was to create a new dashboard tool. We decided to make it very general so it was easy to add new queries and visualizations to it, making it very expressive, but also not super easy to use. I loved to find thee very creative ways non-SWEs used the dashboard primitives to build tools. It also had an intermediate JSON representation that would have made it pretty useful for agents today.

I worked on this tool for a long time (maybe until 2017?) with two other teammates, but it eventually got moved to a different office and started being supported by an entire team. Over time, as this tool got more popular, it turned more into a proper product, so a lot more user-friendly, but also less flexible.

For the longest time, this was the only officially supported dashboard tool at the company, even when it had over 50k employees and this let us save money from licensing tools like Tableau. I think a good part of why it got widely adopted was that we supported both observability and analytics use cases, and that we had a power user who championed it for us when we were starting. In return, we implemented a lot of features for this power user and his org.

**Lesson 3.** When starting, [do things that don't scale](https://www.paulgraham.com/ds.html).

By the time I left the company, this tool was still used, but quickly being replaced by vibecoded UIs. Also, the founder of [Apache Superset](https://superset.apache.org/), Maxime Beauchemin, was a colleague at Facebook and a power user of our tools. I like to think it had some influence over Superset.

### Better Engineering: Reusable Libraries

I've always been a proponent of better engineering, even though I often did this to a fault. I'm glad that Facebook/Meta was supportive of this kind of work.

One of the most impactful results of doing this refactoring was extracting parts of the dashboarding tool into reusable components. Even after the project moved teams, I continued working on the connector layer, and eventually created a standalone React widget which could be embedded in any internal tool.

Last time I counted, before changing teams, there were over 400 usages of it internally. Even more recently when I vibe-coded a dashboard for myself, the agent used this widget without me asking explicitly for it!

### Migrations

I did a lot of migrations during my time on the team, especially to refactor tools owned by our teams to use the widget infra mentioned above. One of the major mistakes we made was to bundle "improvements" in them as we were rewriting the tools. This created unnecessary pushback and slowed down progress considerably.

**Lesson 4.** For UIs, do not club refactoring with changes.

One playbook we developed with migrations was to ask for feedback when people opted out of the new UI. One surprising thing we learned is that when people think feedback is anonymous they go unhinged. We had to explicitly indicate on the forms that their name would be recorded, because the feedback we were receiving was unprofessional to say the least.

### Hack-a-month

One interesting program that Meta had was the opportunity for employees to try a different team for a month to determine whether to switch teams or not. I did four of them over my tenure but only switched teams once!

The most interesting one was doing a hack-a-month in Tel Aviv. I had no plans to switch to that team, but they worked in a similar space and I thought it could be a good idea to use this opportunity to strengthen the collaboration between the different offices. I got an aparthotel and traveled around during weekends.

<figure class="center_children">
  <img src="{{resources_path}}/jerusalem.jpg" alt="See caption" style="width: 600px" />
  <figcaption>Jerusalem, which I visited during a weekend trip.</figcaption>
</figure>

Serendipitously, the team that took over the dashboarding tool was also based in Tel Aviv, so I ended up working with folks there (as a sister team) until I moved teams.

### SQL Notebook

During the pandemic, to reduce the strain on employees figuring out things due to lockdowns, Meta decided to not have a peer feedback cycle that half. I took the opportunity to work on a more risky project: creating a notebook-like tool but that was based on SQL and had spreadsheet semantics, i.e. if you run a cell B that depends on cell A, it also reruns cell A.

This tool ended up being very successful and used across the company to the day I left. I wrote a post about it on Meta's [Engineering Blog](https://engineering.fb.com/2022/04/26/developer-tools/sql-notebooks/) and another on my own [blog]({{blog}}/2022/05/12/sql-notebooks.html). 

Another motto we had in posters all over the campus was "Be bold". I should have heeded this motto more, because every time I took some risk it turned out to be good.

### Backend

Our team had a backend service to generate images from charts, using Chrome's [Puppeteer](https://developer.chrome.com/docs/puppeteer). It was a service implemented in Python and created by a senior team member who had left the company. It was a bespoke project that no one on the team wanted to maintain because people on the team were frontend leaning engineers and this involved dabbling with services, builds, and releases (our frontend stack was so well abstracted that frontend engineers never had to deal with any of this).

Anyway, I decided to support this service and eventually became the PoC. In the process, I learned a lot about backend development which would later help me transition teams. At Meta we also had this poster saying "Nothing is someone else's problem", which I tried to embrace.

Another positive but unexpected outcome was that once I was trying to find cancellations for campgrounds in Kalalau in Hawaii and decided to set up a system to monitor the reservation system website to alert me for cancellations. I ended up using Puppeteer + an SMS sender, and it worked!

<figure class="center_children">
  <img src="{{resources_path}}/kalalau.jpg" alt="See caption" style="width: 600px" />
  <figcaption>Kalalau trail in Kauai, Hawaii. I'll never forget about the way we managed to get a permit. This was before agents made this too easy.</figcaption>
</figure>

**Lesson 5.** Be open to broadening your scope and persevere in unfamiliar territory. You can't predict whether something you learn will be useful later.

### Transition

In [2022]({{blog}}/2023/01/01/2022-in-review.html) I decided to switch teams. As I said, I had wanted to do backend development for a long time, but I had always been afraid to make the jump. The pandemic disrupted a lot of things and made me rethink my values and aspirations, and I was willing to change companies to try something new.

I was very lucky that my manager at the time was receptive to this idea and helped me find a team and the manager of the stream processing team was willing to take in a frontend guy! So I went from doing local, frontend development in JavaScript to a massively distributed real-time data processing system in C++ and somehow I did not get fired in the process!

## Stream Processing (2022-2026)

### Ramp up

One of the first realizations I had was that for distributed systems, development happens mostly locally, so it's not much different from single-node programming. Before, I had the impression that it was super difficult, requiring running things in the cloud and whatnot. You do need to think about how work is distributed across multiple nodes, but this is a different level of abstraction, and we typically don't have to mix them.

Despite that, a stream processing engine *is* very complex, even on a single node and ours was especially so because it grew organically over many years, so it had a lot of quirks. There were also many components, so while an individual can ramp up on a few components, knowing how all components work in detail was really hard.

Compared to my frontend project, where I felt like I knew all the corners of the system, on this new team, I had to rely a lot more on the teammates. I talked about this in [On Complex Systems](https://www.kuniga.me/blog/2025/12/20/complex-systems.html).

One way to make up for my lack of backend experience was to become the full-stack engineer. I helped with the UI for debugging, CLI tools and other efforts like build time and code quality. In hindsight I think I did too much of this instead of spending time deepening my expertise.

One thing that was extremely valuable was to read technical books in particular [Effective Modern C++](https://www.kuniga.me/blog/2022/10/25/review-effective-modern-cpp.html) and [Streaming Systems](https://www.kuniga.me/blog/2022/07/26/review-streaming-systems.html).

### Stream Joins

I worked on a few major projects during this time. The first was implementing stream joins which was a missing feature needed to migrate away from an old system. The guarantees from this feature are pretty bad and I'm not proud of it, but we copied the semantics from the legacy system, and we were rushing to finish the migration, so I think it was the right call at the time.

Algorithm-wise, this was likely the hardest project I ever worked on. To give an idea of the complexity, the feature from the old system even had a paper written on it [Providing streaming joins as a service at Facebook](https://dl.acm.org/doi/abs/10.14778/3229863.3229869). I also read a paper on Google's [Photon](https://www.kuniga.me/blog/2022/09/27/photon.html) which implements a stream join but it's for a more narrow use case, so it wasn't that useful.

After addressing this last feature gap I also helped deprecate the old stream processing system. The work itself wasn't too complicated. The major challenge was accounting for small semantic drift to validate it was acceptable. The other issue was that validation took too long - there were some frustrating occasions where I would wait a day for the data to be captured, only to realize I had messed up some parameter and had to redo.

### Plugin System

Then I worked on a project to offload user code from our main binary, both to keep it leaner (helping with build times, binary size) but also to move ownership of the user code to the proper stakeholders, to reduce operational toil. We ended up using inter-process communication with Unix domain sockets. I did a post on several options for [implementing IPC](https://www.kuniga.me/blog/2024/12/07/local-ipc.html) and the study helped me understand the trade-offs.

The idea is relatively simple, but because we were also migrating existing projects to it, we ran into all sorts of scalability issues because of the overhead. We had to make many improvements to fix bottlenecks, tune parameters, recoup costs due to overhead elsewhere and in the end we had to keep some user code in our binary because they were so simple that the overhead of IPC was too much. In summary, it wasn't a slam dunk win but we did accomplish the goal to reduce operational toil, and I learned a lot about systems during this time.

### AI

Late last year there was a big shift in how we work due to AI. I wrote about it in [Coding with AI]({{blog}}/2026/02/14/on-ai.html). It directly affected my day-to-day work and the types of projects I ended up choosing. 

My last major project was to adopt Meta's [Presto](https://prestodb.github.io/)'s new compiler, [Axiom](https://github.com/facebookincubator/axiom), into our stream processing engine. This seemed like a good candidate for AI, because we had an existing legacy compiler and the list of all the pipelines that we needed to support.

The idea was then to have the AI agent find the code to generate the same output as the legacy compiler was doing for all the pipelines. We hoped that with this closed deterministic feedback loop AI would eventually converge onto something. I let it chug along for a few months, occasionally checking in, and initially it was making a lot of progress. After handling about 50% of the pipelines (the easiest ones), it started to get stuck.

When I took a deep look at the code, it was a complete mess. I ended up having to rewrite most of it, this time, with a lot more handholding. Unfortunately, I didn't finish this project before I left. It was a massive undertaking with no obvious short-term impact and we only signed up for it because we overestimated AI's ability to self-drive on this, so I ended up losing motivation. I also got a bit burnt out from the frustration of fixing the AI mess and having to redo much of the work.

**Lesson 6.** The bitter lesson learned here is that we can't simply trust AI, especially for complex codebases like this. However, I need to make sure to revisit my priors often because models improve rapidly. 

If I were to restart this project I'd try to make progress incrementally by adding one feature at a time and gradually push it to production instead of trying to cover all possible pipelines before doing that, and also take time to study the codebase (see [On Doing Things Manually]({{blog}}/2026/08/01/on-doing-things-manually.html)).

**Velox.** AI led me to explore a lot more big bets. One of them was to adopt Presto's engine called Velox in our stream processing engine. Velox is open source, so I wrote a [series of posts](https://www.kuniga.me/blog/2026/08/04/velox-memory.html) to learn more about it. I got a promising prototype but didn't have bandwidth to push this forward. This is another project that could significantly reduce our team's surface area long term and lead to open-sourcing, but it was harder to justify in the short term.

**Tracer.** Another project an intern in our team and I explored with AI was a tracer for our engine. It was aware of [Folly coroutines](https://www.kuniga.me/blog/2025/06/18/folly-coroutines.html) and had a Gantt-chart visualization. I didn't use it long enough to claim it was very useful, but it was motivated by a debugging instance which took me (and Claude) hours to find but was clearly visible in this tool. This made me realize I'm very interested in working with systems and performance optimization! I'll have more to say about these in the future.

**Cache Tuning.** One last thing I did before leaving was to use AI to find some wins from tuning cache sizes. The automation was mostly handled by AI, so it took me only a day or so of dedicated work but the savings were enough to pay my salary for a year! 

I did study the architecture of our [cache library](https://www.kuniga.me/blog/2026/08/28/cachelib.html) but it was AI (Claude) who found and proposed the improvement after debugging an OOM issue in production. The only motivation to study it was to understand the proposal and why it helped.

Before AI, I used to get some many OOMs during oncall that it was infeasible to go debug them. We typically waited for our auto-scaler to scale horizontally or vertically, but this of course could sweep inefficiencies under the rug. With AI I could fire an agent to go debug while I dealt with other fires and sometimes it found gems like the one above.

**Lesson 7.** Take advantage of AI to get to the bottom of (more) things.

One reflection here is that in the past I really hated tuning configurations because it was a very tedious process and it didn't lead to [durable artifacts](https://www.kuniga.me/blog/2023/07/10/on-lifetime.html). So I much preferred writing and landing code. With AI, not only can it automate most of the process, including experimentation, but since I don't write code anymore by hand, I'm not as attached to landing code.

### Impact vs. Growth

Another thing that I realized looking back on my entire trajectory is that I feel like I had more impact when I was doing data tools.

Part of it is that Facebook was way smaller back then. I think it was much easier to have impact / own bigger chunks as an individual. Relatedly, there are a lot more green fields: I created 2-3 projects from scratch, and was behind a lot of the decisions and had most context about the project. I do think there's a strong first-mover advantage in projects.

When I moved to the backend team, the project was already pretty mature and had existing experts on the bigger areas, so I found that I didn't have strong ownership. As those people moved teams or left the company and I learned more, I did feel like it started changing, so I also think there are also some advantages of staying on a team for longer.

I don't regret my move in the least! It did trigger my imposter syndrome regularly and the oncall was extremely heavy, but I feel like I learned way more in the past 4 years than in the previous 9.

**Lesson 8.** Sometimes growth comes at the detriment of impact. But I now see learning as deferred impact.

## What's Next

If you read to this point, sorry if I bored you with details. If you scrolled past here in hopes of seeing where I'm headed, good insight! I'm at OpenAI now.

As I did during my time at Facebook/Meta, I'll refrain from talking about my projects, but I will share my notes on public topics I study that might be related to my day-to-day work.

## Related Posts

No related posts worth mentioning besides the ones I linked inline.
