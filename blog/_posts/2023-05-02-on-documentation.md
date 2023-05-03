---
layout: post
title: "On Documentation"
tags: [opinion]
vanity: "2023-05-02-on-documentation"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

Recently I got recognized at work for driving improvements to our team's documentation. I have some of thoughts on documentation which I'd like to share in this post.

<!--more-->

## Context

To set some context I joined my current team about year ago. Our team maintains an internal system that has other employees as customers, ranging over a large spectrum of technical expertise.

One of the most obvious low-hanging fruits improvements I found was to improve documentation and I also found that revisiting docs a great way to force myself to read them and get exposed to many corners of the project during ramp up! The docs included pages for customers but also mixed with those for internal developers. It also mixed documentation from a deprecated version of the system.

I don't have experience writing documentation for public facing products, so the scope of this post is only for internal-facing docs, but I think this would apply to an open-source library for example.

## Why Documentation?

I'm passionated about documentation (this blog is, in a sense, documentation of topics I study) and have always been eager to partner with technical writers when they were available to our team.

I know some people get cynical about documentation especially if the company has a culture of bad/no documentation. This leads to a [chicken-and-egg](https://en.wikipedia.org/wiki/Chicken_or_the_egg) problem where people don’t use docs because they suck and teams find little incentive to invest in docs when looking at data or anecdata.

I firmly believe good docs is a pragmatic way to scale engineers on the team. In my experience developing internal systems, when people have questions they often ask questions in a group or worse, directly to the engineers, which is a waste of time.

There's a maxim on writing readable code since code is written once but read multiple times, and I believe the same applies to documentation. More on *Documentation as code* later.

## Who Should Write?

### The Authoring Engineer

Often the person who writes the documentation for a new feature or API is the person who built it. They have the incentive to write docs so people use it and as a point of contact it's on their interest to avoid repeated questions. They also have the most expertise on that.

The downsides is that these people might have a lot of blind spots because they know so much they can't put themselevs into the user's shoes. They might also have a conflict of interest: they want users to use a feature in specific ways to fit their mental model or to help with metrics.

They might also be tempted to include too much implementation detail, possibly from overestimating how much a user might be interested in it or perhaps on it doubling as documentation for teammates. I've seen a case where the engineer documented the heuristic used to compute some result when the user just needed to know what each result meant.

Finally not all engineers write well. Futher, for people like me to whom English is not their native language, there’s an additional source for lack of clarity and improving this doesn't seem incentivized enough by companies. Despite having many technology-specific and soft skills classes available in my company, I've never seen any classes focused on technical writing.

### The Technical Writer

The technical writer avoids a lot of the issues from the *Authoring Engineer*. They’re good writers by design, don’t have the bias of knowing too much and can provide insights as first time learners.

The major drawback is that technical writers are often not that technical. Especially if the system is meant for other engineers they can find it hard to provide needed insights.

Finally tech writers are not users and will often be directed to document how the *Authoring Engineer* envisions it or will themselves do a thorough pass which will lead to docs that are very complete but hard to find information.

### The Volunteer User

In my opinion the best person to document is an actual user with real experience with the system, who documents the system when they’re ramping up.

That's why I often find it easier to follow StackOverflow posts or blogs than going straight to official documentation. A while back I spent a long time trying to use Github's authentication API following the official documentation, and only after reading [this post](https://dev.to/gr2m/github-api-authentication-personal-access-tokens-53kd) I was able to figure it out.

When I switched from Hack/JavaScript to C++/Python last year, I wrote down my learnings in a personal wiki, accessible to the company. It has proven useful to other people given my C++ wikis gets over 100 distinct viewers / month without any advertisement.

The major drawback of the *Volunteer User* that it’s really hard to find people in this situation who happen to enjoy writing.

## Documentation as Code

When I moved from Wordpress to [static pages a few years ago]({{blog}}/2020/07/11/from-wordpress-to-jekyll.html) one of the major upsides for me was having the content as markdown and in source control (Github).

I really like the functionality that comes with using source control, including seeing modified lines (e.g. `git diff`), being able to rollback to an older version, searching raw markdown content inside Github, branching for drafts, and being able to [squash multiple commits into one]({{blog}}/2021/09/01/writing-posts.html).

I know a lot of these functionalities are implemented in a [CMS](https://en.wikipedia.org/wiki/Content_management_system) but having the same flow I use to write code and documentation is much better.

As part of my revamping of the documentation at work, I also moved them to source control. Within a team there's an added benefit: code review! I had teammates correct factual innacuracies on my end and I helped catching some "too much implementation details" from *Authoring Engineers*.

## Conclusion

I find documenting things great but I haven't been able to spread that passion to other people or make a dent on the culture of documentation. I'm sure a lot of people keep personal notes of some sort but don't feel like publicizing it or contributing back to existing documentation and I wonder about the root causes: perphaps lack of time to polish their notes, fear of having their writing being judged or assuming other people won't find it useful?
