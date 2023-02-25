---
layout: post
title: "Google Summer of Code - Conclusion"
tags: [computer graphics]
vanity: "2011-10-09-gsoc-conclusion"
---
{% include blog_vars.html %}

This week the Google Summer of Code certificate of completion arrived, indicating that I successfully completed the program! They also sent a T-shirt as a gift.

<figure class="center_children">
  <img src="{{resources_path}}/gsoc-tshirt.png" alt="Dark-blue T-shirt with Google Summer of CODE 2011 code.google.com/soc written in it" />
</figure>

## Tips for those who want to participate

I had already tried to participate in the Google Summer of Code in 2009 and 2010, through the Scilab and CGAL organizations, respectively, but without success.

What did I do differently to be accepted in 2011? I spoke with the mentors before submitting my proposal.

In addition to showing interest, conversations allow the instructor to assess the skills needed to complete the project. In my case for example, I had to implement a patch to fix a bug.

Here are some tips on choosing the project and the organization.

### Choice of Project

One important thing is the choice of project. There are several points that should be considered when choosing a project, including: competition, relevance, difficulty and motivation.

Game-related projects are probably very competitive, but overall I don't know what attracts applicants the most. You have to follow the mailing lists and see which projects are being sought after the most. My tip is to opt for the less competitive projects.

An organization typically offers several projects, but some have higher priority than others. If there are two equally good proposals and the organization has to choose only one, it will probably opt for the higher priority project. Some organizations explain the relevance of the project in their descriptions, but it is worth talking to the mentors to understand the impact of the project on the organization.

Choosing the proper project difficulty is tricky. On the one hand, very easy projects tend to be either not very relevant or very boring (otherwise probably someone would have volunteered to do it already). Very difficult projects can undermine your chances of completing the program and also make it more difficult to convince the mentor that you are capable of doing it. I would then recommend medium difficulty projects.

<figure class="center_children">
  <img src="{{resources_path}}/raytracing-wikipedia.png" alt="Several spheres rendered using ray-tracing." />
</figure>

Finally, you need to choose a project that motivates you. This does not affect your chances of being accepted, but it does affect your chances of successfully completing the program. In my case, I was quite interested in raytracers, so I looked for projects within that topic.

### Choice of Organization

The choice of organization is quite relevant. Like it or not, some organizations will have more caring mentors than others. From my experience with Scilab, CGAL and BRL-CAD, I can say that Scilab and BRL-CAD have very accessible mentors. At CGAL I had a bad experience with email exchanges, as none of my emails regarding the GSoC were answered.

My tip here is to choose organizations that have a project that you like and start exchanging emails. This will give you an idea of ​​how helpful the mentors are.

### Necessary Skills

I consider the following skills/knowledge to be important to increase the chances of completing the project:

* Having compiled some open-source project from source code, going through all the pain of installing dependencies (even when they are not accessible via `apt-get install` :P).
* Knowing how to get unblocked by yourself. Go after tutorials, documentation, tweaking code, etc.
* It's good to know the programming language that will be used in the project, but maybe it's not something so strict if you have some related experience. For example, if you have knowledge in OOP, you can get by in both C++ and Java. In fact, BRL-CAD has a project in Tcl, but a fellow GSoC student, who did not know that language, successfully completed the project.
* Depending on the project, the required knowledge is so complicated that only people who have worked with it will be able to accomplish it. On the other hand, there are subjects that are easy to learn, at least the basics, such as ray tracing.

## Conclusion

### Apprenticeship

Working with an open-source project increases learning in the different tools used, in the theme of the project, in programming practices, etc. However, in my opinion, the biggest benefit is the familiarity you gain from working with large code written by many hands.

I remember that in my first attempts to work with open-source code (if I'm not mistaken it was with Blender), I got discouraged very quickly because I had difficulty understanding what the code was doing. Recently, I took on a project with poorly commented code and even so I managed to understand it relatively well.

I don't believe that my ability to understand code has improved much, but it's the habit with this process, sometimes tedious and time-consuming, that teaches you to persevere and to not get discouraged so easily.

### Project continuity

As I mentioned in a [previous post]({{blog}}/2011/08/21/gsoc-last-week.html), I wanted to keep working on this project, but so far there hasn't been much free time for that. Let's see if I do something by the end of the year.

## Notes

This is a review and translation done in 2023 of my original post in Portuguese: [Google Summer of Code – Conclusões](https://kuniga.wordpress.com/2011/10/09/google-summer-of-code-comentarios/).
