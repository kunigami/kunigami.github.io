---
layout: page
title: Blog
---

<p>
  <a href="{{ site.url }}">kuniga.me</a> > <a href="{{ site.url }}/blog">NP-Incompleteness</a> > <a href="{{ site.url }}/blog/archive.html">Archive</a>
</p>


# Archive

{% assign technical_posts = site.posts | where: "blog", "technical" %}

<ul>
  {% for post in technical_posts %}
    <li>
      <span class="monospace">{{ post.date | date_to_string }} - </span><a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>
