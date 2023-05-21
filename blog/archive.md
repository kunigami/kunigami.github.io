---
layout: page
title: Blog
---

<p>
  <a href="{{ site.url }}">kuniga.me</a> > <a href="{{ site.url }}/blog">NP-Incompleteness</a> > <a href="{{ site.url }}/blog/archive.html">Archive</a>
</p>


# Archive

<ul>
  {% for post in site.posts %}
    <li>
      <span class="monospace">{{ post.date | date_to_string }} - </span><a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>
