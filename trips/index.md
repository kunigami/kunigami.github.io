---
layout: page
title: Trips
---


{% include blog_vars.html %}

<p>
  <a href="{{ site.url }}">kuniga.me</a> > <a href="{{ site.url }}/trips">Trips</a>
</p>

{% assign trips_posts = site.posts | where: "blog", "trips" %}

Memories on trips.

<ul>
  {% for post in trips_posts %}
    <li>
      <span class="monospace">{{ post.date | date: "%Y" }} - </span>{% if post.country %}{{ post.country }} {% endif %}<a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>
