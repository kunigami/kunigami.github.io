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

{% for post in trips_posts %}
* [{{ post.date | date: "%Y" }} {{ post.title }}]({{ post.url }})
{% endfor %}
