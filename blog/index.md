---
layout: page
title: Blog
---

<p>
  <a href="{{ site.url }}">kuniga.me</a> > <a href="{{ site.url }}/blog">NP-Incompleteness</a>
</p>

# NP-Incompleteness

{% for post in site.posts limit:5 %}
  <h2><a href="{{ post.url }}">{{ post.title }}</a></h2>
  {{ post.date | date_to_string }}
  {{ post.excerpt }}
  <p class="right"><a href="{{ post.url }}">Continue reading...</a></p>
{% endfor %}

## Sub-pages

* [About]({{ site.url }}/blog/about/)
* [Subscribe]({{ site.url }}/feed.xml) - RSS feed
* [Index]({{ site.url }}/blog/selected/) - Posts by topics
* [Tags]({{ site.url }}/blog/tags/) - Posts by tags

## Older Posts

<ul>
  {% for post in site.posts %}
    <li>
      <span class="monospace">{{ post.date | date_to_string }} - </span><a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>
