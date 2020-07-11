---
layout: page
title: Blog
---

<p>
  <a href="{{ site.url }}">kuniga.me</a> > <a href="{{ site.url }}/blog">NP-Incompleteness</a>
</p>

# NP-Incompleteness

<p class="top_margin">Most recent post:</p>

{% assign first_post = site.posts | first %}
<h2><a href="{{ first_post.url }}">{{ first_post.title }}</a></h2>
{{ first_post.date | date_to_string }}
{{ first_post.excerpt }}
<p class="right"><a href="{{ first_post.url }}">Continue reading...</a></p>

## Sub-pages

* [About]({{ site.url }}/blog/about/)
* [Selected Posts]({{ site.url }}/blog/selected/)
* [Tags]({{ site.url }}/blog/tags/) - Posts grouped by tags

## Older Posts

<ul>
  {% for post in site.posts %}
    <li>
      <span class="monospace">{{ post.date | date_to_string }} - </span><a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>
