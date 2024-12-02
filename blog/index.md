---
layout: page
title: Blog
---

<p>
  <a href="{{ site.url }}">kuniga.me</a> > <a href="{{ site.url }}/blog">NP-Incompleteness</a>
</p>

# NP-Incompleteness

{% for post in site.posts limit:5 %}
  <div>
    <h2><a href="{{ post.url }}">{{ post.title }}</a></h2>
    {{ post.date | date_to_string }}
    {{ post.excerpt }}
    <p class="right"><a href="{{ post.url }}">Continue reading...</a></p>
  </div>
{% endfor %}

<p class="center_children">
<a href="{{site.url}}/blog/archive.html" style="margin-top: 50px">Visit archive to see all posts...</a>
</p>

## Sub-pages

* [About]({{ site.url }}/blog/about/)
* [Subscribe]({{ site.url }}/feed.xml) - RSS feed
* [Index]({{ site.url }}/blog/selected/) - Posts by topics
* [Tags]({{ site.url }}/blog/tags/) - Posts by tags
