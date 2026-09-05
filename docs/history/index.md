---
layout: page
title: History
---

<p>
  <a href="{{ site.url }}">kuniga.me</a> &gt;
  <a href="{{ site.url }}/docs">Docs</a> &gt;
  <a href="{{ site.url }}/docs/history/">History</a>
</p>

# History

{% assign docs_by_title = site.docs | sort: "title" %}

<ul>
  {% for doc in docs_by_title %}
    {% if doc.url contains "/docs/history/" %}
      {% assign entry_title = doc.title | remove: "History of " | remove: " Cheatsheet" %}
      <li><a href="{{ doc.url }}">{{ entry_title }}</a></li>
    {% endif %}
  {% endfor %}
</ul>
