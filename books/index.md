---
layout: page
title: Books
---


{% include blog_vars.html %}

<script type="text/javascript" src="{{site.baseurl}}/js/components/books.js"></script>

<p>
  <a href="{{ site.url }}">kuniga.me</a> > <a href="{{ site.url }}/books">Books</a>
</p>

{% assign root = site.url | append: "/books" %}

Notes on some of the books I've read. Since 2025 I've included every book I read. The list below sorted by reverse chronological order.

<book-table
  base_url="{{site.url}}"
  headers="Name, Age, Country"
>
</book-table>
