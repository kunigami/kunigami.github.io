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

Notes on some of the books I've read. List below sorted by reverse chronological order. (Work in progress)

<book-table
  base_url="{{site.url}}"
  headers="Name, Age, Country"
>
</book-table>
