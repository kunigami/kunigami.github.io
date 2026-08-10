---
layout: page
title: Books
---


{% include blog_vars.html %}

<p>
  <a href="{{ site.url }}">kuniga.me</a> > <a href="{{ site.url }}/books">Books</a>
</p>

{% assign books_posts = site.posts | where: "blog", "books" %}

Notes on some of the books I've read. Since 2025 I've included every book I read. The list below sorted by reverse chronological order.

<table style="border-collapse: collapse; width: 100%; table-layout: fixed;">
  <thead>
    <tr>
      <th style="border: 1px solid #ccc; padding: 8px; background: #f0f0f0; width: 25%;">Title</th>
      <th style="border: 1px solid #ccc; padding: 8px; background: #f0f0f0; width: 25%;">Author</th>
      <th style="border: 1px solid #ccc; padding: 8px; background: #f0f0f0; width: 15%;">Rating</th>
      <th style="border: 1px solid #ccc; padding: 8px; background: #f0f0f0; width: 15%;">Category</th>
      <th style="border: 1px solid #ccc; padding: 8px; background: #f0f0f0; width: 10%;">Year Read</th>
      <th style="border: 1px solid #ccc; padding: 8px; background: #f0f0f0; width: 10%;">Cover</th>
    </tr>
  </thead>
  <tbody>
    {% for post in books_posts %}
    <tr>
      <td style="padding: 8px; vertical-align: top;">
        <a href="{{ site.url }}{{ post.url }}">{{ post.title }}</a>
      </td>
      <td style="padding: 8px; vertical-align: top;">{{ post.author }}</td>
      <td style="padding: 8px; vertical-align: top;">{% for i in (1..post.rating) %}⭐{% endfor %}</td>
      <td style="padding: 8px; vertical-align: top;">{{ post.category }}</td>
      <td style="padding: 8px; vertical-align: top;">{{ post.date | date: "%Y" }}</td>
      <td style="padding: 8px; text-align: center;">
        <img
          src="{{ site.url }}/resources/books/{{ post.image }}"
          alt="{{ book_cover_alt }}"
          style="height: 100px; margin-top: 3px;"
        />
      </td>
    </tr>
    {% endfor %}
  </tbody>
</table>
