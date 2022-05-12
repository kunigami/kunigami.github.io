---
layout: post
title: "SQL Notebooks"
tags: ['external writing', 'python']
excerpt_separator: <!--more-->
vanity: "2022-05-12-sql-notebooks"
---

{% include blog_vars.html %}

I'd like to talk about a post I wrote recently at Meta's Engineering Blog: [SQL notebooks](https://engineering.fb.com/2022/04/26/developer-tools/sql-notebooks/).

I'm not going to repeat the contents from there but I'd like to connect to prior posts in this blog and share some of my personal experience that led to it.

<!--more-->

## Background

At Meta I had been working with internal data tools for many years. These tools are essentially web-based UIs for employees to perform data analysis (especially data engineers and data scientists).

One of the things I helped building is a SQL editor tool, which can issue queries to any of Meta's SQL-based databases (MySQL, Presto, Spark, etc.). One of the first versions of this tool inspired an open source called [Airpal](https://airbnb.io/airpal/).

A couple of years ago I had the opportunity to build a prototype that turned the SQL editor into a notebook, and I got inspiration from tools like Observable and Jupyter.

In the next section we go over some previous posts that provide a glimpse on the learning that happened over time.

## Related Posts

**[Observable](https://www.kuniga.me/blog/2020/01/04/observable.html) (2020).** I liked D3 a lot, so when Mike Bostock launched this JavaScript notebook I went to check it out.

One of the things I liked the most about Observable is the "spread-sheet semantics", which means that each cell knows which other cells depend on it, so if cell B depends on A, when A updates cell B does too. The SQL notebook has this semantics too.

**[Linear Predictive Coding in Python](https://www.kuniga.me/blog/2021/05/13/lpc-in-python.html) (2021).** The content of this post is not relevant here on itself but it was the first post in which I used a Jupyter notebook and I have used it for many other posts since.

In that post I translated a Matlab code to Python. It's interesting that Matlab and Mathematica hav had notebooks for a long time but they are relatively niche languages for scientific computing and not free.

Python started as a general purpose language and later an open-source notebook was introduced. Its popularity was helped by the adoption of data science and machine learning communities in the past decade.

**[Namespace Jailing](https://www.kuniga.me/blog/2021/07/02/namespace-jail.html) (2021).** One of the technical aspects I'm most proud of the SQL notebooks work is that it doesn't require a dedicated host to run like Jupyter notebooks do. The heavy-lifting is done by distributed databases like Presto and the Python is executed by stateless servers.

Executing arbitrary Python code in the cloud (multi-tenant) machines is a recipe for disaster so we had to come up with a way to sandbox the process. We ended up using Linux namespaces to restrict access.

This means that the Python code in SQL notebooks can only be used for small data transformation and visualization but can't do network requests, including query databases. For the use cases it covers though, it avoids one of the limitations of Jupyter notebooks: turning a notebook into a report which can be easily shared with others and re-executed on the fly.

**[Jupyter Architecture](https://www.kuniga.me/blog/2022/02/12/jupyter-architecture.html) (2022).** I had actually already written the draft of the Meta's post by the time I studied Jupyter's Architecture in detail.

My learnings from studying Jupyter's Architecture confirmed my suspicion: the architectures of SQL notebooks and Jupyter notebooks are quite different, driven mostly by the fact that one is stateless and the other is stateful. Also, SQL notebooks have spreadsheet semantics while Jupyter notebooks have [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) semantics.

## Conclusion

Overall I was very happy to be able to share my work with the public. I helped building many other tools I'm proud of, including a generic dashboard tool and a query library, which I would have liked to write about had I known it was a possibility.
