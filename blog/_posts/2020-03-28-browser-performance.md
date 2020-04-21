---
layout: post
title: "Browser Performance"
tags: [chrome, css, javascript]
---

In this post we’ll go over the topic of browser performance. We’ll cover tools and techniques to identify, measure and improve performance on the client side of web applications.

**Assumptions.** Our tests use the Chrome browser, which means the tools likely only make sense for this specific browser (general techniques are agnostic). All the screenshots are taken either on a Linux machine running Gnome or the latest MacOS, though it should be relatively similar to Chrome in other platforms. The Chrome version used is v80.

Setup. I’ll be using simple standalone HTML/JavaScript code. I recommend downloading the folder browser-performance.

## Analyzing Performance

FPS Meter
This widget tracks the number of frames per second. To enable it,

In the ... icon menu, click on Show console drawer:

![Testing](/resources/blog/2020-03-28-browser-performance/console-drawer.png)

{% highlight css %}
.moving-element {
  will-change: transform;
}
{% endhighlight %}
