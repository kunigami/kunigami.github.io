---
layout: doc
title: "Blog Cheatsheet"
---

{% include blog_vars.html %}

## Publish new post

Git directions. Suppose the post is being written on a branch named `new_post`.

{% highlight bash %}
git checkout new_post
git reset $(git merge-base master $(git branch --show-current))
git add -A
git commit -m "new post: $(git rev-parse --abbrev-ref HEAD)$"
git checkout master
git merge new_post
{% endhighlight %}
