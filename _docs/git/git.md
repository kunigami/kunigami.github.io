---
layout: doc
title: "Git Cheatsheet"
---

Revert changes to uncommitted file

{% highlight text %}
git checkout -- <file>
{% endhighlight %}

## Branch

Creates new branch `foo`:

{% highlight text %}
git branch foo
{% endhighlight %}

Changes to branch `foo`:

{% highlight text %}
git checkout foo
{% endhighlight %}

List existing branches:

{% highlight text %}
git branch
{% endhighlight %}

Delete branch `foo` (move away from it first):

{% highlight text %}
git branch -d foo
{% endhighlight %}

## Remote Branch

Add new remote repo shorcut/alias. For example, I want `public` to refer to the repository at `git@github.com:kunigami/my-repo.git`.

{% highlight text %}
git remote add public git@github.com:kunigami/my-repo.git
{% endhighlight %}

List remote repos associated to this local repo:

{% highlight text %}
git remote -v
{% endhighlight %}

Send changes to a remote repo + branch, for example, the remote repository aliased as `origin`, branch `test`:

{% highlight text %}
git push origin test
{% endhighlight %}
