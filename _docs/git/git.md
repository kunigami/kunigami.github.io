---
layout: doc
title: "Git Cheatsheet"
---

## Local Change management

### Revert changes to uncommitted file

{% highlight text %}
git checkout -- <file>
{% endhighlight %}

### Discard the most recent local commit

{% highlight text %}
git reset --hard HEAD~1
{% endhighlight %}

### Revert all local changes

NOTE: this doesn't remove untracked files

{% highlight text %}
git checkout .
{% endhighlight %}

### Revert all untracked files and directories

{% highlight text %}
git clean -fd
{% endhighlight %}

### Remove uncommitted files

{% highlight text %}
# (optional) first, see what files will be purged
git clean -n
# purge
git clean -fd
{% endhighlight %}

### Stash / Shelve single file

{% highlight text %}
git stash -- <file>
{% endhighlight %}

### Un-stash

{% highlight text %}
git stash pop
{% endhighlight %}


## (Local) Branch

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

"Pull" remote branch (e.g. `my_remote_branch`) that does not exist in your local repo and switch to it. Note: this only works if  `my_remote_branch` only appears in exactly one of the remote repos:

{% highlight text %}
git switch my_remote_branch
{% endhighlight %}

## Resolving Conflicts

Mark file as resolved:

{% highlight text %}
git add <filemame>
{% endhighlight %}
