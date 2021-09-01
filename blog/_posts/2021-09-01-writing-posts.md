---
layout: post
title: "Writing Posts"
tags: [git, meta]
vanity: "2021-09-01-writing-posts"
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}


This is a meta post to describe my flow for writing posts. When I [moved to static Github pages]({{site.url}}/blog/2020/07/11/from-wordpress-to-jekyll.html) over a year ago, I didn't have a flow that I was satisfied with but I've recently settled in one which I think is worth documenting.

<!--more-->

It's worth recalling that in static Github pages I write in markdown and manage them using git (and Github).

The overall idea is to be able to write a post in draft mode, which is hidden from view and only when it's ready I merge it on master, which is automatically picked up by Github pages and made public.

## Early drafts

I usually have a bunch of topics I'd like to write about at one time, so I keep these early stage drafts as Google docs, which are mostly a collection of links and some comments.

Once I have enough confidence on it becoming a future post, I move to markdown.

## Private Branch

In the past I'd keep an un-committed markdown files plus images for the posts, occasionally backing them up by copying them to Dropbox.

Needless to say it's a subpar flow. I was really looking for a way to use git to backup my changes but didn't want them to show up in my repository while in development.

I started looking for private branches but they don't exist. An alternative was proposed in this Stack Overflow answer [1]. The idea is to keep a private mirror of the main repo and commit only to the private one, occasionally syncing with the public.

Suppose the name of our main blog repository is `blog`.

### Create a new private repo

Github supports creating [private repositories for free](https://github.blog/2019-01-07-new-year-new-github/). Let's call it `draft-blog`.

### Create a mirror repo

Here we copy the steps from the [Github docs](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-on-github/duplicating-a-repository) [3].

Create a bare clone of the repository:

{% highlight bash %}
$ git clone --bare https://github.com/exampleuser/blog.git
{% endhighlight %}

Mirror-push to the new repository:

{% highlight bash %}
cd blog
$ git push --mirror https://github.com/exampleuser/draft-blog.git
{% endhighlight %}

Remove the temporary local repository you created earlier.

{% highlight bash %}
$ cd ..
$ rm -rf blog
{% endhighlight %}

### Clone the new repo locally

{% highlight bash %}
$ git clone git@github.com:exampleuser/draft-blog.git
{% endhighlight %}

### Add remotes

A git remote is basically an alias for the URL of the remote repo. When we use `git clone`, it adds a default alias called `origin`, which points to the original repo. We can inspect via `git remote -v`:

{% highlight bash %}
origin  git@github.com:exampleuser/draft-blog.git (fetch)
origin  git@github.com:exampleuser/draft-blog.git (push)
{% endhighlight %}

Note we have one entry for read (fetch) and one for write (push).

We want to add another remote that points to the public blog, that is `git@github.com:exampleuser/blog.git`, and name it `public`:

{% highlight bash %}
$ git remote add public git@github.com:exampleuser/blog.git
{% endhighlight %}

If we type `git remote -v` we should see 4 entries now:

{% highlight bash %}
origin  git@github.com:exampleuser/draft-blog.git (fetch)
origin  git@github.com:exampleuser/draft-blog.git (push)
public  git@github.com:exampleuser/blog.git (fetch)
public  git@github.com:exampleuser/blog.git (push)
{% endhighlight %}


## Writing

Before we start writing, we create a branch. For this very post I created one called `post-writing`:

{% highlight bash %}
git branch post-writing
git checkout post-writing
{% endhighlight %}

**NOTE:** It's important we create branches off the `master` branch (we'll see why later), so always do `git checkout master` before creating a new branch.

I usually write a bit every other way. When I'm done, I simply create a dummy commit and sync to a similarly named branch in the private repo for backup:

{% highlight bash %}
git commit -am "backup"
git push origin post-writing
{% endhighlight %}

Since I always push to a branch to the same name on remote, I set the `push` behavior to `current`, which pushes the current branch to a branch of the same name in the remote [4]:

{% highlight bash %}
git config push.default current
{% endhighlight %}

So we can simply do:

{% highlight bash %}
git commit -am "backup"
git push origin
{% endhighlight %}

## Merging

Once the post is ready for publishing, we want to merge into the master, but we don't want all those dummy backup commits polluting the logs.

This [Stack Overflow answer](https://stackoverflow.com/questions/25356810/git-how-to-squash-all-commits-on-branch) [2] provides a way to squash all the commits from a branch into a single one:

{% highlight bash %}
git checkout post-writing
git reset $(git merge-base master $(git branch --show-current))
git add -A
git commit -m "new post: writing posts"
{% endhighlight %}

Let's analyze the second command:

{% highlight bash %}
git branch --show-current
{% endhighlight %}

Simply returns the current branch name `post-writing`. Wrapped in `$()` means it's treated as a variable, thus

{% highlight bash %}
git merge-base master $(git branch --show-current)
{% endhighlight %}

is really

{% highlight bash %}
git merge-base master post-writing
{% endhighlight %}

The command above returns the `<hash>` of the commit that is the lowest common ancestor to both `master` and `post-writing`. Finally we do

{% highlight bash %}
git reset <hash>
{% endhighlight %}

This will set the current index back to that ancestor commit and the changes from `post-writing` relative to that ancestor will show up as un-committed changes.

This command assumes `post-writing` was created off the `master`. It it was created off some other branch `foo`, reseting the index would include changes from `foo` as well. Hence the note in the *Writing* section.

To simplify things, we can alias the second command as `compress`:

{% highlight bash %}
git config alias.compress "! git reset $(git merge-base master $(git branch --show-current))"
{% endhighlight %}

Now we can merge it into master:

{% highlight bash %}
git checkout master
git merge post-writing
{% endhighlight %}

## Syncing

Finally we can make the post public by pushing it to the public remote:

{% highlight bash %}
git push public
{% endhighlight %}

## Post-editing

I usually want to fix typos or reword phrases after the post has been published. For this flow I don't bother creating branches nor squash commits and do all from the `master` branch in `draft-blog`.

{% highlight bash %}
git checkout master
# fix / reword ...
git commit -am 'fix typos'
git push public
{% endhighlight %}

## Conclusion

I only use Git for basic stuff (at work we use some flavor of Mercurial) and whenever I have to do some operation I'm not used to, Git gives me impostor syndrome.

I think I got a good handle of working with remotes through this process of trying to document my workflow. Having to setup another remote made and looking into different `push` behaviors [4] made things a lot clearer.

Though it's worth noting my flow is super simple because I mostly write in one computer (occasionally I also write in a Linux machine) and each post is its own file, so conflicts are almost non-existent.

## References

* [[1](https://stackoverflow.com/questions/7983204/having-a-private-branch-of-a-public-repo-on-github)] Stack Overflow: Having a private branch of a public repo on GitHub?
* [[2](https://stackoverflow.com/questions/25356810/git-how-to-squash-all-commits-on-branch)] Git: How to squash all commits on branch
* [[3](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-on-github/duplicating-a-repository)] Duplicating a repository
* [[4](https://stackoverflow.com/questions/948354/default-behavior-of-git-push-without-a-branch-specified)] Default behavior of “git push” without a branch specified
