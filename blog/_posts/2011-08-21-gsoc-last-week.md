---
layout: post
title: "Google Summer of Code - Last Week"
tags: [computer graphics]
vanity: "2011-08-21-gsoc-last-week"
---

{% include blog_vars.html %}

According to the Google Summer of Code schedule, this was the last week of the program, where we should stop writing code and focus on writing documentation, testing, etc.

<figure class="center_children">
  <img src="{{resources_path}}/brlcad.png" alt="BRL-CAD logo. Two overlapping links (like from a bike chain)." />
  <figcaption>Winning logo from a contest BRL-CAD organized recently.</figcaption>
</figure>

## Source Code Availability

BRL-CAD's policy is to provide all the dependencies source code along with its own code. This makes life much easier for the user, who doesn't have to install dependencies, especially those that have to be compiled from source, and doesn't even have to worry about compatibility of library versions. Furthermore, it is possible to choose which of the libraries you want to use from your system and which ones you want BRL-CAD to compile.

So it was decided that I was going to create a new repository in SVN and upload there all the libraries that the OSL needs and also a script to compile them.

Although the OSL library is small, it relies on a lot of use of external libraries like: [OpenEXR](https://github.com/AcademySoftwareFoundation/openexr), Ilmbase, [Open Image IO](https://sites.google.com/site/openimageio/home?pli=1) (which in turn depends on libpng, libjpeg, libtiff), LLVM and Boost. The last two are relatively large, LLVM ~250MB and Boost ~500MB, which makes uploading to the repository problematic.

The decision made was to leave LLVM up to the user and upload only the necessary boost libraries. As Boost is composed of several other modular libraries, there is a boost tool itself called `bcp`, to select only those necessary for the application, including also the dependencies of the boost libraries themselves.

I struggled a little to [compile](https://kuniganotas.wordpress.com/2011/08/13/compiling-boost-bcp/) and [use](https://kuniganotas.wordpress.com/2011/08/16/how-do-i-select-a-minimal-subset-of-boost-libraries-necessary-for-my-project/) the tool, but I ended up getting it down from ~500MB to about 50MB!

## Portability of Files in SVN

For files loaded in an SVN to work transparently across different systems, especially between Linux and Windows, certain care must be taken, such as:

* Linux and Windows use different end-of-line characters for text files.
* Linux supports symlinks and windows doesn't
* Linux decides whether a file is executable through file permissions, while windows uses the file name extension to decide.

That's why it's important to specify whether a file is text or binary, for example. In order not to have to set this information file by file, SVN uses their extensions to decide what type of file it is. For that, it specifies several possibilities in the `config` file (usually in `.subversion/`) through patterns (usually `*.extension`). When the filename doesn't match any pattern, SVN can be configured to not let commits.

A problem I was facing is that SVN only gives this error in the final commit phase and in this case it aborts the commit of all files. This means that depending on the size of the code being added, it can take hours before I realize that the commit has failed and I need to specify the file types either by adding a pattern in the configuration file or manually with the SVN propset command. Then try to commit all the files again.

I decided to write a [Python script](https://gist.github.com/kunigami/1150253) which reads this configuration file, reads all the files to be added and checks which ones don't match any patterns. Then I quickly know which files need to have their `mime-type` and/or `eol-style` set.

## SVN External Dependency

Sometimes the structure of a codebase requires that we checkout from different repository addresses. For example, imagine the directory `foo/` is at `http://some.address.svn/foo`. However, inside that `foo/` directory there must be a `bar/` folder, but that is located at `http://another.address.svn/bar`.

In principle, we can just checkout `foo/`, `cd foo/` and then checkout `bar`. However, requiring the user to know this structure is an additional hassle. For this, there is the `svn:externals` property, which specifies which directories should be pulled from which addresses. In the example, we can set the property of `foo/` by going into that directory and typing:

{% highlight text %}
SVN propset svn:externals 'http://another.address.svn bar' .
{% endhighlight %}

Then, when we checkout `http://some.address.svn/foo`, it will pull up the `bar/` subdirectory from `http://another.address.svn/bar`.

I had to use this functionality from SVN because two libraries that I needed to provide (zlib and libpng) are already provided by BRL-CAD in the main repository. All I had to do was make the external link and now the two libraries are downloaded along with the other dependencies. With that, we don't need to duplicate code in the repository and we have the guarantee that the used versions of these libraries are the same.

## Remaining Tasks

From the code organization perspective, I only tried to remove commented code and exclude some files from SVN. I ended up not having enough time to write a decent documentation :( The only one I have is about how to compile OSL, which was done before I wrote the build script.

Not only for the documentation part, but in general, I had the feeling that my work is incomplete. I would like to get back to work on this project as soon as I have more time (read: defending my thesis). I hope that my mentors consider my work satisfactory and that I pass this second stage of the Google Summer of Code.

## Notes

This is a review and translation done in 2023 of my original post in Portuguese: [Google Summer of Code – Última semana](https://kuniga.wordpress.com/2011/08/21/google-summer-of-code-ultima-semana/).
