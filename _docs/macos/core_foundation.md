---
layout: doc
title: "Core Foundation"
---


## Filesystem

Creates a file or directory.

{% highlight cpp %}
CFURLRef CFURLCreateWithFileSystemPath(
    CFAllocatorRef allocator,
    CFStringRef filePath,
    CFURLPathStyle pathStyle,
    Boolean isDirectory);
{% endhighlight %}

Example: Creating a file from a relative path `foo.txt`:

{% highlight cpp %}
CFURLRef CFURLCreateWithFileSystemPath(
    kCFAllocatorDefault,
    CFSTR("./foo.txt"),
    kCFURLPOSIXPathStyle,
    false);
{% endhighlight %}
