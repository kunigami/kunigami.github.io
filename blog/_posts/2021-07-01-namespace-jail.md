---
layout: post
title: "Namespace Jailing"
vanity: "2021-07-01-namespace-jail"
tags: [operating systems, bash, c]
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

In a previous post we investigated a jail system using chroot with the conclusion that it was not a safe implementation. In this post we'll study a safer alternative using Linux namespaces. We'll develop a C++ application along the way.

<!--more-->


## Linux Namespaces

The idea of Linux namespaces is actually very close to that of a sandbox. We want to create subsystems within a system which are isolated, so if they're tampered with, the hosting sytem is protected.

Linux allows sandboxing different pieces of its system. For example the user namespace consists of a separate set of users and groups.

There are at least 8 different namespaces available, but for the purposes of our simple sandbox, we'll focus on 2: the user and mount namespaces.

## Setup

We'll develop a C++ application to create a jailed process. The idea is to define a base class that does the heavy-lifting and exposes some paremeters that children functions can configure.

The base class stub follows:

{% highlight cpp %}
// sub_process.cpp
struct NamespaceProcess {
    int run() {
        return 0;
    }
}
{% endhighlight %}


and a sample child class:

{% highlight cpp %}
// main.cpp

#include "sub_process.cpp"

class ShellProcess : public NamespaceProcess {
}

int main() {
  auto process = ShellProcess();
  return process.run();
}
{% endhighlight %}

We'll assume the existence of some utils functions:

{% highlight cpp %}
// utils.h

// prints action to sterr + string representation of errno
void error_action(string action);

// error_action + exit program with failure
void fatal_action(string action);

// format string using printf() syntax
string format(const char *format, ...);
{% endhighlight %}

## Cloning

The [`clone()`](https://man7.org/linux/man-pages/man2/clone.2.html) function is a general version of `fork()`, which allows for more granular configuration. It can be used to start a new child process. It takes a few arguments:

* Pointer to a function `f`
* Pointer to a stack
* Clone flags
* Pointer to object which will be passed to `f`

This function will create a child process and make it execute `f` with the provided arguments. The clone flags will determine what capabilities this child process will have, including what namespaces it will use. Let's start with no flags for now.

The parent process will receive the child `pid` from `clone()` and continue its execution.

We're mostly interested in `f` for now. Assume we have a function `allocate_stack()` that will allocate some memory for the stack available to the child process.

We want the child process to call a function in `ShellProcess`, so we define an abstract function `child_function()` which the child class has to implement. We also add `child_function_wrapper()` so our base class can to execute some code when in the child process.

We can't pass non-static methods as function pointers, so we pass `this` as argument to the static function `child_function_with_this()`.

{% highlight cpp %}
// sub_process.cpp
struct NamespaceProcess {
    virtual int child_function() = 0;

    static int child_function_with_this(void *context) {
        return static_cast<NamespaceProcess*>(context)
            ->child_function_wrapper();
    }

    int child_function_wrapper() {
        return child_function();
    }

    int create_child(int clone_flags) {
        char *stack_top = allocate_stack();
        int pid = clone(
            &NamespaceProcess::child_function_with_this, 
            stack_top, 
            clone_flags, 
            (void *)this
        );
    
        if (pid == -1) {
            fatal_action("Cloning");
        }
        return pid;
    }

    int run() {
        int clone_flags = 0;
        int pid = create_child(clone_flags);
        return 0;
    }
}
{% endhighlight %}

The child class looks like:

{% highlight cpp %}
// main.cpp

class ShellProcess : public NamespaceProcess {
    int child_function() {
        cout << "Hello World" << endl;
        return 0;
    }  
}
{% endhighlight %}

We should be able to compile and run this, but we might not see any results because the parent process ends before the child can run. We need some synchonization.

## Synchonization I

We want the parent process to wait for the child to finish. We can wait for the `SIGCHLD` signal, which the child will only emit if we pass the flag to `SIGCHLD` to `clone()`: 

{% highlight cpp %}
// sub_process.cpp
struct NamespaceProcess {
    // ...

    int run() {
        int clone_flags = SIGCHLD;
        int pid = create_child(clone_flags);

        // Wait for child to finish 
        int status = 0;
        while (wait(&status) > 0);

        return 0;
    }
}
{% endhighlight %}

## Shell Process

Let's use a better implementation for `ShellProcess` so we can try out commands in a jailed environment.

In the example below we start a new shell, replacing the current child process. We xustomize it with a new `PS1` so it's more obvious when we are inside the child process.

{% highlight cpp %}
// main.cpp

class ShellProcess : public NamespaceProcess {
    int child_function() {
        const char *env[] = { "PS1=^_^: ", NULL };

        int ret = execle(
            "/bin/bash", 
            "/bin/bash", 
            "--norc", 
            "-i", 
            NULL, 
            env
        );
        if (ret == -1) {
            fatal_action("Failed to start shell");
        }

        return 0;
    }  
}
{% endhighlight %}

We can try it out:

{% highlight text %}
$: g++ -std=c++17 main.cpp utils.cpp
./a.out
^_^: whoami
kunigami
^_^: sudo su
{% endhighlight %}

By default the child processhas access to the same resources as the parent, include root access. We want to restrict that.

## User Isolation

We're ready for our first namespace, the user. We can simply do so by adding the `CLONE_NEWUSER` to `clone()`.

{% highlight cpp %}
// sub_process.cpp
struct NamespaceProcess {
    // ...

    int run() {
        int clone_flags = SIGCHLD | CLONE_NEWUSER;
        int pid = create_child(clone_flags);

        // ...
    }
}
{% endhighlight %}

Once we try this out, we get:

{% highlight text %}
^_^: whoami
nobody
{% endhighlight %}

The user namespace starts blank. We'll resolve this later, but let's leave it as is for now.

## Mount Isolation

Namespacing the user is interesting for us because we don't need root access to run root-only operations in the child process, since `uid=0` is considered root.

{% highlight text %}
^_^: id
uid=0 gid=65534 groups=65534
{% endhighlight %}

Let's also create a mount namespace by adding the `CLONE_NEWNS` flag.

{% highlight cpp %}
// sub_process.cpp
struct NamespaceProcess {
    // ...

    int run() {
        int clone_flags = SIGCHLD | CLONE_NEWUSER | CLONE_NEWNS;
        int pid = create_child(clone_flags);

        // ...
    }
}
{% endhighlight %}

Differently from the user namespace which starts everything empty, the mount namespace starts with a copy of the host's mount system, but we want to restrict that. We'll define a new root for our filesystem and mount only a selected few paths on it, using `pivot_root()`.

This is the most complicated part of the code, so let's go over the high-level steps. 

* Make the current root (`/`) a private mount point (it's shared by default). This [article](https://lwn.net/Articles/689856/) goes over the different types of mount points. From [2]:

> These restrictions ensure that `pivot_root()` never propagates any changes to another mount namespace.

* Make sure the new root is a mount point. From [2]:

> `new_root` must be a path to a mount point, but can't be "/".  A path that is not already a mount point can be converted into one by bind mounting the path onto itself

* Mount a selection of paths `P` (provided by the child class) onto the new root
* Create a temporary directory `put_old` (under the new root), where the old root will be temporarily stored. From [2]:

> `put_old` must be at or underneath `new_root`

* Pivot root - This makes `new_root` the new root and it mounts the old root onto `put_old`
* Re-mount the paths `P` - It seems that `pivot_root()` unmounts prior mounts so we have to remound. I don't actually understand why we need to mount twice.
* Unmount the old root

Most of these steps are described as an example in the man page of [`pivot_root()`](https://man7.org/linux/man-pages/man2/pivot_root.2.html
) [2].

In code it will look like:

{% highlight cpp %}
// sub_process.cpp

int mount_onto_new_root(string path) {
    unsigned long flags = MS_BIND | MS_REC | MS_PRIVATE;
    string dstpath = format("%s%s", NEW_ROOT_DIR, path.c_str());

    create_dir_recursively(dstpath);

    if (mount(path.c_str(), dstpath.c_str(), "proc", flags, "") == -1) {
        fatal_action("mounting directory");
    }
    return 0;
  }

int remount(string path) {
    unsigned long flags = MS_RDONLY | MS_REMOUNT | MS_BIND;
    if (mount(path.c_str(), path.c_str(), NULL, flags, "") == -1) {
        fatal_action("remounting directory");
    }
    return 0;
}

struct NamespaceProcess {

// ...
virtual vector<string> get_mount_paths() = 0;
    
int new_root() {        
    // Create a directory if it doesn't exist
    mkdir(NEW_ROOT_DIR, 0777);

    // For pivot_root to work the root of the current file tree 
    // must not have shared propagation
    if (mount(NULL, "/", NULL, MS_REC | MS_PRIVATE, NULL) == -1) {
        fatal_action("executing mount() on /");
    }  

    // Ensure that 'new_root' is a mount point
    if (mount(NEW_ROOT_DIR, NEW_ROOT_DIR, NULL, MS_BIND, NULL) == -1) {
        fatal_action("executing mount() on new root");
    }

    vector<string> paths = get_mount_paths();
    // Mount paths
    for(auto srcpath:paths) {
        mount_onto_new_root(srcpath);
    }

    // Create temporary directory to store the old root
    string old_root_dir = format("%s%s", NEW_ROOT_DIR, PUT_DIR);
    mkdir(old_root_dir.c_str(), 0777);

    if (pivot_root(NEW_ROOT_DIR, old_root_dir.c_str()) == -1) {
        fatal_action("executing pivot_root()");
    }

    if (chdir("/") == -1) {
        fatal_action("moving to new root");
    }

    // Remount paths
    for(auto srcpath:paths) {
        remount(srcpath);
    }

    int ret;
    if ((ret = umount2(PUT_DIR, MNT_DETACH)) == -1) {
        error_action(format("Failed unmounting"));     
    }

    if (ret < 0) {
        cerr << "Failed creating new root" << endl;
        exit(EXIT_FAILURE);
    }

    return 0;
}

// ...
}
{% endhighlight %}

We need to make sure this functions is run before `child_function()` so we can do:

{% highlight cpp %}
// sub_process.cpp
struct NamespaceProcess {
    // ...

    int child_function_wrapper() {
        new_root();
        child_function();
    }
}
{% endhighlight %}

**Warning:** Make sure `new_root()` is run by the child process and that the mount namespace is used! If you get permission denied and have to use `sudo` you're doing it wrong!

The child just needs to provide some paths that it would like to mount (read-only):

{% highlight cpp %}
// main.cpp
class ShellProcess : public NamespaceProcess {
    vector<string> get_mount_paths() {
        vector<string> paths = {
            "/usr/bin",
            "/usr/sbin",
            "/bin",
            "/usr/lib",
            "/lib",
            "/lib32",
            "/lib64",
            "/libx32"
        };
        return paths;
  }
}

{% endhighlight %}

We should now have a minimal jailed system up and running!

{% highlight text %}
^_^: ls
bin   home   lib   lib32   lib64   libx32   old_root   proc   tmp   usr
{% endhighlight %}

## References

* [[1](https://en.wikipedia.org/wiki/Chroot)] namespaces
* [2] pivot_root 
