---
layout: post
title: "Namespace Jailing"
vanity: "2021-07-01-namespace-jail"
tags: [operating systems, bash, c]
excerpt_separator: <!--more-->
---

{% include blog_vars.html %}

In a [previous post](({{site.url}}/blog/2021/04/19/chroot-jail.html)) we investigated a jail system using chroot with the conclusion that it was not a safe implementation. In this post we'll study a safer alternative using Linux namespaces. We'll develop a C++ application along the way.

<!--more-->


## Linux Namespaces

The idea of Linux namespaces [1] is actually very close to that of a sandbox. We want to create subsystems within a system which are isolated, so if they're tampered with, the hosting sytem is protected.

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

The [`clone()`](https://man7.org/linux/man-pages/man2/clone.2.html) function is a general version of `fork()` [2], which allows for more granular configuration. It can be used to start a new child process. It takes a few arguments:

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

We want the parent process to wait for the child to finish. We can wait for the `SIGCHLD` signal, which the child will only emit if we pass the flag to `SIGCHLD` to `clone()` [2]:

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

In the example below we start a new shell, replacing the current child process. We customize it with a new `PS1` so it's more obvious when we are inside the child process.

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

By default the child process has access to the same resources as the parent, include root access. We want to restrict that.

## User Isolation

We're ready for our first namespace, the user. We can simply do so by adding the `CLONE_NEWUSER` to the flags passed to `clone()`.

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

When we run:

{% highlight bash %}
^_^: whoami
nobody
^_^: id
uid=65534 gid=65534 groups=65534
{% endhighlight %}

The user metadata starts blank and `65534` represents undefined. Let's fix this.

## Mapping User IDs

We can create a mapping between IDs inside the namespace and outside [3]. The map is stored in the file `/proc/<pid>/uid_map`, where `<pid>` is the ID of the current process [4].

So for example, if we have a process with PID 31378, we can inspect that file:

{% highlight bash %}
> cat /proc/31378/uid_map
0 0 4294967295
{% endhighlight %}

Each line represent one mapping. The meaning of each column is "ID_inside-ns", "ID-outside-ns" and "length" [4]. These three numbers represent 2 ranges of the same length, the first is `[ID_inside-ns, ID_inside-ns + length - 1]` and the second is `[ID_outside-ns, ID_outside-ns + length - 1]`, and ids in the first range map to ids in the second range.

This is much easier to understand with an example, if we have a line with `10 1000 3`, it means the range of ids `[10, 11, 12]` in the current process maps to the parent process `[1000, 1001, 1002]`, thus `0 0 4294967295` (which is the default mapping) effectively represent a 1:1 mapping between every id.

We can create a simple map so that the user ID 0 in the child maps to our current user running the parent:

{% highlight cpp %}
map<int, int> get_uid_map() {
  map<int, int> uid_map = {
    {
      0,
      getuid(),
    }
  };
  return uid_map;
}
{% endhighlight %}

Then we write to the file corresponding to a given pid:

{% highlight cpp %}
int set_uid_map(map<int, int> uid_map, int pid) {
  string uid_map_filename = format("/proc/%d/uid_map", pid);

  ofstream fs;
  fs.open(uid_map_filename.c_str());

  if (fs.fail()) {
    fatal_action("opening uid_map file");
    return 1;
  }

  for (auto const& [in_id, out_id]: uid_map) {
    fs << in_id << " " << out_id << " " << 1 << endl;
  }

  fs.close();
  return 0;
}
{% endhighlight %}

The tricky part is that the child process does not have privileges to write to its own `uid_map` file, so it's the parent that has to do it. Let's assume we have a function `before_child_runs()` that takes the child `pid` and as the name suggests runs before the child. This is where we set the uid map:

{% highlight cpp %}
int before_child_runs(int pid) {
    uid_map = get_uid_map();
    set_uid_map(uid_map, pid);
    return 0;
}
{% endhighlight %}

To guarantee the right order of execution, we'll need more synchronization.

## Synchonization II

We'll use pipes for this as in [4]. A pipe `pipe_fd` contains two file descriptors: `pipe_fd[0]` is the *read* end of the pipe, and `pipe_fd[1]` is the *write* end.

When we clone a process the child inherits a copy of the open file descriptors, so pipes can be used as a IPC (inter-process communication) medium. We can also use it as a synchronization mechanism, because the `read()` function blocks until it receives the requested amount of data or the other side closes the file descriptor.

{% highlight cpp %}
struct NamespaceProcess {

private:
  int pipe_fd[2];

  // ...

  int child_function_wrapper() {
    // won't use
    close(pipe_fd[1]);

    // Block on parent - request a non-zero number of chars
    char ch;
    if (read(pipe_fd[0], &ch, 1) != 0) {
      cerr << "Failure in child: read from pipe returned != 0" << endl;
    }

    close(pipe_fd[0]);

    // ...
  }

  int run() {
    if (pipe(pipe_fd) == -1) {
        fatal_action("open pipe");
    }

    int clone_flags = get_custom_clone_flags() | SIGCHLD;
    int pid = create_child(clone_flags);

    // won't use, but has to be closed after the child
    // was created
    close(pipe_fd[0]);

    before_child_runs(pid);

    // Unblocks child
    close(pipe_fd[1]);

    // ...
  }
{% endhighlight %}

Now we can check the user is correct:

{% highlight bash %}
^_^: whoami
kunigami
^_^: id
uid=0 gid=65534 groups=65534
{% endhighlight %}

Note that we have to do the same for the group id, which is a very similar process but we'll skip for the sake of simplicity.

## Mount Isolation

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

* Make the current root (`/`) a private mount point (it's shared by default). This [article](https://lwn.net/Articles/689856/) goes over the different types of mount points. From [5]:

> These restrictions ensure that `pivot_root()` never propagates any changes to another mount namespace.

* Make sure the new root is a mount point. From [5]:

> `new_root` must be a path to a mount point, but can't be "/".  A path that is not already a mount point can be converted into one by bind mounting the path onto itself

* Mount a selection of paths `P` (provided by the child class) onto the new root
* Create a temporary directory `put_old` (under the new root), where the old root will be temporarily stored. From [5]:

> `put_old` must be at or underneath `new_root`

* Pivot root - This makes `new_root` the new root and it mounts the old root onto `put_old`
* Re-mount the paths `P` - It seems that `pivot_root()` unmounts prior mounts so we have to remount. I don't actually understand why we need to mount twice, but it only works if I do this, and this is also what [nsjail](https://github.com/google/nsjail) does [7].
* Unmount the old root

Most of these steps are described as an example in the man page of [`pivot_root()`](https://man7.org/linux/man-pages/man2/pivot_root.2.html
) [6].

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

We need to make sure this function is run before `child_function()` so we can do:

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

**Warning:** Make sure `new_root()` is run by the child process and that the mount namespace is used! If you get permission denied and have to use `sudo` you're doing it wrong! (Speaking from experience >.<)

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

## Conclusion

In this post we went through all the details of creating a shell process with user and mount namespaces. Once we unmount the old root after `pivot_root`, the old root does not stay around (though hidden) like it does via chroot [8].

The process of starting with everything disabled and painfully adding capabilities is a great way to understand how things are implemented behind the scenes, for example the `/proc/<pid>/uid_map`.

Ed King's series [3] on Linux namespaces using Go is very instructive, where they use a higher-level API, which makes it easier to follow. The man pages from man7.org are very helpful, especially the examples!

I'd like to sandbox the network as well, but will leave it to a future post.

## References

* [[1](https://en.wikipedia.org/wiki/Linux_namespaces)] Wikipedia - Linux Namespaces
* [[2](https://man7.org/linux/man-pages/man2/clone.2.html)] clone(2) — Linux manual page
* [[3](https://medium.com/@teddyking/linux-namespaces-850489d3ccf)] Linux Namespaces - Ed King
* [[4](https://man7.org/linux/man-pages/man7/user_namespaces.7.html)] user_namespaces(7) — Linux manual page
* [[5](https://lwn.net/Articles/689856)] LWN.net: Mount namespaces and shared subtrees
* [[6](https://man7.org/linux/man-pages/man2/pivot_root.2.html)] pivot_root(2) — Linux manual page
* [[7](https://github.com/google/nsjail)] Github: google/nsjail
* [[8](https://news.ycombinator.com/item?id=23167383)] Hacker News: comment on *Linux containers in a few lines of code*
