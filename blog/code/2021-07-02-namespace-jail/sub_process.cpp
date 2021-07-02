#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <vector>

#include <fcntl.h>
#include <limits.h>
#include <sched.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/syscall.h>
#include <sys/stat.h>

#include <unistd.h>

#include "utils.h"

using namespace std;

#define STACK_SIZE (1024 * 1024) 
#define PUT_DIR "old_root/"
#define NEW_ROOT_DIR "/tmp/new_root/"

/* Glibc does not provide a wrapper for this system call */
static int pivot_root(const char *new_root, const char *put_old) {
  return syscall(SYS_pivot_root, new_root, put_old);
}

/* Allocate memory to be used for the stack */
static char* allocate_stack() {
  char *stack = (char *)mmap(
     NULL,
     STACK_SIZE,
     PROT_READ | PROT_WRITE,
     MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK,
     -1,
     0
   );

   if (stack == MAP_FAILED) {
     cerr << "Failed to allocate memory" << endl;
   }

  char *stack_top = stack + STACK_SIZE;
  return stack_top;
}

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

map<int, int> get_uid_map() {
  map<int, int> uid_map = {
    {
      0,
      getuid(),
    }
  };
  return uid_map;
}

// Lazy implementation. Use shell commands
int create_dir_recursively(string path) {
  string command = format("mkdir -p %s", path.c_str());
  cout << command << endl;
  return system(command.c_str());
}

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

private:
  int pipe_fd[2];
  
  virtual int child_function() = 0;
  virtual vector<string> get_mount_paths() = 0;

  static int child_function_with_this(void *context) {
    return static_cast<NamespaceProcess*>(context)->child_function_wrapper();
  }

  int new_root() {        
    // Create a directory if it doesn't exist
    mkdir(NEW_ROOT_DIR, 0777);

    // For pivot_root to work the root of the current file tree must not have
    // shared propagation
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

  int child_function_wrapper() {

    close(pipe_fd[1]);

    // Block on parent 
    char ch;
    if (read(pipe_fd[0], &ch, 1) != 0) {
      cerr << "Failure in child: read from pipe returned != 0" << endl;
    }

    close(pipe_fd[0]);

    new_root();

    child_function();

    return 0;
  }

  // Functions to run after the child process is created but before it runs anything
  int before_child_runs(int pid) {
    auto uid_map = get_uid_map();
    set_uid_map(uid_map, pid);

    return 0;
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
    cout << "child pid: " << pid << endl;
    return pid;
  }

public:

  int run() {
    if (pipe(pipe_fd) == -1) {
      fatal_action("open pipe");
    }
 
    int clone_flags = CLONE_NEWUSER | CLONE_NEWNS | SIGCHLD;
    int pid = create_child(clone_flags);

    close(pipe_fd[0]);

    before_child_runs(pid);

    // Unblocks child
    close(pipe_fd[1]);

    // Wait for child to finish 
    int status = 0;
    while (wait(&status) > 0);
    return 0;
  }  
};
