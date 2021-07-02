#include <cstdio>
#include <iostream>
#include <memory>
#include <signal.h>
#include <stdexcept>
#include <string>
#include <string.h>
#include <array>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>

#include "sub_process.cpp"

using namespace std;

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

  int child_function() {
    const char *env[] = { "PS1=^_^: ", NULL };

    int ret = execle("/bin/bash", "/bin/bash", "--norc", "-i", NULL, env);
    if (ret == -1) {
      fatal_action("Failed to start shell");
    }

    return 0;
  }  
};

int main() {

  auto process = ShellProcess();
  return process.run();
}
