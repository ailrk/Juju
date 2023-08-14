#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


// a file descriptor is a number. 
// each process has it's own fd table, and it's maintained by the kernel
// kernel will use fd number as key to query the table to know what file it refers to.

void open_devzero(int *fd_ptr, char const *name) {
    int fd;
    if ((fd = open("/dev/zero", O_RDONLY)) < 0) {
        fprintf(stderr, "Unable to open /dev/zero, %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    } else {
        printf("%s: %d\n", name, fd); 
    }
    *fd_ptr = fd;
}

void stream_devzero(FILE *f, char const *name) {
    if ((f = fopen("/dev/zero", "r")) == NULL) {
        fprintf (stderr, "Unable to open /dev/zero: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    } else {
        printf ("%s: %d\n", name, fileno(f));
    }
}

int main () {

    int fd1, fd2, fd3;
    FILE *f;
    open_devzero(&fd1, "fd1");
    open_devzero(&fd2, "fd2");
    open_devzero(&fd3, "fd3");

    close(fd1);
    close(fd2);

    stream_devzero(f, "file f");
    fclose(f);

    {
        close(STDERR_FILENO);
        open_devzero(&fd3, "fd3");
    }
}
