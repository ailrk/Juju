// how many open files a process can have?

#include <sys/resource.h>
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int count_open_files(int num);

void open_files(int num);

int main() {
    int openmax;
    struct rlimit rlp;

#ifdef OPEN_MAX
    printf("OPEN_MAX is defined as %d\n", OPEN_MAX);
#else
    printf("OPEN_MAX is not defined on this platform. \n");
#endif
    
    printf("getconf OPEN_MAX says: ");
    fflush(stdout);
    system("getconf OPEN_MAX");
    
    // see man: gettablesize(3)
    errno = 0; 
}
