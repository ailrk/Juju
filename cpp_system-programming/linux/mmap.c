#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

// map or unmap files or devices into memory
//
// mmap creats a new mapping in the virtual address space of the calling
// process.
// params:
//  addr: starting address for the new mapping
//  length: length of the mapping
//  prot: memory protectoin level
//  flags: general configs (shared? etc)
//  fd:
//  offset:
//
// What can you do with it?
//   map something to your virtual memory address.
//   the "something" can be very board. It can be actual physical memory, disk,
//   or socket.

//////////////////////////////////////////////////
// basic usage of mmap
int basic() {
  // get the size of a system page
  size_t pagesize = getpagesize();

  printf("System page size: %zu\n", pagesize);

  char *region = (char *)mmap((void *)(1 << 20), pagesize,
                              PROT_READ | PROT_WRITE | PROT_EXEC,
                              MAP_ANON | MAP_PRIVATE, 0, 0);
  strcpy(region, "Hello mmap\n");
  printf("Content of the region: %s", region);

  if (munmap(region, 1 << 10)) {
    perror("Cannot unmap\n");
    return 1;
  }

  return 0;
}

//////////////////////////////////////////////////
// allocate heap anonymous memory
// note leak on memory acquired by mmap will not be detected by valgrind.
void allocate_anonymous_memory_helper(void **p) {
  size_t pagesize = getpagesize();
  *p = mmap(0, pagesize * 10, PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

int allocate_anonymous_memory() {
  char *p = NULL;
  allocate_anonymous_memory_helper((void **)&p);

  strcpy(p, "hi I'm newly allocated memory");
  printf("%s\n", p);

  if (munmap(p, getpagesize() * 10)) {
    perror("Cannot unmap\n");
    return 1;
  }
  return 0;
}

//////////////////////////////////////////////////
// render a piece of memory unreadable

void unreadable() {
  size_t pagesize = getpagesize();
  void *p = malloc(128);
  size_t pg = pagesize > 128 ? pagesize : ceil(pagesize / 128);
  void *q = mmap(p, pg, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  strcpy((char *)p, "try this\n");

  // this will segfault now.
  // printf("\s\n", q);

  if (munmap(q, pg)) {
    perror("Cannot unmap\n");
  }
}

//////////////////////////////////////////////////
// map virtual address space to a disk file directly
// This will not actually load the entire content in to the memory. Instead
// data is loaded when used.
// There is no guarantee that when modificaition will be flushed. Linux
// ensure change on the file will be flushed on the disk, but it can happen
// anytime after the write operation is done.
// msync can be used to force flush.

int map_disk_file() {
  const char str1[] = "string 1";

  void *p, *q;
  int fd = -1;
  if ((fd = open("./sample1", O_RDWR, 0)) == -1) {
    perror("open error\n");
    return -1;
  }

  p = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_ANON | MAP_SHARED, -1, 0);

  // the OS maintains page cache like how db does. When accessing a mmaped
  // file, kernel loads the portion into the cache. The cache resides in the
  // kernel space.
  // After operation is done, the modified content will remain in the page
  // cache. The OS will flush the change when possible.
  q = mmap(NULL, 3096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

  printf("q from file: %s\n", (char *)q);
  strcpy((char *)p, (char *)q);

  // handle mmaped memory as an ordinary memory
  char *cur = NULL;
  for (cur = (char *)p; *cur != '\0'; ++cur)
    ;

  strcpy(cur, str1);

  printf("p copied from q append with string1: %s\n", (char *)p);

  if (munmap(p, 4096)) {
    perror("unmap erroron p");
    return 1;
  }

  if (munmap(q, 4096)) {
    perror("unmap erroron p");
    return 1;
  }

  return 0;
}

int main(void) {
  // allocate_anonymous_memory();

  // unreadable();
  map_disk_file();
  return 0;
}
