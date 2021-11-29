#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

// iovec is a very common facility used for linux system programming.

int main(void) {
  const char *buf1 = "Hello, ";
  const char *buf2 = "Haskell ";
  const char *buf3 = "Curry\n";

#ifdef DEBUG
  fprintf(stderr, "sizeof buf1: %d\n", sizeof(buf1));
#endif

  // sizeof(...) - 1 because we don't want to write \0.
  struct iovec bufs[] = {
      {.iov_base = (void *)buf1, .iov_len = sizeof(buf1) - 1},
      {.iov_base = (void *)buf2, .iov_len = sizeof(buf2) - 1},
      {.iov_base = (void *)buf3, .iov_len = sizeof(buf3) - 1},
  };

  // write data of iovec.
  if (writev(STDOUT_FILENO, bufs, sizeof(bufs) / sizeof(*bufs)) == -1) {
    perror("writev()");
    exit(EXIT_FAILURE);
  }

  return EXIT_SUCCESS;
}
