// how do we avoid blocking?
// 1. set all descriptor in nonblocking mode, and busy polling on all of them.
// (100% cpu usage.)
// 2. enable all descriptors of interest to send singal when IO can be done
// (fcntl, SIGIO)
// 3. System provide a method for asking which descriptor are capable of
// performing IO (epoll)
// 4. Having the process register with OS all events on descriptor that is
// intereted in tracking.

#include <stdio.h>
int main(int argc, char *argv[]) {
  return 0;
}
