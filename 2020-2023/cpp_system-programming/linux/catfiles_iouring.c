#include <linux/fs.h>
#include <linux/io_uring.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/uio.h>

#define QUEUE_DEPTH 1
#define BLOCK_SZ 1024

// To avoid memory reordering.
// Note: memory reordering are ways the compiler uses to utilize some
// cpu technqiues like pipelining.
// But it might not be what we want. For example, int
#define read_barrier() __asm__ __volatile__("" :::"memory")
#define write_barrier() __asm__ __volatile__("" :::"memory")
