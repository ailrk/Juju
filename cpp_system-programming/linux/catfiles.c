#include <fcntl.h>
#include <linux/fs.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/uio.h>

#if defined(Debug)
const size_t BLOCK_SZ = sizeof(void *);
#else
const size_t BLOCK_SZ = 4096;
#endif

off_t get_file_size(int fd) {
  struct stat st;

  // use fstat and fd to get status info of the file.
  if (fstat(fd, &st) < 0) {
    perror("fstat");
    return -1;
  }

  // test file type
  if (S_ISBLK(st.st_mode)) {
    off_t bytes;

    // ioctl generic way to send request to a device driver.
    if (ioctl(fd, BLKGETSIZE64, &bytes) != 0) {
      perror("ioctl");
      return -1;
    }
    return bytes;

  } else if (S_ISREG(st.st_mode)) {
    return st.st_size;
  }

  return -1;
}

void output_to_console(const char *buf, size_t len) {
  fwrite(buf, 1, len, stdout);
}

int read_and_print_file(char *filename) {
  // simple buffer with length
  struct iovec *iovecs;
  int file_fd = open(filename, O_RDONLY);

  if (file_fd < 0) {
    perror("open");
    return 1;
  }

  off_t file_sz = get_file_size(file_fd);
  off_t bytes_remaining = file_sz;

  int blocks = (int)file_sz / BLOCK_SZ;
  if (file_sz % BLOCK_SZ)
    blocks++;
  iovecs = (struct iovec *)malloc(sizeof(struct iovec) * blocks);

  int current_block = 0;

  while (bytes_remaining) {
    off_t bytes_to_read = bytes_remaining;
    if (bytes_to_read > BLOCK_SZ) {
      bytes_to_read = BLOCK_SZ;
    }

    // allocate buffer for iovecs.
    void *buf;
    if (posix_memalign(&buf, BLOCK_SZ, BLOCK_SZ)) {
      perror("posix_memalign");
      return 1;
    }

    iovecs[current_block].iov_base = buf;
    iovecs[current_block].iov_len = bytes_to_read;
    current_block++;
    bytes_remaining -= bytes_to_read;
  }

  // read files into iovecs blocks.
  // (base: [...], len), (base: [...], len ...)
  // readv will block until all buffers are filled.
  int ret = readv(file_fd, iovecs, blocks);
  if (ret < 0) {
    perror("readv");
    return 1;
  }

  for (int i = 0; i < blocks; i++) {
    output_to_console((const char *)iovecs[i].iov_base, iovecs[i].iov_len);
  }

  free(iovecs);
  return 0;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s <filename> [<filename1>...]\n", argv[0]);
  }

  for (int i = 1; i < argc; i++) {
    if (read_and_print_file(argv[i])) {
      fprintf(stderr, "Error reading file\n");
      return 1;
    }
  }

  return 0;
}
