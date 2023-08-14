#include <assert.h>
#include <fcntl.h>
#include <liburing.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// TODO

struct read_descriptor {
  int fd;
  char *buf;
  size_t pos;
  size_t size;
  int result;
};

void free_read_descriptor(struct read_descriptor *desc) {
  free(desc->buf);
  free(desc);
}

int dispatch_reads(struct io_uring *ring, struct read_descriptor *descv,
                   size_t nrdesc) {
  int i;

  for (i = 0; i < nrdesc; i++) {
    struct io_uring_sqe *sqe;
    struct read_descriptor *desc = &descv[i];

    sqe = io_uring_get_sqe(ring);
    io_uring_prep_read(sqe, desc->fd, desc->buf, desc->size, desc->pos);
    io_uring_sqe_set_data(sqe, desc);
  }

  return io_uring_submit(ring);
}

int consume_reads(struct io_uring *ring) {
  int completed;
  int head;
  struct io_uring_cqe *cqe;

  io_uring_for_each_cqe(ring, head, cqe) {
    completed++;
    struct read_descriptor *desc = (struct read_descriptor *)cqe->user_data;
    desc->result = cqe->res;
  }

  io_uring_cq_advance(ring, completed);
}

int init_files(struct io_uring *ring, const char *prefix, int *fds,
               const size_t n_fds) {

  if (strlen(prefix) > 128) {
    fprintf(stderr, "the length of file prefix cannot exceed 128\n");
  }

  if (n_fds > 128) {
    fprintf(stderr, "cannot create more than 128 files at once\n");
  }

  const char *buf = "some text to write";
  int fd;
  char filename[256];

  for (int i = 0; i < n_fds; i++) {
    sprintf(filename, "%s-%d", prefix, i);
    fd = open(filename, O_CREAT | O_RDWR, S_IRUSR | S_IRGRP | S_IROTH);
    fds[i] = fd;

    struct io_uring_sqe *sqe;
    sqe = io_uring_get_sqe(ring);
    io_uring_prep_write(sqe, fd, buf, strlen(buf), 0);
  }
  return io_uring_submit(ring);
}

int complete_write_files(struct io_uring *ring, const size_t n_fds) {}

struct read_descriptor *make_descriptors(struct io_uring *ring,
                                         const size_t n) {
  int fds[n];
  init_files(ring, "sample", fds, n);

  struct read_descriptor *descv =
      (struct read_descriptor *)malloc(sizeof(struct read_descriptor) * n);

  for (int i = 0; i < n; i++) {
    descv[i].fd = fds[i];
  }

  return NULL;
}

int main(void) {
  for (;;) {
  }
  return 0;
}
