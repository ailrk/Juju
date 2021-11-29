#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

// signal handling.
// signal:      specify the signal for the current process.
// sigfillset:  add all semaphore into one signal set.
// execve:      execute
// sigprocmsk:  mask process signal.

// - after fork, the child process will inherits it's parent's setting on signal
// masks and signal handling configs
// - execve will reset the text section, heap, and stack of the current process.
// - every process has their own signal mask set. if a signal in the mask is
// fired, it will be pushed into a queue and defer the handling. We say the
// signal is in a pending state.
// - after child process exit, it will send the parent process a SIGCHLD. It
// indicates that the parent process need to call wait_pid to handle the child
// process. If the parent didn't handle the child process, the child process
// will become a zombie process.
//
// - after a signal is triggered, it will jump to the corresponding signal
// handling function.

void deletejob(pid_t pid) { printf("delete task %d\n", pid); }
void addjob(pid_t pid) { printf("add task %d\n", pid); }

void handler(int sig) {
  int olderrno = errno;
  sigset_t mask_all, prev_all;
  pid_t pid;

  // add all semaphore to the signal set.
  sigfillset(&mask_all);
  while ((pid = waitpid(-1, NULL, 0)) > 0) {
    // block signal to make sure the job will not be interrupted.
    sigprocmask(SIG_BLOCK, &mask_all, &prev_all);
    deletejob(pid);
    // unblock
    sigprocmask(SIG_SETMASK, &prev_all, NULL);
  }

  if (errno != ECHILD) {
    printf("waitpid error");
  }
}

int main(int argc, char *argv[]) {

  int pid;
  sigset_t mask_all, prev_all;
  sigfillset(&mask_all);

  // if receiveSIGCHLD, call handler
  signal(SIGCHLD, handler);
  while (1) {
    if ((pid = fork()) == 0) {
      // fork and execute a new process.
      // this will call date program, and then exit.
      // when a child process exits, it will send it's parent a SIGCHLD singal.
      // The parent process should call wait_pid to handle it.
      execve("/bin/date", argv, NULL);
    }

    // if it's the parent proces, report the new process just created. This is
    // protected by sigprocmask, so if there is a SIG_BLOCK sent in addjob will
    // still be called.
    sigprocmask(SIG_BLOCK, &mask_all, &prev_all);
    addjob(pid);
    sigprocmask(SIG_SETMASK, &prev_all, NULL);
  }
  return 0;
}
