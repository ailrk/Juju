#include <iostream>
#include <sys/wait.h>
#include <unistd.h>

//
// wait
//
// Once we fork a child, it's execution is completely detached from the
// parent. In some cases, a parent can finish before the child. This cause
// a problem because how all processes are structured by the os.
//
// Typically you have a process with pid 1 called init that starts all
// processes, new processes are forked from the inital process, and they
// fork more, eventually form a tree of processes.
//
// In general, when a process is gone, there is no code running from that
// process. But also the process will left with some resources it was using,
// or it's in some state that need somebody to clean it up. For example it
// might acuqiring some socket, and somebody needs to close it.
//
// Because processes form a tree, we let parent processes to clean up their
// children. As long as parents out lives their children, dead children can
// be handled properly.
//
// However, we don't guarantee parents always outlive children. It's totally
// possible that a parent process lives shorter and left he children orphan.
//
// wait() allows us to wait for the temrination of a specific process, so
// we can guarantee some operations to happen after child is dead. This gives
// us more control over processes.

// note: wait() waits for any children to terminate. So once one
// child is done, the parent is no longer waiting anymore.

// what's going on here.
// The main process forks three children first.
// each process will go into a loop to waits it's chldren to terminate.
// for children, their child count is 0, is no child wait return -1 right a way
// for parent, the wait will put the process in sleep until any one of it's
// child is awake. Then it will proceed from wherever it beginned to sleep.

void wait2() {
    int id;

    auto id1 = fork();
    auto id2 = fork();
    auto id3 = fork();
    std::cout << getpid() << "\n";

    while (1) {
        id = wait(nullptr); // wait will return the pid of the exited.child

        if (id == -1) // -1 means no more children.
            break;
        if (id == id1)
            std::cout << "child #1 is done " << getpid() << "\n";
        if (id == id2)
            std::cout << "child #2 is done " << getpid() << "\n";
        if (id == id3)
            std::cout << "child #3 is done " << getpid() << "\n";
    }

    if (id1 != 0 && id2 != 0 && id3 != 0) {
        std::cout << "paren is done\n";
    }
}

int main(void) {
    wait2();
    return 0;
}
