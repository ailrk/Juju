#include <iostream>
#include <sys/wait.h>
#include <unistd.h>

//
// fork
//
// fork creates a new process, copy the parent process's memory,
// file descritptor, and ip.
//
// This means once a child process is forked, and it's scheduled, it gonna
// copy the behavior of the parent from the point that it's forked.
//
// for() returns 0 if it's a child, otherwise it's gonna return the pid of
// the child. So you have info of which child you created is you 're the
// parent, but as child you don't know who created you.

void fork1(void) {

    // - at the beginning we have 2^4 = 16 processes.
    fork();
    fork();
    fork();
    fork();

    // - each process fork, so total double to 32, with 16 children.
    if (auto cid = fork() != 0) {
        // - their children fork again, so add 16, now 48 in total.
        if (auto cid = fork() != 0) { // nested forking.
            std::cout << "I'm child's child" << getpid() << "\n";
        } else {
            std::cout << "I'm child" << getpid() << " of child's child " << cid
                      << "\n";
        }
    } else {
        std::cout << "I'm parent " << getpid() << " of child " << cid << "\n";
    }
}

int main(void) {
    fork1();

    return 0;
}
