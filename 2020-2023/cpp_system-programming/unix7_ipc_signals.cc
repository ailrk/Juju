#include <iostream>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>

//
// signals
//
// Signal is another inter process communication method. It can interrupt a
// process, and let a child to receive the interrupt and run some handler.
//

bool g_loop = true;

void handler(int sig) {
    if (sig == SIGINT) {
        std::cout << "handler called... run in 1 sec\n";
        sleep(1);
    }
    g_loop = false;
}

void system1(char const *cmd) {
    if (auto id = fork(); id > 0) {
        sleep(2);
        kill(id, SIGINT); // kill is used to send signal to a process
    } else {
        execlp(cmd, "", nullptr);
    }
}

int main(void) {

    int pid = fork();
    if (pid != 0) { // parent try to kill child each 2 sec.
        sleep(3);
        kill(pid, SIGINT);
        wait(nullptr);

    } else {
        // singal takes a signal enum, and a call back as it's handler.
        signal(SIGINT, handler);
        while (g_loop) {
            std::cout << "..." << std::endl;
            sleep(1);
        }
    }

    std::cout << "try ls";
    system1("htop");

    return 0;
}
