#include <algorithm>
#include <iostream>
#include <sys/wait.h>
#include <unistd.h>

#include <array>
#include <string_view>

//
// exec
//
// exec family is used to execute a binary file. This is acheived by overriding
// the current process with a completely new process.
//
// it will load an elf file, decode and load it's content into current process's
// memory, set ip at the beginning of the text section, and start the
// execution.
//

// fork + exec is a classic example that unix ppl use to brag about how well
// their system design is.
void system1(char const *cmd, char const *args) {
    if (auto id = fork(); id != 0) {
        // to check if the exit process is the child.
        while (wait(nullptr) != id) wait(nullptr);
    } else {
        execl(cmd, cmd, args, nullptr);
    }
}

int main(void) {
    system1("/bin/ls", "-al");
    std::array<int, 4> ids;

    int pid;

    for (int i = 0; i < ids.size(); ++i) {
        if (auto id = fork() == 0) {
            ids[i] = id;
            pid = id;
            break;
        }
    }

    // execl family are varadic
    // execv family pass char array
    // p means add paraemter, e means add environment variables
    if (std::find(std::begin(ids), std::end(ids), pid) != std::end(ids)) {
        if (pid == ids[0]) {
            std::cout << "child #0\n" << std::endl;
            execl("/bin/echo", "echo", "[execl]", nullptr);
        }
        if (pid == ids[1]) {
            std::cout << "child #1\n" << std::endl;
            execlp("echo", "echo", "[execlp]", nullptr);
        }
        if (pid == ids[2]) {
            std::cout << "child #2\n" << std::endl;
            char *cmd[] = { "/bin/echo", "echo", "[execve]", nullptr };

            setenv("CREW1", "Spike Spiegel", 1);
            setenv("CREW2", "Jet Black", 1);
            setenv("CREW3", "Faye Valetine", 1);
            setenv("CREW4", "Ed", 1);
            setenv("CREW5", "Ein", 1);
            execve(cmd[0], &cmd[1], environ);
        }
        if (pid == ids[3]) {
            std::cout << "child #3\n" << std::endl;
            char *cmd[] = { "/bin/echo", "echo", "[evecve]", nullptr };
            char *envp[] = { "ENV1=1", "ENV2=2", nullptr };
            execve(cmd[0], &cmd[1], envp);
        }
    }

    if (auto it = std::find(std::begin(ids), std::end(ids), pid);
        it == std::end(ids)) {
        std::cout << "end!" << std::endl;
    }

    return 0;
}
