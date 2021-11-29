#include <string>
#include <sys/wait.h>
#include <unistd.h>

#include <array>
#include <iostream>
#include <string_view>

// use dup2 to redirect output of child process to pipe for parent to read.

class Pipe {

    std::array<int, 2> m_handlers;

  public:
    Pipe() {
        if (pipe(m_handlers.data()) < 0) {
            exit(1);
        }
    }

    ~Pipe() {
        for (auto n : m_handlers) close(n);
    }

    std::string read() {
        std::array<char, 256> buf;
        size_t nbytes = ::read(m_handlers[0], buf.data(), buf.size());
        if (nbytes > 0) {
            return { buf.data(), nbytes };
        }
        return {};
    }

    void redirect() {
        dup2(m_handlers[1], STDOUT_FILENO);
        close(m_handlers[0]);
        close(m_handlers[1]);
    }
};

int main(void) { return 0; }
