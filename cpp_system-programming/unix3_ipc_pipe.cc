#include <iostream>
#include <sys/wait.h>
#include <unistd.h>

#include <array>
#include <string_view>

//
// interprocess communication
//
// Once you created a new process, you may want to share some information
// between them.
//
// ipc can also be used for synchronization between processes. e.g One process
// can send some info to another to tell it that it's ok to proceed.
//

// PS: fork only when needed. If you really need a running instance with it's
// own memory space and run completely separate from it's parent, it's the time
// to use process. Otherwise it's better to use thread as they're more light
// weigth. But still you can get lower overhead with green threads run on top
// of system threads.

//
// pipe
//
// Pipe is designed as a communication channel between twp processes.
// First thing to notice: The notion of communication describes the semantic
// of pipe, but the implementation of communication in the same machine are
// generally implemented by sharing a piecie of memory that different instances
// can read and write, or read and write under certain constraints.
//
// So pipe, really under the hood is a piece of memory, be viewed as a file.
//
// Now the problem is how does a process read or write? This is the mechanism
// that we design. For unix like, the kernel will maintain a table of file
// descriptors for each process, the first 3: 0 1 2 corresponds to stdin
// stdout, and stderr correspondly. From a process's stand point, to read
// means to get some data from files represented by fd 0, to write means write
// bytes to file represented by fd1, etc.
//
// fd is just an abstract name to add an indirection on top of the file.
// One can think it as a pointer.
//
// Recall fork() wil copy the fd, meaning our new process have the same access
// to the parent's pipe fds. Whatever we write into we can read out, even in
// two different processes. The implementation is acutally straight forward, if
// you think of it, with out pipe you might just open a common file, one process
// read from the file, another write into.
//
// To communicate, the fd 0 in one process should represents the same file that
// the fd 1 the other process represents. And that's bascially how pipes work.
//
// To use a pipe, you pass an array to the system call pipe(). After the call,
// the first element of the array will be the fd for in, and the second will be
// the fd for out. AFter fork, there will be a exact same copy in both the
// parent and child process. If now write bytes from one end, both child and
// parent can read from the other end.
//
// If we want the channel to be one way only, we can close the corresponding
// fd. e.g in parent close read and in child close write.
//
// For a full duplex channel, we need two pipes. Infomation flow from one
// direction to another always go through the same pipe. This means there will
// be no interference between read and writes.

struct Pipe {
    std::array<int, 2> m_handlers;

  public:
    Pipe() {
        if (pipe(m_handlers.data()) < 0) {
            exit(1);
        }
    }

    ~Pipe() {
        // close() close file descriptor, so can also been used for pipe.
        close(m_handlers.at(0));
        close(m_handlers.at(1));
    }

    // TRICK:
    // a common trick to use stl with c library is to use data() and size()
    // member .functions.
    // This creates a safer layer on top
    std::string read() {
        std::array<char, 256> buf;
        size_t nbytes = ::read(m_handlers[0], buf.data(), buf.size());

        if (nbytes > 0) {
            return { buf.data(), nbytes };
        }
        return {};
    }

    void readonly() { ::close(m_handlers[1]); }
    void writeonly() { ::close(m_handlers[0]); }

    void write(std::string const &msg) {
        ::write(m_handlers[1], msg.data(), msg.size());
    }

    std::pair<uint32_t, uint32_t> get_fd() const {
        return { m_handlers[0], m_handlers[1] };
    }
};

void run1() {
    Pipe p1;
    Pipe p2;

    if (fork() != 0) {
        std::cout << "[parent] parent\n";
        p1.writeonly();
        p2.readonly();
        sleep(1);
        p1.write("[parent -> child]");
        auto msg = p2.read(); // read blocks so it will wait until it finished.
        std::cout << "[parent] msg: " << msg << "\n";
        wait(nullptr);
    } else {
        std::cout << "[child] child\n";
        p1.readonly();
        p2.writeonly();
        p2.write("[child -> parent]");
        auto msg = p1.read();
        std::cout << "[child] msg: " << msg << "\n";
    }
}

int main(void) {

    Pipe const p{};
    std::cout << p.get_fd().first << " " << p.get_fd().second << std::endl;

    run1();

    return 0;
}
