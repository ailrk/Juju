#include <iostream>
#include <sys/shm.h>
#include <sys/wait.h>
#include <unistd.h>

#include <array>
#include <string_view>

//
// shared memory
//
// shmget is the old system V shared memory model. POSIX can generally use
// mmap.
//

char *get_shared_mem(int &shmref) {
    // system V ipc has tok to idenitfy each ipc.
    //
    // Note shared memory can be dettached and attached, the token is
    // important to identify the shared memory when no process holds a
    // handle to it.
    auto key = ftok("myfile", 42);

    // shmget get 0x1000 bytes shared mem with read and write permissions.
    auto shm = shmget(key, 0x1000, 0666 | IPC_CREAT);
    shmref = shm;

    // shm is the handle that can be used by shmat, which return a piece of
    // memory.
    return static_cast<char *>(shmat(shm, nullptr, 0));
}

//
// NOTE: Once a piece of shared memory is allocated, it will always be there.
// you can
//
//

int detatch_shared_mem(void *addr) { return shmdt(addr); }
void delete_shared_mem(int shm_id) { shmctl(shm_id, IPC_RMID, NULL); }

int main() {
    int shm;

    if (fork() != 0) {
        std::cout << "parent\n";
        sleep(1);
        auto msg = get_shared_mem(shm);
        msg[0] = 99;
        wait(nullptr);
    } else {
        auto msg = get_shared_mem(shm);
        while (msg[0] != 99)
            ;
        std::cout << "child\n";
    }

    // if we don't delete this memory, next time the same token will request
    // the same shared memory, and child will exit without waiting since
    // msg[0] is already set by the previous run.
    delete_shared_mem(shm);
}
