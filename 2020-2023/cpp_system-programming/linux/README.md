## Linux system programming

Most parts of linux programming are around how to utilize system calls provided by the linux kernel. It's like the unix provides all the IO facilities, and C is just another layer built on top of kernel for easy access.

C stand library is small with a purpose. The standard doesn't say anything about thread, read file and write file, etc. All we have with pure C is some facilities in stdio.h. We have `stdin`, `stdout`, `stderr`, and a File type `FILE`.

Concepts like `file descriptor`, and `inode` are all provided by the unix api. You can't access them in windows, for example. Instead you use similar concept called `Hanle` for representing opened files.

On Linux, all resources are files. So the socket is file, pipe is file, devices is file, etc. We have a unified interface to talk with all of them. Whenever we want to interact with some resource, no matter it's read or write, the first thing to do is to acquire the file descriptor of the file. Each process will have it's own file descriptor table managed by the kernel, and it's able to find the actual file (or other things) it refers to.

Lots of syscalls are based on file descriptors. Once you have a descriptor, you can choose to read from it, write to it, or let input of something reroute to the output of something else.

## IO

IO is just reading and writing, no matter how complicated the facility looks like, essentially it's asking for enough information to perform some read and write tasks. From the simple `read`, `write` to `io_uring_pre_read`, the scheme changed, but the purpose is still the same. You might also want to `open` a fd or `close` a fd, but that's also serving the read write purpose.

Like the standard c file api, `lseek` allows you to move the cursor to certain position in a fd.

## File sytem

Each file descriptor has some status -- some meta information describe the file itself. On linux we can use `fstat` to retrieve the information. It's good design in the sense for most of the time you only need an integer to represent the file, which is very light weight. Only for times you need to know the meta info you call `fstat`. It can be better abstracted, but I think for c it's a very good solution.


## Macros
Some apis are provided as macros. Fore example if you are using `select`, there are FD_ISET etc.
