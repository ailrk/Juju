#include <stdio.h>
#include<limits.h>

#if defined(__is_libk)
#include <kernel/tty.h>
#endif

int puts(char const *str) { return printf("%s\n", str); }

int putchar(int ic) {
#if defined(__is_libk)
    char c = (char)ic;
    terminal_write(&c, sizeof(c));
#else
    // TOOD implement write system call.
#endif
    return ic;
}
