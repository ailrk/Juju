#ifndef __STDIO_H
#define __STDIO_H

#include <sys/cdefs.h>

#ifdef __cplusplus
extern "C" {
#endif

int printf(char const *__restrict, ...);
int putchar(int);
int puts(char const *);

#ifdef __cplusplus
}
#endif
#endif
