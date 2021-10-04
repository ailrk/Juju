#ifndef __STRING_H
#define __STRING_H

#include <stddef.h>
#include <sys/cdefs.h>

#ifdef __cplusplus
extern "C" {
#endif

int memcmp(void const *, void const *, size_t);
void *memcpy(void *__restrict, void const *__restrict, size_t);
void *memmove(void *, const void *, size_t);
void *memset(void *, int, size_t);
size_t strlen(char const *);

#ifdef __cplusplus
}
#endif
#endif
