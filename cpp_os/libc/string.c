#include <string.h>

int memcmp(void const *ap, void const *bp, size_t size) {
    unsigned char const *a = (unsigned char const *)ap;
    unsigned char const *b = (unsigned char const *)bp;

    for (size_t i = 0; i < size; ++i) {
        if (a[i] < b[i])
            return -1;
        else if (a[i] > b[i])
            return 1;
    }
    return 0;
}

// memcpy assumes tgt and src don't overlap.
void *memcpy(void *__restrict tgt, void const *__restrict src, size_t size) {
    unsigned char *_tgt = (unsigned char *)tgt;
    unsigned char const *_src = (unsigned char const *)src;
    for (size_t i = 0; i < size; ++i) _tgt[i] = _src[i];
    return tgt;
}

// Safer memcpy.
void *memmove(void *tgt, const void *src, size_t size) {
    unsigned char *_tgt = (unsigned char *)tgt;
    unsigned char const *_src = (unsigned char const *)src;

    if (tgt < src)
        for (size_t i = 0; i < size; ++i) _tgt[i] = _src[i];
    else
        for (size_t i = size; i != 0; --i) _tgt[i - 1] = _src[i - 1];
    return tgt;
}

void *memset(void *buf, int value, size_t size) {
    unsigned char *_buf = (unsigned char *)buf;
    for (size_t i = 0; i < size; ++i) _buf[i] = (unsigned char)value;
    return buf;
}

size_t strlen(char const *str) {
    size_t len = 0;
    for (char const *p = str; *p != '\0'; ++p)
        ;
    return len;
}
