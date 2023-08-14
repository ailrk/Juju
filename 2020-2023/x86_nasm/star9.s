section .text
    global _start

_start:
    mov edx, len
    mov ecx, msg
    ; handle linux system call
    ; ----------------------------------------------------------------------
    ; |  %eax |   Name      |      %ebx       |     %ecx      |    %esx    |
    ; ----------------------------------------------------------------------
    ; |   1   |  sys_exit   |      int        |               |            |
    ; |   2   |  sys_fork   |  struct pt_regs |               |            |
    ; |   3   |  sys_read   |   unsigned int  |    char *     |   size_t   |
    ; |   4   |  sys_write  |   unsigned int  |  const char * |   size_t   |
    ; |   5   |  sys_open   |   const char *  |    int        |     int    |
    ; |   6   |  sys_close  |   unsigned int  |               |            |
    ; ----------------------------------------------------------------------

    mov ebx, 1      ; file descriptor
    mov eax, 4      ; system call number goes to eax
    int 0x80        ; call kernal

    mov edx, 9
    mov ecx, s2
    mov ebx, 1
    int 0x80

    mov eax, 1
    int 0x80

section .data
    msg db 'Display 9 stars', 0xa
    len equ $ - msg
    s2 times 9 db '*'
