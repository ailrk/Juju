section .text
    global _start   ; must be declared for linker

_start:             ; tells linker entry point
    mov edx, len    ; message len
    mov ecx, msg    ; message to write
    mov ebx, 1      ; file descriptor
    mov eax, 4      ; system call number (sys_write)
    int 0x80        ; call kernal

    mov eax, 1      ; system call number (sys_exit)
    int 0x80        ; call kernal

section .data
    msg db 'Hello World', 0xa   ; static string
    len equ $ - msg             ; len of the message
