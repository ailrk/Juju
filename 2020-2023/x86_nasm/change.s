global _start


section .data
    addr db "yellow"

section .text

_start:
    mov [addr], byte 'H'
    mov [addr + 5], byte '!'
    mov eax, 4                    ; system call 4
    mov ebx, 1                    ; fd to stdout
    mov ecx, addr                 ; bytes to write
    mov edx, 6                    ; how many bytes
    int 0x80

    mov eax, 1                    ; exit with no error
    mov ebx, 1
    int 0x80
