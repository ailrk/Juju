global _start

section .data
    name1 db "string"       ; string type
    name2 db 0xff           ; hex
    name3 db 100            ; dec
    name4 dw 0x1234         ; double word
    name5 dw 1000           ; double word dec
    name6 dd 0x12341234     ; 4 bytes
    name7 dd 100000         ; dec

section .text
_start:
    mov eax, 4
    mov ebx, 1
    mov ecx, name1          ; needs to be an address.
    mov edx, 3
    int 0x80

    mov eax, 1
    mov ebx, 0
    int 0x80
