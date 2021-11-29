; malloc (bsize)
%macro malloc 1
    mov eax, 45         ; sys_brk
    xor ebx, ebx
    int 0x80

    add eax %1          ; number of bytes to be reserved
    mov ebx, eax
    mov ebx, eax
    mov eax, 45         ; sys_brk
    int 80h

    cmp eax, 0
    jl exit             ; handle error

    mov edi, eax        ; edi: highest available address.
    mov ecx, %1         ; calcuate dword allocated
    shr ecx, 4
    sub edi, ecx        ; point to the last dword
    xor eax, eax
    std                 ; backward
    rep stosd           ; repeate for entire allocated area
    cld                 ; put DF flag to normal state
%endmacro

section .text
    global _start

_start:
    malloc 16384

exit:
    mov eax, 1
    xor ebx, ebx
    int 80h
