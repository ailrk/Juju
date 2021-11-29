; assembly macro
; write_string(msg, len)
%macro write_string 2
    mov eax, 4
    mov ebx, 1
    mov ecx, %1
    mov edx, %2
    int 80h
%endmacro

section .text
    global _start

_start:
    write_string name, 8

    ; assemly type specifier
    ; byte          1
    ; word          2
    ; dword         4
    ; qword         8
    ; tbyte         10
    mov [name], dword 'Nuha'    ; change the name to Nuha Ali

    ; writing name Nuha Ali
    write_string name, 8

    ; separator
    write_string newline, 1

    mov eax, 1
    int 0x80

section .data
name db 'Zara Ali'
word_table dw 132, 333, 213, 434, 844
newline db `\n`
