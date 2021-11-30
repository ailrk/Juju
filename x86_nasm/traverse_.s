global _start
global main

section .data
    string1 db "ABCDEF"
    ref1 db 0

section .text
main:
    push string1
    push 7
    push putchar
    call traverse_
    jmp exit

; &arr -> size(arr) -> op -> a
traverse_:
    push ebp
    mov ebp, esp
    mov eax, [ebp+8]            ; op
    mov ebx, [ebp+12]           ; size
    mov ecx, [ebp+16]           ; arr

    test ebx, ebx
    jz traverse_end

    mov esi, 0
    loop1:
    mov edx, [ecx + esi]        ; get ith element

    push edx
    call eax
    mov [ecx + esi], eax        ; update element

    add esi, 1
    cmp esi, ebx
    jge traverse_end
    jmp loop1


    traverse_end:
    push 'x'
    call putchar

    mov esp, ebp
    pop ebp
    ret

; a proper functional call pushes parameters into stack.
; params | ret | ebp | ...
; a -> a
putchar:
    push ebp
    mov ebp, esp
    mov edi, [ebp+8]
    mov [ref1], edi

    mov eax, 4
    mov ebx, 1
    mov ecx, ref1
    mov edx, 1
    int 0x80

    mov eax, [ebp+8]            ; return the same value

    mov esp, ebp
    pop ebp
    ret

exit:
    mov eax, 1
    int 0x80
