; we can manually define call.
; the bare minimal: we need to be able to save the register, and restore it
; latter.

putstring32:
    mov eax, 4
    mov ebx, 1
    mov ecx, buffer
    mov edx, 32
    int 0x80
    ret

push_eip:
    mov eax, [esp]
    push eax
    ret

%macro call1 1
call1:
    xor ebx, ebx                ; clear ebx
    call push_eip               ; save eip
    test ebx, ebx               ; is_called flag
    jnz exit_call1_%1           ; if has called, exit.
    jmp %1
exit_call1_%1:
%endmacro

%macro ret1 0
    pop eax                     ; restore eip
    pop ebx                     ; set is_called = 1
    add ebx, 1
    push ebx
    jmp eax                     ; jump to next eip
%endmacro

global _start

; call will push eip, ret will restore and jump back
section .data
    addr db "yellow"
    nzmsg db "nz"
    buffer times 32 db 0              ; allocate buffer

;;;;;;;;;;;;;;;;;;;;;;;
section .text
_start:
    mov eax, 10
loop:
    ; save eax
    push eax

    call fn
    call1 fn1

    pop eax
    sub eax, 1

    cmp eax, 0
    jnz loop

    mov eax, 1
    int 0x80
    jmp exit

;;;;;;;;;;;;;;;;;;;;;;;

fn:
    mov eax, 4
    mov ebx, 1
    mov ecx, nzmsg
    mov edx, 2
    int 0x80
    ret

fn1:
    mov eax, 4
    mov ebx, 1
    mov ecx,addr
    mov edx, 6
    int 0x80
    ret1

exit:
