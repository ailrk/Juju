
    bits 16                     ; x86 real mode is 16 bit
    org 0x7c00                  ; this bootloader start from 0x7c00.

boot:
                                ; prepare for 0x10 (video display) call.
    mov si, init_greeting       ; load message
    mov ah, 0x0e                ; set ah to display character

    ; at this point you can already do a lot of things.
    ; print *si until hit null terminator
greet_loop:
    lodsb                       ; load a byte from [ds:si] to al. message here.
    cmp al, 0                   ; if it's null char
    je to_protected             ; enter protected mode
    int 0x10                    ; otherwise call 0x10
    jmp greet_loop              ; do it again.

    mov ax, 0x2401
    int 0x15                    ; enable A20 bit


    ; entering 32 bit protected mode.
to_protected:

halt:
    hlt

init_greeting:
    db "[- boot -]", 0

times 510-($-$$) db 0           ; pad til 510 bytes
dw 0xAA55                       ; magic word 0x55AA in little endian for x86.
