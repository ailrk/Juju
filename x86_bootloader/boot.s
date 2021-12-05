    bits 16                     ; x86 real mode is 16 bit
    org 0x7c00                  ; this bootloader start from 0x7c00.

boot:
    call bios_greet

    mov ax, 0x2401
    int 0x15                    ; enable A20 bit
    mov ax, 0x3
    int 0x10                    ; set VGA text mode to 0x3
    cli                         ; clean interrupt.
                                ; enable 32 bit instructions
    lgdt [gdt_pointer]          ; load gdt table
    mov eax, cr0                ;
    or eax, 0x1                 ; set protected mode bit on register cr0
    mov cr0, eax
    jmp CODE_SEG:boot2          ; long jump to code segment

                                ; a descriptor is just a struct holds some
                                ; information.
                                ; gdt table  should at least have
                                ; null descriptor, code seg and data seg.
gdt_start:                      ; setup gdt table.
    dq 0x0                      ; null descriptor
gdt_code:                       ; code segment
    dw 0xffff
    dw 0x0
    db 0x0
    db 0x9A
    db 0xCF
    db 0x0
gdt_data:                       ; data segment
    dw 0xffff
    dw 0x0
    db 0x0
    db 0x92
    db 0xCF
    db 0x0
gdt_end:
gdt_pointer:
    dw gdt_end - gdt_start
    dd gdt_start
CODE_SEG equ gdt_code - gdt_start
DATA_SEG equ gdt_data - gdt_start

bios_greet:
    mov si, bios_msg                ; point si to bios_msg
    mov al, 0x0e                    ; set al to 0x0e to display character
bios_greet_loop:
    lodsb                           ; al = (*si)++
    int 0x10                        ; print character
    cmp al, 0                       ; while *al != '\0'
    jne bios_greet_loop
    ret
bios_msg:
    db "[- msg from bios 0x10 -]", 0

    ; enter 32 bit mode.
    ; in protected mode we can't use bios anymore
    bits 32
boot2:
    mov ax, DATA_SEG                ; set seg reg points to data segment.
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    push msg
    call protected_mode_greet

    jmp halt

protected_mode_greet:               ; can't use bios anymore. use VGA here.
    push ebp
    mov ebp, esp
    mov esi, [esp + 4]
    mov ebx, 0xb8000
protected_mode_greet_loop:
    lodsb
    cmp al, 0
    je protected_mode_greet_end
    or eax, 0x0100
    mov word [ebx], ax
    add ebx, 2
    jmp protected_mode_greet_loop
protected_mode_greet_end:
    pop ebp
    ret

halt:
    cli
    hlt

msg:
    db "[- msg from protected mode -]", 0


times 510-($-$$) db 0           ; pad til 510 bytes
dw 0xAA55                       ; magic word 0x55AA in little endian for x86.
