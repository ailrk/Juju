    bits 16                     ; x86 real mode is 16 bit
    org 0x7c00                  ; this bootloader start from 0x7c00.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; boot : () -> !
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
    or eax, 0x1                 ; set protected mode bit on eax
    mov cr0, eax                ; mv it to cr0 register.
                                ; this enables protected mode

    mov ax, DATA_SEG            ; set seg reg points to data segment.
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    jmp CODE_SEG:boot2          ; long jump to code segment

                                ; a descritor is another name for struct..

    ; gdt table define the characteristics of various memory areas used
    ; during execution. You can decide how big a mem area is, what's the
    ; access privilege, where the base address starts etc.

    ; we define the table ahead of time, then call lgdt [table] to have
    ; cpu read the config.

    ; 16 bit x86 didn't have builtin paging, and addressing are based on
    ; segmentation. there is a LDT which is a local version of GDT being
    ; used to separate address space between multiple processes. e.g The
    ; os switch the current LDT when scheduling a new process.

    ; Today's os doesn't need LDT anymore because CPU itself already
    ; support paging direclty.

    ; In some way you can think a descriptor table as a fat pointer point
    ; to a chunk of memory, with some meta info of this memory.

                                ; we need at least have
                                ; null descriptor, code seg and data seg.
gdt_null:                       ; setup gdt table.
    dq 0x0                      ; - null descriptor
gdt_code:                       ; code segment
    dw 0xffff                   ; - segment limit (enable 4G for 32 bit )
    dw 0x0                      ; - base 0 - 15 bits
    db 0x0                      ; - base 16 - 23
    db 0x9a                     ; - access byte
    db 0xcf                     ; - hi 4 bit, lo 4 bit
    db 0x0                      ; - base 24 - 32 bits
gdt_data:                       ; data segment
    dw 0xffff                   ; the same
    dw 0x0
    db 0x0
    db 0x92
    db 0xcf
    db 0x0
gdt_end:
gdt_pointer:
    dw gdt_end - gdt_null
    dd gdt_null
CODE_SEG equ gdt_code - gdt_null
DATA_SEG equ gdt_data - gdt_null

    ;
    ; entering 32 bit mode.
    ;
    ; in protected mode we can't use bios anymore
    bits 32

    ; boot2 : () -> ()
boot2:
    push msg
    call protected_mode_greet

    jmp halt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; protected_mode_greet : () -> ()
protected_mode_greet:               ; can't use bios anymore. use VGA here.
    push ebp
    mov ebp, esp
    mov esi, [esp + 8]              ; address of the message
    mov ebx, 0xb8000                ; VGA text buffer starts at 0xb800


                                    ; VGA character are represented as
                                    ; |0               |8            16
                                    ; |bg clr |fg clr  |ascii char
protected_mode_greet_loop:
    lodsb                           ; read next byte
    cmp al, 0                       ; if it's null
    je protected_mode_greet_end     ; protect
    or eax, 0x0100                  ; set bit 0x0100
    mov word [ebx], ax              ;
    add ebx, 2
    jmp protected_mode_greet_loop
protected_mode_greet_end:
    pop ebp
    ret

    ; bios_greet : () -> ()
bios_greet:
    mov si, bios_msg                ; point si to bios_msg
    mov al, 0x0e                    ; set al to 0x0e to display character
bios_greet_loop:
    lodsb                           ; al = (*si)++
    int 0x10                        ; print character
    cmp al, 0                       ; while *al != '\0'
    jne bios_greet_loop
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; define some data
bios_msg:
    db "[- msg from bios 0x10 -]", 0

msg:
    db "[- msg from protected mode -]", 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end
halt:
    cli
    hlt

times 510-($-$$) db 0           ; pad til 510 bytes
dw 0xAA55                       ; magic word 0x55AA, little endian for x86.
