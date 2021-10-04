/* constant for multiboot header */
.set ALIGN,   1<<0                # align loaded modules on page boundaries
.set MEMINFO, 1<<1                # provide meory map
.set FLAGS,   ALIGN | MEMINFO
.set MAGIC,   0x1BADB002          # magic number for multiboot
.set CHECKSU, -(MAGIC + FLAGS)    # check sum

.section .multiboot   # multiboot header.
.align 4
.long MAGIC
.long FLAGS
.long CHECKSU


.section .bss
.align 16

stack_bottom:   # allocate a symbol at bottom, stack grows downwards.
.skip 16384     # 16 Kb

stack_top:


/* Entering x86 32 bit protected mode */
.section .text
.global _start
.type _start, @function
_start:                       # program entrance.


/* set esp to stack top. The stack is later used for C code */
mov $stack_top, %esp

/* initialize CPU state, */

/* ABI requires stack to be 16 bytes aligned. */
call kernel_main


/* return from kernel, keep the computer in an infinite loop */
cli       # disable interrupt.
1:
hlt       # wait for next interrupt. this will lock the system.
jmp 1b    # jump back to hlt. To avoid non maskable interrupt.

/* size of _start <- current location . - it's start */
.size _start, . - _start
