/* set entrance at _strat */
ENTRY(_start)

/* locate sections */
SECTIONS
{
    /* put section at !M. */
    . = 1M;

    /* multiboot header is at the beginning */
    .text BLOCK(4k) : ALIGN(4K) {
        *(.multiboot)
        *(.text)
    }

    /* put read only data at the top */
    .rodata BLOCK(4k) : ALIGN(4K){
        *(.rodata)
    }

    /* initialized read write data */
    .data BLOCK(4K) : ALIGN(4K) {
        *(.data)
    }

    /* uninitialized data and stack */
    .bss BLOCK(4K) : ALIGN(4K) {
        *(COMMON)
        *(.bss)
    }
}
