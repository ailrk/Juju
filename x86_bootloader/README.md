# Programming x86 assembly without an OS.


## Addressing space
The length of ip register determines how many memory a CPU can potentially address. In the case of 16 bit, we can potentially address up to 2^16 addresses. These addressing space is completey abstracted away from CPU's internal mechanism, and what is certain address addressing completely depends on what device is plugged in that range.

All cpu will have n pins expose to the out side world that let it to address. This pins are very flexible, we can connect them with ther device in any way we want.

Here are two examples. In CPU 1, we connected two devices to a 8 bit cpu. dev 1 can be addressed from 0x0 to 0x7, and dev 2 can be addressed from  0x64 to 0x255.
```
          CPU 1
0   . . . . . . . .
    | | |     | | |
    . . .     . . .
     dev1      dev2
```

In CPU 2, we hooked up the CPU with a RAM, but only use part of it's storage. we left the last pin open, so we can address 2^7 bytes from 0x0 to 0x127.

```
        CPU 2
0   . . . . . . . .
    | | | | | | |
  . . . . . . . .
        RAM
```

This is the case for all cpu base on IP and has exposed pins for addressing. In most x86 chips, we usually have BIOS stored in a ROM, addressable from a fixed address.


## BIOS

## boot sequence

## real mode
X86 has different modes, it behaves differently under different modes. Once the computer is on, it's in real mode. Real mode is a simple 16 bit mode with segmented memory model. It's the first x86 mode, and for backward capability it's still used today.

In real mode, we only have access to 16 bit registers. To address 32 bit memory, we need segment the memory in to segmentations. Reigster like ds, ss, cs, es stores the starting address of each segmentation, and we address the content by adding offset on top of the segment.

## protected mode

## GDT (Global descriptor table)

## IDT (Interupt descriptor table)

## Boot loader

## How does interrupts work

## MMU

## What about multi processing?

## Crafting a C stack.

## References

- https://wiki.osdev.org/Global_Descriptor_Table
- https://wiki.osdev.org/BIOS
