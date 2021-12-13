# Boot sequence for x86

Story about cpu before an OS is loaded.


## CPU IO model
CPU itself doesn't do anything, it needs to connect with the outside world to have input and output.

Traditional CPU like 6502 connect to the outside world with `memory mapped IO`. Meaning memory and devices share the same address space. In another word, memory is just another external device for the CPU. Because the mapping mechanism simply map some address to some devices, the CPU has no idea what it's connected on boot. This is a very flexible method, because we can address devices as if we're addressing the memory. One problem with this approach is that we have no idea what's at the other side of the pin, so things need to be hard coded for a specific setup. Also we have to allocate a chunk of addressing space for devices, so the amount of address for memory will be smaller (Tho with 64 bit addressing space it's not a problem anymore).

```
    How does address space looks like for 36 bit x86

       +----------------+ 0xffffffff
[1] +--+ BIOS Flash     |
    |  +----------------+
    |  |  APIC          | for some inptterupt handler
    |  +----------------+
    +--+  PCI Mem RANGE |
       +----------------+
       |internal graphic|
       +----------------+
       |  TSEG          |
       +----------------+
       |  DPR           |
       +----------------+
       |  main memory   |
       |                |
       +----------------+ 0x01000000  so main memory is only part of the space
       | ISA HOLE       |
       +----------------+ 0x00f00000
       | main memory    |
       +----------------+ 0x00100000
       | DOS capability |
       +----------------+ 0x00000000

: [1] are memory mapped IO.
      BIOS is addressable from flash (A rom)
      APIC is the advanced programmable interrupt controller.
      PCI mem range is programmed by BIOS

```

Also there is `port mapped IO`, where IO devices have separated address space from memory space. X86 supports port mapped IO, it has instruction `in` and `out` that allows you to read or write bytes between eax and devices on given IO port.

```
    port IO address for X86

       +----------------+
       | Port 65535     |
       +----------------+ 0xffff
       |                |
       | ...            |
       |                |
       +----------------+ 0x003
       | Port 2         |
       +----------------+ 0x002
       | Port 1         |
       +----------------+ 0x001
       | Port 0         |
       +----------------+ 0x000
```

Port IO is an extra layer of indirection, thus more complex to implement. Before explainning what's going on, we list some components getting involved in x86: `Processor`, `MCH (memory control hub)`, `ICH (io control hub)`, `DMI (direct memory interface)`.

Each processor connects to a MCH for processing the addressing signal. Access to IO space will be forwarded by the MCH to DMI, and DMI further forward it to ICH. ICH


```
     Processor -> HCH -> ICH -> (IO space)
```


In X86 with PCI support, devices are connected on PCI bus, and it's possible for the CPU to query what's connected to the Bus and what can it do with them. Instead of making assumption, CPU can discover devices.

## Mode and BIOS
X86 are backward compatible, all x86 cpus will first boot into a 16 bit real mode, then you need to mannually enable the protected mode to use the full power.


## Boot process


## Setup a C stack.
