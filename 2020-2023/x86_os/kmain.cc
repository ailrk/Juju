// sample C++ function.
// We need to establish a stack from the bool sector to run this function.
extern "C" int kmain(void) {
    short const color = 0x0f00;
    char const *msg = "Hello from C++";
    short *vga = (short *)0xb8000;
    for (int i = 0; i < 16; ++i) vga[i + 80] = color | msg[i];
    return 0;
}
