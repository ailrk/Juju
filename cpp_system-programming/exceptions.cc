#include <exception>
#include <iostream>

// - exceptions are expensive. You should not use it for control flow.
// - exceptions add extra code to the binary. use -fno-exceptions. to reduce
//   code bloat.
//
// - stack unwinding: if the exception handler is exectued, we need to restore
//   volatile registers so after the handler is finished it appears as if
//   nothing happened.


// main:
//  push %rbp
//  mov %rsp, %rbp
//  push %rbp
//  sub $0x8, %rsp
//  mov $0x1, %edi              // argument
//  call b9a <test>             // call test
//  ...
//  call a30 <std::cout>
//  ..
//  mov $0x0, %eax              // return
//  jmp c90                     // jump to exception handler
//  ...
//  call 9f0 <__cxa_begin_catch@plt>    // start of the handler
//  ...
//  call a70 <_Unwind_Resume@plt>       // unwind stack.
//  add $0x8, %rsp
//  pop %rbx
//  pop %rbp
//  ret
//
// test:
//  push %rbp
//  mov %rsp, %rbp
//  sub $0x10, %rsp
//  mov %edi, -0x4(%rbp)
//  cmpl $0x2a, -0x4(%rbp)                      // if i != 42, ret
//  jne a9f
//  mov $0x4, %edi                              //
//  call 8eo <__cxa_allocate_exception@plt>
//  ...
//  call 930 <__cxa_throw@plt>                  // __cax_throw doesn't return.
//                                              // it just jump to the
//                                              // exception handler.
// a9f:
//  nop                                         // normal return
//  leaveq
//  retq


// In test, __cxa_throw is never returned, but we need to restore non volatile
// registers to their previous state. DWARF is a specification that defines
// the procedure of how to handle this.
//
// We embed a table called .eh_frame into the program.
// (exception handling framework). It's a table format that's supported for
// all ELF files, so it's not C++ specific.
//
// We can use `readelf -SW prog` to read elf sections of an elf binary file.
//
// .eh_frame is a table of  FDE (frame descrption entry).
//
//
//
// How do we restore volatile registers? We use DWARF instructions to do a
// so called stack reversal.
//

void test(int i) {
    if (i == 42)
        throw 42;
}

int main(void) {
    try {
        test(1);
        std::cout << "try 1" << std::endl;
        test(21);
        std::cout << "try 2" << std::endl;
        test(42);
        std::cout << "try 2" << std::endl;
    } catch (...) {
        std::cout << "catched" << std::endl;
    }

    return 0;
}
