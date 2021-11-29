#include <iostream>

// - register layout
// - stack frame
// - functon prolog and epilogs
// - calling conventions
// - exception handling
// - debugging
// - binary object format (elf)
// - loading and linking.

//_Z5test1v:
// .LFB1729:
// 	.cfi_startproc
// 	pushq	%rbp
// 	.cfi_def_cfa_offset 16
// 	.cfi_offset 6, -16
// 	movq	%rsp, %rbp
// 	.cfi_def_cfa_register 6
// 	movl	$1, -8(%rbp)
// 	movl	$2, -4(%rbp)
// 	movl	-8(%rbp), %edx
// 	movl	-4(%rbp), %eax
// 	addl	%edx, %eax
// 	popq	%rbp
// 	.cfi_def_cfa 7, 8
// 	ret
// 	.cfi_endproc

int test1() {
    int i = 1;
    int j = 2;
    return i + j;
}
// Note the rsp is not updated when we're setting up local variables.
// We can do that because a leaf function will not further grow the stack,
// so essentially the entire rest of the stack are part of the stack frame of
// a leaf function. Thus there is not need to update sp, as it's used to
// mark the end of the stack.,
//
//
// NOTE: Technically leaf functions don't have a proper stack layout bc they
// don't need to setup sp. If an interrupt fires, it's possible that the
// stack for a leaf function can't be properly stored, thus cause corruption.
//
// Rest of the stack for leaf function is called a RED Zone.
// It's possible to turn off red zone with gcc flag -mno-red-zone.

//
// Calling conventions
//
// calling conventions indicates what registers are volatile. (what can be
// clobbered during a function call), and who do we handle them.
//
// If a register is volatile, it can be changed during the call, and after
// it's returned the updated value in that register can be seen by the caller.
// In contrast, the value in non volatile registers are saved during calls,
// so the caller can expect them to hold the same value before and after
// a function call.
//
// So calling convention is a convetion that tells us which registers' value
// are preseved and which are clobbered.
//
// Designing a calling convention entails specifying how to save value for
// a non volatile register. There are two main approches: Caller saved
// and callee saved.
//
// For caller saved, the caller will push all of non volatiles into the stack
// before calling anything. For callee saved case, the callee push registers
// before changing them.
//
// Callee saved calling convention has the benfit that it knows exaclty which
// register it gonna use, thus the compiler has the information to selective
// choose which register to save. On the other hand, in a caller saved scheme,
// caller has no extra information, thus leave it with no choice but to save
// everything at once. The overhead is inevitable.
//

// volatile registers

int test2(int val1, int val2) { return val1 + val2; }

int main(void) {
    test1();
    auto ret = test2(1, 2);
    return 0;
}

