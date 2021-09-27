#include <array>
#include <iostream>

// usce p-machine
// ancient stack machine with only one stack.

// three pointers for stack:
// 1. SP (stack pointer)
// 2. MP (frame pointer)
// 3. EP (top of the stack (avoid overflow.))

// heap grows down, the top of the heap is marked as NP (new pointer)
// EP > MP means
// (Note stack growing up which is opposite from what we usually have).

const int amax = 2047; // max address
const int levmax = 3;  // block nesting level
const int cxmax = 200; // code array size

enum FCT { LIT, OPR, LOD, STO, CAL, INT, JMP, JPC };

struct Inst {
  FCT f;
  std::array<int, amax> a;
  std::array<int, levmax> l;
};

void interp() {
  const int stacksize = 500;
  int prog_reg, base_reg, topstack_reg;
  Inst i;
  std::array<int, stacksize> store;
}
