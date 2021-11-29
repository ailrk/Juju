// sometimes we don't want the compiler to align struct fields.
// e.g save space.

#include <cassert>
#include <inttypes.h>
#include <iostream>

// change the default packing to byte packing.
#pragma pack(push, 1)
struct alignas(4) A {
    uint16_t f1; // 2
    uint64_t f2; // 8
};
#pragma pack(pop)

#pragma pack(push, 1)
struct alignas(16) B {
    uint16_t f1; // 2
    uint64_t f2; // 8
};
#pragma pack(pop)

struct C {
    uint16_t f1; // 2
    uint64_t f2; // 8
};

//
// misalignment, size, and performance.
//

// it has to be placed at 4 byte boundary.
struct S {
    uint32_t a;
    uint32_t b;
};

// pack with 1 byte boundary
// Note this not only means a and b are packed, the entire struct now can be
// put at any byte boundary.
#pragma pack(push, 1)
struct P {
    uint32_t a;
    uint32_t b;
};
#pragma pack(pop)

static_assert(sizeof(S) == sizeof(P));

struct S1 {
    char c;
    S s;
    char d;
};

struct P1 {
    char c;
    P p;
    char d;
};

// S1 is properly aligned with 4 byte boundary.
static_assert(sizeof(S1) > sizeof(P1));


int main(void) {
    // assert(sizeof(A) == 10);
    std::cout << sizeof(A) << std::endl;
    std::cout << sizeof(B) << std::endl;
    std::cout << sizeof(C) << std::endl;

    std::cout << sizeof(S1) << std::endl;
    std::cout << sizeof(P1) << std::endl;
    return 0;
}

// - compiler align struct automatically
// - #pragma pack(push, 1), #pragma pack(pop) can pack struct
// - alignas(n) will align struct at n address boundary
// - pack is a hint, it will be override by alinas.
// - pack misalign the struct, so member accessing wlll result in extra code.
// - packing or not packing needs to be decided ahead of time because once you
//   are commited you cannot go back. Otherwise it will be a breaking change.


