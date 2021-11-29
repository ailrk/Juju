#include <cassert>
#include <iostream>

auto clear1(auto x) { return x ^ x; }
auto flip1(auto x) { return x ^ (~0); }
auto id1(auto x) { return x ^ 0; }

auto clear2(auto x) { return x & 0; }
auto id2(auto x) { return x & (~0); }
auto id3(auto x) { return x & x; }

auto id4(auto x) { return x | 0; }
auto set1(auto x) { return x | (~0); }
auto id5(auto x) { return x | x; }
