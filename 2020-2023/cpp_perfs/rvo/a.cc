#include <iostream>

template <typename T, size_t N> T * make_array(void *data =alloca(N * sizeof(T))) { return (T*)data; }
int main(void) { int* p = make_array<int, 10>(); return 0; }
