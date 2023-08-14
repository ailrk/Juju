#include <benchmark/benchmark.h>
#include <iostream>

// cache design
// - Direct Map Cache
//   concecutive bins. An address will map to a particular bin.
//   | | | | | - len = 4
//    0 1 2 3  - idicies
// which bin a memory go? idx <- addr `mod` len
//
// - Fully associated Cache
//   | ...  ... | - we can shove cache block in any one of the bin.
//
// - Set associative cache.
//   Combine both DM and FA.
//          way0  way1
//   set 0 |    |    |
//   set 1 |    |    |
//
//   1. mod to get set idx
//   2. find empty slot and save the cache block there.

// modern cpu use set associateive cache, though it mitidate the
// problem of direct map cache and fully associative cache, certain memory
// access pattern can still cache problem.

static void ASSOC(benchmark::State &s) {
    // 2^n
    int step = 1 << s.range(0);

    // 32 MB
    int N = 1 << 25;
    char *a = new char[N];

    for (int i = 0; i < N; i++) {
        a[i] = rand() % 100;
    }

    // try differetn step sizes
    while (s.KeepRunning()) {
        int i = 0;
        for (int iter = 0; iter < 10000; iter++) {
            a[i]++;
            // wrap around the size of the array.
            i = (i + step) % N;
        }
    }

    delete[] a;
}

BENCHMARK(ASSOC)->DenseRange(0, 64)->Unit(benchmark::kMicrosecond);

BENCHMARK_MAIN();
