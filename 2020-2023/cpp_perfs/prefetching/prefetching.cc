#include <benchmark/benchmark.h>
#include <cstdlib>
#include <iostream>

// given Matrix
// [ ... ... ... ...
//   ... ... ... ...
//   ... ... ... ...
// ]

// best case. elements we are accessing actually aligned.
// they will be naturally fetched into a cacheline.
static void ROWMAJOR(benchmark::State &s) {
  int N = 1 << 12;
  int *array = new int[N * N];

  while (s.KeepRunning()) {
    for (int i = 0; i < N; ++i) {
      for (int j = 0; j < N; ++j) {
        array[i * N + j] += j;
      }
    }
  }
}
BENCHMARK(ROWMAJOR)->Unit(benchmark::kMillisecond);

// theoriotically bad for cache usage. Everytime we acecss a new
// element we need to find it somewhere N elemnts apart from the last
// accessed element.
//
// Once cpe recognize this pattern, it will use prefetching helps to load
// elements that are N distance apart all together and fit them into one
// cache line.
static void COLMAJOR(benchmark::State &s) {
  int N = 1 << 12;
  int *array = new int[N * N];
  while (s.KeepRunning()) {
    for (int i = 0; i < N; ++i) {
      for (int j = 0; j < N; ++j) {
        array[j * N + i] += i;
      }
    }
  }
}
BENCHMARK(COLMAJOR)->Unit(benchmark::kMillisecond);

// if you check perf stat, cache misses for this version is 90%.
// almost no cache is used.
static void RANDOM_STRIKE(benchmark::State &s) {
  int N = 1 << 12;
  int *array = new int[N * N];
  while (s.KeepRunning()) {
    for (int i = 0; i < N; ++i) {
      for (int j = 0; j < N; ++j) {
        array[j * N + rand() % N] += i;
      }
    }
  }
}
BENCHMARK(RANDOM_STRIKE)->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
