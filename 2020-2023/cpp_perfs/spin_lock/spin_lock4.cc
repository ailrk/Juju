// passive backoff

#include <benchmark/benchmark.h>

#include "spin_lock.h"
#include <atomic>
#include <emmintrin.h>
void spin_lock::lock() {
  while (1) {
    if (!locked.exchange(true)) // @ only write once
      return;

    do {
      for (int i = 0; i < 4; ++i) {
        _mm_pause(); // intrinsic for spin wait loop.
      }
    } while (locked.load());
  }
}

static void passive_backoff(benchmark::State &s) { spin_lock_bench<spin_lock>(s); }

BENCHMARK(passive_backoff)
    ->RangeMultiplier(2)
    ->Range(1, std::thread::hardware_concurrency())
    ->UseRealTime()
    ->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
