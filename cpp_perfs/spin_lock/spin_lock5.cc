// exponential backoff

#include <benchmark/benchmark.h>

#include "spin_lock.h"
#include <atomic>
#include <emmintrin.h>

#define MIN_BACKOFF 4
#define MAX_BACKOFF 1024
void spin_lock::lock() {
  int back_off_iters = MIN_BACKOFF;
  while (1) {
    if (!locked.exchange(true)) // @ only write once
      return;

    do {
      for (int i = 0; i < back_off_iters; ++i) {
        _mm_pause(); // intrinsic for spin wait loop.
      }
      back_off_iters = std::min(back_off_iters << 1, MAX_BACKOFF);
    } while (locked.load());
  }
}

static void exponential_backoff(benchmark::State &s) {
  spin_lock_bench<spin_lock>(s);
}

BENCHMARK(exponential_backoff)
    ->RangeMultiplier(2)
    ->Range(1, std::thread::hardware_concurrency())
    ->UseRealTime()
    ->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
