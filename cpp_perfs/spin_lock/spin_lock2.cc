// lockly spinning to reduce cash missing.

// four types of cache misses.
// 1. compulsory  (first time)
// 2. capacity    (access more data then cache supports.)
// 3. conflict    (mapped to the same associative set cache)
// 4. coherence   (direct sharing, false sharing, etc.)

#include <benchmark/benchmark.h>

#include "spin_lock.h"
#include <atomic>

// (way less).
void spin_lock::lock() {
  while (1) {
    if (!locked.exchange(true)) // @ only write once, get rid of uncessary xchg
      return;

    // all threads closely monitoring the state of the lock.
    // once the lock is available, all threads will try to grab
    // the lock with locked.exchange. The sudden burst impose a large overheat.
    while (locked.load()) // @read
      ;
  }
}

static void locally_spin(benchmark::State &s) { spin_lock_bench<spin_lock>(s); }

BENCHMARK(locally_spin)
    ->RangeMultiplier(2)
    ->Range(1, std::thread::hardware_concurrency())
    ->UseRealTime()
    ->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
