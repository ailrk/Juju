// active backoff

#include <benchmark/benchmark.h>

#include "spin_lock.h"
#include <atomic>

// Adding some idle operation between each read access.
// this way we separate out the access of the atomic lock.
// When it's availble, less thread will see the change, thus
// less threads will break the loop and do exchange.
// Thus reduce the burst contention.

void spin_lock::lock() {
  while (1) {
    if (!locked.exchange(true)) // @ only write once
      return;

    while (locked.load()) // @read
                          // dummy for loop wasting energy.
      for (volatile int i = 0; i < 100; i += 1)
        ;
  }
}

static void active_backoff(benchmark::State &s) {
  spin_lock_bench<spin_lock>(s);
}

BENCHMARK(active_backoff)
    ->RangeMultiplier(2)
    ->Range(1, std::thread::hardware_concurrency())
    ->UseRealTime()
    ->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
