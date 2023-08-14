// low context switching spin lock.
// native version 1

// this implementation has a coherence missing problem:
//          As we adding new thread, we have more direct sharing among threads.
//          Thus lots of cache misses.
//          Spawning more threads also have large overhead by itself.

#include <benchmark/benchmark.h>

#include "spin_lock.h"
#include <atomic>

//  all threads acccessing the atomic bool, because it's atomic the cpu
//  need to ensure each time a thread trying to write on it,
//  it has it's exclusive copy.

void spin_lock::lock() {
  while (locked.exchange(true)) // @write
    ;
}

static void naive(benchmark::State &s) { spin_lock_bench<spin_lock>(s); }

BENCHMARK(naive)
    ->RangeMultiplier(2)
    ->Range(1, std::thread::hardware_concurrency())
    ->UseRealTime()
    ->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
