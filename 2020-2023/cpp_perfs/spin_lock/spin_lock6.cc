// random backoff

#include <benchmark/benchmark.h>

#include "spin_lock.h"
#include <atomic>
#include <emmintrin.h>
#include <random>

#define MIN_BACKOFF 4
#define MAX_BACKOFF 1024

class random_spin_lock : public spin_lock {
private:
  std::uniform_int_distribution<int> dist;
  std::mt19937 rng;

public:
  void lock();

  random_spin_lock() {
    rng.seed(std::random_device()());
    dist = std::uniform_int_distribution<int>(MIN_BACKOFF, MAX_BACKOFF);
  }
};

void spin_lock::lock() {}

void random_spin_lock::lock() {
  while (1) {
    if (!locked.exchange(true)) // @ only write once
      return;

    do {
      int back_off_iters = dist(rng);
      for (int i = 0; i < back_off_iters; ++i) {
        _mm_pause(); // intrinsic for spin wait loop.
      }
    } while (locked.load());
  }
}

static void random_backoff(benchmark::State &s) {
  spin_lock_bench<random_spin_lock>(s);
}

BENCHMARK(random_backoff)
    ->RangeMultiplier(2)
    ->Range(1, std::thread::hardware_concurrency())
    ->UseRealTime()
    ->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
