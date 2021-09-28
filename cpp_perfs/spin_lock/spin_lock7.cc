// ticket spinlock to improve the fairness.

#include <benchmark/benchmark.h>

#include "spin_lock.h"
#include <atomic>
#include <emmintrin.h>
#include <random>

#define MIN_BACKOFF 4
#define MAX_BACKOFF 1024

void spin_lock::lock() {}

class ticket_lock : public spin_lock {
private:
  std::atomic<std::uint16_t> line{0};
  volatile std::uint16_t serving{0};

public:
  virtual void lock() override;
  virtual void unlock() override;
};

// we aim to optmize for the fairness of the lock. We hope all threads
// have equal opportunity to grab the lock.
void ticket_lock::lock() {
  auto place = line.fetch_add(1);
  while (serving != place)
    ;
}

void ticket_lock::unlock() { serving = serving + 1; }

static void ticket_based(benchmark::State &s) {
  spin_lock_bench<ticket_lock>(s);
}

BENCHMARK(ticket_based)
    ->RangeMultiplier(2)
    ->Range(1, std::thread::hardware_concurrency())
    ->UseRealTime()
    ->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
