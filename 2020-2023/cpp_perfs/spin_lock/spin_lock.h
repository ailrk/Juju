// active backoff

#include <benchmark/benchmark.h>

#include <atomic>
#include <cstdint>
#include <iostream>
#include <thread>
#include <vector>

#define ITERATION 100000

class spin_lock {
protected:
  std::atomic<bool> locked{false};

public:
  virtual void lock();

  virtual void unlock() { locked.store(false); } // @write
};

class scoped_spin_lock {
private:
  spin_lock &lk;

public:
  scoped_spin_lock(spin_lock &lk) : lk(lk) { lk.lock(); }
  ~scoped_spin_lock() { lk.unlock(); }
};

inline std::int64_t inc(spin_lock &s, std::int64_t value) {

  for (int i = 0; i < ITERATION; ++i) {
    scoped_spin_lock lk{s};
    value++; // @write
  }

  return value;
}

template <typename Lock> inline void spin_lock_bench(benchmark::State &s) {
  auto num_threads = s.range(0);

  std::int64_t value = 0;

  std::vector<std::thread> threads;
  threads.reserve(num_threads);

  Lock slk{};

  for (auto _ : s) {
    for (auto i = 0u; i < num_threads; ++i) {
      threads.emplace_back([&] {
          int v = inc(slk, value);
          assert(v == ITERATION);
          });
    }

    for (auto &t : threads) {
      t.join();
    }

    threads.clear();
  }
}
