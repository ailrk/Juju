#include <atomic>
#include <benchmark/benchmark.h>
#include <cassert>
#include <iostream>
#include <pthread.h>
#include <thread>

#define ITERATION = 100000
// pinning threads to specific cores for performance.

void work(std::atomic<int> &a) {
  for (int i = 0; i < 100000; ++i) {
    a++;
  }
}

struct alignas(64) AlignedAtomic {
  AlignedAtomic(int v) { value = v; }
  std::atomic<int> value;
};

// thread 0, 1 wants to access the same atomic value.
// thread 2, 3 wants to access the same atomic value.
// if we leave the scheduling to the operating system, 0, 1, 2, 3
// are equally likely to be bounded to any core.
// So if core 1 was running thread 1 with cache filled with a,
// and next second it's scheduled with thread 3, the cache line
// is invalid and need to refetch.
void os_scheduler() {
  AlignedAtomic a{0};
  AlignedAtomic b{0};

  std::thread t0([&]() { work(a.value); });
  std::thread t1([&]() { work(a.value); });
  std::thread t2([&]() { work(b.value); });
  std::thread t3([&]() { work(b.value); });

  t0.join();
  t1.join();
  t2.join();
  t3.join();
}

static void OS_SCHEDULER(benchmark::State &s) {
  while (s.KeepRunning()) {
    os_scheduler();
  }
}

// We can bound certain threads to one particular core. It's called set
// thread affinity.
// By doing so, thread 0, 1 always get bound to the same cpu, thus the cache
// never get invalidated.
BENCHMARK(OS_SCHEDULER)->UseRealTime()->Unit(benchmark::kMillisecond);

void thread_affinity() {
  AlignedAtomic a{0};
  AlignedAtomic b{0};

  cpu_set_t cpu_set_1;
  cpu_set_t cpu_set_2;

  // zero out
  CPU_ZERO(&cpu_set_1);
  CPU_ZERO(&cpu_set_2);

  // set CPU cores to pin our threads to.
  CPU_SET(0, &cpu_set_1);
  CPU_SET(1, &cpu_set_2);

  // pin 0, 1 to core 0
  std::thread t0([&]() { work(a.value); });
  assert(pthread_setaffinity_np(t0.native_handle(), sizeof(cpu_set_t),
                                &cpu_set_1) == 0);
  std::thread t1([&]() { work(a.value); });
  assert(pthread_setaffinity_np(t1.native_handle(), sizeof(cpu_set_t),
                                &cpu_set_1) == 0);

  // pin 2, 3 to core 1
  std::thread t2([&]() { work(b.value); });
  assert(pthread_setaffinity_np(t2.native_handle(), sizeof(cpu_set_t),
                                &cpu_set_2) == 0);
  std::thread t3([&]() { work(b.value); });
  assert(pthread_setaffinity_np(t3.native_handle(), sizeof(cpu_set_t),
                                &cpu_set_2) == 0);

  t0.join();
  t1.join();
  t2.join();
  t3.join();
}

static void THREAD_AFFINITY(benchmark::State &s) {
  while (s.KeepRunning()) {
    thread_affinity();
  }
}

BENCHMARK(THREAD_AFFINITY)->UseRealTime()->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
