#include <atomic>
#include <benchmark/benchmark.h>
#include <chrono>
#include <iostream>
#include <thread>
#include <vector>

// what do system people do.?

// Four cases to demonstrate the effect of sharing cacheline
// has on performance.

void work(std::atomic<int> &a) {
  for (int i = 0; i < 100000; i++) {
    a++;
  }
}

void single_thread() {
  std::atomic<int> a;
  a = 0;

  for (int i = 0; i < 3; ++i) {
    work(a);
  }
}

static void SINGLE_THREADED(benchmark::State &s) {
  while (s.KeepRunning()) {
    single_thread();
  }
}

BENCHMARK(SINGLE_THREADED)->UseRealTime()->Unit(benchmark::kMillisecond);

// direct sharing.
//  a will bounce around in four different cachelines.
void direct_sharing() {
  std::atomic<int> a;
  a = 0;

  std::vector<std::thread> ts{};

  for (int i = 0; i < 3; ++i) {
    ts.emplace_back(std::thread([&]() { work(a); }));
  }

  for (auto &t : ts) {
    t.join();
  }
}

static void DIRECTED_SHARING(benchmark::State &s) {
  while (s.KeepRunning()) {
    direct_sharing();
  }
}

BENCHMARK(DIRECTED_SHARING)->UseRealTime()->Unit(benchmark::kMillisecond);

// false sharing.
// although different threads are using different atomic values,
// they happen to align in the sme cache line.
// the same cacheline will also bouncing around four threads, but this
// time 3/4 are useless.
void false_sharing() {
  std::atomic<int> a;
  std::atomic<int> b;
  std::atomic<int> c;
  std::atomic<int> d;

  std::thread t1([&]() { work(a); });
  std::thread t2([&]() { work(b); });
  std::thread t3([&]() { work(c); });
  std::thread t4([&]() { work(d); });

  t1.join();
  t2.join();
  t3.join();
  t4.join();
}

static void FALSE_SHARING(benchmark::State &s) {
  while (s.KeepRunning()) {
    false_sharing();
  }
}

BENCHMARK(FALSE_SHARING)->UseRealTime()->Unit(benchmark::kMillisecond);

// No sharing at all among 4 threads.
// cacheline has maximum size 64 bytes. if we align the struct
// to 64 we can guarantee two structs don't fit in one cachline.
struct alignas(64) AlignedType {
  AlignedType() { val = 0; }
  std::atomic<int> val;
};

void no_sharing() {
  AlignedType a{};
  AlignedType b{};
  AlignedType c{};
  AlignedType d{};

  std::thread t1([&]() { work(a.val); });
  std::thread t2([&]() { work(b.val); });
  std::thread t3([&]() { work(c.val); });
  std::thread t4([&]() { work(d.val); });

  t1.join();
  t2.join();
  t3.join();
  t4.join();
}

static void NO_SHARING(benchmark::State &s) {
  while (s.KeepRunning()) {
    no_sharing();
  }
}

BENCHMARK(NO_SHARING)->UseRealTime()->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
