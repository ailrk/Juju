#include <benchmark/benchmark.h>
#include <iostream>
#include <random>
#include <vector>

// underlying implementation of dynamic dispatching will use
// some sort of conditional somewhere, so branch misses applies
// here is merely a consequences of that.

struct JosephJostar {
  virtual float stand() const noexcept { return 2.0; }
};

struct KujoJotaro : public JosephJostar {
  virtual float stand() const noexcept { return 1.0; }
};

struct JosukeHigashikata : public JosephJostar {
  virtual float stand() noexcept { return 3.0; }
};

// this version has less branch misses.
static void vf_sorted(benchmark::State &s) {
  std::vector<JosephJostar *> jojos;

  std::fill_n(std::back_inserter(jojos), 10000, new JosephJostar);
  std::fill_n(std::back_inserter(jojos), 10000, new KujoJotaro);
  std::fill_n(std::back_inserter(jojos), 10000, new JosukeHigashikata);

  float sum = 0;

  while (s.KeepRunning()) {
    for (auto *jojo : jojos) {
      sum += jojo->stand();
    }
  }
}

BENCHMARK(vf_sorted)->UseRealTime()->Unit(benchmark::kMicrosecond);

// more branch misses.
static void vf_unsorted(benchmark::State &s) {
  std::vector<JosephJostar *> jojos;

  std::fill_n(std::back_inserter(jojos), 10000, new JosephJostar);
  std::fill_n(std::back_inserter(jojos), 10000, new KujoJotaro);
  std::fill_n(std::back_inserter(jojos), 10000, new JosukeHigashikata);

  std::random_device rng;
  std::mt19937 gen(rng());
  std::shuffle(std::begin(jojos), std::end(jojos), gen);

  float sum = 0;

  while (s.KeepRunning()) {
    for (auto *jojo : jojos) {
      sum += jojo->stand();
    }
  }
}

BENCHMARK(vf_unsorted)->UseRealTime()->Unit(benchmark::kMicrosecond);

BENCHMARK_MAIN();
