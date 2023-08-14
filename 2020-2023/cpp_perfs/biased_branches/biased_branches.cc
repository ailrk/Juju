#include <benchmark/benchmark.h>
#include <iostream>
#include <random>

// branch predictions
// processors use branch predictors to help the pipeline
// to fille the instructions.

static void custom_args(benchmark::internal::Benchmark *b) {
  for (auto i : {14}) {
    // list of probabilities.
    //
    // when probability is 0 or 100, the brach predictor should always predict the
    // right branch, because there are no other possibilities.
    // as probability goes closer to 50, the branch predictor will start to make
    // more and more mistakes, so there should be more branch misses.
    for (auto j : {0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100}) {
      b = b->ArgPair(i, j);
    }
  }
}

static void BRANCH_RANDOM(benchmark::State &s) {
  auto N = 1 << s.range(0);

  double probability = s.range(1) / 100.0;

  std::random_device rd;
  std::mt19937 gen(rd());
  std::bernoulli_distribution d{probability};

  std::vector<bool> vin(N);
  std::generate(begin(vin), end(vin), [&]() { return d(gen); });

  int sink = 0;

  for (auto _ : s) {
    for (auto b : vin) {
      if (b) {
        benchmark::DoNotOptimize(sink += s.range(0));
      }
    }
  }
}

BENCHMARK(BRANCH_RANDOM)->Apply(custom_args)->Unit(benchmark::kMicrosecond);
BENCHMARK_MAIN();
