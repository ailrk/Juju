#include <algorithm>
#include <benchmark/benchmark.h>
#include <cstdlib>
#include <iostream>
#include <numeric>
#include <vector>

static void accumulate_bench(benchmark::State &s) {
  auto N = 1 << s.range(0);

  std::vector<int> v(N);
  std::generate(std::begin(v), std::end(v), [] { return rand() % 100; });

  int result = 0;
  for (auto _ : s) {
    benchmark::DoNotOptimize(
        result = std::accumulate(std::begin(v), std::end(v), 0));
  }

  // note if you don't use result, it might be optimized away by the compiler.
}

BENCHMARK(accumulate_bench)->DenseRange(20, 25)->Unit(benchmark::kMicrosecond);
BENCHMARK_MAIN();
