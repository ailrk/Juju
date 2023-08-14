#!/usr/bin/bash

bench_ () {
  g++ ./thread_affinity.cc -g -O3 -flto -fuse-linker-plugin -march=native -mtune=native -lbenchmark -lpthread

  if [[ $# -eq 0 ]]; then ./a.out; fi
}

perf_record_ () {
  bench_ 1 && perf record ./a.out --benchmark-filter="$1" && perf report
}

perf_stat_ () {
  bench_ 1 && perf stat -d -d -d ./a.out --benchmark_filter="$1"
}

perf_c2c_ () {
  bench_ 1 && perf c2c record ./a.out --benchmark_filter="$1" && perf c2c report
}
