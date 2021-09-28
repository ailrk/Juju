#!/usr/bin/bash
bench_ () {
  g++ ./false_sharing.cc -lbenchmark -lpthread -g
  if [[ $# -eq 0 ]]; then ./a.out; fi
}

dump_ () {
  bench_ 1 && objdump -C -d ./a.out
}

perf_record_ () {
  bench_ 1 && perf record ./a.out --benchmark_filter="$1" && perf report
}

perf_c2c_ () {
  bench_ 1 && perf c2c record ./a.out && perf c2c report
}

perf_stat_ () {
  bench_ 1 && perf stat -d -d -d ./a.out --benchmark_filter="$1"
}

