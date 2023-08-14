#!/usr/bin/bash

bench_ () {
  g++ dynamic_dispatch.cc -O3 -lbenchmark
  if [[ $# -eq 0 ]]; then ./a.out; fi
}


perf_stat_ () {
  CASE="${1}" && echo $CASE
  bench_ 1 && perf stat -d -d -d ./a.out --benchmark_filter="$CASE"
}
