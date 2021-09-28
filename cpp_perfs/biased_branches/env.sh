#!/usr/bin/bash

bench_ () {
  g++ biased_branches.cc -O3 -lbenchmark
  if [[ $# -eq 0 ]]; then ./a.out; fi
}


perf_stat_ () {
  CASE="BRANCH_RANDOM/14/${1}" && echo $CASE
  bench_ 1 && perf stat ./a.out --benchmark_filter="$CASE"
}
