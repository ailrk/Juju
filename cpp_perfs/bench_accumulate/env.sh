#!/usr/bin/bash
bench_() {
  g++ bench_accumulate.cc -lbenchmark -lpthread -O3
  if [[ $# -eq 0 ]]; then ./a.out; fi
}

bench_native_() {
  g++ bench_accumulate.cc -lbenchmark -lpthread -O3 \
    -march=native -mtune=native -flto -fuse-linker-plugin
  if [[ $# -eq 0 ]]; then ./a.out; fi
}

bench_sz_20_() {
  bench_ 1
  ./a.out --benchmark_filter=accumulate_bench/20
}

dump_() {
  bench_ 1 && objdump -C -d ./a.out > out.asm
}

perf_record_() {
  bench_ && perf record ./a.out && perf report
}

refresh_() {
  bench_ 1 && dump_
}
