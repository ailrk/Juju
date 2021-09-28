bench_ () {
  g++ ./spin_lock"$1".cc -I. -lpthread -lbenchmark -O3 -march=native -mtune=native -flto -fuse-linker-plugin
  if [[ $# -eq 1 ]]; then
    ./a.out
  fi
}

perf_record_ () {
  bench_ $1 1
  perf record ./a.out
  perf report
}

perf_stat_ () {
  bench_ $1 1
  perf stat -d -d -d ./a.out
}
