#include <algorithm>
#include <benchmark/benchmark.h>
#include <iterator>
#include <vector>

struct Fields {
    int v0 = 0;
    int v1 = 0;
    int v2 = 0;
    int v3 = 0;
    int v4 = 0;
    int v5 = 0;
    int v6 = 0;
    int v7 = 0;
    int v8 = 0;
    int v9 = 0;
    int v10 = 0;
    int v11 = 0;
    int v12 = 0;
    int v13 = 0;
    int v14 = 0;
    int v15 = 0;

    void inc_v0() { v0++; }
};

// struct of Array
struct SoA {

    std::vector<int> v0s;
    std::vector<int> v1s;
    std::vector<int> v2s;
    std::vector<int> v3s;
    std::vector<int> v4s;
    std::vector<int> v5s;
    std::vector<int> v6s;
    std::vector<int> v7s;
    std::vector<int> v8s;
    std::vector<int> v9s;
    std::vector<int> v10s;
    std::vector<int> v11s;
    std::vector<int> v12s;
    std::vector<int> v13s;
    std::vector<int> v14s;
    std::vector<int> v15s;
    void update_v0s() {
        for (auto &i : v0s) {
            i++;
        }
    }

    SoA(int N) {
        v0s.resize(N);
        v1s.resize(N);
        v2s.resize(N);
        v3s.resize(N);
        v4s.resize(N);
        v5s.resize(N);
        v6s.resize(N);
        v7s.resize(N);
        v8s.resize(N);
        v9s.resize(N);
        v10s.resize(N);
        v11s.resize(N);
        v12s.resize(N);
        v13s.resize(N);
        v14s.resize(N);
        v15s.resize(N);
    }
};

// oop style
static void ARRAY_OF_STRUCT(benchmark::State &s) {
    int N = s.range(0);

    std::vector<Fields> vs;
    std::fill_n(std::back_inserter(vs), N, Fields{});

    while (s.KeepRunning()) {
        // there will be more cache missies because the field we want to
        // access scatter among different objects.
        // this might require the cpu to do some prefetching to optmize the
        // access.
        for (auto &i : vs) {
            i.inc_v0();
        }
    }
}
BENCHMARK(ARRAY_OF_STRUCT)->DenseRange(8, 16);

// data oriented style.
static void STRUCT_OF_ARRAY(benchmark::State &s) {
    int N = s.range(0);

    SoA soa{ N };

    while (s.KeepRunning()) {
        // all data are in the same vector, so when accessing one they will
        // be fetched into the cachline together:
        soa.update_v0s();
    }
}
BENCHMARK(STRUCT_OF_ARRAY)->DenseRange(8, 16);

BENCHMARK_MAIN();
