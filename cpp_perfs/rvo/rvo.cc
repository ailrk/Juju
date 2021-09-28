#include <array>
#include <benchmark/benchmark.h>
#include <cstdlib>
#include <cstring>
#include <iostream>

class MyString {

  char *data;
  size_t size;

public:
  MyString(const char *msg) {
    size = std::strlen(msg) - 1;
    data = new char[size];
    std::memcpy(data, msg, sizeof(char) * size);
  }

  MyString(const MyString &other) {
    // std::cout << "calling copy constructor" << std::endl;

    if (size < other.size) {
      if (data) {
        delete[] data;
      }
      data = new char[other.size];
    }

    std::memcpy(data, other.data, sizeof(char) * other.size);
    size = other.size;
  }

  MyString(MyString &&other) {
    // std::cout << "calling move constructor" << std::endl;

    if (!other.data) {
      return;
    }

    size = other.size;
    data = other.data;

    other.data = nullptr;
    other.size = 0;
  }

  ~MyString() { delete[] data; }
};

// return type optimization will add a pointer as argument of the
// function.
// the function will assign the address of value ms to the hidden ptr,
// and return the address instead of the value.
// At call site the returned address is dereferenced to get the same effect.
MyString rvo() {
  MyString ms{"hi"};
  return ms;
}

static void RVO(benchmark::State &s) {
  while (s.KeepRunning()) {
    for (int i = 0; i < 100000; ++i) {
      rvo();
    }
  }
}

BENCHMARK(RVO)->UseRealTime()->Unit(benchmark::kMillisecond);

// this cannot have return value optimization. because the problem
// doesn't know which branch will be choosen at runtime, so we don't know
// which MyString we should return to.
MyString norvo(bool cond) {
  MyString ms1{"Yo"};
  MyString ms2{"Sup"};
  if (cond) {
    return ms1;
  } else {
    return ms2;
  }
}

static void NORVO(benchmark::State &s) {
  while (s.KeepRunning()) {
    for (int i = 0; i < 100000; ++i) {
      norvo(rand() > 0.5);
    }
  }
}
BENCHMARK(NORVO)->UseRealTime()->Unit(benchmark::kMillisecond);

BENCHMARK_MAIN();
