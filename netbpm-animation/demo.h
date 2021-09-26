#pragma once
#include "ppm.h"
#include <stdio.h>
#include <stdlib.h>

namespace demo {
char const filename[] = "../img/box.ppm";

inline void draw() {
  char const buffer[] = ( //
      "P6\n"
      "#by jimmy\n" // you can have comment in the file.
      "2 2\n"
      "255\n"
      "z   "
      " z  "
      "  z "
      "   z");
  int size = sizeof(buffer) / sizeof(buffer[0]);
  write_ppm(filename, buffer, size);
}
} // namespace demo
