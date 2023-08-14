#pragma once
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// tiny ppm header

inline void write_ppm(char const fname[], char const data[], int size) {
  FILE *image = fopen(fname, "wb");
  fwrite(data, size, 1, image);
  fclose(image);

  char cmd[256];
  memset(cmd, 0, 256);
  snprintf(cmd, 256, "convert %s %s.png", fname, fname);
  system(cmd);
}
