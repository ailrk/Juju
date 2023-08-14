#include <stdio.h>
#include <stdlib.h>

int is_prime(int x) {
  for (int i = x - 1; i >= 2; i--)
    for (int s = x; s >= 0; s-= i)
      if (s == 0)
        return 0;
  return 1;
}

// does this program halt?
int goldbach(int x) {
  if (x <= 2 || (x & 1) != 0) return 0;
  for (int i = x; i > 0; i--)
    if (is_prime(x) && is_prime(x - i))
      return i;
  exit(1);
}
