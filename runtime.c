#include <stdio.h>
#include <stdlib.h>

void panic(const char *message) {
  puts("PANIC:");
  puts(message);
  exit(-1);
}

