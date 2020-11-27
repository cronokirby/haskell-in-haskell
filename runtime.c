#include <stdio.h>
#include <stdlib.h>

void panic(const char *message) {
  puts("PANIC:");
  puts(message);
  exit(-1);
}

const size_t BASE_HEAP_SIZE = 1 << 16;

// The heap is a pointer to pointers, basically
void **H;
void **H_base;
size_t H_size;

void H_reserve(size_t count) {
  size_t diff = H - H_base;
  if (diff < count) {
    H_base = realloc(H_base, H_size);
    H = H_base + H_size + diff;
    H_size *= 2;
  }
}

const size_t BASE_STACK_SIZE = 1 << 10;

void **SA;
void **SA_base;
size_t SA_size;

void SA_reserve(size_t count) {
  size_t diff = SA - SA_base;
  if (diff < count) {
    SA_base = realloc(SA_base, SA_size);
    SA = SA_base + SA_size + diff;
    SA_size *= 2;
  }
}

void **SB;
void **SB_base;
size_t SB_size;

void SB_reserve(size_t count) {
  size_t diff = SB - SB_base;
  if (diff < count) {
    SB_base = realloc(SB_base, SB_size);
    SB = SB_base + SB_size + diff;
    SB_size *= 2;
  }
}

void setup() {
  H_base = malloc(sizeof(void*) * BASE_HEAP_SIZE);
  if (H_base == NULL) {
    panic("Failed to initialize heap");
  }
  H = H_base + BASE_HEAP_SIZE;
  H_size = BASE_HEAP_SIZE;

  SA_base = malloc(sizeof(void*) * BASE_STACK_SIZE);
  if (SA_base == NULL) {
    panic("Failed to initialize stack A");
  }
  SA = SA_base + BASE_STACK_SIZE;
  SA_size = BASE_STACK_SIZE;

  SB_base = malloc(sizeof(void*) * BASE_STACK_SIZE);
  if (SB_base == NULL) {
    panic("Failed to initialize stack A");
  }
  SB = SB_base + BASE_STACK_SIZE;
  SB_size = BASE_STACK_SIZE;
}
