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

void SA_push(void *arg) {
  if (SA == SA_base + SA_size) {
    SA_base = realloc(SA_base, SA_size * 2);
    SA = SA_base + SA_size;
    SA_size *= 2;
  }
  *SB = arg;
  ++SA;
}

void *SA_pop() {
  if (SA == SA_base) {
    panic("Trying to pop from empty SB");
  }
  return *(SA--);
}

void **SB_base;
void **SB;
size_t SB_size;

void SB_push(void *arg) {
  if (SB == SB_base + SB_size) {
    SB_base = realloc(SB_base, SB_size * 2);
    SB = SB_base + SB_size;
    SB_size *= 2;
  }
  *SB = arg;
  ++SB;
}

void *SB_pop() {
  if (SB == SB_base) {
    panic("Trying to pop from empty SB");
  }
  return *(SB--);
}

void setup() {
  H_base = malloc(sizeof(void *) * BASE_HEAP_SIZE);
  if (H_base == NULL) {
    panic("Failed to initialize heap");
  }
  H = H_base + BASE_HEAP_SIZE;
  H_size = BASE_HEAP_SIZE;

  SA_base = malloc(sizeof(void *) * BASE_STACK_SIZE);
  if (SA_base == NULL) {
    panic("Failed to initialize stack A");
  }
  SA = SA_base + BASE_STACK_SIZE;
  SA_size = BASE_STACK_SIZE;

  SB_base = malloc(sizeof(void *) * BASE_STACK_SIZE);
  if (SB_base == NULL) {
    panic("Failed to initialize stack A");
  }
  SB = SB_base + BASE_STACK_SIZE;
  SB_size = BASE_STACK_SIZE;
}

// A CodeLabel is a pointer to a function returning a void*
typedef void *(*CodeLabel)(void);

typedef struct InfoTable {
  CodeLabel entry;
  CodeLabel evacuate;
  CodeLabel scavenge;
} InfoTable;

int RegInt = 0;

char *RegString = NULL;
