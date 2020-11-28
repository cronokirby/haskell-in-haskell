#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void panic(const char *message) {
  puts("PANIC:");
  puts(message);
  exit(-1);
}

const size_t BASE_HEAP_SIZE = 1 << 16;

void *H;
void *H_base;
size_t H_size;

const size_t BASE_STACK_SIZE = 1 << 10;

const char *H_concat(const char *s1, const char *s2) {
  size_t len1 = strlen(s1);
  size_t len2 = strlen(s2);
  void *next_H = H + len1 + len2 + 1;
  if (next_H >= H_base) {
    panic("Heap overflow!");
  }
  // Don't copy the null terminator
  memcpy(H, s1, len1);
  // Copy the null terminator as well
  memcpy(H + len1, s2, len2 + 1);
  const char *ret = H;
  H = next_H;
  return ret;
}

void **SA;
void **SA_base;
size_t SA_size;

void SA_push(void *arg) {
  if (SA == SA_base + SA_size) {
    SA_base = realloc(SA_base, SA_size * 2);
    SA = SA_base + SA_size;
    SA_size *= 2;
  }
  *SA = arg;
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

// Magic numbers, to a certain degree
int64_t RegInt = 0xBAD;
int64_t Regtag = 0xBAD;

const char *RegString = NULL;

void *RegNode = NULL;
