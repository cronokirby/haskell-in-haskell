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

char *H;
char *H_base;
size_t H_size;

const size_t BASE_STACK_SIZE = 1 << 10;

void H_alloc(void *stuff, size_t size) {
  size_t allocated = H - H_base;
  if (allocated + size > H_size) {
    // TODO: Trigger GC here
    panic("Heap Overflow!");
  }
  memcpy(H, stuff, size);
  H += size;
}

const char *H_concat(const char *s1, const char *s2) {
  size_t len1 = strlen(s1);
  size_t len2 = strlen(s2);
  const char *ret = H;
  H_alloc((void *)s1, len1);
  H_alloc((void *)s2, len2 + 1);
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
  --SA;
  return *SA;
}

char *SB_base;
char *SB;
size_t SB_size;

void SB_reserve(size_t size) {
  size_t allocated = SB - SB_base;
  if (allocated + size > SB_size) {
    SB_base = realloc(SB_base, SB_size * 2);
    SB = SB_base + allocated;
    SB_size *= 2;
  }
}

void SB_push(void *arg) {
  SB_reserve(sizeof(arg));
  memcpy(SB, &arg, sizeof(arg));
  SB += sizeof(arg);
}

void SB_push_int(int64_t arg) {
  SB_reserve(sizeof(arg));
  memcpy(SB, &arg, sizeof(arg));
  SB += sizeof(arg);
}

void SB_push_str(const char *arg) {
  SB_reserve(sizeof(arg));
  memcpy(SB, &arg, sizeof(arg));
  SB += sizeof(arg);
}

void *SB_pop() {
  void *ret;
  SB -= sizeof(void *);
  if (SB < SB_base) {
    panic("SB underflow!");
  }
  memcpy(&ret, SB, sizeof(void *));
  return ret;
}

int64_t SB_pop_int() {
  int64_t ret;
  SB -= sizeof(ret);
  if (SB < SB_base) {
    panic("SB underflow!");
  }
  memcpy(&ret, SB, sizeof(ret));
  return ret;
}

const char *SB_pop_str() {
  const char *ret;
  SB -= sizeof(ret);
  if (SB < SB_base) {
    panic("SB underflow!");
  }
  memcpy(&ret, SB, sizeof(ret));
  return ret;
}

void setup() {
  H_base = malloc(sizeof(char) * BASE_HEAP_SIZE);
  if (H_base == NULL) {
    panic("Failed to initialize heap");
  }
  H = H_base;
  H_size = BASE_HEAP_SIZE;

  SA_base = malloc(sizeof(void *) * BASE_STACK_SIZE);
  if (SA_base == NULL) {
    panic("Failed to initialize stack A");
  }
  SA = SA_base;
  SA_size = BASE_STACK_SIZE;

  SB_base = malloc(sizeof(char) * BASE_STACK_SIZE);
  if (SB_base == NULL) {
    panic("Failed to initialize stack A");
  }
  SB = SB_base;
  SB_size = BASE_STACK_SIZE;
}

// A CodeLabel is a pointer to a function returning a void*
typedef void *(*CodeLabel)(void);

// An evac function takes a base pointer to the closure, and returns
// a pointer with the new location of the closure
typedef void *(*EvacFunction)(void *);

// A scavenge function is just called with a base pointer
typedef void (*ScavengeFunction)(void *);

typedef struct InfoTable {
  // Entry holds either a codelabel, or an indirection to a new location,
  // during garbage collection
  void *entry;
  EvacFunction evacuate;
  ScavengeFunction scavenge;
} InfoTable;

// Magic numbers, to a certain degree
int64_t RegInt = 0xBAD;
int64_t RegTag = 0xBAD;
int64_t RegConstrArgs = 0xBAD;

const char *RegString = NULL;

void *RegNode = NULL;
