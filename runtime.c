#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void panic(const char *message) {
  puts("PANIC:");
  puts(message);
  exit(-1);
}

// A CodeLabel is a pointer to a function returning a void*
typedef void *(*CodeLabel)(void);

// An evac function takes a base pointer to the closure, and returns
// a pointer with the new location of the closure
typedef void *(*EvacFunction)(void *);

void *null_evac(void *base) {
  return base;
}

typedef struct InfoTable {
  CodeLabel entry;
  EvacFunction evac;
} InfoTable;

void *already_evac(void *base) {
  void* ret;
  memcpy(&ret, base + sizeof(InfoTable*), sizeof(void*));
  return ret;
}

InfoTable already_evac_table = { NULL, &already_evac };

// Magic numbers, to a certain degree
int64_t RegInt = 0xBAD;
int64_t RegTag = 0xBAD;
int64_t RegConstrArgs = 0xBAD;

const char *RegString = NULL;

void *RegNode = NULL;

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

const size_t BASE_STACK_SIZE = 1 << 10;

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

const size_t BASE_HEAP_SIZE = 1 << 18;

char *H;
char *H_base;
size_t H_size;
// These are used for garbage collection
char *H_new;
char *H_new_base;
size_t H_new_size;

void H_garbage_collect(size_t requested_size) {
  H_new_base = malloc(requested_size * 2);
  if (H_new_base == NULL) {
    panic("Failed to add new heap when garbage collecting");
  }
  H_new = H_new_base;

  puts("SA:");
  for (void **p = SA_base; p <= SA; ++p) {
    void *base = *p;
    InfoTable *table;
    memcpy(&table, base, sizeof(InfoTable *));
  }

  for (void **p = SA_base; p <= SA; ++p) {
    void *base = *p;
    InfoTable *table;
    memcpy(&table, base, sizeof(InfoTable *));
    *p = table->evac(base);
  }

  size_t allocated = H_new - H_new_base;
  free(H_base);
  H = H_new;
  H_base = H_new_base;
  // We might have allocated too much mem at this point, but this is a
  // reasonable way to shrink the heap if we compact more than 50%
  H_size = allocated * 2;
}

void *H_relocate(void *src, size_t size) {
  void *ret = H_new;
  memcpy(H_new, src, size);
  H_new += size;
  return ret;
}

char *H_relocate_string(char *src) {
  size_t size = strlen(src);
  // Copy all the characters, and the null byte
  return H_relocate(src, size + 1);
}

void H_check_gc_for(size_t size) {
  size_t allocated = H - H_base;
  if (allocated + size > H_size) {
    H_garbage_collect(allocated + size);
  }
}

void H_bump(size_t size) {
  H_check_gc_for(size);
  H += size;
}

void* H_alloc(void *stuff, size_t size) {
  H_check_gc_for(size);
  memcpy(H, stuff, size);
  void* ret = H;
  H += size;
  return ret;
}

const char *H_concat(const char *s1, const char *s2) {
  size_t len1 = strlen(s1);
  size_t len2 = strlen(s2);
  const char *ret = H;
  H_alloc((void *)s1, len1);
  H_alloc((void *)s2, len2 + 1);
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
