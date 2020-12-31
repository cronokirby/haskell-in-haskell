#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/// exit the program, displaying an error message
void panic(const char *message) {
  fputs("PANIC:", stderr);
  fputs(message, stderr);
  exit(-1);
}

/// A code label takes no arguments, and returns the next function.
///
/// We have to return a void*, because we can't easily have a recursive
/// type here. But, this is basically always an `EntryFunction*`.
typedef void *(*CodeLabel)(void);

/// An InfoTable contains the information about the functions of a closure
typedef struct InfoTable {
  /// The function we can call to enter the closure
  CodeLabel entry;
} InfoTable;

/// A data structure representing our global Heap of memory
typedef struct Heap {
  /// The data contained in this heap
  uint8_t *data;
  /// The part of the data we're currently writing to
  uint8_t *cursor;
  /// The total capacity of the data, in bytes
  size_t capacity;
} Heap;

/// "The Heap", as a global variable.
///
/// This is static, since we always use it through functions provided
/// in this runtime file.
static Heap g_Heap = {NULL, NULL, 0};

/// Grow the heap, removing useless objects
void collect_garbage() {
  panic("I don't know how to collect garbage");
}

/// Reserve a certain amount of bytes in the Heap
///
/// The point of this function is to trigger garbage collection, growing
/// the Heap, if necessary.
///
/// No bounds checking of the Heap is done otherwise.
void heap_reserve(size_t amount) {
  // We'd need to write beyond the capacity of our buffer
  if (g_Heap.cursor + amount > g_Heap.data + g_Heap.capacity) {
    collect_garbage();
  }
}

/// Represents the argument stack
typedef struct StackA {
  /// The top of the argument stack.
  ///
  /// The stack grows downward, with the current pointer always
  /// pointing at valid memory, but containing no "live" value.
  InfoTable *top;
  /// The base pointer of the argument stack.
  ///
  /// We need to keep this around to free the stack on program exit.
  InfoTable *base;
} StackA;

/// The "A" or argument stack
StackA g_SA = {NULL, NULL};

/// The register holding integer returns
int64_t g_IntRegister = 0xBAD;
/// The register holding constructor tag returns
int64_t g_TagRegister = 0xBAD;

/// The starting size for the Heap
static const size_t BASE_HEAP_SIZE = 1 << 16;
/// The starting size for each Stack
static const size_t STACK_SIZE = 1 << 10;

/// Setup all the memory areas that we need
void setup() {
  g_Heap.data = malloc(BASE_HEAP_SIZE);
  if (g_Heap.data == NULL) {
    panic("Failed to initialize Heap");
  }
  g_Heap.cursor = g_Heap.data;
  g_Heap.capacity = BASE_HEAP_SIZE;

  g_SA.base = malloc(STACK_SIZE);
  if (g_SA.base == NULL) {
    panic("Failed to initialize Stack");
  }
  g_SA.top = g_SA.base + STACK_SIZE - 1;
}

/// Cleanup all the memory areas that we've created
void cleanup() {
  free(g_Heap.data);
  free(g_SA.base);
}
