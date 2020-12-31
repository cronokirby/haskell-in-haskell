#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/// exit the program, displaying an error message
void panic(const char *message) {
  fputs("PANIC:", stderr);
  fputs(message, stderr);
  exit(-1);
}

/// A data structure representing our global Heap of memory
typedef struct Heap {
  /// The data contained in this heap
  uint8_t* data;
  /// The part of the data we're currently writing to
  uint8_t* cursor;
  /// The total capacity of the data, in bytes
  size_t capacity;
} Heap;

/// "The Heap", as a global variable.
///
/// This is static, since we always use it through functions provided
/// in this runtime file.
static Heap g_Heap = { NULL, NULL, 0 };

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
