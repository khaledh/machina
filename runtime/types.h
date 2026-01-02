#include <stdint.h>
#include <stddef.h>

typedef struct mc_string {
  uint64_t ptr;     // address of bytes
  uint32_t len;     // byte length
  uint32_t cap;     // capacity (0 means view/static)
} mc_string_t;

typedef struct mc_slice {
  uint64_t ptr;     // address of bytes
  uint64_t len;     // byte length
} mc_slice_t;

typedef struct mc_fmt {
  uint64_t ptr;     // buffer address
  uint64_t len;     // bytes written
  uint64_t cap;     // buffer capacity
} mc_fmt_t;

// Dynamic Memory

typedef struct mc_heap_vtable_t {
  void *(*alloc)(size_t size);
  void *(*realloc)(void *ptr, size_t size);
  void (*free)(void *ptr);
} mc_heap_vtable_t;

typedef struct mc_heap_t {
  mc_heap_vtable_t *vtable;
  void *ctx;
} mc_heap_t;

// Debug helpers
void __mc_set_alloc_trace(uint8_t enabled);
