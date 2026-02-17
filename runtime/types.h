#include <stdint.h>
#include <stddef.h>

typedef struct mc_string {
  uint64_t ptr;     // address of bytes
  uint32_t len;     // byte length
  uint32_t cap;     // capacity (high bit marks owned)
} mc_string_t;

#define MC_CAP_OWNED 0x80000000u
#define MC_CAP_MASK  0x7fffffffu

static inline uint32_t mc_cap_value(uint32_t cap) {
  return cap & MC_CAP_MASK;
}

static inline uint8_t mc_cap_is_owned(uint32_t cap) {
  return (cap & MC_CAP_OWNED) != 0;
}

static inline uint32_t mc_cap_with_owned(uint32_t cap) {
  return (cap & MC_CAP_MASK) | MC_CAP_OWNED;
}

typedef struct mc_slice {
  uint64_t ptr;     // address of bytes
  uint64_t len;     // byte length
} mc_slice_t;

typedef struct mc_dyn_array {
  uint64_t ptr;     // address of elements
  uint32_t len;     // element length
  uint32_t cap;     // element capacity (high bit marks owned)
} mc_dyn_array_t;

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
uint64_t __mc_alloc_live_blocks(void);
uint64_t __mc_alloc_total_allocs(void);
uint64_t __mc_alloc_total_frees(void);
