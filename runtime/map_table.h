#ifndef MC_MAP_TABLE_H
#define MC_MAP_TABLE_H

#include <stdint.h>

#include "hash_table.h"

// Byte-oriented map-table adapter built on top of the shared hash-table kernel.
// Keys are hashed/compared by key bytes only. Values are opaque payload bytes.

uint8_t __mc_map_table_contains_key_bytes(
    const mc_dyn_array_t *map,
    const uint8_t *key,
    uint64_t key_size,
    uint64_t value_size
);

// Returns 1 if key was newly inserted, 0 if existing key was updated in place.
uint8_t __mc_map_table_insert_or_assign_bytes(
    mc_dyn_array_t *map,
    const uint8_t *key,
    const uint8_t *value,
    uint64_t key_size,
    uint64_t value_size
);

uint8_t __mc_map_table_remove_key_bytes(
    mc_dyn_array_t *map,
    const uint8_t *key,
    uint64_t key_size,
    uint64_t value_size
);

// Returns 1 and copies value bytes to out_value on hit, 0 otherwise.
uint8_t __mc_map_table_get_value_bytes(
    const mc_dyn_array_t *map,
    const uint8_t *key,
    uint64_t key_size,
    uint64_t value_size,
    uint8_t *out_value
);

void __mc_map_table_clear(mc_dyn_array_t *map);
void __mc_map_table_drop(mc_dyn_array_t *map);

uint32_t __mc_map_table_iter_init(const mc_dyn_array_t *map);
uint8_t __mc_map_table_iter_is_done(const mc_dyn_array_t *map, uint32_t cursor);
void __mc_map_table_iter_load_bytes(
    const mc_dyn_array_t *map,
    uint32_t cursor,
    uint64_t key_size,
    uint64_t value_size,
    uint8_t *out_key,
    uint8_t *out_value
);
uint32_t __mc_map_table_iter_advance(const mc_dyn_array_t *map, uint32_t cursor);

uint8_t __rt_map_contains_string_key(
    uint64_t map_ptr,
    uint64_t key_ptr,
    uint64_t value_size
);
uint8_t __rt_map_insert_or_assign_string_key(
    uint64_t map_ptr,
    uint64_t key_ptr,
    uint64_t value_ptr,
    uint64_t value_size
);
uint8_t __rt_map_remove_string_key(
    uint64_t map_ptr,
    uint64_t key_ptr,
    uint64_t value_size
);
uint8_t __rt_map_get_value_string_key(
    uint64_t map_ptr,
    uint64_t key_ptr,
    uint64_t value_size,
    uint64_t out_value_ptr
);
void __rt_map_clear_string_keys(uint64_t map_ptr, uint64_t value_size);
void __rt_map_drop_string_keys(uint64_t map_ptr, uint64_t value_size);

uint32_t __rt_map_iter_init(uint64_t map_ptr);
uint8_t __rt_map_iter_is_done(uint64_t map_ptr, uint32_t cursor);
void __rt_map_iter_load_string_key(
    uint64_t map_ptr,
    uint32_t cursor,
    uint64_t value_size,
    uint64_t out_key_ptr,
    uint64_t out_value_ptr
);
uint32_t __rt_map_iter_advance(uint64_t map_ptr, uint32_t cursor);

#endif
