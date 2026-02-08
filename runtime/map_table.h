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

#endif
