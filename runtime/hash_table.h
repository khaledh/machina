#ifndef MC_HASH_TABLE_H
#define MC_HASH_TABLE_H

#include <stdint.h>

#include "types.h"

#define MC_HASH_CTRL_EMPTY 0u
#define MC_HASH_CTRL_FULL 1u
#define MC_HASH_CTRL_TOMB 2u

uint64_t __mc_hash_bytes(const uint8_t *data, uint64_t len);

uint64_t __mc_hash_table_total_bytes(uint32_t cap, uint64_t slot_size);
uint64_t __mc_hash_table_slot_offset(uint32_t index, uint64_t slot_size);

uint8_t *__mc_hash_table_ctrl(uint8_t *base);
uint8_t *__mc_hash_table_slots(uint8_t *base, uint32_t cap);
uint8_t *__mc_hash_table_slot(uint8_t *slots, uint32_t index, uint64_t slot_size);

uint32_t __mc_hash_table_probe_start(uint64_t hash, uint32_t cap);

uint32_t __mc_hash_table_find_slot_bytes(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const uint8_t *needle,
    uint64_t slot_size,
    uint64_t hash,
    uint8_t *found
);

void __mc_hash_table_insert_unique_bytes(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const uint8_t *elem,
    uint64_t slot_size
);

uint8_t *__mc_hash_table_alloc(uint32_t cap, uint64_t slot_size);
void __mc_hash_table_clear_ctrl(uint8_t *ctrl, uint32_t cap);
uint32_t __mc_hash_table_next_cap(uint32_t cap, uint32_t initial_cap);

void __mc_hash_table_rehash_bytes(mc_dyn_array_t *table, uint32_t new_cap, uint64_t slot_size);

void __mc_hash_table_ensure_for_insert_bytes(
    mc_dyn_array_t *table,
    uint64_t slot_size,
    uint32_t initial_cap,
    uint32_t max_load_num,
    uint32_t max_load_den
);

#endif
