#ifndef MC_MACHINE_HOSTED_INSTANCE_H
#define MC_MACHINE_HOSTED_INSTANCE_H

#include <stdint.h>
#include <stddef.h>

// Low-level storage for hosted linear machine instances.
//
// This table only manages keyed instance storage and conditional state updates.
// It does not implement mailbox, dispatch, or waiter semantics on its own.

#define MC_HOSTED_UPDATE_OK 0u
#define MC_HOSTED_UPDATE_STALE 1u
#define MC_HOSTED_UPDATE_NOT_FOUND 2u

typedef struct mc_hosted_instance {
    uint64_t key;
    uint64_t state_tag;
    uintptr_t state_payload;
    uint8_t active;
} mc_hosted_instance_t;

typedef struct mc_hosted_instance_table {
    mc_hosted_instance_t *slots;
    uint32_t len;
    uint32_t cap;
    uint64_t next_key;
} mc_hosted_instance_table_t;

void mc_hosted_instance_table_init(mc_hosted_instance_table_t *table);
void mc_hosted_instance_table_drop(mc_hosted_instance_table_t *table);

uint64_t mc_hosted_instance_table_create(
    mc_hosted_instance_table_t *table,
    uint64_t initial_state_tag,
    uintptr_t initial_payload
);

uint8_t mc_hosted_instance_table_lookup(
    const mc_hosted_instance_table_t *table,
    uint64_t key,
    uint64_t *out_state_tag,
    uintptr_t *out_payload
);

uint8_t mc_hosted_instance_table_update(
    mc_hosted_instance_table_t *table,
    uint64_t key,
    uint64_t expected_tag,
    uint64_t new_tag,
    uintptr_t new_payload,
    uint64_t *out_actual_tag
);

uint8_t mc_hosted_instance_table_remove(mc_hosted_instance_table_t *table, uint64_t key);
uint32_t mc_hosted_instance_table_count(const mc_hosted_instance_table_t *table);

#endif
