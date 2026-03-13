#include <stdint.h>
#include <stddef.h>

#include "machine/hosted_instance.h"

int main(void) {
    mc_hosted_instance_table_t table;
    mc_hosted_instance_table_init(&table);

    uint64_t key1 = mc_hosted_instance_table_create(&table, 10, (uintptr_t)111);
    if (key1 == 0 || mc_hosted_instance_table_count(&table) != 1) {
        return 1;
    }

    uint64_t state = 0;
    uintptr_t payload = 0;
    if (!mc_hosted_instance_table_lookup(&table, key1, &state, &payload)) {
        return 2;
    }
    if (state != 10 || payload != (uintptr_t)111) {
        return 3;
    }

    uint64_t key2 = mc_hosted_instance_table_create(&table, 20, (uintptr_t)222);
    uint64_t key3 = mc_hosted_instance_table_create(&table, 30, (uintptr_t)333);
    if (key2 <= key1 || key3 <= key2) {
        return 4;
    }

    if (!mc_hosted_instance_table_lookup(&table, key2, &state, &payload) || state != 20 || payload != (uintptr_t)222) {
        return 5;
    }
    if (!mc_hosted_instance_table_lookup(&table, key3, &state, &payload) || state != 30 || payload != (uintptr_t)333) {
        return 6;
    }

    uint64_t actual = 0;
    if (mc_hosted_instance_table_update(&table, key1, 10, 11, (uintptr_t)444, &actual) != MC_HOSTED_UPDATE_OK) {
        return 7;
    }
    if (!mc_hosted_instance_table_lookup(&table, key1, &state, &payload) || state != 11 || payload != (uintptr_t)444) {
        return 8;
    }

    actual = 0;
    if (mc_hosted_instance_table_update(&table, key1, 10, 12, (uintptr_t)555, &actual) != MC_HOSTED_UPDATE_STALE) {
        return 9;
    }
    if (actual != 11) {
        return 10;
    }

    if (mc_hosted_instance_table_update(&table, 999999, 1, 2, (uintptr_t)0, &actual) != MC_HOSTED_UPDATE_NOT_FOUND) {
        return 11;
    }

    if (!mc_hosted_instance_table_remove(&table, key2)) {
        return 12;
    }
    if (mc_hosted_instance_table_lookup(&table, key2, &state, &payload)) {
        return 13;
    }

    for (uint64_t i = 0; i < 32; i++) {
        if (mc_hosted_instance_table_create(&table, 100 + i, (uintptr_t)(1000 + i)) == 0) {
            return 14;
        }
    }
    if (mc_hosted_instance_table_count(&table) != 34) {
        return 15;
    }

    for (uint64_t i = 4; i < table.next_key; i++) {
        if (!mc_hosted_instance_table_lookup(&table, i, &state, &payload)) {
            return 16;
        }
    }

    mc_hosted_instance_table_drop(&table);
    if (table.slots != NULL || table.len != 0 || table.cap != 0 || table.next_key != 0) {
        return 17;
    }

    return 0;
}
