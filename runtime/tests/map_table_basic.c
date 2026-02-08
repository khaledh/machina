#include <stdint.h>

#include "map_table.h"

int main(void) {
    mc_dyn_array_t map = {0};

    for (uint64_t i = 0; i < 256; i++) {
        uint64_t key = i * 17;
        uint64_t value = i * 3;
        uint8_t inserted = __mc_map_table_insert_or_assign_bytes(
            &map,
            (const uint8_t *)&key,
            (const uint8_t *)&value,
            sizeof(key),
            sizeof(value)
        );
        if (!inserted) {
            return 1;
        }
    }

    for (uint64_t i = 0; i < 256; i++) {
        uint64_t key = i * 17;
        uint64_t value = i * 5;
        uint8_t inserted = __mc_map_table_insert_or_assign_bytes(
            &map,
            (const uint8_t *)&key,
            (const uint8_t *)&value,
            sizeof(key),
            sizeof(value)
        );
        if (inserted) {
            return 2;
        }
    }

    for (uint64_t i = 0; i < 256; i++) {
        uint64_t key = i * 17;
        uint8_t has = __mc_map_table_contains_key_bytes(
            &map,
            (const uint8_t *)&key,
            sizeof(key),
            sizeof(uint64_t)
        );
        if (!has) {
            return 3;
        }

        uint64_t out = 0;
        uint8_t got = __mc_map_table_get_value_bytes(
            &map,
            (const uint8_t *)&key,
            sizeof(key),
            sizeof(out),
            (uint8_t *)&out
        );
        if (!got || out != i * 5) {
            return 4;
        }
    }

    for (uint64_t i = 1; i < 256; i += 2) {
        uint64_t key = i * 17;
        uint8_t removed = __mc_map_table_remove_key_bytes(
            &map,
            (const uint8_t *)&key,
            sizeof(key),
            sizeof(uint64_t)
        );
        if (!removed) {
            return 5;
        }
    }

    for (uint64_t i = 0; i < 256; i++) {
        uint64_t key = i * 17;
        uint8_t has = __mc_map_table_contains_key_bytes(
            &map,
            (const uint8_t *)&key,
            sizeof(key),
            sizeof(uint64_t)
        );
        if ((i % 2 == 0 && !has) || (i % 2 == 1 && has)) {
            return 6;
        }
    }

    __mc_map_table_clear(&map);
    if (map.len != 0) {
        return 7;
    }
    if (mc_cap_value(map.cap) == 0) {
        return 8;
    }

    __mc_map_table_drop(&map);
    if (map.ptr != 0 || map.len != 0 || map.cap != 0) {
        return 9;
    }

    return 0;
}
