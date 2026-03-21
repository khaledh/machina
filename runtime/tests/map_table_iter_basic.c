#include <stdint.h>

#include "map_table.h"

int main(void) {
    mc_dyn_array_t map = {0};

    for (uint64_t i = 0; i < 64; i++) {
        uint64_t key = i * 13;
        uint64_t value = i * 7;
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

    for (uint64_t i = 1; i < 64; i += 3) {
        uint64_t key = i * 13;
        if (!__mc_map_table_remove_key_bytes(
                &map,
                (const uint8_t *)&key,
                sizeof(key),
                sizeof(uint64_t)
            )) {
            return 2;
        }
    }

    uint64_t seen = 0;
    uint64_t key_sum = 0;
    uint64_t value_sum = 0;
    uint32_t cursor = __mc_map_table_iter_init(&map);
    while (!__mc_map_table_iter_is_done(&map, cursor)) {
        uint64_t key = 0;
        uint64_t value = 0;
        __mc_map_table_iter_load_bytes(
            &map,
            cursor,
            sizeof(key),
            sizeof(value),
            (uint8_t *)&key,
            (uint8_t *)&value
        );
        seen += 1;
        key_sum += key;
        value_sum += value;
        cursor = __mc_map_table_iter_advance(&map, cursor);
    }

    if (seen != map.len) {
        return 3;
    }

    uint64_t expect_seen = 0;
    uint64_t expect_key_sum = 0;
    uint64_t expect_value_sum = 0;
    for (uint64_t i = 0; i < 64; i++) {
        if (i % 3 == 1) {
            continue;
        }
        expect_seen += 1;
        expect_key_sum += i * 13;
        expect_value_sum += i * 7;
    }

    if (seen != expect_seen || key_sum != expect_key_sum || value_sum != expect_value_sum) {
        return 4;
    }

    __mc_map_table_drop(&map);
    return 0;
}
