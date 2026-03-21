#include <stdint.h>

#include "map_table.h"

void __mc_string_from_bytes(mc_string_t *dst, const mc_slice_t *s);
void __mc_string_drop(mc_string_t *s);

static mc_string_t make_string(const char *ptr, uint64_t len) {
    mc_string_t out = {0};
    mc_slice_t bytes = { .ptr = (uint64_t)(uintptr_t)ptr, .len = len };
    __mc_string_from_bytes(&out, &bytes);
    return out;
}

static uint8_t string_eq_bytes(const mc_string_t *s, const char *ptr, uint64_t len) {
    if ((uint64_t)s->len != len) {
        return 0;
    }
    const uint8_t *lhs = (const uint8_t *)(uintptr_t)s->ptr;
    const uint8_t *rhs = (const uint8_t *)(uintptr_t)ptr;
    for (uint64_t i = 0; i < len; i++) {
        if (lhs[i] != rhs[i]) {
            return 0;
        }
    }
    return 1;
}

int main(void) {
    mc_dyn_array_t map = {0};

    mc_string_t alice = make_string("alice", 5);
    mc_string_t bob = make_string("bob", 3);
    mc_string_t cara = make_string("cara", 4);

    uint64_t alice_value = 11;
    uint64_t bob_value = 22;
    uint64_t cara_value = 33;

    if (!__rt_map_insert_or_assign_string_key(
            (uint64_t)&map,
            (uint64_t)&alice,
            (uint64_t)&alice_value,
            sizeof(uint64_t)
        )) {
        return 1;
    }
    if (!__rt_map_insert_or_assign_string_key(
            (uint64_t)&map,
            (uint64_t)&bob,
            (uint64_t)&bob_value,
            sizeof(uint64_t)
        )) {
        return 2;
    }
    if (!__rt_map_insert_or_assign_string_key(
            (uint64_t)&map,
            (uint64_t)&cara,
            (uint64_t)&cara_value,
            sizeof(uint64_t)
        )) {
        return 3;
    }

    __mc_string_drop(&alice);
    __mc_string_drop(&bob);
    __mc_string_drop(&cara);

    uint64_t seen = 0;
    uint64_t sum = 0;
    uint8_t saw_alice = 0;
    uint8_t saw_bob = 0;
    uint8_t saw_cara = 0;

    uint32_t cursor = __rt_map_iter_init((uint64_t)&map);
    while (!__rt_map_iter_is_done((uint64_t)&map, cursor)) {
        mc_string_t key = {0};
        uint64_t value = 0;
        __rt_map_iter_load_string_key(
            (uint64_t)&map,
            cursor,
            sizeof(uint64_t),
            (uint64_t)&key,
            (uint64_t)&value
        );

        seen += 1;
        sum += value;
        if (string_eq_bytes(&key, "alice", 5)) {
            saw_alice = value == 11;
        } else if (string_eq_bytes(&key, "bob", 3)) {
            saw_bob = value == 22;
        } else if (string_eq_bytes(&key, "cara", 4)) {
            saw_cara = value == 33;
        } else {
            __mc_string_drop(&key);
            return 4;
        }

        __mc_string_drop(&key);
        cursor = __rt_map_iter_advance((uint64_t)&map, cursor);
    }

    if (seen != 3 || sum != 66 || !saw_alice || !saw_bob || !saw_cara) {
        return 5;
    }

    __rt_map_drop_string_keys((uint64_t)&map, sizeof(uint64_t));
    return 0;
}
