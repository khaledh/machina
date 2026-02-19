#ifndef MC_MACHINE_TEST_HELPERS_H
#define MC_MACHINE_TEST_HELPERS_H

#include "machine/runtime.h"

// Zero-initialize all staged txn output fields.
// Use at the top of dispatch callbacks to establish a clean baseline.
#define MC_TEST_TXN_INIT(txn)                                                  \
    do {                                                                        \
        (txn)->has_next_state = 0;                                              \
        (txn)->next_state = 0;                                                  \
        (txn)->next_state_tag = 0;                                              \
        (txn)->outbox = NULL;                                                   \
        (txn)->outbox_len = 0;                                                  \
        (txn)->subscriptions = NULL;                                            \
        (txn)->subscriptions_len = 0;                                           \
        (txn)->requests = NULL;                                                 \
        (txn)->requests_len = 0;                                                \
        (txn)->replies = NULL;                                                  \
        (txn)->replies_len = 0;                                                 \
    } while (0)

#endif
