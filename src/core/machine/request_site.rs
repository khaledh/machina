//! Deterministic request-site key encoding.
//!
//! Labeled typestate request sites and `for RequestType:label(...)` handlers
//! must agree on a stable key so runtime dispatch can match correlated replies
//! deterministically.

/// Hashes a source request-site label to a non-zero 64-bit key.
///
/// We reserve the high bit so labeled keys live in a disjoint range from the
/// default unlabeled per-expression keys (`NodeId`-derived).
pub fn labeled_request_site_key(label: &str) -> u64 {
    // FNV-1a 64-bit: tiny, deterministic, and enough for internal keying.
    const OFFSET: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x100000001b3;

    let mut hash = OFFSET;
    for byte in label.as_bytes() {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(PRIME);
    }

    let low63 = hash & 0x7fff_ffff_ffff_ffff;
    0x8000_0000_0000_0000 | low63.max(1)
}
