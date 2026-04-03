// Minimal bare-metal style entrypoint for the x86-64 bare target.
//
// Build this with a target config that uses:
// - arch = "x86-64"
// - platform = "none"
// - linker-script = "x86_64.ld" (or equivalent path)
//
// The linker script below sets ENTRY(kmain), so this file deliberately uses a
// non-hosted entry name instead of `main`.

fn kmain() -> u64 {
    42
}
