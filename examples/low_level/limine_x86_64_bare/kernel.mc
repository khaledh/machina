@layout(fixed)
type LimineMemmapRequest = {
    id: u64[4],
    revision: u64,
    response: view<LimineMemmapResponse>?,
}

@layout(fixed)
type LimineMemmapResponse = {
    revision: u64,
    entry_count: u64,
    entries: view<view<LimineMemmapEntry>[entry_count]>?,
}

@layout(fixed, size: 24)
type LimineMemmapEntry = {
    base: paddr,
    length: u64,
    typ: u64,
}

@runtime
@noreturn
fn __rt_trap(kind: u64, arg0: u64, arg1: u64, arg2: u64);

@noreturn
fn halt_forever() {
    __rt_trap(0, 0, 0, 0);
}

// Limine scans this request section before transferring control to `kmain`.
@section(".limine_requests")
static var memmap_request = LimineMemmapRequest {
    id: [
        0xc7b1dd30df4c8b88,
        0x0a82e883a194f07b,
        0x67cf3d9d378a806f,
        0xe304acdfc50c3c62,
    ],
    revision: 0,
    response: None,
};

fn memmap_total() -> u64 {
    match unsafe { memmap_request.response } {
        some(response) => match response.entries {
            some(entries) => {
                var total = response.entry_count;
                for entry in entries {
                    total += entry.length;
                    total += entry.typ;
                }
                total
            }
            none => 0,
        },
        none => 0,
    }
}

@noreturn
fn kmain() {
    memmap_total();

    halt_forever();
}
