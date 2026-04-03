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

// Minimal bare entry: unwrap the typed response field directly in bare mode.
fn kmain() -> u64 {
    let response = unsafe { memmap_request.response } or {
        return 0;
    };

    response.entry_count
}
