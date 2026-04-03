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

fn kmain() -> u64 {
    match unsafe { memmap_request.response } {
        some(response) => match response.entries {
            some(entries) => {
                let resolved_entries: view<view<LimineMemmapEntry>[]> = entries;
                var total = response.entry_count;
                for entry in resolved_entries {
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
