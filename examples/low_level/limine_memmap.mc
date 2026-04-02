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

// Use a Mach-O-compatible section spelling here: Mach-O section names are
// capped at 16 bytes, so this local smoke-test uses a shortened Limine section.
@section("__DATA,__limine_req")
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

fn main() {
    let response = memmap_request.response or {
        println("no response");
        return;
    };

    let entries = response.entries or {
        println("no entries");
        return;
    };

    println(response.entry_count);
    for entry in entries {
        println(entry.length);
    }
}
