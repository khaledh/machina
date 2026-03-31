requires {
    std::io::println
}

@layout(fixed, size: 24)
type LimineMemmapEntry = {
    base: paddr,
    length: u64,
    typ: u64,
}

@layout(fixed)
type LimineMemmapResponse = {
    revision: u64,
    entry_count: u64,
    entries_ptr: vaddr?,
}

@layout(fixed)
type LimineMemmapRequest = {
    id: u64[4],
    revision: u64,
    response_ptr: vaddr?,
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
    response_ptr: None,
};

fn dump_memmap() {
    if memmap_request.response_ptr.is_none() {
        println("no response");
        return;
    }

    let response_addr = memmap_request.response_ptr.unwrap();
    let response: view<LimineMemmapResponse> = view_at(response_addr);

    if response.entries_ptr.is_none() {
        println("no entries");
        return;
    }

    let entries_addr = response.entries_ptr.unwrap();
    let entries: view_slice<LimineMemmapEntry> =
        view_slice_at(entries_addr, response.entry_count);

    println(response.entry_count);
    for entry in entries {
        println(entry.length);
    }
}

fn main() {
    dump_memmap();
}
