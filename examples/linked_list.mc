requires {
    std::io::println
    std::io as io
}

// Linked list using heap nodes and enum end marker.
type Link = None | Some(^Node)
type Node = { value: u64, next: Link }

fn sum(list: Link) -> u64 {
    match list {
        Link::None => 0,
        Link::Some(n) => n.value + sum(n.next),
    }
}

fn main() {
    let c = ^Node { value: 3, next: Link::None };
    let b = ^Node { value: 2, next: Link::Some(c) };
    let a = ^Node { value: 1, next: Link::Some(b) };
    let list = Link::Some(a);

    let total = sum(list);
    println(total);
}
