import "allocator"

struct StringBuilder {
    data: *var U8
    count: UInt
    capacity: UInt
    allocator: Allocator = current_allocator
}

fn append(use builder: *var StringBuilder, string: String) {
    let required_count = count + string.count
    if required_count > capacity {
        let new_capacity = ceil_to_power_of_2(required_count)
        let reallocated = allocator.reallocate(
            old = Allocation(data = data, size = count),
            new = Allocation(size = new_capacity, alignment = 8)
        )

        data = @reallocated.data
        capacity = reallocated.size
    }

    memcpy(data + count, string.data, string.count)

    count += string.count
}

fn as_implicit(use builder: StringBuilder): String => String(data, count)
