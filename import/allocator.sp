import "windows"

// Allocator Actions
const AA_ALLOCATE   = 0
const AA_REALLOCATE = 1
const AA_DEALLOCATE = 2

const Allocation = struct {
    data: *var None
    size: U64
    alignment: U64
}

// 
// Dynamic Allocator
//
const Allocator = struct {
    func: fn (state: *None, action: Int, old: Allocation, new: Allocation): Allocation
    state: *None
}

const allocate = fn (allocator: Allocator, new: Allocation): Allocation => allocator.func(allocator.state, AA_ALLOCATE, Allocation(), new)
const reallocate = fn (allocator: Allocator, old: Allocation, new: Allocation): Allocation => allocator.func(allocator.state, AA_REALLOCATE, old, new)
const deallocate = fn (allocator: Allocator, old: Allocation): None => allocator.func(allocator.state, AA_DEALLOCATE, old, Allocation())

// 
// Page Allocator
//
const PAGE_SIZE = 4096

const PageAllocator = struct {}
let page_allocator = PageAllocator()

const allocate = fn (allocator: PageAllocator, new: Allocation): Allocation => {
    assert(8 <= new.alignment && new.alignment <= PAGE_SIZE)
    assert(is_power_of_2(new.alignment))
    var result: Allocation
    result.data = VirtualAlloc(none, new.size as SIZE_T, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE)
    result.size = ceil(new.size, PAGE_SIZE)
    result.alignment = PAGE_SIZE
    result
}
const reallocate = fn (allocator: PageAllocator, old: Allocation, new: Allocation): Allocation => {
    assert(8 <= new.alignment && new.alignment <= PAGE_SIZE)
    assert(is_power_of_2(new.alignment))
    var result = old

    let first_page = floor(old.data as U64, PAGE_SIZE)
    let last_page  = floor(old.data as U64 + new.size - 1, PAGE_SIZE)
    if first_page != last_page {
        result = allocate(allocator, new)
        memcpy(result.data, old.data, old.size)
        deallocate(allocator, old)
    }
    result
}

const deallocate = fn (allocator: PageAllocator, old: Allocation): None => {
    VirtualFree(old.data, 0, MEM_RELEASE)
}

let dyn_page_allocator = Allocator(
    func = fn (state: *None, action: Int, old: Allocation, new: Allocation): Allocation => {
        match (action) {
            AA_ALLOCATE => page_allocator.allocate(new)
            AA_REALLOCATE => page_allocator.reallocate(old, new)
            AA_DEALLOCATE => {page_allocator.deallocate(old); Allocation() }
            else => Allocation()
        }
    }
)
