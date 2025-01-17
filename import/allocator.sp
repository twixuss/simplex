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
    func: fn (state: *None, old: Allocation, new: Allocation): Allocation
    state: *None
}

// 
// Page Allocator
//
const PAGE_SIZE = 4096

const PageAllocator = struct {}
let page_allocator = PageAllocator()

const allocate = fn (allocator: PageAllocator, new: Allocation): Allocation => {
    // assert(8 <= new.alignment && new.alignment <= PAGE_SIZE)
    // assert(is_power_of_2(new.alignment))
    var result: Allocation
    result.data = VirtualAlloc(none, new.size as SIZE_T, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE)
    result.size = ceil(new.size, PAGE_SIZE)
    result.alignment = PAGE_SIZE
    result
}
const reallocate = fn (allocator: PageAllocator, old: Allocation, new: Allocation): Allocation => {
    // assert(8 <= new.alignment && new.alignment <= PAGE_SIZE)
    // assert(is_power_of_2(new.alignment))
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
