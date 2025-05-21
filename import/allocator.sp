import "windows"

// Allocator Actions
enum AllocatorAction {
    allocate
    reallocate
    free
}

struct Allocation {
    data: *var None
    size: U64
    alignment: U64
    is_zeroed: Bool
}

// 
// Dynamic Allocator
//
struct Allocator {
    func: fn (state: *None, action: AllocatorAction, old: Allocation, new: Allocation): Allocation
    state: *None
}

var current_allocator: Allocator

fn allocate(allocator: Allocator, new: Allocation): Allocation => allocator.func(allocator.state, AllocatorAction.allocate, Allocation(), new)
fn reallocate(allocator: Allocator, old: Allocation, new: Allocation): Allocation => allocator.func(allocator.state, AllocatorAction.reallocate, old, new)
fn free(allocator: Allocator, old: Allocation): None => allocator.func(allocator.state, AllocatorAction.free, old, Allocation())

// 
// Page Allocator
//
const PAGE_SIZE = 4096

struct PageAllocator {}
let page_allocator = PageAllocator()

fn allocate(allocator: PageAllocator, new: Allocation): Allocation => {
    assert(8 <= new.alignment && new.alignment <= PAGE_SIZE)
    assert(is_power_of_2(new.alignment))
    var result: Allocation
    result.data = VirtualAlloc(none, new.size as SIZE_T, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE)
    result.size = ceil(new.size, PAGE_SIZE)
    result.alignment = PAGE_SIZE
    result.is_zeroed = true
    result
}
fn reallocate(allocator: PageAllocator, old: Allocation, new: Allocation): Allocation => {
    if old.data == none
        return allocator.allocate(new)

    assert(8 <= new.alignment && new.alignment <= PAGE_SIZE)
    assert(is_power_of_2(new.alignment))
    var result = old

    let first_page = floor(old.data as U64, PAGE_SIZE)
    let last_page  = floor(old.data as U64 + new.size - 1, PAGE_SIZE)
    if first_page != last_page {
        result = allocate(allocator, new)
        memcpy(result.data, old.data, old.size)
        free(allocator, old)
    }
    result
}

fn free(allocator: PageAllocator, old: Allocation): None => {
    VirtualFree(old.data, 0, MEM_RELEASE)
}

// TODO: add implicit conversion from PageAllocator to Allocator
let dyn_page_allocator = Allocator(
    func = fn (state: *None, action: AllocatorAction, old: Allocation, new: Allocation): Allocation => {
        match (action) {
            AllocatorAction.allocate => page_allocator.allocate(new)
            AllocatorAction.reallocate => page_allocator.reallocate(old, new)
            AllocatorAction.free => {page_allocator.free(old); Allocation() }
            else => Allocation()
        }
    }
)
