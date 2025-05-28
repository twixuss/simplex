import "allocator.sp"

struct List[T: Type] {
    data: *var T
    count: Int
    capacity: Int
    allocator: Allocator
}

//const Appendable = concept[T] #compiles {
concept[T] Appendable = #compiles {
    builder: StringBuilder
    t: T
    builder.append(t)
}

fn reallocate[T: Type](var list: *List[T], new_capacity: Int) => {
    let new = list.allocator.reallocate(
        old = .(size =     capacity * sizeof(T), alignment = alignof(T), data = list.data),
        new = .(size = new_capacity * sizeof(T), alignment = alignof(T)),
    )
    list.data = new.data
    list.capacity = new.size / sizeof(T)
}
fn reserve[T: Type](var list: *List[T], desired_capacity: Int) => {
    if list.capacity >= desired_capacity
        return false
    let new_capacity = desired_capacity.ceil_to_power_of_2().max(16)
    list.reallocate(new_capacity)
    return true
}
fn add[T: Type](var list: *List[T], value: T) => {
    list.reserve(list.count + 1)
    list.data[list.count] = value
    list.count += 1
    return &list.data[list.count - 1]
}


self[T: Type]: List[T] {
    fn reallocate(new_capacity: Int) {
        let new = allocator.reallocate(
            old = .(size =     capacity * sizeof(T), alignment = alignof(T), data = data),
            new = .(size = new_capacity * sizeof(T), alignment = alignof(T)),
        )
        data = new.data
        capacity = new.size / sizeof(T)
    }
    fn reserve(desired_capacity: Int) {
        if capacity >= desired_capacity
            return false
        let new_capacity = desired_capacity.ceil_to_power_of_2().max(16)
        reallocate(new_capacity)
        return true
    }
    fn add(value: T) {
        reserve(count + 1)
        data[count] = value
        count += 1
        return &data[count - 1]
    }
}
