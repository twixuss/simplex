// COMPILER ERROR
// COMPILER OUTPUT Value of type UnsizedInteger can't be converted to Bool
// COMPILER OUTPUT Block's type Bool was deduced from this expression
const foo = fn () => {:a
    break :a 123
    break :a false
}

const main = fn () => {
    foo()
    return 42
}
