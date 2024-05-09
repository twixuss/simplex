// COMPILER ERROR
// COMPILER OUTPUT Value of type UnsizedInteger can't be converted to Bool
// COMPILER OUTPUT Block's type Bool was deduced from this expression
const foo = () => {:a
    break :a 123
    break :a false
}

const main = () => {
    foo()
    return 42
}
