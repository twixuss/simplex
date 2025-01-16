// COMPILER ERROR
// COMPILER OUTPUT Error: Return value of type UnsizedInteger can't be converted to Bool
// COMPILER OUTPUT Info: Return type Bool was deduced from this expression:
// COMPILER OUTPUT Error: Expression of type `UnsizedInteger` is not implicitly convertible to `Bool`.
const main = fn () => {
    if true {
        return 42
    }
    false
}