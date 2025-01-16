// COMPILER ERROR
// COMPILER OUTPUT Error: Return value of type Bool can't be converted to S64
// COMPILER OUTPUT Error: Expression of type `Bool` is not implicitly convertible to `S64`.
const main = fn () => {
    let a = 32
    let b = false
    return a
    return b
}