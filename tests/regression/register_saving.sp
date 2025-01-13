// COMPILER OUTPUT 4608
const add = (a: U64, b: U64): U64 => {
    return a + b
}
const main = (): U64 => {
    var result: U64
    // address of result is computed and stored in a register before calling `add`.
    // makes sure register saving and restoring works.
    result = add(512, 4096)
    result
}