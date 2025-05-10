// COMPILER OUTPUT 4608
const add = fn (a: S64, b: S64): S64 => {
    return a + b
}
const main = fn () {
    var result: S64
    // address of result is computed and stored in a register before calling `add`.
    // makes sure register saving and restoring works.
    result = add(512, 4096)
    println(result)
}