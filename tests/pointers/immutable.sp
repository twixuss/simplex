// COMPILER ERROR
// COMPILER OUTPUT This expression can not be modified
// COMPILER OUTPUT Because p is a pointer to read-only
// COMPILER OUTPUT Because a is marked as let. Mark it with `var` instead to make it mutable

const main = fn () => {
    let a = 42
    let b = 1337
    let p = &a
    *p = b
    println(a)
}
