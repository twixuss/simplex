// COMPILER ERROR
// COMPILER OUTPUT Too many arguments. Expected 2, but got 3
const foo = fn (a: Int, b: Int) => {
    a - b
}

const main = fn (): None => {
    var c = foo(1, 2, 3)
    println(c)
}