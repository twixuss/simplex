// COMPILER ERROR
// COMPILER OUTPUT Parameter "b" was already assigned
const foo = fn (a: Int, b: Int) => {
    a - b
}

const main = fn (): None => {
    var c = foo(b = 10, b = 50)
    println(c)
}