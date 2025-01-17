// COMPILER ERROR
// COMPILER OUTPUT Too few arguments. Value for a was not provided
const foo = fn (a: Int, b: Int) => {
    a - b
}

const main = fn (): None => {
    var c = foo(b = 10)
    println(c)
}