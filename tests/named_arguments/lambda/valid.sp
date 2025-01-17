// COMPILER OUTPUT 40
const foo = fn (a: Int, b: Int) => {
    a - b
}

const main = fn (): None => {
    var c = foo(b = 10, 50)
    println(c)
}