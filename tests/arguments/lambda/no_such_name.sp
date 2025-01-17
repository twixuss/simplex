// COMPILER ERROR
// COMPILER OUTPUT Lambda "foo" does not have parameter named blabla
const foo = fn (a: Int, b: Int) => {
    a - b
}

const main = fn (): None => {
    var c = foo(blabla = 10, 50)
    println(c)
}