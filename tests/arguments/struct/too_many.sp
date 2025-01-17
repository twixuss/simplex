// COMPILER ERROR
// COMPILER OUTPUT Too many arguments. Expected 2, but got 3
const Foo = struct {
    a: Int
    b: Int
}

const main = fn (): None => {
    var foo = Foo(1, 2, 3)
    println(foo.a - foo.b)
}