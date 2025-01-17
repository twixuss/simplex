// COMPILER OUTPUT 40
const Foo = struct {
    a: Int
    b: Int
}

const main = fn (): None => {
    var foo = Foo(b = 10, 50)
    println(foo.a - foo.b)
}