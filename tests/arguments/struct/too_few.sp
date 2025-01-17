// COMPILER ERROR
// COMPILER OUTPUT Too few arguments. Value for a was not provided
const Foo = struct {
    a: Int
    b: Int
}

const main = fn (): None => {
    var foo = Foo(b = 10)
    println(foo.a - foo.b)
}