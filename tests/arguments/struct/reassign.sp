// COMPILER ERROR
// COMPILER OUTPUT Member "b" was already assigned
const Foo = struct #must_be_fully_initialized {
    a: Int
    b: Int
}

const main = fn (): None => {
    var foo = Foo(b = 10, b = 50)
    println(foo.a - foo.b)
}