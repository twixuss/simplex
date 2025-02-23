// COMPILER ERROR
// COMPILER OUTPUT Struct "Foo" does not have member named blabla
const Foo = struct #must_be_fully_initialized {
    a: Int
    b: Int
}

const main = fn (): None => {
    var foo = Foo(blabla = 10, 50)
    println(foo.a - foo.b)
}