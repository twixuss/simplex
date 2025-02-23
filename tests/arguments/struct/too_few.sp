// COMPILER ERROR
// COMPILER OUTPUT Member `a` must be initialized
const Foo = struct #must_be_fully_initialized {
    a: Int
    b: Int
}

const main = fn (): None => {
    var foo = Foo(b = 10)
    println(foo.a - foo.b)
}