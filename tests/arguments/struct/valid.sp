// COMPILER OUTPUT 40\n-10
const Foo = struct #must_be_fully_initialized {
    a: Int
    b: Int
}

const Bar = struct {
    a: Int
    b: Int
}

const main = fn (): None => {
    var foo = Foo(b = 10, 50)
    var bar = Bar(b = 10)
    println(foo.a - foo.b)
    println(bar.a - bar.b)
}