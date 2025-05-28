// COMPILER ERROR
// COMPILER OUTPUT `bar` was not declared
struct Foo {
    bar: Int
}

fn test(foo: *var Foo) {
    bar = 82
    use foo;
}
fn main() {
    var foo = Foo(1)
    test(&foo)
    println(foo.bar)
}

