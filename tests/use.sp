// COMPILER OUTPUT 42\n
struct Foo {
    bar: Int
}

fn test(use foo: *var Foo) {
    bar = 42
}
fn main() {
    var foo = Foo(1)
    test(&foo)
    println(foo.bar)
}

