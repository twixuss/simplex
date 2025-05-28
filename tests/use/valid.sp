// COMPILER OUTPUT 42\n82\n
struct Foo {
    bar: Int
}

fn test(use foo: *var Foo) {
    bar = 42
}
fn test2(foo: *var Foo) {
    use foo;
    bar = 82
}
fn main() {
    var foo = Foo(1)
    test(&foo)
    println(foo.bar)
    foo = Foo(2)
    test2(&foo)
    println(foo.bar)
}

