// COMPILER ERROR
// COMPILER OUTPUT Multiple `use`d definitions contain member bar
struct Foo {
    bar: Int
}
fn main() {
    var a = Foo(1)
    var b = Foo(2)
    use a;
    use b;

    println(bar)
}