fn main() {
    fn foo() {}
    fn foo(a: Int) {}
    fn foo(a: Int,) {} // trailing separator
    fn foo(
        a: Int // new line instead of separator
        b: Int,
    ) {}

    0
}