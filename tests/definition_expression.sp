// COMPILER OUTPUT 42
fn foo(x: *var Int) {
    *x = 42
}
fn main() {
    foo(&var x: Int)
    println(x)
}