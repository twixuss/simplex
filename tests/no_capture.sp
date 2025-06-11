// COMPILER ERROR
fn main() {
    let bar = 42
    fn foo() {
        return bar
    }

    println(foo())
}