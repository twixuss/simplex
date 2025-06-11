// COMPILER ERROR
// COMPILER OUTPUT Redefinition of a cast operator

fn as_implicit(a: String): S64 => 0
fn as_implicit(a: String): S64 => 0

fn main() {
    let a: S64 = "hi"
}