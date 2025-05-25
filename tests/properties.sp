// COMPILER OUTPUT 42\n
fn get_abs(x: Int) => if x < 0 then -x else x

fn main() {
    let x = -42
    println(x.abs)
}