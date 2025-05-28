// COMPILER OUTPUT 42\n81\n
fn get_abs(x: Int) => if x < 0 then -x else x

fn set_sqrt(x: *var Int, s: Int) => *x = s*s

fn main() {
    var x = -42
    println(x.abs)

    x.sqrt = 9
    println(x)
}