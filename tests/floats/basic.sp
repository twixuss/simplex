// COMPILER OUTPUT 124\n122\n28\n20\n63\n
fn main() {
    var a = 123.456
    var b = 0.6
    a += b
    println(a as S64)

    a = 123.456
    b = 0.5
    a -= b
    println(a as S64)

    a = 5.9
    b = 4.9
    a *= b
    println(a as S64)

    a = 100.0
    b = 4.9
    a /= b
    println(a as S64)

    a = -37.0
    b = 100.0
    a %= b
    println(a as S64)
}