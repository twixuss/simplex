// COMPILER OUTPUT 1\n2\n1\n1\n0\n
fn main() {
    let a = 1
    let b = 2
    
    let x = &a
    let y = &b
    let z: *Int = 0

    println(*(x || y))
    println(*(y || x))
    println(*(x || z))
    println(*(z || x))
    println((z || z) as S64)
}