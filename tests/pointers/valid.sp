// COMPILER OUTPUT 1337\n8\n8\n1\n1\n

const main = fn () => {
    var a = 42
    let b = 1337
    let p = &a
    *p = b
    println(a)

    let q = (p as S64 + 8) as *S64
    let r = p + 1
    println(q as S64 - p as S64)
    println(r as S64 - p as S64)
    println(q - p)
    println(r - p)
}
