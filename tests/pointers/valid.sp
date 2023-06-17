// COMPILER OUTPUT 1337

const main = () => {
    var a = 42
    let b = 1337
    let p = &a
    *p = b
    println(a)
}

const println = (value: S64): None => #intrinsic
