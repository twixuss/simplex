const main = (): {
    var a = 0
    var b = 1
    while a < 100 {
        println(a)
        let c = a + b
        a = b
        b = c
    }
}

const println = (value: S64): #intrinsic
