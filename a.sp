const main = (): {
    var a = 0
    var b = 1
    while a < 100 {
        print(a)
        let c = a + b
        a = b
        b = c
    }
}

const print = (value: S64): #intrinsic
