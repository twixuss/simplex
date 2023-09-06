// COMPILER OUTPUT 0\n1\n2\n3\n5\n7\n11\n13\n17\n19\n23\n29\n31\n37\n41\n43\n47\n53\n59\n61\n67\n71\n73\n79\n83\n89\n97
const foo = () => 32;
const is_prime = (x: S64) => {
    var a = 2
    while a < x {
        if x % a == 0
            return false
        a = a + 1
    }
    return true
}
const main = () => {
    var b = 0
    while b < 100 {
        if is_prime(b) println(b)
        b = b + 1
    }
}

const println = (value: S64) => #intrinsic
