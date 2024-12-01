// COMPILER OUTPUT 0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89

const main = () => {
    var a = 0
    var b = 1
    while a < 100 {
        println(a)
        let c = a + b
        a = b
        b = c
    }
}
