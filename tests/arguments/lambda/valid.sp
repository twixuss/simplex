// COMPILER OUTPUT 40\ndefault_args(1, 2, 3)\ndefault_args(1, 2, 333)\ndefault_args(2, 1, 333)\ndefault_args(2, 1, 3)\n
const foo = fn (a: Int, b: Int) => {
    a - b
}

fn default_args(a: Int, b: Int, c: Int = 333) {
    print("default_args(")
    print(a) print(", ")
    print(b) print(", ")
    print(c) print(")\n")
}

const main = fn (): None => {
    var c = foo(b = 10, 50)
    println(c)

    default_args(1, 2, 3)
    default_args(1, 2)
    default_args(b = 1, 2)
    default_args(b = 1, 2, 3)
}