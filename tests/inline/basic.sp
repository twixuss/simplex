// COMPILER OUTPUT 111\n222\n42

var bar = fn () => 69

const foo = inline fn (param: S64) => {
    {
        println(111);
        println(222);
    }
    var a: S64 = 40 + param

    var b = if true a+a else bar()

    var c = &b

    while a < 0 {
        continue
        break

        match a {
            0 => 1
            1 => 2
            2 => {fn () => 4}()
        }
    }

    return a
}
const main = fn () => {
    var a = foo(2)
    println(a)
}
