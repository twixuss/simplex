// COMPILER OUTPUT 111
// COMPILER OUTPUT 222
// COMPILER OUTPUT 42

var bar = () => 69

const foo = inline (param: S64) => {
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
            2 => {() => 4}()
        }
    }

    return a
}
const main = () => {
    var a = foo(2)
    println(a)
}

const println = (value: S64) => #intrinsic