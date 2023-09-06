// COMPILER OUTPUT 111
// COMPILER OUTPUT 222
// COMPILER OUTPUT 42
const foo = inline () => {
    {
        println(111);
        println(222);
    }
    var a: S64 = 42

    var b = if true a+a else 333

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
    var a = foo()
    println(a)
}

const println = (value: S64) => #intrinsic