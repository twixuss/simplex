// COMPILER OUTPUT hello\na\nb\nc\nworld

const foo = fn () => {
    defer println("c")
    {
        defer println("b")
        println("a")
    }
    return
}

const main = fn (): None => {
    defer println("world")
    println("hello")
    foo()
}