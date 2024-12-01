// COMPILER OUTPUT 123
// NO COMPILER OUTPUT 456
// NO COMPILER OUTPUT 789
const main = () => {
    var a = {:a
        if true {
            break :a 123
        }
        println(456)
        789
    }
    println(a)
}