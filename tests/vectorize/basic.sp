// COMPILER OUTPUT 21

const add5 = fn (x: S64) => x + 5

const main = fn () => {
    var result = add5(.[1, 2, 3])
    println(result[0] + result[1] + result[2])
}
