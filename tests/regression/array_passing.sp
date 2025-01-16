// COMPILER OUTPUT 501
// COMPILER OUTPUT 502
// COMPILER OUTPUT 503

const print_array = fn (x: [3]S64) => {
    println(x[0])
    println(x[1])
    println(x[2])
}

const main = fn () => {
    print_array(.[501, 502, 503])
}
