// COMPILER ERROR
// COMPILER OUTPUT Expression of type `None` is not implicitly convertible to `S64`
// COMPILER OUTPUT no default case
const main = (): S64 => {
    var a = 2;
    return match a {
        1 => 111
        2 => 222
        3 => 333
    }
}