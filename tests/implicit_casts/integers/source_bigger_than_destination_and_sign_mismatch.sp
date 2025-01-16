// COMPILER ERROR
// COMPILER OUTPUT because source is bigger than destination, meaning that there could be information loss, and the signs don't match
const main = fn () => {
    var s64: S64 = 12
    var a: U8 = s64
}