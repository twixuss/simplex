// COMPILER ERROR
// COMPILER OUTPUT because source is bigger than destination, meaning that there could be information loss
const main = fn () => {
    var s64: S64 = 12
    var a: S8 = s64
}