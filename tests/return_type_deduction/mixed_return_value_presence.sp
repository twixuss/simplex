// COMPILER ERROR
// COMPILER OUTPUT Error: Right now you are not allowed to mix return statements with values and without
const main = fn () => {
    if true {
        return
    } else {
        return 1
    }
}