// COMPILER OUTPUT 555
const Vec2 = struct {
    x: S32
    y: S32
}

const foo = fn (v: *var Vec2) => {
    v.x = 333
}

const main = fn () => {
    var v = Vec2(111, 222)
    v.foo()
    return v.x + v.y
}