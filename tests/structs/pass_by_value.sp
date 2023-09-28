// COMPILER OUTPUT 333
const Vec2 = struct {
    x: S32
    y: S32
}

const foo = (var v: Vec2) => {
    v.x = 333
}

const main = () => {
    var v = Vec2(111, 222)
    v.foo()
    return v.x + v.y
}