// COMPILER OUTPUT 333
const Vec2 = struct {
    x: S32
    y: S32
}

const main = () => {
    var v = Vec2(111, 222)
    return v.x + v.y
}