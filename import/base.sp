const Int = S64
const UInt = U64

const debug_break = fn () => #intrinsic
const panic = fn () => #intrinsic
const println = fn (value: S64) => #intrinsic
const println = fn (value: String) => #intrinsic

const floor = fn (a: U64, b: U64) => {
    return a / b * b
}
const ceil = fn (a: U64, b: U64) => {
    return (a + b - 1) / b * b
}

const memcpy = fn (dst: *var None  src: *let None size: U64): *var None => {
    var d = dst as *var U8
    var s = src as *let U8
    var e = ((d as U64) + size) as *var U8
    while d as U64 < e as U64 {
        *d = *s
        d = ((d as U64) + 1) as *var U8
        s = ((d as U64) + 1) as *let U8
    }
    return dst
}

const assert = fn (x: Bool) => { if !x then panic() }
