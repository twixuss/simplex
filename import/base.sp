const Int = S64
const UInt = U64

fn debug_break() => #intrinsic
fn panic() => #intrinsic
fn print(value: S64) => #intrinsic
fn print(value: String) => #intrinsic
fn println(value: S64) => { print(value); print("\n"); }
fn println(value: String) => { print(value); print("\n"); }
fn assert(x: Bool) => #intrinsic // intrinsic because theres no expression string and caller location yet

fn memcpy(dst: *var None  src: *let None size: U64): *var None => {
    var d = dst as *var U8
    var s = src as *let U8
    var e = ((d as U64) + size) as *var U8
    while d as U64 < e as U64 {
        *d = *s
        d = ((d as U64) + 1) as *var U8
        s = ((s as U64) + 1) as *let U8
    }
    return dst
}

fn floor(a: U64, b: U64) => {
    return a / b * b
}
fn ceil(a: U64, b: U64) => {
    return (a + b - 1) / b * b
}

fn is_power_of_2(a: U64) => a & (a - 1) == 0