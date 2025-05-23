const Int = S64
const UInt = U64

fn debug_break() => #intrinsic
fn panic() => #intrinsic
fn print(value: S64) => #intrinsic #linkname "print_S64"
fn print(value: String) => #intrinsic #linkname "print_String"
fn println(value: S64) => { print(value); print("\n"); }
fn println(value: String) => { print(value); print("\n"); }
fn assert(x: Bool, location := #caller_location, expression := #argument_string x) {
    if !x {
        println("Assertion failed:")
        println(location)
        println(expression)
        panic()   
    }
}

fn memcpy(dst: *var None  src: *let None size: U64): *var None => {
    var d = dst as *var U8
    var s = src as *let U8
    var e = d + size
    while d < e {
        *d = *s
        d = d + 1
        s = s + 1
    }
    return dst
}

fn floor(a: U64, b: U64) => a / b * b
fn ceil(a: U64, b: U64) => (a + b - 1) / b * b
fn is_power_of_2(a: U64) => a & (a - 1) == 0