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
fn ceil_to_power_of_2(v: U64) => if v == 0 then 0 else 1 as U64 << (64 - count_leading_zeros(v - 1))


fn count_bits(v: U64) {
	var s: U64 = 0;
	for i in 0..64 {
		s += v & 1;
		v >>= 1;
	}
	return s;
}

fn count_leading_zeros(v: U64): U64 => {
	v |= v >> (1 << 0)
	v |= v >> (1 << 1)
	v |= v >> (1 << 2)
	v |= v >> (1 << 3)
	v |= v >> (1 << 4)
	v |= v >> (1 << 5)
	count_bits(~v)
}