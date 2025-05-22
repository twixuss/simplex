// COMPILER OUTPUT caller_location.sp:7:5\n2 + 2
fn foo(arg: Int, loc := #caller_location, str := #argument_string arg) {
    println(loc)
    println(str)
}
fn main() {
    foo(2 + 2)
}