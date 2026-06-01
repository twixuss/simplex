// NO COMPILER OUTPUT FAIL AT
fn ass(b: Bool, s: String) {
    if !b {
        print("FAIL AT ")
        println(s)
    }
}
fn main() {
    {
        fn f(a: S8, b: S8, c: S8, s := #caller_location) => ass(a % b == c, s)
        f(-5, 3, 1)
        f(-4, 3, 2)
        f(-3, 3, 0)
        f(-2, 3, 1)
        f(-1, 3, 2)
        f( 0, 3, 0)
        f( 1, 3, 1)
        f( 2, 3, 2)
        f( 3, 3, 0)
        f( 4, 3, 1)
        f( 5, 3, 2)
    }
}