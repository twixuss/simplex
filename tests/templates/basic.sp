// COMPILER OUTPUT 1\nhello
fn foo[T: Type, U: Type](t: T, u: U) {
    println(t)
    println(u)
}
fn main(): None => {
    foo(1, "hello")
}
