// COMPILER OUTPUT 43\n44\n15\n20\n

fn foo(x: S64, c: Bool) {
    if c
        x + 42
    else
        x * 5
}

fn main() {
    let arr = foo(.[1, 2, 3, 4], .[true, true, false, false])
    println(arr[0])
    println(arr[1])
    println(arr[2])
    println(arr[3])
}