// COMPILER OUTPUT 43\n44\n15\n20\n

// Use this until it's built-in / in base.sp
fn all_true(c: [4]Bool): Bool => c[0] && c[1] && c[2] && c[3]
fn all_false(c: [4]Bool): Bool => !c[0] && !c[1] && !c[2] && !c[3]
fn select(c: [4]Bool, a: [4]S64, b: [4]S64): [4]S64 =>
    .[
        if c[0] then a[0] else b[0],
        if c[1] then a[1] else b[1],
        if c[2] then a[2] else b[2],
        if c[3] then a[3] else b[3],
    ]

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