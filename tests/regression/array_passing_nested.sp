// COMPILER OUTPUT 6\n7\n8\n9\n
fn foo(var a: [4]S64, var b: [4]S64) {
    a[0] += b[0]
    a[1] += b[1]
    a[2] += b[2]
    a[3] += b[3]
    a
}

fn main() {
    let arr = foo(.[1, 2, 3, 4], foo(.[1, 2, 3, 4], .[4, 3, 2, 1]));
    println(arr[0])
    println(arr[1])
    println(arr[2])
    println(arr[3])
}
