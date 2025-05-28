// COMPILER OUTPUT 1\n2\n3\n32\n5\n32\n7\n8\n9\n
struct Vector2 {
    x: Int
    y: Int
}

struct Vector3 {
    x: Int
    y: Int
    z: Int
}

fn get_sum(v: Vector2) => v.x + v.y
fn set_sum(v: *var Vector2, l: Int) {
    v.x = l / 2
    v.y = l / 2
}

fn get_xz(v: Vector3) => Vector2(v.x, v.z)
fn set_xz(v: *var Vector3, xz: Vector2) {
    v.x = xz.x
    v.z = xz.y
}

fn set_sqrt(x: *var Int, s: Int) => *x = s*s

fn get_dup(x: Vector2) => .[x, x]
fn set_dup(x: *var Vector2, d: [2]Vector2) => *x = d[1]

fn main() {
    var vs = .[
        Vector3(1, 2, 3),
        Vector3(4, 5, 6),
        Vector3(7, 8, 9),
    ]

    // Square of 8 is 64
    // Assigning 64 to sum of Vector2 splits it in two.
    // dup is dummy property to verify subscripts in the middle work.
    // So x and z should be 32, y should be unchanged

    vs[1].xz.dup[1].sum.sqrt = 8

    println(vs[0].x)
    println(vs[0].y)
    println(vs[0].z)
    println(vs[1].x)
    println(vs[1].y)
    println(vs[1].z)
    println(vs[2].x)
    println(vs[2].y)
    println(vs[2].z)
}

