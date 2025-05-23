fn main() {
    var s: U64 = 1

    print_state(s)

    for j in 0..64 {
        var n: U64 = 1
        for i in 1..64 {
            var p = (s >> (i - 1)) & 7
            n |= ((0b01101110 >> p) & 1) << i
        }
        s = n
        print_state(s)
    }
}

fn print_state(s: U64) {
    for i in reverse 0..64 {
        if s & ((1 as S64) << i)
            print("@")
        else
            print(" ")
    }
    print("\n")
}