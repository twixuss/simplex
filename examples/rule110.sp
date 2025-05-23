fn main() {
    var s: U64 = 1
    var n: U64 = 1

    var j = 0
    var i = 0

    print_state(s)

    while j < 64 {
        n = 1
        i = 1
        while i < 64 {
            var p = (s >> (i - 1)) & 7
            n |= ((0b01101110 >> p) & 1) << i
            i += 1
        }
        s = n

        print_state(s)

        j += 1
    }
}

fn print_state(s: U64) {
    var i = 63
    while i >= 0 {
        if s & ((1 as S64) << i)
            print("@")
        else
            print(" ")
        i -= 1
    }
    print("\n")
}