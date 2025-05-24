fn main() {
    var s: U64 = 1

    print_state(s)

    for j in 0..64 {
        var n: U64 = 1
        for i in 1..64 
            n |= ((0b01101110 >> ((s >> (i - 1)) & 7)) & 1) << i
        s = n
        print_state(s)
    }
}

fn print_state(s: U64) {
    for i in reverse 0..64 {
        //print(.[" ", "@"][(s >> i) & 1])
        print(if s & ((1 as S64) << i) then "@" else " ")
    }
    print("\n")
}