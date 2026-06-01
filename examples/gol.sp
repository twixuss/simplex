#extern "kernel32"

struct COORD { x: S16; y: S16 }

const STD_OUTPUT_HANDLE = -11

fn Sleep(n: U32) => #extern
fn GetStdHandle(nStdHandle: U32): *None => #extern
fn SetConsoleCursorPosition(hConsoleOutput: *None, dwCursorPosition: COORD): Bool => #extern

const N = 48

fn main() {
    var b1: [N][N]U8
    var b2: [N][N]U8

    var curr = &b1
    var next = &b2

    var r: U64 = 0
    for i in 0..N {
        for j in 0..N {
            const k = 11400714819323198485
            const l = 5871781006564002452
            (*curr)[i][j] = (((i*k ^ j*l)*k) as U64 >> 63) as U8
        }
    }

    while true {
        SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), COORD());

        for a in 0..N {
            for b in 0..N {
                print(if (*curr)[a][b] then "██" else "  ")
            }
            print("\n")
        }

        for i in 0..N {
            for j in 0..N {
                let n =
                    (*curr)[(i-1)%N][(j-1)%N] +
                    (*curr)[(i-1)%N][(j+0)%N] +
                    (*curr)[(i-1)%N][(j+1)%N] +
                    (*curr)[(i+0)%N][(j-1)%N] +
                    (*curr)[(i+0)%N][(j+1)%N] +
                    (*curr)[(i+1)%N][(j-1)%N] +
                    (*curr)[(i+1)%N][(j+0)%N] +
                    (*curr)[(i+1)%N][(j+1)%N]

                if false {
                    if (*curr)[i][j] {
                        (*next)[i][j] = if 2 <= n && n <= 3 then 1 else 0
                    } else {
                        (*next)[i][j] = if n == 3 then 1 else 0
                    }
                } else {
                    let t = -((*curr)[i][j] as S16) as U16 & 3327 | 8
                    
                    (*next)[i][j] = (t >> (n + (*curr)[i][j] * 8)) & 1
                }
            }
        }

        //Sleep(10);

        var tmp = curr
        curr = next
        next = tmp
    }
}