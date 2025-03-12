import "file"

const Lexer = struct {
    cursor: *U8
}

const main = fn () {
    var source_path = "F:\\projects\\simplex\\self\\simplex.sp"
    var source = read_entire_file(source_path)
    println(source)
}