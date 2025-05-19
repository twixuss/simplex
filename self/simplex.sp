import "file"

const Lexer = struct {
    cursor: *U8
}

const main = fn () {
    current_allocator = dyn_page_allocator

    var source_path = "F:\\projects\\simplex\\self\\simplex.sp"
    if !read_entire_file(source_path, &var source: Buffer) {
        print("Could not read ")
        println(source_path)
        return 1
    }


    println(source.span)
    return 0
}