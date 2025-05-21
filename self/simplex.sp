import "file"

const Lexer = struct {
    cursor: *U8
}

const TokenKind = enum {
    eof
    eol = '\n'
    name = 'a'
    string = '"'
    number = '0'
    directive = '#'
}

const main = fn () {
    current_allocator = dyn_page_allocator

    //var source_path = "F:\\projects\\simplex\\self\\simplex.sp"
    var source_path = "F:\\projects\\simplex\\a.sp"
    if !read_entire_file(source_path, &var source: Buffer) {
        print("Could not read ")
        println(source_path)
        return 1
    }


    println(source.span)
    return 0
}