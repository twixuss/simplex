import "file"

fn end(s: String) => s.data + s.count

enum TokenKind #allow_to_int #allow_from_int {
    eof
    eol = '\n'
    name = 'a'
    string = '"'
    character = '\''
    number = '0'
    directive = '#'
}

struct Lexer {
    source: String
    c: *var U8 // Cursor
    location: String // View to part of source code
    kind: TokenKind
}

fn create_Lexer(source: String) {
    assert(*(source.data - 1) == 0)
    assert(*(source.data + source.count) == 0)

    return Lexer(source = source, c = source.data)
}

fn next(l: *var Lexer): Bool => {
    //let end = l.source.end()
    let end = l.source.data + l.source.count

    while true {
        match *l.c {
            ' ' or '\t' or '\n' or '\r' or '\v' or '\f' => l.c += 1
            else => { break } // TODO: don't want braces
        }
    }

    l.location.data = l.c;

    match *l.c {
        '\0' => {
            l.kind = TokenKind.eof
        }
        '`' or '~' or '!' or '@' or
        '#' or '$' or '%' or '^' or
        '&' or '*' or '(' or ')' or
        '-' or '=' or '+' or '\\' or
        '|' or '[' or ']' or '{' or
        '}' or ';' or ':' or ',' or
        '<' or '.' or '>' or '/' or
        '?' => {
            l.kind = *l.c
            l.c += 1
        }
        '\'' => {
			l.kind = TokenKind.character
			l.c += 1
			while true {
				if *l.c == '\'' && l.c[-1] != '\\'
					break
				l.c += 1
				if l.c > end {
                    println("Unclosed character literal")
                    l.kind = TokenKind.eof
                    break
                }
			}

			l.c += 1
        }
        '"' => {
			l.kind = TokenKind.string
			l.c += 1
			while true {
				if *l.c == '"' && l.c[-1] != '\\'
					break
				l.c += 1
				if l.c > end {
                    println("Unclosed string literal")
                    l.kind = TokenKind.eof
                    break
                }
			}

			l.c += 1
        }
        else => {
            l.kind = TokenKind.name
            l.c += 1
            while true {
                if (0x00 <= *l.c && *l.c <= 0x2f) ||
                    (0x3a <= *l.c && *l.c <= 0x40) ||
                    (0x5b <= *l.c && *l.c <= 0x5e) ||
                    (0x7c <= *l.c && *l.c <= 0x7f) ||
                    (*l.c == 0x60)
                    break

                l.c += 1
            }
        }
    }

    l.location.count = l.c as U64 - l.location.data as U64 // TODO: pointer sub
    return l.kind != TokenKind.eof
}

fn main() {
    current_allocator = dyn_page_allocator

    //var source_path = "F:\\projects\\simplex\\self\\simplex.sp"
    var source_path = "F:\\projects\\simplex\\a.sp"
    if !read_entire_file(
            source_path,
            &var source_buffer: Buffer,
            extra_space_before = 1,
            extra_space_after = 1)
    {
        print("Could not read ")
        println(source_path)
        return 1
    }

    var source = String(source_buffer.span.data + 1, source_buffer.span.count - 2)
    //println(source)

    var l = create_Lexer(source)

    while l.next() {
        print(l.kind as S64)
        print(" ")
        println(l.location)
    }


    return 0
}