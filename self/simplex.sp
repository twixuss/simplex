
import "file"

fn end(s: String) => s.data + s.count

enum TokenKind #allow_to_int #allow_from_int {
    eof
    eol = '\n'
    name = 'a'
    string = '"'
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

fn next(use l: *var Lexer): Bool => {
    let e = source.end()

    while true {
        match *c {
            ' ' or '\t' or '\n' or '\r' or '\v' or '\f' => c += 1
            else => { break } // TODO: don't want braces
        }
    }

    location.data = c;

    match *c {
        0 => {
            kind = TokenKind.eof
        }
        '`' or '~' or '!' or '@' or
        '#' or '$' or '%' or '^' or
        '&' or '*' or '(' or ')' or
        '-' or '=' or '+' or '\\' or
        '|' or '[' or ']' or '{' or
        '}' or ';' or ':' or ',' or
        '<' or '.' or '>' or '?' => {
            kind = *c
            c += 1
        }
        '"' => {
			kind = TokenKind.string
			c += 1
			while (true) {
				if *c == '"' && c[-1] != '\\' {
					break
				}
				c += 1
				if (c > e) {
					println("Unclosed string literal")
				}
			}

			c += 1
        }
        '\'' => {
			kind = TokenKind.string
			c += 1
			while (true) {
				if *c == '\\' {
                    c += 1
				} else if *c == '\'' {
    				c += 1
                    break
                }
                c += 1
				if (c > e) {
					println("Unclosed character literal")
				}
			}

			c += 1
        }
        '/' => {
            kind = *c
			c += 1
            if *c == '/' {
                while *c != '\n' {
                    c += 1
                }
                c += 1
                
                // goto retry
                return next(l)
            }
        }
        else => {
            kind = TokenKind.name
            c += 1
            while true {
                if (0x00 <= *c && *c <= 0x2f) ||
                    (0x3a <= *c && *c <= 0x40) ||
                    (0x5b <= *c && *c <= 0x5e) ||
                    (0x7c <= *c && *c <= 0x7f) ||
                    (*c == 0x60)
                    break

                c += 1
            }
        }
    }

    location.count = c as U64 - location.data as U64 // TODO: pointer sub
    return kind != TokenKind.eof
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
    println(source)

    var l = create_Lexer(source)

    while l.next() {
        println(l.location)
    }


    return 0
}