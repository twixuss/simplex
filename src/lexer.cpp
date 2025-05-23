#pragma once
#include "lexer.h"
#include "reporter.h"
#include "escape.h"

using namespace tl;

Lexer Lexer::create(String source) {
	Lexer result;
	result.source = source;
	result.cursor = source.data;
	
	assert(source.data[-1] == 0, "Source must be terminated with zero at the beginning!");
	assert(source.data[source.count] == 0, "Source must be terminated with zero at the end!");

	for (umm i = 0; i < source.count; ++i) {
		auto c = source.data[i];
		if (c <= 0x08 || (0x0b <= c && c <= 0x0c) || (0x0e <= c && c <= 0x1f)) {
			immediate_reporter.error("Invalid character at byte {}: {} (0x{})", i, escape_character(c).span(), FormatInt{.value=(u32)c,.radix=16});
			return {};
		}
	}

	return result;
}
	
Token Lexer::next_token() {
	auto end = source.end();

	Token token;
	token.kind = Token_eof;
	token.string = {source.end() - 1, source.end()};

restart:

	while (cursor < end) {
		switch (*cursor) {
			case '\t':
			case '\v':
			case '\f':
			case '\r':
			case ' ':
				++cursor;
				continue;
		}
		break;
	}

	if (cursor >= source.end()) {
		goto finish;
	}

	token.string.data = cursor;
	
	switch (*cursor) {
		case '\0':
			goto finish;

		//
		//  Single char
		// 
		case '(': case ')':
		case '[': case ']':
		case '{': case '}':
		case '`': case '~':
		case '@':
		case '$': case ';':
		case ':': case ',':
		case '?':
		case '\\': case '\n': {
			token.kind = (TokenKind)*cursor++;
			goto finish;
		}

		//
		//  x  xx
		// 
		case '.': {
			auto first = *cursor++;
			u64 kind = first;

			bool same = *cursor == first;
			cursor += same;
			kind = same ? first | (first << 8) : first;

			token.kind = (TokenKind)kind;
			goto finish;
		}

		//
		//  x  x=
		//
		case '*':
		case '%':
		case '^':
		case '!': {
			u64 kind = *cursor++;
			if (*cursor == '=') {
				kind = kind << 8 | *cursor++;
			}
			token.kind = (TokenKind)kind;
			goto finish;
		}
			
		//
		//  x  xx  x=
		//
		case '+':
		case '-': {
			u8 first = *cursor++;
			u64 kind = first;
			if (*cursor == '=' || *cursor == first) {
				kind = kind << 8 | *cursor++;
			}
			token.kind = (TokenKind)kind;

			if (token.kind == (first | (first << 8))) {
				immediate_reporter.error(String{token.string.data, cursor}, "This language does not support {}{} operation. Use {}= instead", (char)first, (char)first, (char)first);
				token = {};
				goto finish;
			}

			goto finish;
		}
				
		//
		//  x  xx  x>
		// 
		case '=': {
			u8 first = *cursor++;
			u64 kind = first;
			if (*cursor == '>' || *cursor == first) {
				kind = kind << 8 | *cursor++;
			}
			token.kind = (TokenKind)kind;
			goto finish;
		}
		//
		//  x  xx  x=  xx=
		//
		case '&':
		case '|':
		case '<':
		case '>': {
			u8 first = *cursor++;
			u64 kind = first; // x
			if (*cursor == '=') {
				kind = kind << 8 | *cursor++; // x=
			} else if (*cursor == first) {
				kind = kind << 8 | *cursor++; // xx
				if (*cursor == '=') {
					kind = kind << 8 | *cursor++; // xx=
				}
			}
			token.kind = (TokenKind)kind;
			goto finish;
		}

		case '/': {
			token.kind = (TokenKind)*cursor;
			++cursor;
			if (*cursor == '/') {
				while (*cursor != '\n' && cursor != source.end()) {
					++cursor;
				}
				goto restart;
			} else if (*cursor == '*') {
				int level = 1;
				enum class In : u8 {
					nothing,
					string,
				};
				In in = In::nothing;
				while (true) {
					switch (in) {
						case In::nothing: {
							if (String(cursor, 2) == "/*") {
								cursor += 2;
								level += 1;
							} else if (String(cursor, 2) == "*/") {
								cursor += 2;
								level -= 1;
								if (level == 0)
									goto restart;
							} else if (*cursor == '"') {
								in = In::string;
								++cursor;
							} else {
								++cursor;
							}
							break;
						}
						case In::string: {
							if (String(cursor, 2) == "\\\"") {
								cursor += 2;
							} else if (*cursor == '"') {
								in = In::nothing;
								++cursor;
							} else {
								++cursor;
							}
							break;
						}
					}
					if (cursor >= source.end()) {
						immediate_reporter.error(token.string.take(2), "Unclosed comment");
						token = {};
						goto finish;
					}
				}
				goto restart;
			} else {
				token.kind = (TokenKind)'/';
				goto finish;
			}
			break;
		}
		case '"': {
			token.kind = Token_string;
			++cursor;
			while (true) {
				if (*cursor == '"' && cursor[-1] != '\\') {
					break;
				}
				++cursor;
				if (cursor > source.end()) {
					immediate_reporter.error(token.string.take(2), "Unclosed string literal");
					token = {};
					goto finish;
				}
			}

			++cursor;
			goto finish;
		}
		case '\'': {
			token.kind = Token_number;
			++cursor;
		
			string_value.clear();

			while (1) {
				switch (*cursor) {
					case '\0': {
						immediate_reporter.error(token.string.take(1), "Unclosed character literal");
						token = {};
						goto finish;
					}
					case '\\': {
						++cursor;
						switch (*cursor) {
							case '\0':
								immediate_reporter.error(token.string.take(1), "Unclosed character literal");
								token = {};
								goto finish;
							case 'a': string_value.add('\a'); ++cursor; break;
							case 'b': string_value.add('\b'); ++cursor; break;
							case 'f': string_value.add('\f'); ++cursor; break;
							case 'n': string_value.add('\n'); ++cursor; break;
							case 'r': string_value.add('\r'); ++cursor; break;
							case 't': string_value.add('\t'); ++cursor; break;
							case 'v': string_value.add('\v'); ++cursor; break;
							case '\\': string_value.add('\\'); ++cursor; break;
							case 'x': case 'X': {
								++cursor;

								switch (*cursor) {
									case '\0': {
										immediate_reporter.error(token.string.take(1), "Unclosed character literal");
										token = {};
										goto finish;
									}
									case '0':case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
									case 'a':case 'b':case 'c':case 'd':case 'e':case 'f':
									case 'A':case 'B':case 'C':case 'D':case 'E':case 'F': {
										break;
									}
									default: {
										immediate_reporter.error(Span(cursor, (umm)1), "Invalid hexadecimal escape sequence");
										token = {};
										goto finish;
									}
								}

								char x = 0;
								while (is_hex_digit(*cursor)) {
									x = (x << 4) | hex_digit_to_int_unchecked(*cursor++);
								}
								string_value.add(x);
								break;
							}
							default: {
								string_value.add(*cursor++);
								break;
							}
						}
						case '\'': {
							++cursor;
							int_value = 0;
							umm max_length = 8;
							if (string_value.count > max_length) {
								immediate_reporter.error(Span(token.string.data, cursor), "Multi-character can't be longer than number of bytes in the biggest integer, in this case {}.", max_length);
								token = {};
								goto finish;
							}
							for (umm i = 0; i < string_value.count; ++i) {
								int_value |= (u64)string_value.data[i] << (i * 8);
							}
							goto finish;
						} 
						default: {
							string_value.add(*cursor);
							++cursor;
							break;
						}
					}
				}
			}
		}
		case '#': {
			token.kind = Token_directive;

			++cursor;
			while (true) {
				switch (*cursor) {
					ENUMERATE_CHARS_ALPHA(PASTE_CASE)
					ENUMERATE_CHARS_DIGIT(PASTE_CASE)
					case '_': {
						++cursor;
						break;
					}
					default:
						goto directive_loop_end;
				}
			}
		directive_loop_end:;
			goto finish;
		}

		ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
			token.kind = Token_number;

			int_value = 0;

			if (*cursor == '0') {
				++cursor;
				switch (*cursor) {
					default: goto finish;
					case 'b': {
						++cursor;
						while (1) {
							switch (*cursor) {
								default:
									goto finish;
								case '0': case '1':
									int_value = (int_value << 1) | (*cursor++ - '0'); break;
								case '_':
									*cursor++; break;
							}
						}
						break;
					}
					case 'o': {
						++cursor;
						while (1) {
							switch (*cursor) {
								default:
									goto finish;
								case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
									int_value = (int_value << 3) | (*cursor++ - '0'); break;
								case '_':
									*cursor++; break;
							}
						}
						break;
					}
					case 'x': {
						++cursor;
						while (1) {
							switch (*cursor) {
								default:
									goto finish;
								case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
									int_value = (int_value << 4) | (*cursor++ - '0'); break;
								case 'a':case 'b':case 'c':case 'd':case 'e':case 'f':
									int_value = (int_value << 4) | (*cursor++ - ('a' - 10)); break;
								case 'A':case 'B':case 'C':case 'D':case 'E':case 'F':
									int_value = (int_value << 4) | (*cursor++ - ('A' - 10)); break;
								case '_':
									*cursor++; break;
							}
						}
						break;
					}

					case '0':case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
					case '_': {
						while (1) {
							switch (*cursor) {
								case '0':case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
								case '_':
									++cursor;
									continue;
							}
							break;
						}
						immediate_reporter.error(Span(token.string.data, cursor), "C-like octal literals are not supported. Use 0o{}", Span(token.string.data + 1, cursor));
						token = {};
						goto finish;
					}
				}
			} else {
				while (1) {
					switch (*cursor) {
						default:
							goto finish;
						case '0':case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':
							int_value = int_value * 10 + (*cursor++ - '0'); break;
						case '_':
							++cursor; break;
					}
				}
			}
			goto finish;
		}
		default: {
			token.kind = Token_name;

			while (true) {
				if ((0x00 <= *cursor && *cursor <= 0x2f) ||
					(0x3a <= *cursor && *cursor <= 0x40) ||
					(0x5b <= *cursor && *cursor <= 0x5e) ||
					(0x7c <= *cursor && *cursor <= 0x7f) ||
					(*cursor == 0x60)
					) {
					goto name_loop_end;
				}

				++cursor;
			}
		name_loop_end:;

			token.string.set_end(cursor);
			if (token.string.count == 0) {
				immediate_reporter.error({token.string.data, 1}, "Invalid character ({}).", (u32)*cursor);
				return {};
			}

			static constexpr umm max_keyword_size = []{
				umm max = 0;
				#define x(name) max = tl::max(max, u8###name##s.count);
				ENUMERATE_KEYWORDS(x)
				#undef x
				return max; 
			}();
			
			auto swp = [](u64 x) {
				u64 r = 0;
				while (x) {
					r = (r << 8) | (x & 0xff);
					x >>= 8;
				}
				return r;
			};

			auto swp2 = [](Span<char> x) {
				u64 r = 0;
				for (umm i = 0; i < x.count; ++i) {
					r = (r << 8) | x.data[x.count-i-1];
				}
				return r;
			};

			#if 0
				
			if (false) {}
			#define x(name) else if (token.string == u8###name##s) { token.kind = Token_##name; }
			ENUMERATE_KEYWORDS(x)
			#undef x

			#else
			// NOTE:
			// This relies on compiler being able to optimize out branches.
			// Based on string size, check only keywords with that size.
			switch (token.string.count) {
				#if 0
				// Preprocessor way with switch

				#define x(name) \
					case const_string_to_token_kind(#name##s): { token.kind = Token_##name; break; }

				#define CASE(N) \
					case N: { \
						umm const COUNT = N; \
						auto token_as_int = chars_as_int<N>(token.string.data); \
						switch (token_as_int) { \
							ENUMERATE_KEYWORDS_L##N(x) \
						} \
						break; \
					}
				CASE(1)
				CASE(2)
				CASE(3)
				CASE(4)
				CASE(5)
				CASE(6)
				CASE(7)
				CASE(8)
				static_assert(max_keyword_size <= 9, "");
				#undef CASE
				#undef x

				#elif 1
				// Manual switch
				case 2: {
					u16 token_as_int = *(u16 *)token.string.data;
					switch (token_as_int) {
						case swp('or'): { token.kind = Token_or; break; }
						case swp('U8'): { token.kind = Token_U8; break; }
						case swp('S8'): { token.kind = Token_S8; break; }
						case swp('if'): { token.kind = Token_if; break; }
						case swp('as'): { token.kind = Token_as; break; }
						case swp('fn'): { token.kind = Token_fn; break; }
						case swp('in'): { token.kind = Token_in; break; }
					}
					break;
				}
				case 3: {
					u32 token_as_int = *(u32 *)token.string.data & 0xff'ff'ff;
					switch (token_as_int) {
						case swp('U16'): { token.kind = Token_U16; break; }
						case swp('U32'): { token.kind = Token_U32; break; }
						case swp('U64'): { token.kind = Token_U64; break; }
						case swp('S16'): { token.kind = Token_S16; break; }
						case swp('S32'): { token.kind = Token_S32; break; }
						case swp('S64'): { token.kind = Token_S64; break; }
						case swp('let'): { token.kind = Token_let; break; }
						case swp('var'): { token.kind = Token_var; break; }
						case swp('for'): { token.kind = Token_for; break; }
					}
					break;
				}
				case 4: {
					u32 token_as_int = *(u32 *)token.string.data;
					switch (token_as_int) {
						case swp('Type'): { token.kind = Token_Type; break; }
						case swp('Bool'): { token.kind = Token_Bool; break; }
						case swp('None'): { token.kind = Token_None; break; }
						case swp('none'): { token.kind = Token_none; break; }
						case swp('then'): { token.kind = Token_then; break; }
						case swp('else'): { token.kind = Token_else; break; }
						case swp('true'): { token.kind = Token_true; break; }
						case swp('enum'): { token.kind = Token_enum; break; }
					}
					break;
				}
				case 5: {
					u64 token_as_int = *(u64 *)token.string.data & 0xff'ff'ff'ff'ff;
					switch (token_as_int) {
						case swp2("const"s): { token.kind = Token_const; break; }
						case swp2("false"s): { token.kind = Token_false; break; }
						case swp2("while"s): { token.kind = Token_while; break; }
						case swp2("break"s): { token.kind = Token_break; break; }
						case swp2("match"s): { token.kind = Token_match; break; }
						case swp2("defer"s): { token.kind = Token_defer; break; }
					}
					break;
				}
				case 6: {
					u64 token_as_int = *(u64 *)token.string.data & 0xff'ff'ff'ff'ff'ff;
					switch (token_as_int) {
						case swp2("String"s): { token.kind = Token_String; break; }
						case swp2("return"s): { token.kind = Token_return; break; }
						case swp2("typeof"s): { token.kind = Token_typeof; break; }
						case swp2("inline"s): { token.kind = Token_inline; break; }
						case swp2("struct"s): { token.kind = Token_struct; break; }
						case swp2("import"s): { token.kind = Token_import; break; }
					}
					break;
				}
				case 8: {
					u64 token_as_int = *(u64 *)token.string.data;
					switch (token_as_int) {
						case swp2("continue"s): { token.kind = Token_continue; break; }
						case swp2("noinline"s): { token.kind = Token_noinline; break; }
					}
					break;
				}
				#elif 0
				// Manual ifs
				case 2: {
					if (false) {}
					else if (token.string == "U8") { token.kind = Token_U8; }
					else if (token.string == "S8") { token.kind = Token_S8; }
					else if (token.string == "if") { token.kind = Token_if; }
					else if (token.string == "as") { token.kind = Token_as; }
					break;
				}
				case 3: {
					if (false) {}
					else if (token.string == "U16") { token.kind = Token_U16; }
					else if (token.string == "U32") { token.kind = Token_U32; }
					else if (token.string == "U64") { token.kind = Token_U64; }
					else if (token.string == "S16") { token.kind = Token_S16; }
					else if (token.string == "S32") { token.kind = Token_S32; }
					else if (token.string == "S64") { token.kind = Token_S64; }
					else if (token.string == "let") { token.kind = Token_let; }
					else if (token.string == "var") { token.kind = Token_var; }
					break;
				}
				case 4: {
					if (false) {}
					else if (token.string == "Type") { token.kind = Token_Type; }
					else if (token.string == "Bool") { token.kind = Token_Bool; }
					else if (token.string == "None") { token.kind = Token_None; }
					else if (token.string == "then") { token.kind = Token_then; }
					else if (token.string == "else") { token.kind = Token_else; }
					else if (token.string == "true") { token.kind = Token_true; }
					break;
				}
				case 5: {
					if (false) {}
					else if (token.string == "const") { token.kind = Token_const; }
					else if (token.string == "false") { token.kind = Token_false; }
					else if (token.string == "while") { token.kind = Token_while; }
					else if (token.string == "break") { token.kind = Token_break; }
					else if (token.string == "match") { token.kind = Token_match; }
					break;
				}
				case 6: {
					if (false) {}
					else if (token.string == "String") { token.kind = Token_String; }
					else if (token.string == "return") { token.kind = Token_return; }
					else if (token.string == "typeof") { token.kind = Token_typeof; }
					else if (token.string == "inline") { token.kind = Token_inline; }
					else if (token.string == "struct") { token.kind = Token_struct; }
					break;
				}
				case 8: {
					if (false) {}
					else if (token.string == "continue") { token.kind = Token_continue; }
					else if (token.string == "noinline") { token.kind = Token_noinline; }
					break;
				}
				#else
				// Preprocessor way with ifs


				#define x(name) \
					else if (u8###name##s.count == COUNT && token.string == #name) { \
						token.kind = Token_##name; \
					}

				#define CASE(N) \
					case N: { \
						umm const COUNT = N; \
						if (false) {} \
						ENUMERATE_KEYWORDS(x); \
						break; \
					}
				CASE(1)
				CASE(2)
				CASE(3)
				CASE(4)
				CASE(5)
				CASE(6)
				CASE(7)
				CASE(8)
				CASE(9)
				static_assert(max_keyword_size <= 9, "");
				#undef CASE
				#undef x
				#endif
			}
			#endif
			
			goto finish;
		}
	} 

finish:
	token.string.set_end(cursor);
	return token;
}

void Lexer::print_invalid_character_error() {
	immediate_reporter.error({cursor, 1}, "Invalid uft8 character.");
}
