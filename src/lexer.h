#pragma once
#include "common.h"

#include <tl/list.h>

using namespace tl;

struct Lexer {
	static Lexer create(String source) {
		Lexer result;
		result.source = source;
		result.cursor = source.data;
		
		for (umm i = 0; i < source.count; ++i) {
			auto c = source.data[i];
			if (c <= 0x08 || (0x0b <= c && c <= 0x0c) || (0x0e <= c && c <= 0x1f)) {
				immediate_reporter.error("Invalid character at byte {}: {} (0x{})", i, escape_character(c).span(), FormatInt{.value=(u32)c,.radix=16});
				return {};
			}
		}

		return result;
	}
	
	Token next_token() {
		Token eof;
		eof.kind = Token_eof;
		eof.string = {source.end() - 1, source.end()};

	restart:
		if (cursor >= source.end()) {
			return eof;
		}

		while (true) {
			if (*cursor != ' ' && *cursor != '\t' && *cursor != '\r')
				break;
			next();
			if (cursor == source.end())
				return eof;
		}


		Token token;
		token.string.data = cursor;

		// ("&", "=")
		// "&"
		// "&="
#define CASE_SINGLE_OR_DOUBLE(a, b)                                   \
	case a: {                                                         \
		next();                                                       \
		if (*cursor == b) {                                           \
			next();                                                   \
			token.kind = (TokenKind)const_string_to_token_kind(a, b); \
			token.string.count = 2;                                   \
		} else {                                                      \
			token.kind = (TokenKind)a;                                \
			token.string.count = 1;                                   \
		}                                                             \
		return token;                                                 \
	}

		// ("&", "=")
		// "&"
		// "&&"
		// "&="
#define CASE_SINGLE_OR_TWO_DOUBLES(a, b)                                  \
	case a: {                                                             \
		next();                                                           \
		switch (*cursor) {                                                \
			case a:                                                       \
				next();                                                   \
				token.kind = (TokenKind)const_string_to_token_kind(a, a); \
				token.string.count = 2;                                   \
				break;                                                    \
			case b:                                                       \
				next();                                                   \
				token.kind = (TokenKind)const_string_to_token_kind(a, b); \
				token.string.count = 2;                                   \
				break;                                                    \
			default: {                                                    \
				token.kind = (TokenKind)a;                                \
				token.string.count = 1;                                   \
				break;                                                    \
			}                                                             \
		}                                                                 \
		return token;                                                     \
	}
		// ("<", "=")
		// "<"
		// "<="
		// "<<"
		// "<<="
#define CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE(a, b)                               \
	case a: {                                                                    \
		next();                                                                  \
		switch (*cursor) {                                                       \
			case b:                                                              \
				token.kind = (TokenKind)const_string_to_token_kind(a, b);        \
				next();                                                          \
				token.string.count = 2;                                          \
				break;                                                           \
			case a: {                                                            \
				next();                                                          \
				if (*cursor == b) {                                              \
					next();                                                      \
					token.kind = (TokenKind)const_string_to_token_kind(a, a, b); \
					token.string.count = 3;                                      \
				} else {                                                         \
					token.kind = (TokenKind)const_string_to_token_kind(a, a);    \
					token.string.count = 2;                                      \
				}                                                                \
				break;                                                           \
			}                                                                    \
			default: {                                                           \
				token.kind = (TokenKind)a;                                       \
				token.string.count = 1;                                          \
				break;                                                           \
			}                                                                    \
		}                                                                        \
		return token;                                                            \
	}
		switch (*cursor) {
			case '(': case ')':
			case '[': case ']':
			case '{': case '}':
			case '`': case '~':
			case '@':
			case '$': case ';':
			case ':': case ',':
			case '?':
			case '\\': case '\n': {
				token.kind = (TokenKind)*cursor;
				token.string.count = 1;
				next();
				return token;
			}
			CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE('>', '=')
			CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE('<', '=')
			CASE_SINGLE_OR_DOUBLE('.', '.');
			case '+':
			case '-':
			case '*':
			case '%':
			case '^':
			case '!': {
				char a = *cursor;
				next();

				u8 shift_table[] = {0, 8};
				u16 or_table[] = {0, '=' << 8};
				bool is_equals = *cursor == '=';

				cursor += is_equals;
				token.kind = (TokenKind)(a | or_table[is_equals]);
				token.string.count = is_equals + 1;

				return token;
			}

			CASE_SINGLE_OR_TWO_DOUBLES('=', '>');
			CASE_SINGLE_OR_TWO_DOUBLES('&', '=');
			CASE_SINGLE_OR_TWO_DOUBLES('|', '=');
			case '/': {
				token.kind = (TokenKind)*cursor;
				next();
				if (*cursor == '/') {
					while (*cursor != '\n' && cursor != source.end()) {
						next();
					}
					goto restart;
				} else if (*cursor == '*') {
					while (true) {
						if (String(cursor, 2) == "*/") {
							cursor += 2;
							break;
						}
						next();
						if (cursor > source.end()) {
							immediate_reporter.error(token.string.take(2), "Unclosed comment");
							return {};
						}
					}
					goto restart;
				} else {
					token.kind = (TokenKind)'/';
					token.string.count = 1;
					return token;
				}
				break;
			}
			case '"': {
				token.kind = Token_string;
				next();
				while (true) {
					if (*cursor == '"' && cursor[-1] != '\\') {
						break;
					}
					next();
					if (cursor > source.end()) {
						immediate_reporter.error(token.string.take(2), "Unclosed string literal");
						return {};
					}
				}

				next();

				token.string.set_end(cursor);
				return token;
			}
			case '#': {
				token.kind = Token_directive;

				next();
				while (true) {
					switch (*cursor) {
						ENUMERATE_CHARS_ALPHA(PASTE_CASE)
						ENUMERATE_CHARS_DIGIT(PASTE_CASE)
						case '_': {
							next();
							break;
						}
						default:
							goto directive_loop_end;
					}
				}
			directive_loop_end:;

				token.string.set_end(cursor);
				return token;
			}

			ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
				token.kind = Token_number;

				next();
				
				if (*cursor == 'x') {
					next();

					while (true) {
						switch (*cursor) {
							ENUMERATE_CHARS_HEX(PASTE_CASE) {
								next();
								break;
							}
							default:
								goto number_loop_end;
						}
					}

				} else {
					while (true) {
						switch (*cursor) {
							ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
								next();
								break;
							}
							default:
								goto number_loop_end;
						}
					}
				}
			number_loop_end:;

				token.string.set_end(cursor);
				return token;
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

					next();
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
							case const_string_to_token_kind("U8"s): { token.kind = Token_U8; break; }
							case const_string_to_token_kind("S8"s): { token.kind = Token_S8; break; }
							case const_string_to_token_kind("if"s): { token.kind = Token_if; break; }
							case const_string_to_token_kind("as"s): { token.kind = Token_as; break; }
							case const_string_to_token_kind("fn"s): { token.kind = Token_fn; break; }
						}
						break;
					}
					case 3: {
						u32 token_as_int = *(u32 *)token.string.data & 0xff'ff'ff;
						switch (token_as_int) {
							case const_string_to_token_kind("U16"s): { token.kind = Token_U16; break; }
							case const_string_to_token_kind("U32"s): { token.kind = Token_U32; break; }
							case const_string_to_token_kind("U64"s): { token.kind = Token_U64; break; }
							case const_string_to_token_kind("S16"s): { token.kind = Token_S16; break; }
							case const_string_to_token_kind("S32"s): { token.kind = Token_S32; break; }
							case const_string_to_token_kind("S64"s): { token.kind = Token_S64; break; }
							case const_string_to_token_kind("let"s): { token.kind = Token_let; break; }
							case const_string_to_token_kind("var"s): { token.kind = Token_var; break; }
						}
						break;
					}
					case 4: {
						u32 token_as_int = *(u32 *)token.string.data;
						switch (token_as_int) {
							case const_string_to_token_kind("Type"s): { token.kind = Token_Type; break; }
							case const_string_to_token_kind("Bool"s): { token.kind = Token_Bool; break; }
							case const_string_to_token_kind("None"s): { token.kind = Token_None; break; }
							case const_string_to_token_kind("none"s): { token.kind = Token_none; break; }
							case const_string_to_token_kind("then"s): { token.kind = Token_then; break; }
							case const_string_to_token_kind("else"s): { token.kind = Token_else; break; }
							case const_string_to_token_kind("true"s): { token.kind = Token_true; break; }
						}
						break;
					}
					case 5: {
						u64 token_as_int = *(u64 *)token.string.data & 0xff'ff'ff'ff'ff;
						switch (token_as_int) {
							case const_string_to_token_kind("const"s): { token.kind = Token_const; break; }
							case const_string_to_token_kind("false"s): { token.kind = Token_false; break; }
							case const_string_to_token_kind("while"s): { token.kind = Token_while; break; }
							case const_string_to_token_kind("break"s): { token.kind = Token_break; break; }
							case const_string_to_token_kind("match"s): { token.kind = Token_match; break; }
							case const_string_to_token_kind("defer"s): { token.kind = Token_defer; break; }
						}
						break;
					}
					case 6: {
						u64 token_as_int = *(u64 *)token.string.data & 0xff'ff'ff'ff'ff'ff;
						switch (token_as_int) {
							case const_string_to_token_kind("String"s): { token.kind = Token_String; break; }
							case const_string_to_token_kind("return"s): { token.kind = Token_return; break; }
							case const_string_to_token_kind("typeof"s): { token.kind = Token_typeof; break; }
							case const_string_to_token_kind("inline"s): { token.kind = Token_inline; break; }
							case const_string_to_token_kind("struct"s): { token.kind = Token_struct; break; }
							case const_string_to_token_kind("import"s): { token.kind = Token_import; break; }
						}
						break;
					}
					case 8: {
						u64 token_as_int = *(u64 *)token.string.data;
						switch (token_as_int) {
							case const_string_to_token_kind("continue"s): { token.kind = Token_continue; break; }
							case const_string_to_token_kind("noinline"s): { token.kind = Token_noinline; break; }
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

				return token;
			}
		} 
	}

private:
	String source;
	utf8 *cursor;
	
	void print_invalid_character_error() {
		immediate_reporter.error({cursor, 1}, "Invalid uft8 character.");
	}
	
	void next() {
		assert(cursor <= source.end());
		++cursor;
	}

};
