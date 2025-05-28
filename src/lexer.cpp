#pragma once
#include "lexer.h"
#include "reporter.h"
#include "escape.h"

#if ARCH_AVX512
using VMask = u64;
#define vload(p) _mm512_loadu_si512((__m512i *)(p))
#define vcmpeq(a, b) _mm512_cmpeq_epi8_mask(a, b)
#define vset1(a) _mm512_set1_epi8(a)
#define vmset1(a) _cvtu64_mask64(a ? ~0 : 0)
#define vmor(a, b) _kor_mask64(a, b)
#define vmask(a) (u16)(a)
#elif ARCH_AVX2
using VMask = u32;
#define vload(p) _mm256_loadu_si256((__m256i *)(p))
#define vcmpeq(a, b) _mm256_cmpeq_epi8(a, b)
#define vset1(a) _mm256_set1_epi8(a)
#define vmset1(a) (a ? ~0 : 0)
#define vmor(a, b) _mm256_or_si256(a, b)
#define vmask(a) (u16)_mm256_movemask_epi8(a)
#else
using VMask = u16;
#define vload(p) _mm_loadu_si128((__m128i *)(p))
#define vcmpeq(a, b) _mm_cmpeq_epi8(a, b)
#define vset1(a) _mm_set1_epi8(a)
#define vmset1(a) (a ? ~0 : 0)
#define vmor(a, b) _mm_or_si128(a, b)
#define vmask(a) (u16)_mm_movemask_epi8(a)
#endif

u64 broadcast_u8_to_u64(u8 x) {
	u64 r = x;
	r = (r << 8) | r;
	r = (r << 16) | r;
	r = (r << 32) | r;
	return r;
}

#define fail() tl::yield_reuse(parent_fiber, current_fiber)

Lexer Lexer::create(String source, Reporter *reporter, Fiber parent_fiber, ReusableFiber current_fiber) {
	assert(!any(Span(source.begin() - LEXER_PADDING_SIZE, LEXER_PADDING_SIZE)), "Source must be padded with {} zero bytes at the beginning!", LEXER_PADDING_SIZE);
	assert(!any(Span(source.end(), LEXER_PADDING_SIZE)), "Source must be padded with {} zero bytes at the end!", LEXER_PADDING_SIZE);

	Lexer result;
	result.source = source;
	result.cursor = source.data;
	result.reporter = reporter;
	result.parent_fiber = parent_fiber;
	result.current_fiber = current_fiber;

	for (umm i = 0; i < source.count; ++i) {
		auto c = source.data[i];
		if (c <= 0x08 || (0x0b <= c && c <= 0x0c) || (0x0e <= c && c <= 0x1f)) {
			reporter->error("Invalid character at byte {}: {} (0x{})", i, escape_character(c).span(), FormatInt{.value=(u32)c,.radix=16});
			fail();
		}
	}

	return result;
}
	
// https://stackoverflow.com/a/68717720/11870423
u64 vcmpltu8 (u64 a, u64 b) {
	constexpr u64 sign_bits = 0x8080808080808080U;
	a = ~a;
    a = (a & b) + (((a ^ b) >> 1) & ~sign_bits);
    a = a & sign_bits;
    a = a + a - (a >> 7);
    return a;
}

Token Lexer::next_token() {
	auto end = source.end();

	Token token;
	token.kind = Token_eof;
	token.string = {end - 1, end};
	
	auto parse_string = [&]<char closing_char>(auto on_char) {
		while (1) {
			switch (*cursor) {
				case '\0': {
					reporter->error(token.string.take(1), "Unclosed literal");
					fail();
				}
				case '\\': {
					++cursor;
					switch (*cursor) {
						// unnecessary because of default case
						// case '\0':
						// 	reporter->error(token.string.take(1), "Unclosed literal");
						// 	fail();
						case 'a':  on_char('\a'); ++cursor; break;
						case 'b':  on_char('\b'); ++cursor; break;
						case 'f':  on_char('\f'); ++cursor; break;
						case 'n':  on_char('\n'); ++cursor; break;
						case 'r':  on_char('\r'); ++cursor; break;
						case 't':  on_char('\t'); ++cursor; break;
						case 'v':  on_char('\v'); ++cursor; break;
						case '\\': on_char('\\'); ++cursor; break;
						case '0': {
							++cursor;
							switch (*cursor) {
								case 'b': case 'B': {
									++cursor;

									u8 digits[] = {
										(u8)(cursor[0] - '0'),
										(u8)(cursor[1] - '0'),
										(u8)(cursor[2] - '0'),
										(u8)(cursor[3] - '0'),
										(u8)(cursor[4] - '0'),
										(u8)(cursor[5] - '0'),
										(u8)(cursor[6] - '0'),
										(u8)(cursor[7] - '0'),
									};



									vcmpltu8(broadcast_u8_to_u64(2), *(u64 *)digits);

									if (digits[0] >= 2 ||
										digits[1] >= 2 ||
										digits[2] >= 2 ||
										digits[3] >= 2 ||
										digits[4] >= 2 ||
										digits[5] >= 2 ||
										digits[6] >= 2 ||
										digits[7] >= 2)
									{
										reporter->error(Span(cursor, (umm)1), "\\0b must be followed by exactly eight binary digits.");
										fail();
									}

									u32 x = 
										(digits[0] << 7) |
										(digits[1] << 6) |
										(digits[2] << 5) |
										(digits[3] << 4) |
										(digits[4] << 3) |
										(digits[5] << 2) |
										(digits[6] << 1) |
										(digits[7] << 0);

									on_char(x);

									cursor += 8;
									break;
								}
								case 'o': case 'O': {
									++cursor;
									
									u8 digits[] = {
										(u8)(cursor[0] - '0'),
										(u8)(cursor[1] - '0'),
										(u8)(cursor[2] - '0'),
									};

									if (digits[0] >= 8 ||
										digits[1] >= 8 ||
										digits[2] >= 8)
									{
										reporter->error(Span(cursor, (umm)1), "\\0o must be followed by exactly three octal digits.");
										fail();
									}
									
									u32 x = 
										(digits[0] << 6) |
										(digits[1] << 3) |
										(digits[2] << 0);

									if (x >= 256) {
										reporter->error(Span(cursor, (umm)1), "Octal value {} (decimal {}) is too big for a character.", FormatInt{.value = x, .radix = 8}, x);
										fail();
									}

									on_char(x);

									cursor += 3;
									break;
								}
								case 'x': case 'X': {
									++cursor;

									if (!is_hex_digit(*cursor) ||
										!is_hex_digit(cursor[1]))
									{
										reporter->error(Span(cursor, (umm)1), "\\0x must be followed by exactly two hexadecimal digits.");
										fail();
									}

									u32 x = 0;
									x = (x << 4) | hex_digit_to_int_unchecked(cursor[0]);
									x = (x << 4) | hex_digit_to_int_unchecked(cursor[1]);

									on_char(x);

									cursor += 2;
									break;
								}
								default: {
									reporter->error(Span(cursor, (umm)1), "\\0 must be followed by a base character just like in integer literals - b for binary, o for octal, x for hexadecimal.");
									fail();
								}
							}
							break;
						}
						default: {
							reporter->error(Span(cursor, (umm)1), "\\ must be followed by a valid escape sequence.");
							if (*cursor == 'x') {
								reporter->help("If you wanted a hex literal, use \\0x (backslash zero x), followed by exactly two hex digits. You can also use binary and octal.");
							}
							fail();
						}
					}
					break;
				}
				case closing_char: {
					++cursor;
					return;
				} 
				default: {
					on_char(*cursor++);
					break;
				}
			}
		}
	};


restart:

	{
		VMask mask = vmset1(1);
		while (mask == (VMask)~0) {
			auto chars = vload(cursor);
			auto m1 = vcmpeq(chars, vset1('\t'));
			auto m2 = vcmpeq(chars, vset1('\v'));
			auto m3 = vcmpeq(chars, vset1('\f'));
			auto m4 = vcmpeq(chars, vset1('\r'));
			auto m5 = vcmpeq(chars, vset1(' '));
			auto masks = vmor(vmor(vmor(m1, m2), vmor(m3, m4)), m5);
			mask = vmask(masks);

			cursor += count_trailing_ones(mask);
		}
	}

	if (cursor >= end) {
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
				reporter->error(String{token.string.data, cursor}, "This language does not support {}{} operation. Use {}= instead", (char)first, (char)first, (char)first);
				fail();
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
				while (*cursor != '\n' && cursor != end) {
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
					if (cursor >= end) {
						reporter->error(token.string.take(2), "Unclosed comment");
						fail();
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
		
			string_value.clear();
			
			parse_string.operator()<'"'>([&](char c) {
				string_value.add(c);
			});

			goto finish;
		}
		case '\'': {
			token.kind = Token_number;
			++cursor;
		
			u64 i = 0;
			int_value = 0;
			
			parse_string.operator()<'\''>([&](char c) {
				int_value |= (u64)c << (i++ * 8);
			});

			umm max_length = 8;
			if (i > max_length) {
				reporter->error(Span(token.string.data, cursor), "Multi-character can't be longer than number of bytes in the biggest integer, in this case {}.", max_length);
				fail();
			}
			goto finish;
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
						reporter->error(Span(token.string.data, cursor), "C-like octal literals are not supported. Use 0o{}", Span(token.string.data + 1, cursor));
						fail();
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
				reporter->error({token.string.data, 1}, "Invalid character ({}).", (u32)*cursor);
				fail();
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
						case swp('use'): { token.kind = Token_use; break; }
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

