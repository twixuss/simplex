#include "escape.h"

StaticList<char, 4> escape_character(char ch) {
	StaticList<char, 4> result;
	switch (ch) {
		case '\x0': result.add("\\x0"s); break;
		case '\x1': result.add("\\x1"s); break;
		case '\x2': result.add("\\x2"s); break;
		case '\x3': result.add("\\x3"s); break;
		case '\x4': result.add("\\x4"s); break;
		case '\x5': result.add("\\x5"s); break;
		case '\x6': result.add("\\x6"s); break;
		case '\a': result.add("\\a"s); break;
		case '\b': result.add("\\b"s); break;
		case '\t': result.add("\\t"s); break;
		case '\n': result.add("\\n"s); break;
		case '\v': result.add("\\v"s); break;
		case '\f': result.add("\\f"s); break;
		case '\r': result.add("\\r"s); break;
		case '\x0e': result.add("\\x0e"s); break;
		case '\x0f': result.add("\\x0f"s); break;
		case '\x10': result.add("\\x10"s); break;
		case '\x11': result.add("\\x11"s); break;
		case '\x12': result.add("\\x12"s); break;
		case '\x13': result.add("\\x13"s); break;
		case '\x14': result.add("\\x14"s); break;
		case '\x15': result.add("\\x15"s); break;
		case '\x16': result.add("\\x16"s); break;
		case '\x17': result.add("\\x17"s); break;
		case '\x18': result.add("\\x18"s); break;
		case '\x19': result.add("\\x19"s); break;
		case '\x1a': result.add("\\x1a"s); break;
		case '\x1b': result.add("\\x1b"s); break;
		case '\x1c': result.add("\\x1c"s); break;
		case '\x1d': result.add("\\x1d"s); break;
		case '\x1e': result.add("\\x1e"s); break;
		case '\x1f': result.add("\\x1f"s); break;
		case '\\': result.add("\\\\"s); break;
		default:
			result.add(ch);
			break;
	}
	return result;
}

umm append(StringBuilder &builder, EscapedString string) {
	return escape_string(string.unescaped_string, [&](auto s) { append(builder, s); });
}

UnescapeResult unescape_string(Span<utf8> string) {
	if (string.count == 0) {
		return { .string = to_list(string) };
	}

	List<utf8> result;

	UnescapeResult unfinished_result = {
		.failed_at = {&string.back(), 1},
		.fail_reason = u8"Unfinished escape sequence"s,
	}; 

#define NEXT() if (++cursor == string.end()) return unfinished_result

	for (auto cursor = string.begin(); cursor != string.end(); ++cursor) {
		if (*cursor == '\\') {
			NEXT();
			switch (*cursor) {
				case 'a': result.add('\a'); break;
				case 'b': result.add('\b'); break;
				case 'f': result.add('\f'); break;
				case 'v': result.add('\v'); break;
				case 'n': result.add('\n'); break;
				case 't': result.add('\t'); break;
				case 'r': result.add('\r'); break;
				case '\\': result.add('\\'); break;
				case 'x': {
					u32 digits[2];
					
					for (auto &digit : digits) {
						NEXT();
						digit = *cursor;
						/**/ if ('0' <= digit && digit <= '9') digit -= '0';
						else if ('a' <= digit && digit <= 'f') digit = digit - 'a' + 10;
						else if ('A' <= digit && digit <= 'F') digit = digit - 'A' + 10;
						else return {
							.failed_at = {cursor, 1},
							.fail_reason = u8"Invalid hex digit"s,
						};
					}

					result.add(digits[0] * 16 + digits[1]);

					break;
				}
				default: 
					return {
						.failed_at = {cursor, 1},
						.fail_reason = u8"Unknown escape sequence"s,
					};
			}
		} else {
			result.add(*cursor);
		}
	}

#undef NEXT

	return {result};
}
