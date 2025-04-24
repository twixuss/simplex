#pragma once
#include "common.h"

StaticList<char, 4> escape_character(char ch);

inline umm escape_string(Span<utf8> string, auto write) {
	StaticList<utf8, 4096> buffer;
	umm count = 0;
	for (auto ch : string) {
		auto escaped = escape_character((char)ch);
		for (auto ch : escaped) {
			buffer.add(ch); 
			if (buffer.count == buffer.capacity) {
				write(buffer.span());
				count += buffer.count;
				buffer.clear();
			}
		}
	}
	write(buffer.span());
	count += buffer.count;
	return count;
}

struct EscapedString {
	String unescaped_string;
};

umm append(StringBuilder &builder, EscapedString string);

struct UnescapeResult {
	List<utf8> string;
	String failed_at;
	String fail_reason;

	inline operator bool() { return failed_at.count == 0; }
};

UnescapeResult unescape_string(Span<utf8> string);
