#pragma once
#include <tl/common.h>
#include <tl/hash_map.h>

namespace tl {
namespace c_parser {

struct Macro {
	Span<utf8> name;
	Span<utf8> value;
};

enum class TokenKind {
	
};

struct Token {
	TokenKind kind;
	Span<utf8> string;
};

enum class PreprocessError {
	unexpected_end_of_source,
};

Result<Empty, PreprocessError> preprocess_source(Span<utf8> source, Span<Macro> macros, auto on_parsed_define) {
	utf8 *cursor = source.begin();

	while (cursor < source.end()) {
		auto rest = [&] { return Span(cursor, source.end()); };

		switch (*cursor) {
			case ' ':
			case '\t':
			case '\r':
			case '\n': {
				++cursor;
				continue;
			}

			case '#' {
				++cursor;
				auto define_s = "define "s;
				if (starts_with(rest(), define_s)) {
					cursor += define_s.count;

					while (1) {
						if (cursor == source.end()) {
							return PreprocessError::unexpected_end_of_source;
						}
						
						switch (*cursor) {
							case ' ':
							case '\t':
								++cursor;
								continue;
							default:
								goto break_outer;
						}
					}
				break_outer:;

					auto name_start = cursor;
					while (1) {
						if (cursor == source.end()) {
							return PreprocessError::unexpected_end_of_source;
						}

						if (('a' <= * cursor && *cursor <= 'z')
							|| ('A' <= * cursor && *cursor <= 'Z')) {
							not_implemented();
						}
					}
				}
				break;
			}
		}
	}
}

}
}