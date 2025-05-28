#pragma once
#include "token.h"

#include <tl/list.h>

#define LEXER_PADDING_SIZE 64

struct Lexer {
	u64 int_value = 0;
	f64 float_value = 0;
	GList<utf8> string_value;

	String source;
	utf8 *cursor = 0;

	// NOTE: WARNING:
	// padded_source must start and end with at least LEXER_PADDING_SIZE zero bytes.
	// They allow simd code to not do additional checks.
	static Lexer create(String padded_source);
	
	Token next_token();

	void print_invalid_character_error();
};
