#pragma once
#include "token.h"

#include <tl/list.h>

struct Lexer {
	u64 int_value = 0;
	f64 float_value = 0;
	GList<utf8> string_value;

	String source;
	utf8 *cursor = 0;

	// NOTE: WARNING:
	// source.data[-1] must be '\0'
	// source.data[source.count] must be '\0'
	static Lexer create(String source);
	
	Token next_token();

	void print_invalid_character_error();
};
