#pragma once
#include "token.h"
#include "reporter.h"

#include <tl/list.h>
#include <tl/reusable_fiber.h>

#define LEXER_PADDING_SIZE 64

struct Lexer {
	u64 int_value = 0;
	f64 float_value = 0;
	GList<utf8> string_value;

	String source;
	utf8 *cursor = 0;

	Fiber parent_fiber;
	ReusableFiber current_fiber;

	Reporter *reporter = 0;

	// NOTE: WARNING:
	// padded_source must start and end with at least LEXER_PADDING_SIZE zero bytes.
	// They allow simd code to not do additional checks.
	static Lexer create(String source, Reporter *reporter, Fiber parent_fiber, ReusableFiber current_fiber);
	
	Token next_token();
};
