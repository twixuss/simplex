#pragma once
#include "common.h"
#include "x.h"

// String TO 64-bit.
constexpr u64 sto64(Span<char> x) {
	u64 r = 0;
	for (umm i = 0; i < x.count; ++i)
		r ^= (u64)x.data[i] << ((i * 8) & 63);
	return r;
}

constexpr u64 operator""_t(char const *string, umm length) {
	return sto64({string, length});
}

enum TokenKind : u64 {
	#define x(name) Token_##name = #name##_t,
	#define y(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KIND(x, y)
	#undef y
	#undef x
};

String enum_name(TokenKind k);

void append(StringBuilder &builder, TokenKind kind);

struct Token {
	TokenKind kind = {};
	String string;
};

void append(StringBuilder &builder, Token token);

void print_token(umm i, Token token);
