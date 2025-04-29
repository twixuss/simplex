#pragma once
#include "common.h"
#include "x.h"

consteval u64 const_string_to_token_kind(Span<char> string) {
	assert(string.count <= 8);
	u64 result = 0;
	for (umm i = string.count - 1; i != -1; --i) {
		result <<= 8;
		result |= string.data[i];
	}
	return result;
}
consteval u64 const_string_to_token_kind(char a, char b) {
	char buffer[] { a, b };
	return const_string_to_token_kind(array_as_span(buffer));
}
consteval u64 const_string_to_token_kind(char a, char b, char c) {
	char buffer[] { a, b, c };
	return const_string_to_token_kind(array_as_span(buffer));
}

enum TokenKind : u64 {
	#define x(name) Token_##name = const_string_to_token_kind(#name##s),
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
