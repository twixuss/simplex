#pragma once
#include "token.h"
#include "escape.h"

String enum_name(TokenKind k) {
	switch (k) {
		#define x(name) case Token_##name: return u8 ## #name ## s;
		#define y(name, value) x(name)
			ENUMERATE_TOKEN_KIND(x, y)
		#undef y
		#undef x
	}
	List<utf8, TemporaryAllocator> s;
	if (k >= 256) {
		s.add((utf8)(k >> 8));
	}
	s.add((utf8)k);
	return s;
}

void append(StringBuilder &builder, TokenKind kind) {
	switch (kind) {
		case Token_eof:    return append(builder, "end of file");
		case Token_eol:    return append(builder, "end of line");
		case Token_name:   return append(builder, "name");
		case Token_number: return append(builder, "number");
		case Token_string: return append(builder, "string");
	}

	if ((u32)kind <= 0xff)
		return append(builder, (char)kind);

	switch (kind) {
#define x(name) case Token_##name: return append(builder, #name);
#define y(name, value) x(name)
		ENUMERATE_TOKEN_KIND(x, y)
#undef y
#undef x
	}

	char buf[8];
	char *c = buf + sizeof(buf);
	while (kind) {
		*--c = kind;
		(u64 &)kind >>= 8;
	}

	return append(builder, Span(c, buf + sizeof(buf)));
	return append_format(builder, "(unknown TokenKind 0x{} \"{}\")", FormatInt{.value=(u64)kind, .radix=16}, as_chars(value_as_bytes(kind)));
}

void append(StringBuilder &builder, Token token) {
	switch (token.kind) {
		case Token_eof: return append(builder, "end of file");
		case Token_eol: return append(builder, "end of line");
	}

	return append(builder, token.string);
}

void print_token(umm i, Token token) {
	print("{}) {} ", i, enum_name(token.kind));

	switch (token.kind) {
		case Token_number:
		case Token_name: 
		case Token_directive: 
			print("\"{}\"", EscapedString{token.string}); 
			break;
		case Token_string: 
			print("{}", EscapedString{token.string}); 
			break;
	}

	println();
}
