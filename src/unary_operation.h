#pragma once
#include "common.h"
#include "x.h"
#include "token.h"

enum class UnaryOperation : u8 {
#define x(name, token) name,
#define y(name) name,
	ENUMERATE_UNARY_OPERATIONS(x, y)
#undef y
#undef x
};

void append(StringBuilder &builder, UnaryOperation operation);

Optional<UnaryOperation> as_unary_operation(TokenKind kind);
