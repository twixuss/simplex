#pragma once
#include "common.h"
#include "x.h"
#include "nameable.h"
#include "token.h"

constexpr u32 custom_precedence = 4;

enum class BinaryOperation : u8 {
	unknown,
#define x(name, token, precedence) name,
	ENUMERATE_BINARY_OPERATIONS(x)
#undef x
	_count_plus_one,
	count = _count_plus_one - 1,
};

void append(StringBuilder &builder, BinaryOperation operation);
void append(StringBuilder &builder, Nameable<BinaryOperation> op);

Optional<BinaryOperation> as_binary_operation(TokenKind kind);

bool is_ass(BinaryOperation op);
bool is_modass(BinaryOperation op);
BinaryOperation deass(BinaryOperation op);
bool could_be_unary(BinaryOperation op);
bool is_right_associative(BinaryOperation operation);
u32 get_precedence(BinaryOperation operation);
