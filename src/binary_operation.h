#pragma once
#include "common.h"
#include "x.h"
#include "nameable.h"
#include "token.h"

constexpr u32 custom_precedence = 4;

enum class BinaryOperation : u8 {
#define x(name, token, precedence) name,
	ENUMERATE_BINARY_OPERATIONS(x)
#undef x
};

umm append(StringBuilder &builder, BinaryOperation operation);
umm append(StringBuilder &builder, Nameable<BinaryOperation> op);

Optional<BinaryOperation> as_binary_operation(TokenKind kind);

bool is_ass(BinaryOperation op);
bool could_be_unary(BinaryOperation op);
bool is_right_associative(BinaryOperation operation);
u32 get_precedence(BinaryOperation operation);


enum class LowBinaryOperation : u8 {
	unknown,
	// 2s complement addition
	add8,
	add16,
	add32,
	add64,
	// 2s complement subtraction
	sub8,
	sub16,
	sub32,
	sub64,
	// 2s complement multiplication
	mul8,
	mul16,
	mul32,
	mul64,
	// unsigned division
	divu8,
	divu16,
	divu32,
	divu64,
	// signed division
	divs8,
	divs16,
	divs32,
	divs64,
	// binary xor
	bxor8,
	bxor16,
	bxor32,
	bxor64,
	// binary and
	band8,
	band16,
	band32,
	band64,
	// binary or
	bor8,
	bor16,
	bor32,
	bor64,
	// shift left
	bsl8,
	bsl16,
	bsl32,
	bsl64,
	// unsigned(logical) shift right
	bsru8,
	bsru16,
	bsru32,
	bsru64,
	// signed(arithmetic) shift right
	bsrs8,
	bsrs16,
	bsrs32,
	bsrs64,
	// equality
	equ8,
	equ16,
	equ32,
	equ64,
	// notequality
	neq8,
	neq16,
	neq32,
	neq64,
	// less than signed
	lts8,
	lts16,
	lts32,
	lts64,
	// greater than signed
	gts8,
	gts16,
	gts32,
	gts64,
	// less-equals signed
	les8,
	les16,
	les32,
	les64,
	// greater-equals signed
	ges8,
	ges16,
	ges32,
	ges64,
	// less than unsigned
	ltu8,
	ltu16,
	ltu32,
	ltu64,
	// greater than unsigned
	gtu8,
	gtu16,
	gtu32,
	gtu64,
	// less-equals unsigned
	leu8,
	leu16,
	leu32,
	leu64,
	// greater-equals unsigned
	geu8,
	geu16,
	geu32,
	geu64,
	// logical and with short circuiting
	land,
	// logical or with short circuiting
	lor,
};
