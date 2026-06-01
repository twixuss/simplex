#pragma once
#include "common.h"

/*
#define x(name)
ENUMERATE_LOW_BINARY_OPERATIONS(x)
#undef x
*/
#define ENUMERATE_LOW_BINARY_OPERATIONS(x) \
	/* 2s complement addition */ \
	x(add8) \
	x(add16) \
	x(add32) \
	x(add64) \
	/* 2s complement subtraction */ \
	x(sub8) \
	x(sub16) \
	x(sub32) \
	x(sub64) \
	/* 2s complement multiplication */ \
	x(mul8) \
	x(mul16) \
	x(mul32) \
	x(mul64) \
	/* unsigned division */ \
	x(divu8) \
	x(divu16) \
	x(divu32) \
	x(divu64) \
	/* signed division */ \
	x(divs8) \
	x(divs16) \
	x(divs32) \
	x(divs64) \
	/* unsigned modision */ \
	x(modu8) \
	x(modu16) \
	x(modu32) \
	x(modu64) \
	/* signed modision */ \
	x(mods8) \
	x(mods16) \
	x(mods32) \
	x(mods64) \
	/* binary xor */ \
	x(bxor8) \
	x(bxor16) \
	x(bxor32) \
	x(bxor64) \
	/* binary and */ \
	x(band8) \
	x(band16) \
	x(band32) \
	x(band64) \
	/* binary or */ \
	x(bor8) \
	x(bor16) \
	x(bor32) \
	x(bor64) \
	/* shift left */ \
	x(bsl8) \
	x(bsl16) \
	x(bsl32) \
	x(bsl64) \
	/* unsigned(logical) shift right */ \
	x(bsru8) \
	x(bsru16) \
	x(bsru32) \
	x(bsru64) \
	/* signed(arithmetic) shift right */ \
	x(bsrs8) \
	x(bsrs16) \
	x(bsrs32) \
	x(bsrs64) \
	/* equality */ \
	x(equ8) \
	x(equ16) \
	x(equ32) \
	x(equ64) \
	/* notequality */ \
	x(neq8) \
	x(neq16) \
	x(neq32) \
	x(neq64) \
	/* less than signed */ \
	x(lts8) \
	x(lts16) \
	x(lts32) \
	x(lts64) \
	/* greater than signed */ \
	x(gts8) \
	x(gts16) \
	x(gts32) \
	x(gts64) \
	/* less-equals signed */ \
	x(les8) \
	x(les16) \
	x(les32) \
	x(les64) \
	/* greater-equals signed */ \
	x(ges8) \
	x(ges16) \
	x(ges32) \
	x(ges64) \
	/* less than unsigned */ \
	x(ltu8) \
	x(ltu16) \
	x(ltu32) \
	x(ltu64) \
	/* greater than unsigned */ \
	x(gtu8) \
	x(gtu16) \
	x(gtu32) \
	x(gtu64) \
	/* less-equals unsigned */ \
	x(leu8) \
	x(leu16) \
	x(leu32) \
	x(leu64) \
	/* greater-equals unsigned */ \
	x(geu8) \
	x(geu16) \
	x(geu32) \
	x(geu64) \
	/* logical and with short circuiting */ \
	x(land) \
	/* logical or with short circuiting */ \
	x(lor) \
	/* zero extension */ \
	x(zex8to16) \
	x(zex8to32) \
	x(zex16to32) \
	x(zex8to64) \
	x(zex16to64) \
	x(zex32to64) \
	/* sign extension */ \
	x(sex8to16) \
	x(sex8to32) \
	x(sex16to32) \
	x(sex8to64) \
	x(sex16to64) \
	x(sex32to64) \
	/* memory comparisons */ \
	x(memcmp_equ) \
	x(memcmp_neq) \
	/* output `get_size(binary->right)` zero bytes */ \
	x(zeroinit) \
	/* output binary->left != 0 */ \
	x(left_to_bool) \
	/* output binary->right != 0 */ \
	x(right_to_bool) \
	/* output binary->left == 0 */ \
	x(left_to_bool_not) \
	/* output binary->right == 0 */ \
	x(right_to_bool_not) \
	/* output binary->left */ \
	x(left) \
	\
	x(u8_to_f32) \
	x(u16_to_f32) \
	x(u32_to_f32) \
	x(u64_to_f32) \
	x(s8_to_f32) \
	x(s16_to_f32) \
	x(s32_to_f32) \
	x(s64_to_f32) \
	\
	x(u8_to_f64) \
	x(u16_to_f64) \
	x(u32_to_f64) \
	x(u64_to_f64) \
	x(s8_to_f64) \
	x(s16_to_f64) \
	x(s32_to_f64) \
	x(s64_to_f64) \
	\
	x(f32_to_u8) \
	x(f32_to_u16) \
	x(f32_to_u32) \
	x(f32_to_u64) \
	x(f32_to_s8) \
	x(f32_to_s16) \
	x(f32_to_s32) \
	x(f32_to_s64) \
	\
	x(f64_to_u8) \
	x(f64_to_u16) \
	x(f64_to_u32) \
	x(f64_to_u64) \
	x(f64_to_s8) \
	x(f64_to_s16) \
	x(f64_to_s32) \
	x(f64_to_s64) \
	\
	x(f32_to_f64) \
	x(f64_to_f32) \

enum class LowBinaryOperation : u8 {
	#define x(name) \
		name,
	ENUMERATE_LOW_BINARY_OPERATIONS(x)
	#undef x

	count,
};

inline void append(StringBuilder &builder, LowBinaryOperation op) {
	switch (op) {
		#define x(name)                    \
			case LowBinaryOperation::name: \
				return append(builder, #name##s);
		ENUMERATE_LOW_BINARY_OPERATIONS(x)
		#undef x

		case LowBinaryOperation::count:
			break;
	}

	return append_format(builder, "LowBinaryOperation({})", (u64)op);
}
