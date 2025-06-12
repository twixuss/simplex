#pragma once
#include "common.h"

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
	// unsigned modision
	modu8,
	modu16,
	modu32,
	modu64,
	// signed modision
	mods8,
	mods16,
	mods32,
	mods64,
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
	// zero extension
	zex8to16,
	zex8to32,
	zex16to32,
	zex8to64,
	zex16to64,
	zex32to64,
	// sign extension
	sex8to16,
	sex8to32,
	sex16to32,
	sex8to64,
	sex16to64,
	sex32to64,
	// memory comparisons
	memcmp_equ,
	memcmp_neq,
	// output `get_size(binary->right)` zero bytes
	zeroinit,
	// output binary->left != 0
	left_to_bool,
	// output binary->right != 0
	right_to_bool,
	// output binary->left == 0
	left_to_bool_not,
	// output binary->right == 0
	right_to_bool_not,
	// output binary->left
	left,

	u8_to_f32,
	u16_to_f32,
	u32_to_f32,
	u64_to_f32,
	s8_to_f32,
	s16_to_f32,
	s32_to_f32,
	s64_to_f32,

	u8_to_f64,
	u16_to_f64,
	u32_to_f64,
	u64_to_f64,
	s8_to_f64,
	s16_to_f64,
	s32_to_f64,
	s64_to_f64,

	f32_to_u8,
	f32_to_u16,
	f32_to_u32,
	f32_to_u64,
	f32_to_s8,
	f32_to_s16,
	f32_to_s32,
	f32_to_s64,

	f64_to_u8,
	f64_to_u16,
	f64_to_u32,
	f64_to_u64,
	f64_to_s8,
	f64_to_s16,
	f64_to_s32,
	f64_to_s64,

	f32_to_f64,
	f64_to_f32,
};
