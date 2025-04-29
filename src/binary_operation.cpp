#include "binary_operation.h"

void append(StringBuilder &builder, BinaryOperation operation) {
	switch (operation) {
#define x(name, token, precedence) case BinaryOperation::name: return append(builder, token);
		ENUMERATE_BINARY_OPERATIONS(x)
#undef x
	}
	return append_format(builder, "(unknown binary {})", (u32)operation);
}

void append(StringBuilder &builder, Nameable<BinaryOperation> op) {
	switch (op.value) {
		#define x(name, token, precedence) case BinaryOperation::name: return append(builder, #name##s);
		ENUMERATE_BINARY_OPERATIONS(x)
		#undef x
	}
	return append_format(builder, "unknown_BinaryOperation_{}", (u32)op.value);
}

bool is_ass(BinaryOperation op) {
	switch (op) {
		case BinaryOperation::ass:
		case BinaryOperation::addass:
		case BinaryOperation::subass:
		case BinaryOperation::mulass:
		case BinaryOperation::divass:
		case BinaryOperation::modass:
		case BinaryOperation::borass:
		case BinaryOperation::banass:
		case BinaryOperation::bxoass:
		case BinaryOperation::bslass:
		case BinaryOperation::bsrass: {
			return true;
		}
	}
	return false;
}

bool could_be_unary(BinaryOperation op) {
	switch (op) {
		case BinaryOperation::add: // plus
		case BinaryOperation::sub: // minus
		case BinaryOperation::mul: // deref/pointer
		case BinaryOperation::ban: // addr
			return true;
	}
	return false;
}

u32 get_precedence(BinaryOperation operation) {
	switch (operation) {
#define x(name, token, precedence) case BinaryOperation::name: return precedence;
		ENUMERATE_BINARY_OPERATIONS(x)
#undef x
	}
	invalid_code_path();
}

Optional<BinaryOperation> as_binary_operation(TokenKind kind) {
	switch (kind) {
#define x(name, token, precedence) case (TokenKind)const_string_to_token_kind(token##s): return BinaryOperation::name;
		ENUMERATE_BINARY_OPERATIONS(x)
#undef x
	}
	return {};
}

bool is_right_associative(BinaryOperation operation) {
	return false;
}
