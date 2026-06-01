#include "binary_operation.h"

void append(StringBuilder &builder, BinaryOperation operation) {
	switch (operation) {
		#define x(name, token, precedence, associativity) \
			case BinaryOperation::name:                   \
				return append(builder, token);
		ENUMERATE_BINARY_OPERATIONS(x)
		#undef x

		default:
			return append_format(builder, "BinaryOperation({})", (u32)operation);
	}
}

void append(StringBuilder &builder, Nameable<BinaryOperation> op) {
	switch (op.value) {
		#define x(name, token, precedence, associativity) \
			case BinaryOperation::name:                   \
				return append(builder, #name##s);
		ENUMERATE_BINARY_OPERATIONS(x)
		#undef x

		default:
			return append_format(builder, "BinaryOperation_{}", (u32)op.value);
	}
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
		case BinaryOperation::bsrass:
			return true;

		case BinaryOperation::dot:
		case BinaryOperation::as:
		case BinaryOperation::mul:
		case BinaryOperation::div:
		case BinaryOperation::mod:
		case BinaryOperation::add:
		case BinaryOperation::sub:
		case BinaryOperation::bor:
		case BinaryOperation::ban:
		case BinaryOperation::bxo:
		case BinaryOperation::bsl:
		case BinaryOperation::bsr:
		case BinaryOperation::equ:
		case BinaryOperation::neq:
		case BinaryOperation::les:
		case BinaryOperation::leq:
		case BinaryOperation::grt:
		case BinaryOperation::grq:
		case BinaryOperation::lan:
		case BinaryOperation::lor:
		case BinaryOperation::ran:
			return false;

		case BinaryOperation::count:
			break;
	}
	invalid_code_path("is_ass: invalid BinaryOperation {}", (u32)op);
	return {};
}

bool is_opass(BinaryOperation op) {
	switch (op) {
		case BinaryOperation::addass:
		case BinaryOperation::subass:
		case BinaryOperation::mulass:
		case BinaryOperation::divass:
		case BinaryOperation::modass:
		case BinaryOperation::borass:
		case BinaryOperation::banass:
		case BinaryOperation::bxoass:
		case BinaryOperation::bslass:
		case BinaryOperation::bsrass:
			return true;

		case BinaryOperation::ass:
		case BinaryOperation::dot:
		case BinaryOperation::as:
		case BinaryOperation::mul:
		case BinaryOperation::div:
		case BinaryOperation::mod:
		case BinaryOperation::add:
		case BinaryOperation::sub:
		case BinaryOperation::bor:
		case BinaryOperation::ban:
		case BinaryOperation::bxo:
		case BinaryOperation::bsl:
		case BinaryOperation::bsr:
		case BinaryOperation::equ:
		case BinaryOperation::neq:
		case BinaryOperation::les:
		case BinaryOperation::leq:
		case BinaryOperation::grt:
		case BinaryOperation::grq:
		case BinaryOperation::lan:
		case BinaryOperation::lor:
		case BinaryOperation::ran:
			return false;

		case BinaryOperation::count:
			break;
	}
	invalid_code_path("is_opass: invalid BinaryOperation {}", (u32)op);
	return {};
}
BinaryOperation deass(BinaryOperation op) {
	switch (op) {
		case BinaryOperation::addass: return BinaryOperation::add;
		case BinaryOperation::subass: return BinaryOperation::sub;
		case BinaryOperation::mulass: return BinaryOperation::mul;
		case BinaryOperation::divass: return BinaryOperation::div;
		case BinaryOperation::modass: return BinaryOperation::mod;
		case BinaryOperation::borass: return BinaryOperation::bor;
		case BinaryOperation::banass: return BinaryOperation::ban;
		case BinaryOperation::bxoass: return BinaryOperation::bxo;
		case BinaryOperation::bslass: return BinaryOperation::bsl;
		case BinaryOperation::bsrass: return BinaryOperation::bsr;

		case BinaryOperation::ass:
		case BinaryOperation::dot:
		case BinaryOperation::as:
		case BinaryOperation::mul:
		case BinaryOperation::div:
		case BinaryOperation::mod:
		case BinaryOperation::add:
		case BinaryOperation::sub:
		case BinaryOperation::bor:
		case BinaryOperation::ban:
		case BinaryOperation::bxo:
		case BinaryOperation::bsl:
		case BinaryOperation::bsr:
		case BinaryOperation::equ:
		case BinaryOperation::neq:
		case BinaryOperation::les:
		case BinaryOperation::leq:
		case BinaryOperation::grt:
		case BinaryOperation::grq:
		case BinaryOperation::lan:
		case BinaryOperation::lor:
		case BinaryOperation::ran:
			invalid_code_path("deass: can't remove assignment from binary operation {}", op);
			return {};
			
		case BinaryOperation::count:
			break;
	}
	invalid_code_path("deass: invalid BinaryOperation {}", (u32)op);
	return {};
}

bool could_be_unary(BinaryOperation op) {
	switch (op) {
		case BinaryOperation::add: // plus
		case BinaryOperation::sub: // minus
		case BinaryOperation::mul: // deref/pointer
		case BinaryOperation::ban: // addr
			return true;

		case BinaryOperation::addass:
		case BinaryOperation::subass:
		case BinaryOperation::mulass:
		case BinaryOperation::divass:
		case BinaryOperation::modass:
		case BinaryOperation::borass:
		case BinaryOperation::banass:
		case BinaryOperation::bxoass:
		case BinaryOperation::bslass:
		case BinaryOperation::bsrass:
		case BinaryOperation::ass:
		case BinaryOperation::dot:
		case BinaryOperation::as:
		case BinaryOperation::div:
		case BinaryOperation::mod:
		case BinaryOperation::bor:
		case BinaryOperation::bxo:
		case BinaryOperation::bsl:
		case BinaryOperation::bsr:
		case BinaryOperation::equ:
		case BinaryOperation::neq:
		case BinaryOperation::les:
		case BinaryOperation::leq:
		case BinaryOperation::grt:
		case BinaryOperation::grq:
		case BinaryOperation::lan:
		case BinaryOperation::lor:
		case BinaryOperation::ran:
			return false;
			
		case BinaryOperation::count:
			break;
	}
	invalid_code_path("deass: invalid BinaryOperation {}", (u32)op);
	return {};
}

u32 get_precedence(BinaryOperation operation) {
	switch (operation) {
		#define x(name, token, precedence, associativity) \
			case BinaryOperation::name:                   \
				return precedence;
		ENUMERATE_BINARY_OPERATIONS(x)
		#undef x

		case BinaryOperation::count:
			break;
	}
	invalid_code_path("get_precedence: invalid BinaryOperation {}", (u32)operation);
}

Optional<BinaryOperation> as_binary_operation(TokenKind kind) {
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wswitch"
	switch (kind) {
		#define x(name, token, precedence, associativity) \
			case (TokenKind)token##_t:              \
				return BinaryOperation::name;
		ENUMERATE_BINARY_OPERATIONS(x)
		#undef x

		default:
			return {};
	}
	#pragma GCC diagnostic pop
}

bool is_right_associative(BinaryOperation operation) {
	switch (operation) {
		#define x(name, token, precedence, associativity) \
			case BinaryOperation::name:                   \
				return associativity == '>';
		ENUMERATE_BINARY_OPERATIONS(x)
		#undef x

		case BinaryOperation::count:
			break;
	}
	invalid_code_path("is_right_associative: invalid BinaryOperation {}", (u32)operation);
}
