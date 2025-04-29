#include "unary_operation.h"

void append(StringBuilder &builder, UnaryOperation operation) {
	switch (operation) {
		case UnaryOperation::pointer: return append(builder, '*');
		case UnaryOperation::dereference: return append(builder, '*');
	}
	switch (operation) {
#define x(name, token) case UnaryOperation::name: return append(builder, token);
#define y(name) case UnaryOperation::name: return append(builder, #name);
		ENUMERATE_UNARY_OPERATIONS(x, y)
#undef y
#undef x
	}
	return append_format(builder, "(unknown unary {})", (u32)operation);
}

Optional<UnaryOperation> as_unary_operation(TokenKind kind) {
	switch (kind) {
#define x(name, token) case (TokenKind)const_string_to_token_kind(token##s): return UnaryOperation::name;
#define y(name)
		ENUMERATE_UNARY_OPERATIONS(x, y)
#undef y
#undef x
	}
	return {};
}
