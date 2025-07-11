#pragma once
#include "common.h"
#include "node.h"
#include "nodes_fwd.h"
#include "token.h"
#include "mutability.h"
#include "unsized_types.h"

enum class BuiltinType : u8 {
	#define x(name) name,
	ENUMERATE_BUILTIN_TYPES(x)
	#undef x
	count,
};

void append(StringBuilder &builder, BuiltinType type_kind);
BuiltinType to_builtin_type_kind(TokenKind kind);

bool is_type(Expression *expression);

#if CHECK_THAT_TYPES_ARE_TYPES
struct Type {
	Expression *expression = 0;
	Type() = default;
	Type(Expression *expression);
	explicit Type(Node *node);
	operator Expression *() { return expression; }
	Expression *operator->() { return expression; }
	Expression &operator*() { return *expression; }
	template <class Expr>
	operator Expr*() {
		static_assert(std::is_same_v<Expr, Node> || CExpression<Expr>);
		return (Expr *)expression;
	}

	// auto operator&() {
	// 	struct Castable {
	// 		Type *type;
	// 		operator Type *() { return type; }
	// 		operator Node **() { return (Node **)&type->expression; }
	// 		operator Expression **() { return &type->expression; }
	// 	};
	// 
	// 	return Castable{this};
	// }
};
#else
using Type = Expression *;
#endif

void append(StringBuilder &builder, Type type);

// NOTE: Do not use this for types in the source code. These do not have a location.
BuiltinTypeName *get_builtin_type(BuiltinType kind);

Expression *direct(Expression *node);
Type direct(Type type);

template <class T>
T *direct_as(Expression *node) {
	node = direct(node);
	if (((Node *)node)->kind == NodeTypeToKind<T>::kind) {
		return (T *)node;
	}

	return 0;
}

struct CheckResult2 {
	bool result = {};
	Node *failed_node1 = {};
	Node *failed_node2 = {};

	CheckResult2(bool result) : result(result) {}
	CheckResult2(bool result, Node *failed_node1, Node *failed_node2) : result(result), failed_node1(failed_node1), failed_node2(failed_node2) {}

	operator bool() { return result; }
};

CheckResult2 types_match(Type a, Type b);
CheckResult2 types_match(Type a, BuiltinType b);
CheckResult2 types_match(BuiltinType a, Type b);

bool is_concrete_integer(Type type);
bool is_any_integer(Type type);
bool is_signed_integer(Type type);
bool is_unsigned_integer(Type type);
bool is_concrete(Type type);

bool is_concrete_float(Type type);

void propagate_concrete_type(Expression *expression, Type type);
void make_concrete(Expression *expression);

u64 get_size(BuiltinType type_kind);

u64 get_size(Type type);

enum class Sign : u8 {
	Unsigned,
	Signed
};
 
Sign get_sign(BuiltinType type_kind);

Unary *as_pointer(Type type);
Unary *as_some_enum(Expression *expression);
Expression *is_pointer_to_none_comparison(Expression *left, Expression *right);

std::pair<Lambda *, LambdaHead *> get_lambda_and_head(Expression *expression);
std::tuple<Lambda *, LambdaHead *, Struct *> get_lambda_and_head_or_struct(Expression *expression);
