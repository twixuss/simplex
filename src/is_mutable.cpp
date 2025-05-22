#include "is_mutable.h"
#include "nodes.h"
#include "debug.h"

#define MUST_BE_MUTABLE(node) \
	if (auto _ = is_mutable(node); !_) \
		return _

Result<Empty, Expression *> is_mutable(Expression *expression);
Result<Empty, Expression *> is_mutable_impl(Block *block) { 
	if (block->children.count != 0) {
		if (auto expression = as<Expression>(block->children.back())) {
			MUST_BE_MUTABLE(expression);
			return Empty{};
		}
	}
	return block;
}
Result<Empty, Expression *> is_mutable_impl(Definition *definition) {
	if (definition->mutability == Mutability::variable)
		return Empty{};
	return definition;
}
Result<Empty, Expression *> is_mutable_impl(IntegerLiteral *literal) { return literal; }
Result<Empty, Expression *> is_mutable_impl(BooleanLiteral *literal) { return literal; }
Result<Empty, Expression *> is_mutable_impl(NoneLiteral *literal) { return literal; }
Result<Empty, Expression *> is_mutable_impl(StringLiteral *literal) { return literal; }
Result<Empty, Expression *> is_mutable_impl(Lambda *lambda) { return lambda; }
Result<Empty, Expression *> is_mutable_impl(LambdaHead *head) { return head; }
Result<Empty, Expression *> is_mutable_impl(Name *name) { 
	auto definition = name->definition();
	assert(definition);
	return is_mutable_impl(definition);
}
Result<Empty, Expression *> is_mutable_impl(Call *call) { return call; }
Result<Empty, Expression *> is_mutable_impl(IfExpression *If) { return If; }
Result<Empty, Expression *> is_mutable_impl(BuiltinTypeName *type) { return type; }
Result<Empty, Expression *> is_mutable_impl(Binary *binary) {
	if (binary->operation == BinaryOperation::dot) {
		if (auto pointer = direct_as<Unary>(binary->left->type); pointer && pointer->operation == UnaryOperation::pointer) {
			if (pointer->mutability == Mutability::variable) {
				return Empty{};
			}
		} else {
			return is_mutable(binary->left);
		}
	}
	return binary;
}
Result<Empty, Expression *> is_mutable_impl(Match *match) { return match; }
Result<Empty, Expression *> is_mutable_impl(Unary *unary) {
	if (unary->operation == UnaryOperation::dereference) {
		auto pointer = as_pointer(unary->expression->type);
		assert(pointer);
		if (pointer->mutability == Mutability::variable) {
			return Empty{};
		}
	}

	return unary;
}
Result<Empty, Expression *> is_mutable_impl(Struct *Struct) { return Struct; }
Result<Empty, Expression *> is_mutable_impl(Enum *Enum) { return Enum; }
Result<Empty, Expression *> is_mutable_impl(ArrayType *Array) { return Array; }
Result<Empty, Expression *> is_mutable_impl(Subscript *Subscript) { return is_mutable(Subscript->subscriptable); }
Result<Empty, Expression *> is_mutable_impl(ArrayConstructor *Array) { return Array; }
Result<Empty, Expression *> is_mutable_impl(ZeroInitialized *zi) { return zi; }
Result<Empty, Expression *> is_mutable_impl(CallerLocation *node) { return node; }
Result<Empty, Expression *> is_mutable_impl(CallerArgumentString *node) { return node; }
Result<Empty, Expression *> is_mutable(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return is_mutable_impl((name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path();
}

#undef MUST_BE_MUTABLE
