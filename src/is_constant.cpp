#include "is_constant.h"
#include "nodes.h"
#include "reporter.h"
#include "debug.h"

#define MUST_BE_CONSTANT(node) \
	if (auto _ = is_constant(node); !_) \
		return _

Result<Empty, Expression *> is_constant_impl(Block *block) {
	if (block->children.count != 1) {
		immediate_reporter.error(block->location, "is_constant_impl: not implemented for blocks with more that one expression");
		invalid_code_path();
	}

	auto last_expression = as<Expression>(block->children.back());
	assert(last_expression, "not implemented");

	return is_constant(last_expression);
}
Result<Empty, Expression *> is_constant_impl(Definition *definition) {
	if (definition->mutability == Mutability::constant)
		return Empty{};

	return definition;
}
Result<Empty, Expression *> is_constant_impl(IntegerLiteral *literal) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(BooleanLiteral *literal) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(NoneLiteral *literal) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(StringLiteral *literal) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(Lambda *lambda) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(LambdaHead *head) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(Name *name) { 
	auto definition = name->definition();
	assert(definition);
	return is_constant_impl(definition);
}
Result<Empty, Expression *> is_constant_impl(Call *call) { 
	MUST_BE_CONSTANT(call->callable);

	for (auto argument : call->arguments) {
		MUST_BE_CONSTANT(argument.expression);
	}

	return Empty{};
}
Result<Empty, Expression *> is_constant_impl(IfExpression *If) { 
	MUST_BE_CONSTANT(If->condition);
	MUST_BE_CONSTANT(If->true_branch);
	MUST_BE_CONSTANT(If->false_branch);
	return Empty{};
}
Result<Empty, Expression *> is_constant_impl(BuiltinTypeName *type) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(Binary *binary) {
	MUST_BE_CONSTANT(binary->left);
	MUST_BE_CONSTANT(binary->right);
	return Empty{};
}
Result<Empty, Expression *> is_constant_impl(Match *match) {
	MUST_BE_CONSTANT(match->expression);
	for (auto &Case : match->cases) {
		if (Case.to_expression()) {
			MUST_BE_CONSTANT(Case.to_expression());
		}
	}

	return Empty{};
}
Result<Empty, Expression *> is_constant_impl(Unary *unary) { 
	return is_constant(unary->expression);
}
Result<Empty, Expression *> is_constant_impl(Struct *) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(Enum *) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(ArrayType *) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(Subscript *node) {
	MUST_BE_CONSTANT(node->subscriptable);
	MUST_BE_CONSTANT(node->index);
	return Empty{};
}
Result<Empty, Expression *> is_constant_impl(ArrayConstructor *node) {
	for (auto element : node->elements) {
		MUST_BE_CONSTANT(element);
	}
	return Empty{};
}
Result<Empty, Expression *> is_constant_impl(ZeroInitialized *) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(CallerLocation *) { return Empty{}; }
Result<Empty, Expression *> is_constant_impl(CallerArgumentString *) { return Empty{}; }
Result<Empty, Expression *> is_constant(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return is_constant_impl((name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path();
}

#undef MUST_BE_CONSTANT
