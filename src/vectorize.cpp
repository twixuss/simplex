#include "common.h"
#include "typechecker.h"
#include "nodes.h"

Node *Typechecker::vectorize_node(Node *node) {
	scoped_replace(debug_current_location, node->location);
	switch (node->kind) {
		#define x(name) case NodeKind::name: return vectorize_node_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
		#undef x
		default: invalid_code_path();
	}
}
Expression *Typechecker::vectorize_node(Expression *expression) {
	expression = as<Expression>(vectorize_node((Node *)expression));
	assert(expression);
	return expression;
}
Statement *Typechecker::vectorize_node(Statement *statement) {
	statement = as<Statement>(vectorize_node((Node *)statement));
	assert(statement);
	return statement;
}
Block                *Typechecker::vectorize_node_impl(Block *original) {
	auto result = Block::create();
	result->location = original->location;
	result->container = current_container;
	result->parent = current_block;
	scoped_replace(current_block, result);

	for (auto child : original->children) {
		result->add(vectorize_node(child));
	}
	if (result->children.count) {
		if (auto last_expr = as<Expression>(result->children.back())) {
			result->type = last_expr->type;
		}
	}

	return result;
}
Call                 *Typechecker::vectorize_node_impl(Call *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Definition           *Typechecker::vectorize_node_impl(Definition *original) {
	auto result = Definition::create();
	result->initial_value = vectorize_node(original->initial_value);
	result->container = current_container;
	result->name = original->name;

	if (original->parsed_type) {
		reporter.warning(original->parsed_type->location, "TODO: account for parsed_type");
	}

	vc.original_to_copy_definitions.get_or_insert(original) = result;

	result->type = result->initial_value->type;
	return result;
}
Expression           *Typechecker::vectorize_node_impl(IntegerLiteral *original) {
	return make_broadcast(original, vc.vector_size);
}
FloatLiteral         *Typechecker::vectorize_node_impl(FloatLiteral *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
BooleanLiteral       *Typechecker::vectorize_node_impl(BooleanLiteral *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
NoneLiteral          *Typechecker::vectorize_node_impl(NoneLiteral *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
StringLiteral        *Typechecker::vectorize_node_impl(StringLiteral *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Lambda               *Typechecker::vectorize_node_impl(Lambda *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
LambdaHead           *Typechecker::vectorize_node_impl(LambdaHead *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Name                 *Typechecker::vectorize_node_impl(Name *original) {
	if (original->possible_definitions.count == 1) {
		auto &original_definition = original->possible_definitions[0];

		// TODO: something better than linear search
		auto found_index = find_index_of(vc.original_lambda->head.parameters_block.definition_list, original_definition);

		if (found_index < vc.original_lambda->head.parameters_block.definition_list.count) {
			auto result = Name::create();
			auto instantiated_definition = vc.instantiated_lambda->head.parameters_block.definition_list[found_index];
			result->possible_definitions.set(instantiated_definition);
			result->type = instantiated_definition->type;
			result->location = original->location;
			result->name = original->name;
			return result;
		}
	}
	if (original->definition()) {
		auto result = Name::create();
		auto definition = *vc.original_to_copy_definitions.find(original->definition()).value;
		result->possible_definitions.set(definition);
		result->type = definition->type;
		result->location = original->location;
		result->name = original->name;
		return result;
	}

	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Expression           *Typechecker::vectorize_node_impl(IfExpression *original) {
	auto result = IfExpression::create();
	result->location = original->location;
	result->condition = vectorize_node(original->condition);
	result->true_branch = vectorize_node(original->true_branch);
	result->false_branch = vectorize_node(original->false_branch);
	result->type = result->true_branch->type;
	result->is_array = true;
	return result;
}
BuiltinTypeName      *Typechecker::vectorize_node_impl(BuiltinTypeName *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Expression           *Typechecker::vectorize_node_impl(Binary *original) {
	switch (original->operation) {
		case BinaryOperation::add:
		case BinaryOperation::mul: {
			auto result = Binary::create();
			result->operation = original->operation;
			result->left = vectorize_node(original->left);
			result->right = vectorize_node(original->right);
			return result;
		}
	}
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Match                *Typechecker::vectorize_node_impl(Match *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Unary                *Typechecker::vectorize_node_impl(Unary *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Struct               *Typechecker::vectorize_node_impl(Struct *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
ArrayType            *Typechecker::vectorize_node_impl(ArrayType *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Enum                 *Typechecker::vectorize_node_impl(Enum *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Subscript            *Typechecker::vectorize_node_impl(Subscript *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
ArrayConstructor     *Typechecker::vectorize_node_impl(ArrayConstructor *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
ZeroInitialized      *Typechecker::vectorize_node_impl(ZeroInitialized *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
CallerLocation       *Typechecker::vectorize_node_impl(CallerLocation *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
CallerArgumentString *Typechecker::vectorize_node_impl(CallerArgumentString *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
IfStatement          *Typechecker::vectorize_node_impl(IfStatement *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Return               *Typechecker::vectorize_node_impl(Return *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
While                *Typechecker::vectorize_node_impl(While *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
For                  *Typechecker::vectorize_node_impl(For *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Continue             *Typechecker::vectorize_node_impl(Continue *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Break                *Typechecker::vectorize_node_impl(Break *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Import               *Typechecker::vectorize_node_impl(Import *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Defer                *Typechecker::vectorize_node_impl(Defer *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Use                  *Typechecker::vectorize_node_impl(Use *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}