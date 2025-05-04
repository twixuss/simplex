#pragma once
#include "common.h"
#include "nodes.h"

#define VISIT(node)                                      \
	do {                                                 \
		auto **x = node;                                 \
		if (visit((Node **)x, visitor) == ForEach_break) \
			return ForEach_break;                        \
	} while (0)

#define VISIT_STATIC(node)                                 \
	do {                                                   \
		auto x = &node;                                    \
		auto result = visit((Node **)&x, visitor);         \
		assert(x == &node, "Unexpected node replacement"); \
		if (result == ForEach_break)                       \
			return ForEach_break;                          \
	} while (0)

ForEachDirective visit(Node **node, auto &&visitor);
ForEachDirective visit_impl(Block *block, auto &&visitor) {
	for (auto &child : block->children) {
		VISIT(&child);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Return *ret, auto &&visitor) {
	if (ret->value) {
		VISIT(&ret->value);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(IfStatement *If, auto &&visitor) {
	VISIT(&If->condition);
	VISIT(&If->true_branch);
	if (If->false_branch)
		VISIT(&If->false_branch);
	return ForEach_continue;
}
ForEachDirective visit_impl(IfExpression *If, auto &&visitor) {
	VISIT(&If->condition);
	VISIT(&If->true_branch);
	VISIT(&If->false_branch);
	return ForEach_continue;
}
ForEachDirective visit_impl(Definition *definition, auto &&visitor) {
	if (definition->parsed_type) {
		VISIT(&definition->parsed_type);
	}
	if (definition->initial_value) {
		VISIT(&definition->initial_value);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Call *call, auto &&visitor) { 
	VISIT(&call->callable);
	for (auto &argument : call->arguments) {
		VISIT(&argument.expression);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Lambda *lambda, auto &&visitor) {
	VISIT_STATIC(lambda->head);
	if (lambda->body) {
		VISIT(&lambda->body);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(LambdaHead *head, auto &&visitor) {
	VISIT_STATIC(head->parameters_block);
	if (head->parsed_return_type) {
		VISIT(&head->parsed_return_type);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Binary *binary, auto &&visitor) {
	VISIT(&binary->left);
	VISIT(&binary->right);
	return ForEach_continue;
}
ForEachDirective visit_impl(Unary *unary, auto &&visitor) {
	VISIT(&unary->expression);
	return ForEach_continue;
}
ForEachDirective visit_impl(Match *match, auto &&visitor) {
	VISIT(&match->expression);
	for (auto &Case : match->cases) {
		if (Case.from) {
			VISIT(&Case.from);
		}
		VISIT(&Case.to);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(While *While, auto &&visitor) {
	VISIT(&While->condition);
	VISIT(&While->body);
	return ForEach_continue;
}
ForEachDirective visit_impl(Name *name, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(IntegerLiteral *literal, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(BooleanLiteral *literal, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(NoneLiteral *literal, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(StringLiteral  *literal, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(BuiltinTypeName *type_name, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(Continue *Continue, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(Break *Break, auto &&visitor) {
	if (Break->value) {
		VISIT(&Break->value);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Struct *Struct, auto &&visitor) {
	for (auto &member : Struct->members) {
		VISIT(&member);
	}
	return ForEach_continue; 
}
ForEachDirective visit_impl(ArrayType *arr, auto &&visitor) {
	VISIT(&arr->count_expression);
	VISIT(&arr->element_type);
	return ForEach_continue;
}
ForEachDirective visit_impl(Subscript *subscript, auto &&visitor) {
	VISIT(&subscript->subscriptable);
	VISIT(&subscript->index);
	return ForEach_continue;
}
ForEachDirective visit_impl(ArrayConstructor *arr, auto &&visitor) {
	for (auto &element : arr->elements) {
		VISIT(&element);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Import *import, auto &&visitor) {
	return ForEach_continue;
}
ForEachDirective visit_impl(Defer *defer_, auto &&visitor) {
	VISIT(&defer_->body);
	return ForEach_continue;
}
ForEachDirective visit_impl(ZeroInitialized *zi, auto &&visitor) {
	// VISIT(&zi->parsed_type);
	return ForEach_continue;
}

ForEachDirective visit(Node **node, auto &&visitor) {

	auto visitor_wrapper = [&] (auto node) {
		if constexpr (requires { { visitor(*node) }; }) {
			if constexpr (requires { { visitor(*node) } -> std::same_as<ForEachDirective>; }) {
				if (visitor(*node) == ForEach_break)
					return ForEach_break;
			} else if constexpr (std::is_convertible_v<decltype(visitor(*node)), Node *>) {
				// Sketchy
				*(Node **)node = visitor(*node);
			} else if constexpr (requires { { visitor(*node) } -> std::same_as<void>; }) {
				visitor(*node);
			} else {
				static_error_v(node, "Unexpected return type in visitor");
			}
		} else {
			static_error_v(node, "Unexpected argument type in visitor");
		}
		return ForEach_continue;
	};

	// Visit the actual node. It might be replaced, so do two separate switches.
	switch ((*node)->kind) {
		#define x(name)                                              \
			case NodeKind::name:                                     \
				if (visitor_wrapper((name **)node) == ForEach_break) \
					return ForEach_break;                            \
				break;

		ENUMERATE_NODE_KIND(x)
		#undef x
		default: invalid_code_path();
	}

	switch ((*node)->kind) {
		#define x(name) case NodeKind::name: return visit_impl((name *)*node, visitor);
		ENUMERATE_NODE_KIND(x)
		#undef x
		default: invalid_code_path();
	}
	return ForEach_continue;
}
ForEachDirective visit(Node *node, auto &&visitor) {
	auto old_node = node;
	auto result = visit(&node, visitor);
	assert(node == old_node);
	return result;
}

#undef VISIT
#undef VISITOR

// Does not recurse
auto visit_one(Node *node, auto &&visitor) {
	switch (node->kind) {
		#define x(name) case NodeKind::name: return visitor((name *)node);
		ENUMERATE_NODE_KIND(x)
		#undef x
	}
	invalid_code_path();
}
// Does not recurse
auto visit_one(Expression *expression, auto &&visitor) {
	switch (expression->kind) {
		#define x(name) case NodeKind::name: return visitor((name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
		#undef x
	}
	invalid_code_path();
}
// Does not recurse
auto visit_one(Statement *statement, auto &&visitor) {
	switch (statement->kind) {
		#define x(name) case NodeKind::name: return visitor((name *)statement);
		ENUMERATE_STATEMENT_KIND(x)
		#undef x
	}
	invalid_code_path();
}
