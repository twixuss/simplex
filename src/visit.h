#pragma once
#include "common.h"

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
ForEachDirective visit_impl(Block **node, auto &&visitor) {
	auto block = *node;
	for (auto &child : block->children) {
		VISIT(&child);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Return **node, auto &&visitor) {
	auto ret = *node;
	if (ret->value) {
		VISIT(&ret->value);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(IfStatement **node, auto &&visitor) {
	auto If = *node;
	VISIT(&If->condition);
	VISIT(&If->true_branch);
	if (If->false_branch)
		VISIT(&If->false_branch);
	return ForEach_continue;
}
ForEachDirective visit_impl(IfExpression **node, auto &&visitor) {
	auto If = *node;
	VISIT(&If->condition);
	VISIT(&If->true_branch);
	VISIT(&If->false_branch);
	return ForEach_continue;
}
ForEachDirective visit_impl(Definition **node, auto &&visitor) {
	auto definition = *node;
	if (definition->parsed_type) {
		VISIT(&definition->parsed_type);
	}
	if (definition->initial_value) {
		VISIT(&definition->initial_value);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Call **node, auto &&visitor) { 
	auto call = *node;
	VISIT(&call->callable);
	for (auto &argument : call->arguments) {
		VISIT(&argument.expression);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Lambda **node, auto &&visitor) {
	auto lambda = *node;
	VISIT_STATIC(lambda->head);
	if (lambda->body) {
		VISIT(&lambda->body);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(LambdaHead **node, auto &&visitor) {
	auto head = *node;
	VISIT_STATIC(head->parameters_block);
	if (head->parsed_return_type) {
		VISIT(&head->parsed_return_type);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Binary **node, auto &&visitor) {
	auto binary = *node;
	VISIT(&binary->left);
	VISIT(&binary->right);
	return ForEach_continue;
}
ForEachDirective visit_impl(Unary **node, auto &&visitor) {
	auto unary = *node;
	VISIT(&unary->expression);
	return ForEach_continue;
}
ForEachDirective visit_impl(Match **node, auto &&visitor) {
	auto match = *node;
	VISIT(&match->expression);
	for (auto &Case : match->cases) {
		if (Case.from) {
			VISIT(&Case.from);
		}
		VISIT(&Case.to);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(While **node, auto &&visitor) {
	auto While = *node;
	VISIT(&While->condition);
	VISIT(&While->body);
	return ForEach_continue;
}
ForEachDirective visit_impl(Name **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(IntegerLiteral **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(BooleanLiteral **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(NoneLiteral **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(StringLiteral  **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(BuiltinTypeName **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(Continue **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(Break **node, auto &&visitor) {
	auto Break = *node;
	if (Break->value) {
		VISIT(&Break->value);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Struct **node, auto &&visitor) {
	auto Struct = *node;
	for (auto &member : Struct->members) {
		VISIT(&member);
	}
	return ForEach_continue; 
}
ForEachDirective visit_impl(ArrayType **node, auto &&visitor) {
	auto arr = *node;
	VISIT(&arr->count_expression);
	VISIT(&arr->element_type);
	return ForEach_continue;
}
ForEachDirective visit_impl(Subscript **node, auto &&visitor) {
	auto subscript = *node;
	VISIT(&subscript->subscriptable);
	VISIT(&subscript->index);
	return ForEach_continue;
}
ForEachDirective visit_impl(ArrayConstructor **node, auto &&visitor) {
	auto arr = *node;
	for (auto &element : arr->elements) {
		VISIT(&element);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Import **node, auto &&visitor) {
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
#define x(name) case NodeKind::name: return visit_impl((name **)node, visitor);
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
