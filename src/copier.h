#pragma once
#include "common.h"
#include "nodes_fwd.h"

struct Copier {
	HashMap<Node *, Node *> copied_nodes;

	template <class T>
	[[nodiscard]] void copy_base(T *from, T *to) {
		copied_nodes.get_or_insert(from) = to;
		to->location = from->location;
	}
	template <class T>
	[[nodiscard]] T *copy_base(T *from) {
		auto to = T::create();
		copy_base(from, to);
		return to;
	}
	
	template <class T>
	[[nodiscard]] void deep_copy(T *from, T *to) {
		copy_base(from, to);
		deep_copy_impl(from, to);
		if (auto to_expression = as<Expression>(to)) {
			auto from_expression = as<Expression>(from);
			assert(from_expression);
			assert((bool)from_expression->type == (bool)to_expression->type);
		}
	}
	template <class T>
	[[nodiscard]] T *deep_copy(T *from) {
		auto to = T::create();
		deep_copy(from, to);
		return to;
	}

	[[nodiscard]] Node *deep_copy(Node *from);
	[[nodiscard]] Expression *deep_copy(Expression *from);
	[[nodiscard]] Statement *deep_copy(Statement *from);
	[[nodiscard]] CallArgument deep_copy(CallArgument from);

	#define x(name) void deep_copy_impl(name *from, name *to);
	ENUMERATE_NODE_KIND(x)
	#undef x
	void deep_copy_impl(CallArgument *from, CallArgument *to);
};
