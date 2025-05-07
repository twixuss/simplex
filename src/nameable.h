#pragma once
#include "common.h"
#include "nodes_fwd.h"
#include "x.h"
#include "type.h"

template <class T>
struct Nameable {
	T value = {};
};

void append(StringBuilder &builder, Nameable<String> str);

void append(StringBuilder &builder, Nameable<Node *> expr);
#define x(name) void append(StringBuilder &builder, Nameable<name *> expr);
ENUMERATE_NODE_KIND(x)
#undef x

inline void append(StringBuilder &builder, Nameable<Expression *> node) {
	append(builder, Nameable((Node *)node.value));
}
inline void append(StringBuilder &builder, Nameable<Statement *> node) {
	append(builder, Nameable((Node *)node.value));
}
inline void append(StringBuilder &builder, Nameable<Type> node) {
	append(builder, Nameable(node.value.expression));
}
