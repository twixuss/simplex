#pragma once
#include "common.h"
#include "nodes_fwd.h"
#include "x.h"

template <class T>
struct Nameable {
	T value = {};
};

umm append(StringBuilder &builder, Nameable<String> str);

umm append(StringBuilder &builder, Nameable<Node *> expr);
#define x(name) umm append(StringBuilder &builder, Nameable<name *> expr);
ENUMERATE_NODE_KIND(x)
#undef x

inline umm append(StringBuilder &builder, Nameable<Expression *> node) {
	return append(builder, Nameable((Node *)node.value));
}
inline umm append(StringBuilder &builder, Nameable<Statement *> node) {
	return append(builder, Nameable((Node *)node.value));
}
