#pragma once
#include "node.h"
#include "x.h"

#define ENABLE_NOTE_LEAK 0

struct Expression;
struct Statement;

#define x(name) struct name;
ENUMERATE_NODE_KIND(x)
#undef x
		
template <class T>
struct NodeTypeToKind;

#define x(name)                                                 \
	template <>                                                 \
	struct NodeTypeToKind<name> {                               \
		inline static constexpr NodeKind kind = NodeKind::name; \
	};
ENUMERATE_NODE_KIND(x)
#undef x

#if ENABLE_NOTE_LEAK

GList<String> leaks;

void note_leak(String expression, Node *node, String message = {}, std::source_location location = std::source_location::current());
#define NOTE_LEAK(node, ...) note_leak(u8#node##s, node, __VA_ARGS__)

#else

#define NOTE_LEAK(node, ...)

#endif
