#pragma once
#include "common.h"
#include "x.h"

enum class NodeKind : u8 {
	unknown,
#define x(name) name,
	ENUMERATE_NODE_KIND(x)
#undef x
	count,
};

inline void append(StringBuilder &builder, NodeKind kind) {
	switch (kind) {
#define x(name) case NodeKind::name: return append(builder, #name);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
	return append_format(builder, "((NodeKind){})", (u64)kind);
}

inline volatile u32 uid_counter;

struct Node {
	u32 uid = atomic_add(&uid_counter, 1);
	NodeKind kind = {};
	String location;

	Node();
};
