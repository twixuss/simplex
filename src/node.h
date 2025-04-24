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

inline umm append(StringBuilder &builder, NodeKind kind) {
	switch (kind) {
#define x(name) case NodeKind::name: return append(builder, #name);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
	return 0;
}

inline volatile u32 uid_counter;

struct Node {
	u32 uid = atomic_add(&uid_counter, 1);
	NodeKind kind = {};
	String location;

	Node() {
		int x = 0;
	}
};
