#pragma once
#include "nodes.h"
#include "type.h"
#include "builtin_structs.h"

struct CompilerContext : CompilerContextBase {
	// TODO: make LockProtected
	Block global_block;
	SpinLock global_block_lock;

	BuiltinStructs builtin_structs;
	BuiltinTypeName builtin_types[(u32)BuiltinType::count];
};

inline Block *get_global_block() { return &context->global_block; }
inline SpinLock *get_global_block_lock() { return &context->global_block_lock; }
