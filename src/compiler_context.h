#pragma once
#include "nodes.h"
#include "type.h"
#include "builtin_structs.h"

struct CompilerContext : CompilerContextBase {
	LockProtected<Block, RecursiveSpinLock> global_block;

	BuiltinStructs builtin_structs;
	BuiltinTypeName builtin_types[(u32)BuiltinType::count];
};

inline LockProtected<Block, RecursiveSpinLock> *get_global_block() { return &context->global_block; }
inline Block *get_global_block_unprotected() { return &context->global_block.unprotected; }
