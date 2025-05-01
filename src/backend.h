#pragma once
#include "common.h"
#include "bytecode/bytecode.h"

/*
#define x(ret, name, decls, defns, args)
ENUMERATE_BACKEND_API(x)
#undef x
*/
#define ENUMERATE_BACKEND_API(x) \
	x(void, init,             (CompilerContext *c),          (CompilerContext *c),          (c)) \
	x(void, convert_bytecode, (Bytecode::Bytecode bytecode), (Bytecode::Bytecode bytecode), (bytecode)) \
