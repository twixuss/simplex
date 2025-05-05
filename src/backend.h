#pragma once
#include "common.h"
#include "bytecode/bytecode.h"

/*
#define x(ret, name, decls, defns, args)
ENUMERATE_BACKEND_API(x)
#undef x
*/
#define ENUMERATE_BACKEND_API(x) \
	x(void, init,             (CompilerContext *c), \
	                          (CompilerContext *c), \
	                          (c)) \
	x(bool, convert_ast,      (Block *global_block, Lambda *main_lambda, Definition *main_lambda_definition), \
	                          (Block *global_block, Lambda *main_lambda, Definition *main_lambda_definition), \
	                          (global_block, main_lambda, main_lambda_definition)) \
	x(bool, convert_bytecode, (Bytecode::Bytecode bytecode), \
	                          (Bytecode::Bytecode bytecode), \
	                          (bytecode)) \
