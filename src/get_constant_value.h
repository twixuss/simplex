#pragma once
#include "common.h"
#include "unsized_types.h"
#include "nodes_fwd.h"
#include "x.h"

struct Value;

#define x(name) Result<Value, Expression *> get_constant_value(name *expression);
ENUMERATE_NODE_KIND(x)
#undef x

Result<Value, Expression *> get_constant_value(Expression *expression);
Result<UnsizedInteger, Expression *> get_constant_integer(Expression *expression);
