#pragma once
#include "common.h"
#include "nodes_fwd.h"
#include "x.h"

// Returns Empty if expression is constant.
// Otherwise returns first expression which is not constant.
Result<Empty, Expression *> is_constant(Expression *expression);

