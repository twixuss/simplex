#pragma once
#include "common.h"
#include "nodes_fwd.h"
#include "x.h"

// Returns Empty if expression is mutable.
// Otherwise returns first expression which is not mutable.
Result<Empty, Expression *> is_mutable(Expression *expression);

