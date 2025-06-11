#pragma once
#include "common.h"

ReusableFiber get_new_fiber();
void add_fiber_to_reuse(ReusableFiber fiber);
