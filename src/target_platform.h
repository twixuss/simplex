#pragma once
#include "nodes_fwd.h"

struct TargetPlatform {
	void *(*create_bridge)(Lambda *lambda);
};
