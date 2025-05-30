#pragma once
#include "bytecode.h"

namespace Bytecode {

struct PackedInstructions {
	GList<Instruction> instructions;
	GList<umm> old_to_new;
	GList<umm> new_to_old;
};

PackedInstructions optimize(Span<Instruction> instructions);

}
