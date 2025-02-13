#pragma once
#include "../../bytecode/bytecode.h"

namespace target_x64 {
namespace b = Bytecode;
void emit(String target_executable_path, b::Bytecode bytecode);
}
