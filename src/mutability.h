#pragma once
#include <tl/string.h>
#include "meaning.h"

using namespace tl;

enum class Mutability : u8 {
	readonly,  // can not be modified by anyone.
	immutable, // can not be modified directly, can be modified by someone else (e.g. other thread)
	constant,  // known at compile time. can be casted to readonly
	variable,  // can be modified by anyone.
};

inline umm append(StringBuilder &builder, Mutability mutability) {
	switch (mutability) {
		case Mutability::constant: return append(builder, "const");
		case Mutability::readonly: return append(builder, "let");
		case Mutability::variable: return append(builder, "var");
	}
	return append_format(builder, "(unknown Mutability {})", (u32)mutability);
}

inline umm append(StringBuilder &builder, Meaning<Mutability> mutability) {
	switch (mutability.value) {
		case Mutability::constant: return append(builder, "constant");
		case Mutability::readonly: return append(builder, "read-only");
		case Mutability::variable: return append(builder, "variable");
	}
	return append_format(builder, "(unknown Mutability {})", (u32)mutability.value);
}
