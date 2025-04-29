#pragma once
#include "common.h"
#include "meaning.h"
#include "token.h"

enum class Mutability : u8 {
	readonly,  // can not be modified by anyone.
	immutable, // can not be modified directly, can be modified by someone else (e.g. other thread)
	constant,  // known at compile time. can be casted to readonly
	variable,  // can be modified by anyone.
};

void append(StringBuilder &builder, Mutability const &mutability);
void append(StringBuilder &builder, Meaning<Mutability> const &mutability);

Optional<Mutability> to_mutability(TokenKind token_kind);
