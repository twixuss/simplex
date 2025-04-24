#include "mutability.h"

umm append(StringBuilder &builder, Mutability mutability) {
	switch (mutability) {
		case Mutability::constant: return append(builder, "const");
		case Mutability::readonly: return append(builder, "let");
		case Mutability::variable: return append(builder, "var");
	}
	return append_format(builder, "(unknown Mutability {})", (u32)mutability);
}

umm append(StringBuilder &builder, Meaning<Mutability> mutability) {
	switch (mutability.value) {
		case Mutability::constant: return append(builder, "constant");
		case Mutability::readonly: return append(builder, "read-only");
		case Mutability::variable: return append(builder, "variable");
	}
	return append_format(builder, "(unknown Mutability {})", (u32)mutability.value);
}

Optional<Mutability> to_mutability(TokenKind token_kind) {
	switch (token_kind) {
		case Token_const: return Mutability::constant;
		case Token_let:   return Mutability::readonly;
		case Token_var:   return Mutability::variable;
	}
	return {};
}
