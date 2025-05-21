#include "nameable.h"
#include "nodes.h"

void append(StringBuilder &builder, Nameable<String> str) {
	char c = str.value[0];
	if (c == '_' || is_alpha(c)) {
		append(builder, c);
	} else {
		append(builder, '_');
	}
	for (auto c : str.value.skip(1)) {
		if (c == '_' || is_alpha((ascii)c) || is_digit((ascii)c)) {
			append(builder, c);
		} else {
			append(builder, '_');
		}
	}
}

void append(StringBuilder &builder, Nameable<Block *> expr) { not_implemented("Block"); }
void append(StringBuilder &builder, Nameable<Call *> expr) { not_implemented("Call"); }
void append(StringBuilder &builder, Nameable<Definition *> expr) { not_implemented("Definition"); }
void append(StringBuilder &builder, Nameable<IntegerLiteral *> expr) { not_implemented("IntegerLiteral"); }
void append(StringBuilder &builder, Nameable<BooleanLiteral *> expr) { not_implemented("BooleanLiteral"); }
void append(StringBuilder &builder, Nameable<NoneLiteral *> expr) { not_implemented("NoneLiteral"); }
void append(StringBuilder &builder, Nameable<StringLiteral *> expr) { not_implemented("StringLiteral"); }
void append(StringBuilder &builder, Nameable<Lambda *> expr) { not_implemented("Lambda"); }
void append(StringBuilder &builder, Nameable<LambdaHead *> expr) { not_implemented("LambdaHead"); }
void append(StringBuilder &builder, Nameable<Name *> name) { return append(builder, name.value->name); }
void append(StringBuilder &builder, Nameable<IfExpression *> expr) { not_implemented("IfExpression"); }
void append(StringBuilder &builder, Nameable<BuiltinTypeName *> name) { return append(builder, name.value->type_kind); }
void append(StringBuilder &builder, Nameable<Binary *> expr) { not_implemented("Binary"); }
void append(StringBuilder &builder, Nameable<Match *> expr) { not_implemented("Match"); }
void append(StringBuilder &builder, Nameable<Unary *> expr) { not_implemented("Unary"); }
void append(StringBuilder &builder, Nameable<Struct *> expr) {
	if (expr.value->definition)
		return append(builder, expr.value->definition->name);
	else
		return append_format(builder, "__{}", expr.value->uid);
}
void append(StringBuilder &builder, Nameable<Enum *> expr) {
	if (expr.value->definition)
		return append(builder, expr.value->definition->name);
	else
		return append_format(builder, "__{}", expr.value->uid);
}
void append(StringBuilder &builder, Nameable<ArrayType *> expr) { not_implemented("ArrayType"); }
void append(StringBuilder &builder, Nameable<Subscript *> expr) { not_implemented("Subscript"); }
void append(StringBuilder &builder, Nameable<ArrayConstructor *> expr) { not_implemented("ArrayConstructor"); }
void append(StringBuilder &builder, Nameable<ZeroInitialized *> expr) { not_implemented("ZeroInitialized"); }
void append(StringBuilder &builder, Nameable<IfStatement *> expr) { not_implemented("IfStatement"); }
void append(StringBuilder &builder, Nameable<Return *> expr) { not_implemented("Return"); }
void append(StringBuilder &builder, Nameable<While *> expr) { not_implemented("While"); }
void append(StringBuilder &builder, Nameable<Continue *> expr) { not_implemented("Continue"); }
void append(StringBuilder &builder, Nameable<Break *> expr) { not_implemented("Break"); }
void append(StringBuilder &builder, Nameable<Import *> expr) { not_implemented("Import"); }
void append(StringBuilder &builder, Nameable<Defer *> expr) { not_implemented("Defer"); }

void append(StringBuilder &builder, Nameable<Node *> node) {
	switch (node.value->kind) {
		#define x(name) case NodeKind::name: return append(builder, Nameable{(name *)node.value});
		ENUMERATE_NODE_KIND(x)
		#undef x
	}
	invalid_code_path();
}

