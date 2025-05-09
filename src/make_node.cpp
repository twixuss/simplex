#include "make_node.h"
#include "builtin_structs.h"
#include "compiler_context.h"

Type make_pointer(Type type, Mutability mutability) {
	auto pointer = Unary::create();
	pointer->expression = type;
	pointer->operation = UnaryOperation::pointer;
	pointer->mutability = mutability;
	pointer->type = get_builtin_type(BuiltinType::Type);
	return pointer;
}

BuiltinTypeName *make_name(BuiltinType type, String location) {
	auto name = BuiltinTypeName::create();
	name->type_kind = type;
	name->location = location;
	name->type = get_builtin_type(BuiltinType::Type);
	return name;
}
Name *make_name(Definition *definition, String location) {
	auto name = Name::create();
	name->possible_definitions.set(definition);
	name->location = location;
	name->type = definition->type;
	name->name = definition->name;
	return name;
}

IntegerLiteral *make_integer(UnsizedInteger value, String location, Type type) {
	auto result = IntegerLiteral::create();
	result->value = value;
	result->type = type;
	result->location = location;
	return result;
}
#if UNSIZED_INTEGER_BITS != 64
IntegerLiteral *make_integer(u64 value, String location, Type type) {
	return make_integer(convert<UnsizedInteger>(value), location, type);
}
#endif

BooleanLiteral *make_boolean(bool value, String location) {
	auto result = BooleanLiteral::create();
	result->value = value;
	result->type = get_builtin_type(BuiltinType::Bool);
	result->location = location;
	return result;
}
StringLiteral *make_string(String value, String location) {
	auto result = StringLiteral::create();
	result->value = value;
	result->type = make_name(context->builtin_structs.String->definition);
	result->location = location;
	return result;
}

ArrayType *make_array_type(Type element_type, u64 count) {
	auto result = ArrayType::create();
	result->element_type = element_type;
	result->count = count;
	result->type = get_builtin_type(BuiltinType::Type);
	return result;
}

Binary *make_cast(Expression *expression, Type type) {
	auto as = Binary::create();
	as->operation = BinaryOperation::as;
	as->left = expression;
	as->right = type;
	as->type = type;
	as->location = expression->location;
	return as;
}
