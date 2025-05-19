#pragma once
#include "nodes.h"
#include "type.h"

BooleanLiteral *make_boolean(bool value, String location = {});

IntegerLiteral *make_integer(UnsizedInteger value, String location, Type type = get_builtin_type(BuiltinType::UnsizedInteger));
inline IntegerLiteral *make_integer(UnsizedInteger value, Type type = get_builtin_type(BuiltinType::UnsizedInteger)) {
	return make_integer(value, {}, type);
}
#if UNSIZED_INTEGER_BITS != 64
IntegerLiteral *make_integer(u64 value, String location, Type type = get_builtin_type(BuiltinType::UnsizedInteger));
inline IntegerLiteral *make_integer(u64 value, Type type = get_builtin_type(BuiltinType::UnsizedInteger)) {
	return make_integer(value, {}, type);
}
#endif

StringLiteral *make_string(String value, String location = {});

BuiltinTypeName *make_name(BuiltinType type, String location = {});
Name *make_name(Definition *definition, String location = {});

Type make_pointer(Type type, Mutability mutability);
Expression *make_address(Expression *expression);

ArrayType *make_array_type(Type element_type, u64 count);

Binary *make_binary(BinaryOperation operation, Expression *left, Expression *right, Type type, String location = {});
Binary *make_cast(Expression *expression, Type type);
