#include "get_constant_value.h"
#include "value.h"
#include "debug.h"
#include "nodes.h"

Result<Value, Expression *> get_constant_value_impl(Block *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(Call *call) {
	switch (call->call_kind) {
		case CallKind::constructor: {
			Value result;
			result.kind = ValueKind::struct_;
			result.elements = {};
			for (auto argument : call->arguments) {
				auto argument_value = get_constant_value(argument.expression);
				if (argument_value.is_error()) {
					return argument_value.error();
				}
				result.elements.add(argument_value.value());
			}
			return result;
		}
	}
	return call;
}
Result<Value, Expression *> get_constant_value_impl(Definition *node) {
	if (node->mutability != Mutability::constant) {
		return node;
	}
	return node->constant_value.value();
}
Result<Value, Expression *> get_constant_value_impl(IntegerLiteral *node) { 
	switch (as<BuiltinTypeName>(node->type)->type_kind) {
		case BuiltinType::U8:  return Value((u8 )node->value);
		case BuiltinType::U16: return Value((u16)node->value);
		case BuiltinType::U32: return Value((u32)node->value);
		case BuiltinType::U64: return Value((u64)node->value);
		case BuiltinType::S8:  return Value((s8 )node->value);
		case BuiltinType::S16: return Value((s16)node->value);
		case BuiltinType::S32: return Value((s32)node->value);
		case BuiltinType::S64: return Value((s64)node->value);
		case BuiltinType::UnsizedInteger: return Value(unsized_integer_tag, node->value);
		default: invalid_code_path();
	}
}
Result<Value, Expression *> get_constant_value_impl(FloatLiteral *node) { 
	switch (as<BuiltinTypeName>(node->type)->type_kind) {
		case BuiltinType::F32: return Value((f32)node->value);
		case BuiltinType::F64: return Value((f64)node->value);
		case BuiltinType::UnsizedFloat: return Value(unsized_float_tag, node->value);
		default: invalid_code_path();
	}
}
Result<Value, Expression *> get_constant_value_impl(BooleanLiteral *node) { return Value(((BooleanLiteral *)node)->value); }
Result<Value, Expression *> get_constant_value_impl(NoneLiteral *node) { return Value(ValueKind::none); }
Result<Value, Expression *> get_constant_value_impl(StringLiteral *node) { return Value(((StringLiteral *)node)->value); }
Result<Value, Expression *> get_constant_value_impl(Lambda *lambda) { return Value(lambda); }
Result<Value, Expression *> get_constant_value_impl(LambdaHead *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(Name *node) { 
	auto definition = node->definition();
	if (!definition) {
		return node;
	}
	return get_constant_value_impl(definition);
}
Result<Value, Expression *> get_constant_value_impl(IfExpression *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(BuiltinTypeName *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(Binary *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(Match *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(Unary *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(Struct *node) { return Value(Type(node)); }
Result<Value, Expression *> get_constant_value_impl(Enum *node) { return Value(Type(node)); }
Result<Value, Expression *> get_constant_value_impl(ArrayType *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(Subscript *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(ArrayConstructor *node) {
	Value result;
	result.kind = ValueKind::array;
	result.elements = {};
	result.elements.reserve(node->elements.count);
	for (auto element : node->elements) {
		if (auto element_value = get_constant_value(element)) {
			result.elements.add(element_value.value());
		} else {
			return element_value;
		}
	}
	return result;
}
Result<Value, Expression *> get_constant_value_impl(ZeroInitialized *zi) { return zero_of_type(zi->type); }
Result<Value, Expression *> get_constant_value_impl(CallerLocation *node) { return node; }
Result<Value, Expression *> get_constant_value_impl(CallerArgumentString *node) { return node; }
Result<Value, Expression *> get_constant_value(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
		#define x(name) case NodeKind::name: return get_constant_value_impl((name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
		#undef x
	}
	invalid_code_path("get_constant_value: Invalid node kind {}", expression->kind);
}

Result<UnsizedInteger, Expression *> get_constant_integer(Expression *expression) {
	auto result = get_constant_value(expression);
	if (result.is_error())
		return result.error();

	auto value = result.value();
	switch (value.kind) {
		case ValueKind::UnsizedInteger: return value.UnsizedInteger;
		case ValueKind::U8:             return value.U8;
		case ValueKind::U16:            return value.U16;
		case ValueKind::U32:            return value.U32;
		case ValueKind::U64:            return value.U64;
		case ValueKind::S8:             return value.S8;
		case ValueKind::S16:            return value.S16;
		case ValueKind::S32:            return value.S32;
		case ValueKind::S64:            return value.S64;
	}

	return expression;
}

