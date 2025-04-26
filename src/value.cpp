#include "value.h"
#include "builtin_structs.h"
#include "make_node.h"

umm append(StringBuilder &builder, Value value) {
	switch (value.kind) {
		case ValueKind::none: return 0;
		case ValueKind::U8: return append(builder, value.U8);
		case ValueKind::U16: return append(builder, value.U16);
		case ValueKind::U32: return append(builder, value.U32);
		case ValueKind::U64: return append(builder, value.U64);
		case ValueKind::S8: return append(builder, value.S8);
		case ValueKind::S16: return append(builder, value.S16);
		case ValueKind::S32: return append(builder, value.S32);
		case ValueKind::S64: return append(builder, value.S64);
		case ValueKind::UnsizedInteger: return append(builder, value.UnsizedInteger);
		case ValueKind::Bool: return append(builder, value.Bool);
		case ValueKind::Type:    return append(builder, value.Type);
		case ValueKind::lambda:  return append(builder, value.lambda);
		case ValueKind::array: {
			umm result = 0;
			result += append(builder, ".[");
			result += append(builder, value.elements[0]);
			for (auto element : value.elements.skip(1)) {
				result += append(builder, ", ");
				result += append(builder, element);
			}
			result += append(builder, "]");
			return result;
		}
	}
	return append_format(builder, "(unknown Value {})", value.kind);
}

Expression *to_node(Value value) {
	switch (value.kind) {
		case ValueKind::U8:  return make_integer(value.U8,  get_builtin_type(BuiltinType::U8));
		case ValueKind::U16: return make_integer(value.U16, get_builtin_type(BuiltinType::U16));
		case ValueKind::U32: return make_integer(value.U32, get_builtin_type(BuiltinType::U32));
		case ValueKind::U64: return make_integer(value.U64, get_builtin_type(BuiltinType::U64));
		case ValueKind::S8:  return make_integer(value.S8,  get_builtin_type(BuiltinType::S8));
		case ValueKind::S16: return make_integer(value.S16, get_builtin_type(BuiltinType::S16));
		case ValueKind::S32: return make_integer(value.S32, get_builtin_type(BuiltinType::S32));
		case ValueKind::S64: return make_integer(value.S64, get_builtin_type(BuiltinType::S64));
		case ValueKind::UnsizedInteger: return make_integer(value.UnsizedInteger, get_builtin_type(BuiltinType::UnsizedInteger));
		case ValueKind::Bool: return make_boolean(value.Bool);
		case ValueKind::String: return make_string(value.String);
		case ValueKind::Type: return value.Type;
		default: invalid_code_path();
	}
}

Value zero_of_type(Type type) {
	Value result = {};
	auto direct_type = direct(type);
	if (auto struct_ = as<Struct>(direct_type)) {
		for (int i = 0; i < struct_->members.count; ++i) {
			result.elements.add(zero_of_type(struct_->members[i]->type));
		}
	} 
	else if (types_match(direct_type, BuiltinType::Bool)) { result = Value(false); }
	else if (types_match(direct_type, BuiltinType::U8)) { result = Value((u8)0); }
	else if (types_match(direct_type, BuiltinType::U16)) { result = Value((u16)0); }
	else if (types_match(direct_type, BuiltinType::U32)) { result = Value((u32)0); }
	else if (types_match(direct_type, BuiltinType::U64)) { result = Value((u64)0); }
	else if (types_match(direct_type, BuiltinType::S8)) { result = Value((s8)0); }
	else if (types_match(direct_type, BuiltinType::S16)) { result = Value((s16)0); }
	else if (types_match(direct_type, BuiltinType::S32)) { result = Value((s32)0); }
	else if (types_match(direct_type, BuiltinType::S64)) { result = Value((s64)0); }
	else if (types_match(direct_type, builtin_structs.String)) { result = Value(String{}); }
	else if (types_match(direct_type, BuiltinType::UnsizedInteger)) { result = Value(unsized_integer_tag, UnsizedInteger{}); }
	else if (auto pointer = as_pointer(direct_type)) { result = Value((Value *)0); }
	else {
		invalid_code_path("zero_of_type({}) is invalid", direct_type);
	}
	return result;
}
