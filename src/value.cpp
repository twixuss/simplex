#include "value.h"
#include "builtin_structs.h"
#include "make_node.h"
#include "compiler_context.h"

ValueKind to_value_kind(Type type) {
	type = direct(type);
	switch (type->kind) {
		case NodeKind::BuiltinTypeName: {
			REDECLARE_VAL(type, (BuiltinTypeName *)type);
			switch (type->type_kind) {
				case BuiltinType::Type:   return ValueKind::Type;
				case BuiltinType::U8:     return ValueKind::U8;
				case BuiltinType::U16:    return ValueKind::U16;
				case BuiltinType::U32:    return ValueKind::U32;
				case BuiltinType::U64:    return ValueKind::U64;
				case BuiltinType::S8:     return ValueKind::S8;
				case BuiltinType::S16:    return ValueKind::S16;
				case BuiltinType::S32:    return ValueKind::S32;
				case BuiltinType::S64:    return ValueKind::S64;
				case BuiltinType::Bool:   return ValueKind::Bool;
			}
			break;
		}
		case NodeKind::Struct: {
			if (type == context->builtin_structs.String) {
				return ValueKind::String;
			}
			break;
		}
		case NodeKind::ArrayType: { return ValueKind::array; }
	}
	invalid_code_path("to_value_kind: can't convert from {}", type->kind);
}

void append(StringBuilder &builder, Value value) {
	switch (value.kind) {
		case ValueKind::none: return append(builder, "none");
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
			append(builder, ".[");
			append(builder, value.elements[0]);
			for (auto element : value.elements.skip(1)) {
				append(builder, ", ");
				append(builder, element);
			}
			append(builder, "]");
			break;
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
		for (int i = 0; i < struct_->member_list.count; ++i) {
			result.elements.add(zero_of_type(struct_->member_list[i]->type));
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
	else if (types_match(direct_type, context->builtin_structs.String)) { result = Value(String{}); }
	else if (types_match(direct_type, BuiltinType::UnsizedInteger)) { result = Value(unsized_integer_tag, UnsizedInteger{}); }
	else if (auto pointer = as_pointer(direct_type)) { result = Value((Value *)0); }
	else {
		invalid_code_path("zero_of_type({}) is invalid", direct_type);
	}
	return result;
}

void default_initialize(Value *value, Type type) {
	value->kind = to_value_kind(type);
	switch (value->kind) {
		case ValueKind::none: { return; }
		case ValueKind::U8: { value->U8 = 0; return; }
		case ValueKind::U16: { value->U16 = 0; return; }
		case ValueKind::U32: { value->U32 = 0; return; }
		case ValueKind::U64: { value->U64 = 0; return; }
		case ValueKind::S8: { value->S8 = 0; return; }
		case ValueKind::S16: { value->S16 = 0; return; }
		case ValueKind::S32: { value->S32 = 0; return; }
		case ValueKind::S64: { value->S64 = 0; return; }
		case ValueKind::Bool: { value->Bool = false; return; }
		case ValueKind::String: { value->String = {}; return; }
		case ValueKind::lambda: { value->lambda = {}; return; }
		case ValueKind::Type: { value->Type = {}; return; }
		case ValueKind::pointer: { value->pointer = {}; return; }
		case ValueKind::struct_: { 
			auto struct_ = direct_as<Struct>(type);
			assert(struct_);
			value->elements = {};
			value->elements.resize(struct_->member_list.count);
			for (umm i = 0; i < struct_->member_list.count; ++i) {
				default_initialize(&value->elements[i], struct_->member_list[i]->type);
			}
			return; 
		}
		case ValueKind::array: {
			auto array = direct_as<ArrayType>(type);
			assert(array);
			value->elements = {};
			value->elements.resize(array->count.value());
			for (umm i = 0; i < array->count.value(); ++i) {
				default_initialize(&value->elements[i], array->element_type);
			}
			return;
		}
	}
	invalid_code_path("default_initialize: invalid value kind {}", value->kind);
}
