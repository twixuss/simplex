#pragma once
#include "common.h"
#include "unsized_integer.h"
#include "type.h"
#include "x.h"

enum class ValueKind : u8 {
#define x(name) name,
	ENUMERATE_EXECUTION_VALUE_KIND
#undef x
};

inline void append(StringBuilder &builder, ValueKind kind) {
	switch (kind) {
#define x(name) case ValueKind::name: return append(builder, #name);
		ENUMERATE_EXECUTION_VALUE_KIND
#undef x
	}
	return append_format(builder, "(unknown ValueKind {})", (u32)kind);
}

ValueKind to_value_kind(Type type);

inline struct StructTag {} struct_tag;
inline struct ArrayTag {} array_tag;
inline struct UnsizedIntegerTag {} unsized_integer_tag;

struct Value {
	ValueKind kind = {};
	union {
		u8 U8;
		u16 U16;
		u32 U32;
		u64 U64;
		s8 S8;
		s16 S16;
		s32 S32;
		s64 S64;
		UnsizedInteger UnsizedInteger;
		bool Bool;
		String String;
		Lambda *lambda;
		Type Type;
		Value *pointer;
		List<Value> elements;
	};
	Value() {
		memset(this, 0, sizeof(*this));
	}
	Value(const Value &that) {
		memcpy(this, &that, sizeof(Value));
	}
	Value(Value &&that) { 
		memcpy(this, &that, sizeof(Value));
		memset(&that, 0, sizeof(Value));
	}
	~Value() {
		memset(this, 0, sizeof(Value));
	}
	Value &operator=(const Value &that) { return this->~Value(), *new(this) Value(that); }
	Value &operator=(Value &&that) { return this->~Value(), *new(this) Value(std::move(that)); }
	explicit Value(ValueKind kind) : kind(kind) {
		switch (kind) {
			case ValueKind::return_:
			case ValueKind::break_:
			case ValueKind::continue_:
			case ValueKind::none:
				break;
			default:
				invalid_code_path();
		}
	}
	explicit Value(u8          value) : kind(ValueKind::U8     ), U8     (value) {}
	explicit Value(u16         value) : kind(ValueKind::U16    ), U16    (value) {}
	explicit Value(u32         value) : kind(ValueKind::U32    ), U32    (value) {}
	explicit Value(u64         value) : kind(ValueKind::U64    ), U64    (value) {}
	explicit Value(s8          value) : kind(ValueKind::S8     ), S8     (value) {}
	explicit Value(s16         value) : kind(ValueKind::S16    ), S16    (value) {}
	explicit Value(s32         value) : kind(ValueKind::S32    ), S32    (value) {}
	explicit Value(s64         value) : kind(ValueKind::S64    ), S64    (value) {}
	explicit Value(bool        value) : kind(ValueKind::Bool   ), Bool   (value) {}
	explicit Value(::String    value) : kind(ValueKind::String ), String (value) {}
	explicit Value(Lambda     *value) : kind(ValueKind::lambda ), lambda (value) {}
	explicit Value(::Type      value) : kind(ValueKind::Type   ), Type   (value) {}
	explicit Value(Value      *value) : kind(ValueKind::pointer), pointer(value) {}
	explicit Value(StructTag, Span<Value> value) : kind(ValueKind::struct_), elements(to_list(value)) {}
	explicit Value(ArrayTag,  Span<Value> value) : kind(ValueKind::array),   elements(to_list(value)) {}
	explicit Value(UnsizedIntegerTag, ::UnsizedInteger value) : kind(ValueKind::UnsizedInteger), UnsizedInteger(value) {}
	
	Value copy() {
		Value result = *this;
		switch (kind) {
			case ValueKind::array:
			case ValueKind::struct_:
				result.elements = tl::copy(result.elements);
				break;
		}
		return result;
	}
	void free() {
		switch (kind) {
			case ValueKind::array:
			case ValueKind::struct_:
				tl::free(elements);
				break;
		}
	}
};

void append(StringBuilder &builder, Value value);

Expression *to_node(Value value);

Value zero_of_type(Type type);

decltype(auto) element_at(auto &&collection, Value index) {
	switch (index.kind) {
		case ValueKind::U8: return collection[index.U8];
		case ValueKind::U16: return collection[index.U16];
		case ValueKind::U32: return collection[index.U32];
		case ValueKind::U64: return collection[index.U64];
		case ValueKind::S8: return collection[index.S8];
		case ValueKind::S16: return collection[index.S16];
		case ValueKind::S32: return collection[index.S32];
		case ValueKind::S64: return collection[index.S64];
		default: invalid_code_path("invalid index kind: {}", index.kind);
	}
}

void default_initialize(Value *value, Type type);
