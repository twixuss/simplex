#pragma once
#include "common.h"
#include "nodes_fwd.h"
#include "mutability.h"
#include "value.h"
#include "x.h"
#include "binary_operation.h"
#include "unary_operation.h"

enum class CallKind {
	unknown,
	lambda,
	constructor,
};

enum class InlineStatus : u8 {
#define x(name) name,
	ENUMERATE_INLINE_STATUS
#undef x
};

inline void append(StringBuilder &builder, InlineStatus status) {
	switch (status) {
#define x(name) case InlineStatus::name: return append(builder, #name ## s);
		ENUMERATE_INLINE_STATUS
#undef x
	}
	append_format(builder, "((InlineStatus){})", (u64)status);
}


struct Expression : Node {
	Expression *type = 0;
};

struct Statement : Node {

};

template <class T>
inline T *as(Node *node) {
	if (node->kind == NodeTypeToKind<T>::kind)
		return (T *)node;
	return 0;
}

template <>
inline Expression *as(Node *node) {
	switch (node->kind) {
#define x(name) case NodeKind::name:
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
			return (Expression *)node;
	}
	return 0;
}

template <>
inline Statement *as(Node *node) {
	switch (node->kind) {
#define x(name) case NodeKind::name:
		ENUMERATE_STATEMENT_KIND(x)
#undef x
			return (Statement *)node;
	}
	return 0;
}

struct AtomicArenaAllocator : AllocatorBase<AtomicArenaAllocator> {
	umm buffer_size = 0;
	u8 *base = 0;
	u8 * volatile cursor = 0;

	forceinline static AtomicArenaAllocator create(umm size TL_LP) {
		AtomicArenaAllocator result = {};
		result.buffer_size = size;
		result.cursor = result.base = page_allocator.allocate<u8>(size TL_LA);
		return result;
	}

	forceinline static AtomicArenaAllocator current() { return {}; }

	forceinline AllocationResult allocate_impl(umm size, umm alignment TL_LP) {
		assert(cursor, "arena allocator was not initialized");

		u8 *target = 0;

		atomic_update(&cursor, [=, this, &target](u8 *cursor) {
			target = ceil(cursor, alignment);
			cursor = target + size;
			assert(cursor <= base + buffer_size, "Out of arena memory");
			return cursor;
		});

		return AllocationResult { .data = target, .count = size, .is_zeroed = true };
	}
	forceinline AllocationResult reallocate_impl(void *old_data, umm old_size, umm new_size, umm alignment TL_LP) {
		auto new_data = allocate_impl(new_size, alignment);
		memcpy(new_data.data, old_data, old_size);
		return new_data;
	}
	forceinline void deallocate_impl(void *data, umm size, umm alignment TL_LP) {
		(void)data;
		(void)size;
		(void)alignment;
	}

	forceinline operator Allocator() {
		return {
			.func = [](AllocatorAction action, void *data, umm old_size, umm new_size, umm align, void *state TL_LPD) -> AllocationResult {
				return ((AtomicArenaAllocator *)state)->execute(action, data, old_size, new_size, align TL_LA);
			},
			.state = this
		};
	}

	forceinline void clear() {
		atomic_set(&cursor, base);
	}
	forceinline void free() {
		if (!base)
			return;

		page_allocator.free(base);
		base = 0;
		cursor = 0;
		buffer_size = 0;
	}
};

inline AtomicArenaAllocator node_arena;

template <class T>
struct NodeBase {
	NodeBase() {
		((T *)this)->kind = NodeTypeToKind<T>::kind;
	}
	static T *create() {
		return node_arena.allocate<T>();
	}
	void free() {
		((T *)this)->free_impl();
	}
};

#define DEFINE_EXPRESSION(name) struct name : Expression, NodeBase<name>
#define DEFINE_STATEMENT(name) struct name : Statement, NodeBase<name>

DEFINE_EXPRESSION(Block) {
	Block *parent = 0;
	Expression *container = 0;

	GList<Node *> children;
	GList<Definition *> definition_list;
	GHashMap<String, GList<Definition *>> definition_map;

	String tag;
	GList<Break *> breaks;

	GList<Defer *> defers;

	void add(Node *child);
	void free_impl() {
		tl::free(children);
		tl::free(definition_list);
		definition_map.free();
	}
};

struct CallArgument {
	String name = {};
	Expression *expression = 0;
	Definition *parameter = 0;
};

DEFINE_EXPRESSION(Call) {
	using Argument = CallArgument;

	Expression *callable = 0;
	GList<Argument> arguments;

	InlineStatus inline_status = {};
	CallKind call_kind = {};
};
DEFINE_EXPRESSION(Definition) {
	inline static constexpr u64 invalid_offset = 1ull << 63;

	Expression *container = 0;
	String name;
	Expression *parsed_type = 0;
	Expression *initial_value = 0;
	Optional<Value> constant_value = {};
	Mutability mutability = {};
	u64 offset = invalid_offset;
	bool is_parameter : 1 = false;
	bool is_template_parameter : 1 = false;
};
DEFINE_EXPRESSION(IntegerLiteral) {
	UnsizedInteger value = {};
};
DEFINE_EXPRESSION(BooleanLiteral) {
	bool value = false;
};
DEFINE_EXPRESSION(NoneLiteral) {};
DEFINE_EXPRESSION(StringLiteral) {
	String value;
};
DEFINE_EXPRESSION(LambdaHead) {
	LambdaHead() {
		template_parameters_block.container = this;
		parameters_block.container = this;

		parameters_block.parent = &template_parameters_block;
	}
	// Definitions in these block must have unique names.
	// Assume .definition_map[...].count == 1
	Block template_parameters_block;
	Block parameters_block;
	Expression *parsed_return_type = 0;
	Expression *return_type = 0;
	u64 total_parameters_size = 0;

	// When true, template parameters are unresolved
	// If false, all template parameters must have their initial values set to resolved types.
	bool is_template : 1 = false;
};
DEFINE_EXPRESSION(Lambda) {
	Definition *definition = 0;
	Expression *body = 0;
	LambdaHead head;

	GList<Return *> returns;
	
	u64 first_instruction_index = -1;

	InlineStatus inline_status = {};

	String extern_library = {};

	GList<Definition *> locals = {};
	u64 space_for_call_arguments = 0;
	u64 temporary_size = 0;
	u64 locals_size = 0;
	u64 stack_frame_size = 0;

	bool is_intrinsic : 1 = false;
	bool is_extern    : 1 = false;
};
DEFINE_EXPRESSION(Name) {
	String name;
	List<Definition *> possible_definitions;

	bool allow_overload : 1 = false;

	Definition *definition() {
		if (possible_definitions.count == 1)
			return possible_definitions[0];
		return 0;
	}
};
DEFINE_EXPRESSION(IfExpression) {
	Expression *condition = 0;
	Expression *true_branch = 0;
	Expression *false_branch = 0;
};
DEFINE_EXPRESSION(BuiltinTypeName) {
	BuiltinType type_kind = {};
};
DEFINE_EXPRESSION(Binary) {
	Expression *left = 0;
	Expression *right = 0;
	BinaryOperation operation = {};
	LowBinaryOperation low_operation = {};
};
DEFINE_EXPRESSION(Match) {
	struct Case {
		Expression *from = 0; // null in default case.
		Expression *to = 0;
	};

	Expression *expression = 0;
	GList<Case> cases;
	Case *default_case = 0;
};
DEFINE_EXPRESSION(Unary) {
	Expression *expression = 0;
	UnaryOperation operation = {};
	Mutability mutability = {};
};
DEFINE_EXPRESSION(Struct) {
	Struct() {
		template_parameters_block.container = this;
	}

	Block template_parameters_block;
	Definition *definition = 0;
	GList<Definition *> members;
	s64 size = -1;
	bool must_be_fully_initialized : 1 = false;
	bool is_template : 1 = false;
};
DEFINE_EXPRESSION(ArrayType) {
	Expression *element_type = 0;
	Expression *count_expression = 0;
	Optional<u64> count;
};
DEFINE_EXPRESSION(Subscript) {
	Expression *subscriptable = 0;
	Expression *index = 0; // TODO: replace with GList<Argument> arguments;
};
DEFINE_EXPRESSION(ArrayConstructor) {
	List<Expression *> elements;
};
DEFINE_EXPRESSION(ZeroInitialized) {}; // Might get rid of this if `none as type` becomes real
DEFINE_STATEMENT(Return) {
	Expression *value = 0;
	Lambda *lambda = 0;
};
DEFINE_STATEMENT(While) {
	Expression *condition = 0;
	Node *body = 0;
};
DEFINE_STATEMENT(Continue) {
	While *loop = 0;
};
DEFINE_STATEMENT(Break) {
	While *loop = 0;
	Block *tag_block = 0;
	Expression *value = 0;
};
DEFINE_STATEMENT(IfStatement) {
	Expression *condition = 0;
	Node *true_branch = 0;
	Node *false_branch = 0; // May be null
};
DEFINE_STATEMENT(Import) {
	String path;
};
DEFINE_STATEMENT(Defer) {
	Node *body = 0;
};

#if CHECK_THAT_TYPES_ARE_TYPES
template <class Expr>
Type::operator Expr*() {
	static_assert(std::is_same_v<Expr, Node> || CExpression<Expr>);
	return (Expr *)expression;
}
#endif

bool is_substitutable(Block *block);