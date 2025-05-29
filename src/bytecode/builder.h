#pragma once
#include "../common.h"
#include "../x.h"
#include "../nodes_fwd.h"
#include "../value.h"
#include "../target_platform.h"
#include "bytecode.h"
#include <tl/bucket_hash_map.h>
#include <tl/variant.h>

/*

Calling convention:

	| stack |
	v       v

	return value      <|
	argument3         <|
	argument2         <|
	argument1         <|
	return address    <| Current Stack Frame
	rbp               <|
	local3            <|
	local2            <|
	local1            <|
	temporary3        <|
	temporary2        <|
	temporary1 <- rsp <|
	return value
	argument2  
	argument1
	return address
	rbp
	...

*/

using namespace tl;

namespace Bytecode {

struct Builder {
	static constexpr s64 register_size = 8;
	static constexpr s64 pointer_size = 8;

	// return value
	// arg3
	// arg2
	// arg1
	// arg0
	// return address
	// old base <- base

#define MI(name, ...)                  \
	Instruction {                      \
		.kind = InstructionKind::name, \
		.v_##name = { __VA_ARGS__ },   \
		.file = __FILE_NAME__, \
		.line = __LINE__, \
		.source_location = current_location, \
	}
#define I(name, ...) (output_bytecode.instructions.add(MI(name, __VA_ARGS__)), 0)

#define tmpreg(name) \
	auto name = allocate_register(); \
	defer { deallocate(name); }

#define tmpaddr(name, size) \
	auto CONCAT(_size_, __LINE__) = size; \
	auto name = allocate_temporary(CONCAT(_size_, __LINE__)); \
	defer { temporary_offset -= CONCAT(_size_, __LINE__); }

#define tmpval(name, size) \
	auto name = create_destination(size); \
	defer { deallocate(name); }

	Builder();

	TargetPlatform *target_platform;
	Bytecode output_bytecode;

	Bytecode build(Expression *expression);


	void append_global_definition(Definition *definition);

	void append_lambda(Lambda *lambda);

	struct BlockInfo {
		Site destination;
		List<umm> break_jump_indices;
	};

	BitSet<(umm)Register::base> available_registers;
	u64 temporary_offset = 0;
	u64 max_temporary_size = 0;
	u64 max_size_reserved_for_arguments = 0;
	umm locals_size = 0;
	List<std::tuple<umm, Lambda *>> calls_to_patch;
	List<umm> jumps_to_ret;
	BucketHashMap<Block *, BlockInfo> block_infos;
	BucketHashMap<While *, List<umm>> continue_jump_indices;
	BucketHashMap<While *, List<umm>> loop_break_indices;

	ContiguousHashMap<String, umm> string_literal_offsets;

	String current_location = {};
	Lambda *current_lambda = 0;
	Block *current_block = 0;

	struct PointerInSection {
		List<u8> *in_section = 0;
		umm in_section_offset = 0;

		struct ToSection {
			List<u8> *section = 0;
			umm offset = 0;
		};

		Variant<Lambda *, ToSection> to;
	};

	List<PointerInSection> pointers_to_patch;

	// Writes value with specified type into section at dst
	void write(List<u8> const &section, u8 *dst, Value value, Type type, u64 size);

	inline void append_to_section(List<u8> &section, Value value, Type type) {
		auto size = get_size(type);
		section.resize(section.count + size);
		write(section, section.end() - size, value, type, size);
	}

	u64 align_size(u64 x);
	s64 align_size(s64 x);

	Register allocate_register();
	void deallocate(Register r);

	Address allocate_temporary(u64 size);

	void reserve_space_for_arguments(u64 size);

	Site create_destination(u64 size);
	void deallocate(Site d);
	
	void output_integer_conversion(Site destination, Expression *expression, u64 target_size, u64 source_size, bool source_signed);

	void output(Site destination, Expression *expression);

	void output(Statement *statement);

	void output_discard(Node *node);

	void output_local_definition(Optional<Site> destination, Definition *definition);

	Address get_definition_address(Definition *definition);

	u64 string_literal_offset(String string);

	void output_defers_up_until(Node *last_node);

	void output_bounds_check(Register index_reg, umm index_size, umm count);
	
	#define x(name) void output_impl(Site destination, name *); 
	ENUMERATE_EXPRESSION_KIND(x)
	#undef x

	#define x(name) void output_impl(name *); 
	ENUMERATE_STATEMENT_KIND(x)
	#undef x

	void load_address(Site destination, Expression *expression);
	
	#define x(name) void load_address_impl(Site destination, name *); 
	ENUMERATE_EXPRESSION_KIND(x)
	#undef x

#undef MI
#undef I
#undef tmpreg
#undef tmpaddr
#undef tmpval
};
}