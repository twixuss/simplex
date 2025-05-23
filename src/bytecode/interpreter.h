#pragma once
#include "bytecode.h"
#include "../type.h"
#include "../target_platform.h"

#include <tl/variant.h>

namespace Bytecode {

struct Interpreter {
	inline static Interpreter *current_interpreter;
	Bytecode *bytecode = 0;
	bool interactive = false;
	jmp_buf stop_interpering_jmp_buf = {};

	enum class DebugWindowKind {
		source,
		arguments,
		locals,
		call_stack,
		bytecode,
		registers,
		stack,
		output,
		count,
	};
	struct DebugWindowFlag {
		static constexpr int bytecode   = 1 << (int)DebugWindowKind::bytecode;
		static constexpr int registers  = 1 << (int)DebugWindowKind::registers;
		static constexpr int stack      = 1 << (int)DebugWindowKind::stack;
		static constexpr int source     = 1 << (int)DebugWindowKind::source;
		static constexpr int arguments  = 1 << (int)DebugWindowKind::arguments;
		static constexpr int locals     = 1 << (int)DebugWindowKind::locals;
		static constexpr int call_stack = 1 << (int)DebugWindowKind::call_stack;
		static constexpr int output     = 1 << (int)DebugWindowKind::output;
	};

	int enabled_windows = ~DebugWindowFlag::stack;

	struct Commands {
		static constexpr char next_expression  = '.';
		static constexpr char next_instruction = 'n';
		static constexpr char next_line        = 'l';
		static constexpr char continuee        = 'c';
		static constexpr char redraw_window    = 'r';
		static constexpr char toggle_hex       = 'x';
	};
	
	static constexpr ConsoleColor next_instruction_color = ConsoleColor::green;

	Optional<u64> run(Bytecode *bytecode, umm entry_index, bool interactive);
	void run_one_instruction();
	
	void run_while(auto predicate) {
		while (predicate()) {
			run_one_instruction();
		}
	}
	
	enum PrintIntBase {
		decimal,
		hex,
		binary,
	};

	struct PrintValueOptions {
		PrintIntBase base = PrintIntBase::decimal;
	};

	inline static PrintValueOptions current_print_options = {};

	template <class T>
	int print_int(auto address, PrintValueOptions options = {}) {
		auto val = [&](Address address) {
			if constexpr (sizeof(T) == 1) return val1(address);
			if constexpr (sizeof(T) == 2) return val2(address);
			if constexpr (sizeof(T) == 4) return val4(address);
			if constexpr (sizeof(T) == 8) return val8(address);
		};

		auto v = (T)val(address);
		if (options.base == PrintIntBase::hex) {
			print("0x");
			return (int)print(format_hex(v));
		} else if (options.base == PrintIntBase::binary) {
			print("0b");
			return (int)print(FormatInt{.value = v, .radix = 2});
		} else {
			return (int)print(v);
		}
	}

	StringBuilder output_builder;

	struct RunNever {};
	struct RunAlways {};
	struct RunWhileLocationIs {
		String location;
	};
	struct RunToLineAfter {
		umm call_stack_size;
		umm instruction_index;
		String file;
		u64 line;
	};
	struct RunWhileInstructionIndexIsNot {
		u64 i;
	};
	Variant<RunNever, RunAlways, RunWhileLocationIs, RunToLineAfter, RunWhileInstructionIndexIsNot> run_strategy;

	ContiguousHashMap<s8 *, Empty> printed_value_addresses;

	int print_value_inner(Address address, Type type, PrintValueOptions options);
	int print_value(Address address, Type type, PrintValueOptions options = current_print_options);

	umm current_instruction_index = (umm)-1;
	Array<s64, 256> registers = {};
	Array<s64, 256> previous_registers = {};
	u8 *stack = 0;
	List<u64> debug_stack;
	List<s64> debug_call_stack;

	struct Library {
		Dll dll;
		HashMap<String, void *> functions;
	};
	HashMap<String, Library> libraries;

	s64 &reg(Register r);
	s64 &val8(Register r);
	s64 &val8(Address a);
	s64 &val8(Site s);
	s64 &val8(InputValue v);
	s32 &val4(Register   x);
	s32 &val4(Address    x);
	s32 &val4(Site       x);
	s32 &val4(InputValue x);
	s16 &val2(Register   x);
	s16 &val2(Address    x);
	s16 &val2(Site       x);
	s16 &val2(InputValue x);
	s8  &val1(Register   x);
	s8  &val1(Address    x);
	s8  &val1(Site       x);
	s8  &val1(InputValue x);

	void *load_extern_function(String libname, String name);
	
	#define y(type, name)
	#define x(name, fields) void execute(Instruction::name##_t i);
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
	#undef x
	#undef y
	 
	#define x(name) void execute_intrinsic_##name(Instruction::intrinsic_t i);
	ENUMERATE_INTRINSICS
	#undef x

	static TargetPlatform target_platform();

	u64 ffi_callback(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda);

	static u64 ffi_callback_static(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda);
	static void *create_bridge(Lambda *lambda);
};

}
