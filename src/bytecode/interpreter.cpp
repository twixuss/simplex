#include "interpreter.h"
#include "../reporter.h"
#include "../nodes.h"
#include "../builtin_structs.h"
#include "../escape.h"
#include "../compiler_context.h"

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <tl/win32.h>

#include <conio.h>

namespace Bytecode {

Optional<u64> Interpreter::run(Bytecode *bytecode, umm entry_index, bool interactive) {
	scoped_replace(current_interpreter, this);
	this->bytecode = bytecode;
	this->interactive = interactive;
	current_instruction_index = entry_index;

	// Allocate stack and setup guard pages around it
	constexpr u64 page_size = 4096;
	constexpr u64 stack_size = 4*page_size;
	stack = (u8 *)VirtualAlloc(0, stack_size + page_size*2, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	VirtualFree(stack, page_size, MEM_DECOMMIT);
	VirtualFree(stack + page_size + stack_size, page_size, MEM_DECOMMIT);
	stack += page_size;
	stack[0] = 0;
	stack[stack_size-1] = 0;

	reg(Register::stack) = (s64)(stack + stack_size);
	reg(Register::global_mutable) = (s64)bytecode->global_mutable_data.data;
	reg(Register::global_readonly) = (s64)bytecode->global_readonly_data.data;

	//
	// Preload all extern references
	//
	for (auto i : bytecode->instructions) {
		switch (i.kind) {
			case InstructionKind::callext:
				load_extern_function(i.v_callext.lib, i.v_callext.name);
				break;
			case InstructionKind::copyext:
				load_extern_function(i.v_copyext.lib, i.v_copyext.name);
				break;
		}
	}

	if (setjmp(stop_interpering_jmp_buf) == 1) {
		return {};
	}

	__try {
		run_while([&] { return current_instruction_index < bytecode->instructions.count; });
	}
	__except (EXCEPTION_EXECUTE_HANDLER) {
		with(ConsoleColor::red, println("Caught exception during bytecode interpreting"));
		println("Instruction index: {}", current_instruction_index);
		if (current_instruction_index < bytecode->instructions.count) {
			auto i = bytecode->instructions[current_instruction_index];
			println("Instruction:");
			print("    ");
			print_instruction(current_instruction_index, i);
			println();

			immediate_reporter.error(i.source_location, "Source:");
		}
		println("Call stack (old first):");
		for (auto index : debug_call_stack) {
			if (auto found = bytecode->first_instruction_to_lambda.find(index)) {
				auto lambda = *found.value;
				auto loc = get_source_location(lambda->location);
				println("{}:{}: {}", loc.file, loc.location_line_number, lambda->definition ? lambda->definition->name : u8"(unnamed)"s);
			} else {
				println("unknown at #{}", index);
			}
		}
		return {};
	}

	return val8(Address { .offset = (s64)((stack + stack_size) - 8)});
}
inline static thread_local char spaces[] = "                                                                                                                                                                                                                                                               ";
void Interpreter::run_one_instruction() {
	auto i = bytecode->instructions[current_instruction_index];
	auto std_out = GetStdHandle(STD_OUTPUT_HANDLE);

	bool skip = false;

	auto find_ret_index = [&](umm from_index) {
		while (from_index < bytecode->instructions.count && bytecode->instructions[from_index].kind != InstructionKind::ret) {
			++from_index;
		}
		return from_index;
	};

	run_strategy.visit(Combine{
		[](Empty) {},
		[&](RunWhileLocationIs r) {
			if (r.location.data == i.source_location.data && r.location.count == i.source_location.count) {
				skip = true;
			}
		},
		[&](RunToLineAfter r) {
			if (debug_call_stack.count < r.call_stack_size) {
				skip = false;
				return;
			}
			if (debug_call_stack.count > r.call_stack_size) {
				skip = true;
				return;
			}

			skip = true;
			if (i.source_location) {
				auto l = get_source_location(i.source_location);
				if (l.file == r.file) {
					if (l.location_line_number != r.line) {
						if (find_ret_index(current_instruction_index) == find_ret_index(r.instruction_index)) {
							skip = false;
						}
					} 
				}
			}
		},
		[&](RunWhileInstructionIndexIsNot r) {
			if (current_instruction_index != r.i) {
				skip = true;
			}
		},
	});

	if (skip) {
		if (GetAsyncKeyState('S')) {
			skip = false;
			run_strategy = Empty{};
		}
	} else {
		run_strategy = Empty{};
	}

	if (interactive && !skip) {
		scoped_replace(current_printer, Printer{
			[](Span<utf8> span, void *) {
				auto std_out = GetStdHandle(STD_OUTPUT_HANDLE);
				auto start = span.data;
				for (auto c = span.data; c != span.data + span.count; ++c) {
					if (*c == '\n') {
						print_to_console(Span(start, c));
						CONSOLE_SCREEN_BUFFER_INFO buffer_info;
						GetConsoleScreenBufferInfo(std_out, &buffer_info);
						auto spaces_count = buffer_info.dwMaximumWindowSize.X - buffer_info.dwCursorPosition.X - 1;
						spaces[spaces_count] = '\n';
						print_to_console(Span(spaces, spaces_count + 1));
						spaces[spaces_count] = ' ';
						start = c + 1;
					}
				}
				print_to_console(Span(start, span.data + span.count));
			}
		});

	redraw:
				
		SetConsoleCursorPosition(std_out, {0, 0});

		auto header = [&](Span<char> name, int flag) {
			with(((enabled_windows & flag) ? ConsoleColor::cyan : ConsoleColor::gray), println("==== {} {} ====", log2(flag), name));
			return enabled_windows & flag;
		};
				
				

		Lambda *lambda = 0;
		if (enabled_windows & (DebugWindowFlag::locals | DebugWindowFlag::arguments | DebugWindowFlag::stack)) {
			u64 max_lambda_first_instruction = 0;
			if (current_instruction_index < bytecode->instructions.count - 3) { // Ignore initial instructions that don't belong to any lambda
				for (auto it = bytecode->first_instruction_to_lambda.iter(); it; it.next()) {
					auto [first_instruction, some_lambda] = it.key_value();
					if (current_instruction_index >= first_instruction) {
						if (first_instruction > max_lambda_first_instruction) {
							max_lambda_first_instruction = first_instruction;
							lambda = some_lambda;
						}
					}
				}
			}
		}

		for (int debug_window_index = 0; debug_window_index != (int)DebugWindowKind::count; ++debug_window_index) {
			if (debug_window_index == (int)DebugWindowKind::bytecode && header("Bytecode"s, DebugWindowFlag::bytecode)) {
				for (s64 o = -5; o <= 5; ++o) {
					scoped_if(next_instruction_color, o == 0);

					u64 instruction_index = current_instruction_index + o;
					if (instruction_index >= bytecode->instructions.count) {
						println("...");
					} else {
						print_instruction(instruction_index, bytecode->instructions[instruction_index]);
					}
				}
			}
			else if (debug_window_index == (int)DebugWindowKind::registers && header("Registers"s, DebugWindowFlag::registers)) {
				auto print_register = [&] (String name, Register r) {
					print("{}: ", name);
					u64 current = registers[(int)r];
					u64 previous = previous_registers[(int)r];
					if (current == previous) {
						print("0x{}", format_hex(current));
					} else {
						with(ConsoleColor::red, print("0x{}", format_hex(current)));
						with(ConsoleColor::gray, print(" 0x{}", format_hex(previous)));
					}
					println();
				};

				print_register(u8"base"s, Register::base);
				print_register(u8"stack"s, Register::stack);
				for (int i = 0; i < 8; ++i) {
					print_register(tformat(u8"r{}", i), (Register)i);
				}
			}
			else if (debug_window_index == (int)DebugWindowKind::stack && header("Stack"s, DebugWindowFlag::stack)) {
				if (lambda) {
					for (s64 i = lambda->stack_frame_size - 8; i >= 0; i -= 8) {
						s64 addr = reg(Register::stack) + i;
						print("0x{}: ", format_hex(addr));
						print_value(Address{.offset = addr}, get_builtin_type(BuiltinType::U64), {.hex = true});
						if (i == 0)
							print(" <- stack");
						if (i == lambda->space_for_call_arguments && lambda->temporary_size)
							print(" <- temporary");
						if (i == lambda->space_for_call_arguments + lambda->temporary_size && lambda->locals_size)
							print(" <- locals");
						if (i == lambda->space_for_call_arguments + lambda->temporary_size + lambda->locals_size)
							print(" <- base");
						if (i == lambda->space_for_call_arguments + lambda->temporary_size + lambda->locals_size + 8)
							print(" <- return address");
						if (i == lambda->space_for_call_arguments + lambda->temporary_size + lambda->locals_size + 16 && lambda->head.total_parameters_size)
							print(" <- arguments");
						if (i == lambda->space_for_call_arguments + lambda->temporary_size + lambda->locals_size + 16 + lambda->head.total_parameters_size && get_size(lambda->head.return_type))
							print(" <- return value");
						println();
					}
				}
			}
			else if (debug_window_index == (int)DebugWindowKind::source && header(tformat("Source {}", i.source_location ? get_source_location(i.source_location).file : u8""s), DebugWindowFlag::source)) {
				if (i.source_location.count) {
					auto location = get_source_location(i.source_location, {.lines_before = 8, .lines_after = 8});

					print_source_chunk(location, 0, next_instruction_color);
				} else {
					println("unknown source");
				}
			}
			else if (debug_window_index == (int)DebugWindowKind::arguments && header("Arguments"s, DebugWindowFlag::arguments)) {
				if (lambda) {
					for (auto parameter : lambda->head.parameters_block.definition_list) {
						with(ConsoleColor::white, print("{}: ", parameter->name));
						Address address = {
							.base = Register::base,
							.offset = (s64)(16 + parameter->offset),
						};
						print_value(address, parameter->type);
						println();
					}
				}
			}
			else if (debug_window_index == (int)DebugWindowKind::locals && header("Locals"s, DebugWindowFlag::locals)) {
				if (lambda) {
					for (auto local : lambda->locals) {
						with(ConsoleColor::white, print("{}: ", local->name));
						Address address = {
							.base = Register::base,
							.offset = -(s64)lambda->locals_size + (s64)local->offset,
						};
						print_value(address, local->type);
						println();
					}
				}
			}
			else if (debug_window_index == (int)DebugWindowKind::call_stack && header("Call stack"s, DebugWindowFlag::call_stack)) {
				for (auto index : debug_call_stack) {
					if (auto found = bytecode->first_instruction_to_lambda.find(index)) {
						auto lambda = *found.value;
						auto loc = get_source_location(lambda->location);
						print("{}:{}: ", loc.file, loc.location_line_number);
						with(ConsoleColor::white, println(lambda->definition ? lambda->definition->name : u8"(unnamed)"s));
					} else {
						println("unknown at #{}", index);
					}
				}
			}
			else if (debug_window_index == (int)DebugWindowKind::output && header("Output"s, DebugWindowFlag::output)) {
				println(output_builder);
			}
		}

		println();
		println("{} - next expression", Commands::next_expression);
		println("{} - next instruction", Commands::next_instruction);
		println("{} - next line", Commands::next_line);
		println("{} - redraw window", Commands::redraw_window);
		println("{} - toggle hex", Commands::toggle_hex);
		CONSOLE_SCREEN_BUFFER_INFO csbi;
		GetConsoleScreenBufferInfo(std_out, &csbi);
		for (int i = csbi.dwCursorPosition.Y; i < csbi.dwMaximumWindowSize.Y - 1; ++i) {
			println();
		}
		print("                           \r");

	retry_char:
		int pressed_key = _getch();
		switch (pressed_key) {
			ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
				(int &)enabled_windows ^= 1 << (pressed_key - '0');
				goto redraw;
			}
			case Commands::next_expression: {
				run_strategy = RunWhileLocationIs{i.source_location};
				break;
			}
			case Commands::next_instruction: {
				break;
			}
			case Commands::next_line: {
				if (i.source_location) {
					auto location = get_source_location(i.source_location);
					run_strategy = RunToLineAfter{
						.call_stack_size = debug_call_stack.count,
						.instruction_index = current_instruction_index,
						.file = location.file,
						.line = location.location_line_number,
					};
				}
				break;
			}
			case Commands::redraw_window: {
				goto redraw;
			}
			case Commands::toggle_hex: {
				current_print_options.hex ^= 1;
				goto redraw;
			}
			default: {
				goto retry_char;
			}
		}

		print("... RUNNING ...");
	}

	previous_registers = registers;

	__try {
		switch (i.kind) {
			#define x(name, fields) case InstructionKind::name: execute(i.v_##name); break;
			ENUMERATE_BYTECODE_INSTRUCTION_KIND
			#undef x
		}
	} __except ([&](EXCEPTION_POINTERS *ep){
		if (context->run_interactive) {
			println("EXCEPTION: {}", ep->ExceptionRecord->ExceptionCode);
			println("ADDRESS: {}", ep->ExceptionRecord->ExceptionAddress);
			current_instruction_index -= 1;
			run_strategy = Empty{};
			return EXCEPTION_EXECUTE_HANDLER;
		} else {
			return EXCEPTION_EXECUTE_FAULT;
		}
	}(GetExceptionInformation())) {
	}

	++current_instruction_index;
}
	
int Interpreter::print_value_inner(Address address, Type type, PrintValueOptions options) {
	__try {
		if (types_match(type, BuiltinType::None)) return 0;
		if (types_match(type, BuiltinType::Bool)) return print((bool)val1(address));
		if (types_match(type, BuiltinType::U8 )) { return print_int<u8 >(address, options); }
		if (types_match(type, BuiltinType::U16)) { return print_int<u16>(address, options); }
		if (types_match(type, BuiltinType::U32)) { return print_int<u32>(address, options); }
		if (types_match(type, BuiltinType::U64)) { return print_int<u64>(address, options); }
		if (types_match(type, BuiltinType::S8 )) { return print_int<s8 >(address, options); }
		if (types_match(type, BuiltinType::S16)) { return print_int<s16>(address, options); }
		if (types_match(type, BuiltinType::S32)) { return print_int<s32>(address, options); }
		if (types_match(type, BuiltinType::S64)) { return print_int<s64>(address, options); }
		if (types_match(type, context->builtin_structs.String)) {
			auto string = Span((utf8 *)val8(address), (umm)val8(address withx { it.offset += 8; }));
			if (string.count > 32) {
				return print("\"{}...{}\"", EscapedString(string.take(16)), EscapedString(string.take(-16)));
			} else {
				return print("\"{}\"", EscapedString(string));
			}
		}
		auto directed = direct(type);
		if (auto struct_ = as<Struct>(directed)) {
			print("{}(", struct_->definition ? struct_->definition->name : u8"unnamed_struct"s);
			for (int i = 0; i < struct_->members.count; ++i) {
				if (i) print(", ");

				auto member = struct_->members[i];

				print("{} = ", member->name);
				print_value_inner(address withx { it.offset += member->offset; }, member->type, options);
			}
			print(")");
			return 0;
		}

		if (auto pointer = as_pointer(directed)) {
			auto ptr = val8(address);
			if (ptr) {
				print_int<u64>(address, {.hex = true});
				print(" ");
				if (printed_value_addresses.find(&val1(address))) {
					print("<recursive>");
				} else {
					printed_value_addresses.get_or_insert(&val1(address));
					print_value_inner(Address{.offset = ptr}, pointer->expression, options);
				}
			} else {
				print("null");
			}
			return 0;
		}

		if (auto array = as<ArrayType>(directed)) {
			print("[");
			auto element_size = get_size(array->element_type);
			for (umm i = 0; i < array->count.value(); ++i) {
				if (i) print(", ");
				auto element_address = address;
				element_address.offset += i * element_size;
				print_value_inner(element_address, array->element_type, options);
			}
			print("]");
			return 0;
		}

		return print("unsupported type");
	} __except (EXCEPTION_EXECUTE_HANDLER) {
		return print("???");
	}
}
int Interpreter::print_value(Address address, Type type, PrintValueOptions options) {
	printed_value_addresses.clear();
	return print_value_inner(address, type, options);
}

s64 &Interpreter::reg(Register r) {
	return registers[to_underlying(r)];
}
s64 &Interpreter::val8(Register r) {
	return registers[to_underlying(r)];
}
s64 &Interpreter::val8(Address a) {
	s64 x = a.offset;
	if (a.base) {
		x += reg(a.base.value());
	}
	if (a.element_size) {
		x += reg(a.element_index) * a.element_size;
	}

	return *(s64 *)x;
}
s64 &Interpreter::val8(Site s) {
	if (s.is_register()) {
		return reg(s.get_register());
	} else {
		return val8(s.get_address());
	}
}
s64 &Interpreter::val8(InputValue v) {
	if (v.is_register()) {
		return reg(v.get_register());
	} else if (v.is_address()) {
		return val8(v.get_address());
	} else {
		return v.get_constant();
	}
}
s32 &Interpreter::val4(Register   x) { return (s32 &)val8(x); }
s32 &Interpreter::val4(Address    x) { return (s32 &)val8(x); }
s32 &Interpreter::val4(Site       x) { return (s32 &)val8(x); }
s32 &Interpreter::val4(InputValue x) { return (s32 &)val8(x); }
s16 &Interpreter::val2(Register   x) { return (s16 &)val8(x); }
s16 &Interpreter::val2(Address    x) { return (s16 &)val8(x); }
s16 &Interpreter::val2(Site       x) { return (s16 &)val8(x); }
s16 &Interpreter::val2(InputValue x) { return (s16 &)val8(x); }
s8  &Interpreter::val1(Register   x) { return (s8  &)val8(x); }
s8  &Interpreter::val1(Address    x) { return (s8  &)val8(x); }
s8  &Interpreter::val1(Site       x) { return (s8  &)val8(x); }
s8  &Interpreter::val1(InputValue x) { return (s8  &)val8(x); }

#define E(name, ...) execute(Instruction{.kind = InstructionKind::name, .v_##name = { __VA_ARGS__ }}.v_##name)
	
void *Interpreter::load_extern_function(String libname, String name) {
	auto &lib = libraries.get_or_insert(libname);
	if (!lib.dll) {
		auto lib_name = tformat(u8"{}{}"s, libname, dll_extension);
		lib.dll = load_dll(lib_name);
		assert(lib.dll);
	}

	auto &fn = lib.functions.get_or_insert(name);
	if (!fn) {
		fn = get_symbol(lib.dll, name);
		assert(fn, "{}.dll does not contain {}", libname, name);
	}

	return fn;
}

void Interpreter::execute(Instruction::pop_t i) {
	E(copy, .d = i.d, .s = Address{.base = Register::stack}, .size = 8);
	E(add8, .d = Register::stack, .a = Register::stack, .b = 8);
}
void Interpreter::execute(Instruction::nop_t i) {}
void Interpreter::execute(Instruction::push_t i) {
	E(sub8, .d = Register::stack, .a = Register::stack, .b = 8);
	E(copy, .d = Address{.base = Register::stack}, .s = i.s, .size = 8);
}
void Interpreter::execute(Instruction::copy_t i) {
	auto d = &val8(i.d);
	auto s = &val8(i.s);
	memcpy(d, s, i.size);
}
void Interpreter::execute(Instruction::set_t i) {
	auto d = &val8(i.d);
	memset(d, i.value, i.size);
}
void Interpreter::execute(Instruction::lea_t i) { val8(i.d) = (s64) &val8(i.s); }
void Interpreter::execute(Instruction::add1_t i) { val1(i.d) = val1(i.a) + val1(i.b); }
void Interpreter::execute(Instruction::add2_t i) { val2(i.d) = val2(i.a) + val2(i.b); }
void Interpreter::execute(Instruction::add4_t i) { val4(i.d) = val4(i.a) + val4(i.b); }
void Interpreter::execute(Instruction::add8_t i) { val8(i.d) = val8(i.a) + val8(i.b); }
void Interpreter::execute(Instruction::sub1_t i) { val1(i.d) = val1(i.a) - val1(i.b); }
void Interpreter::execute(Instruction::sub2_t i) { val2(i.d) = val2(i.a) - val2(i.b); }
void Interpreter::execute(Instruction::sub4_t i) { val4(i.d) = val4(i.a) - val4(i.b); }
void Interpreter::execute(Instruction::sub8_t i) { val8(i.d) = val8(i.a) - val8(i.b); }
void Interpreter::execute(Instruction::mul1_t i) { val1(i.d) = val1(i.a) * val1(i.b); }
void Interpreter::execute(Instruction::mul2_t i) { val2(i.d) = val2(i.a) * val2(i.b); }
void Interpreter::execute(Instruction::mul4_t i) { val4(i.d) = val4(i.a) * val4(i.b); }
void Interpreter::execute(Instruction::mul8_t i) { val8(i.d) = val8(i.a) * val8(i.b); }
void Interpreter::execute(Instruction::divu1_t i) { val1(i.d) = (u8 )val1(i.a) / (u8 )val1(i.b); }
void Interpreter::execute(Instruction::divu2_t i) { val2(i.d) = (u16)val2(i.a) / (u16)val2(i.b); }
void Interpreter::execute(Instruction::divu4_t i) { val4(i.d) = (u32)val4(i.a) / (u32)val4(i.b); }
void Interpreter::execute(Instruction::divu8_t i) { val8(i.d) = (u64)val8(i.a) / (u64)val8(i.b); }
void Interpreter::execute(Instruction::divs1_t i) { val1(i.d) = (s8 )val1(i.a) / (s8 )val1(i.b); }
void Interpreter::execute(Instruction::divs2_t i) { val2(i.d) = (s16)val2(i.a) / (s16)val2(i.b); }
void Interpreter::execute(Instruction::divs4_t i) { val4(i.d) = (s32)val4(i.a) / (s32)val4(i.b); }
void Interpreter::execute(Instruction::divs8_t i) { val8(i.d) = (s64)val8(i.a) / (s64)val8(i.b); }
void Interpreter::execute(Instruction::modu1_t i) { val1(i.d) = (u8 )val1(i.a) % (u8 )val1(i.b); }
void Interpreter::execute(Instruction::modu2_t i) { val2(i.d) = (u16)val2(i.a) % (u16)val2(i.b); }
void Interpreter::execute(Instruction::modu4_t i) { val4(i.d) = (u32)val4(i.a) % (u32)val4(i.b); }
void Interpreter::execute(Instruction::modu8_t i) { val8(i.d) = (u64)val8(i.a) % (u64)val8(i.b); }
void Interpreter::execute(Instruction::mods1_t i) { val1(i.d) = (s8 )val1(i.a) % (s8 )val1(i.b); }
void Interpreter::execute(Instruction::mods2_t i) { val2(i.d) = (s16)val2(i.a) % (s16)val2(i.b); }
void Interpreter::execute(Instruction::mods4_t i) { val4(i.d) = (s32)val4(i.a) % (s32)val4(i.b); }
void Interpreter::execute(Instruction::mods8_t i) { val8(i.d) = (s64)val8(i.a) % (s64)val8(i.b); }
void Interpreter::execute(Instruction::xor1_t i) { val1(i.d) = val1(i.a) ^ val1(i.b); }
void Interpreter::execute(Instruction::xor2_t i) { val2(i.d) = val2(i.a) ^ val2(i.b); }
void Interpreter::execute(Instruction::xor4_t i) { val4(i.d) = val4(i.a) ^ val4(i.b); }
void Interpreter::execute(Instruction::xor8_t i) { val8(i.d) = val8(i.a) ^ val8(i.b); }
void Interpreter::execute(Instruction::and1_t i) { val1(i.d) = val1(i.a) & val1(i.b); }
void Interpreter::execute(Instruction::and2_t i) { val2(i.d) = val2(i.a) & val2(i.b); }
void Interpreter::execute(Instruction::and4_t i) { val4(i.d) = val4(i.a) & val4(i.b); }
void Interpreter::execute(Instruction::and8_t i) { val8(i.d) = val8(i.a) & val8(i.b); }
void Interpreter::execute(Instruction::or1_t i)  { val1(i.d) = val1(i.a) | val1(i.b); }
void Interpreter::execute(Instruction::or2_t i)  { val2(i.d) = val2(i.a) | val2(i.b); }
void Interpreter::execute(Instruction::or4_t i)  { val4(i.d) = val4(i.a) | val4(i.b); }
void Interpreter::execute(Instruction::or8_t i)  { val8(i.d) = val8(i.a) | val8(i.b); }
void Interpreter::execute(Instruction::sll1_t i) { val1(i.d) = val1(i.a) << val1(i.b); }
void Interpreter::execute(Instruction::sll2_t i) { val2(i.d) = val2(i.a) << val2(i.b); }
void Interpreter::execute(Instruction::sll4_t i) { val4(i.d) = val4(i.a) << val4(i.b); }
void Interpreter::execute(Instruction::sll8_t i) { val8(i.d) = val8(i.a) << val8(i.b); }
void Interpreter::execute(Instruction::srl1_t i) { val1(i.d) = (u8)val1(i.a) >> (u8)val1(i.b); }
void Interpreter::execute(Instruction::srl2_t i) { val2(i.d) = (u16)val2(i.a) >> (u16)val2(i.b); }
void Interpreter::execute(Instruction::srl4_t i) { val4(i.d) = (u32)val4(i.a) >> (u32)val4(i.b); }
void Interpreter::execute(Instruction::srl8_t i) { val8(i.d) = (u64)val8(i.a) >> (u64)val8(i.b); }
void Interpreter::execute(Instruction::sra1_t i) { val1(i.d) = (s8)val1(i.a) >> (s8)val1(i.b); }
void Interpreter::execute(Instruction::sra2_t i) { val2(i.d) = (s16)val2(i.a) >> (s16)val2(i.b); }
void Interpreter::execute(Instruction::sra4_t i) { val4(i.d) = (s32)val4(i.a) >> (s32)val4(i.b); }
void Interpreter::execute(Instruction::sra8_t i) { val8(i.d) = (s64)val8(i.a) >> (s64)val8(i.b); }
void Interpreter::execute(Instruction::cmp1_t i) {
	switch (i.cmp) {
		case Comparison::equals:                  val1(i.d) =     val1(i.a) ==     val1(i.b); break;
		case Comparison::not_equals:              val1(i.d) =     val1(i.a) !=     val1(i.b); break;
		case Comparison::signed_less:             val1(i.d) = (s8)val1(i.a) <  (s8)val1(i.b); break;
		case Comparison::signed_greater:          val1(i.d) = (s8)val1(i.a) >  (s8)val1(i.b); break;
		case Comparison::signed_less_equals:      val1(i.d) = (s8)val1(i.a) <= (s8)val1(i.b); break;
		case Comparison::signed_greater_equals:   val1(i.d) = (s8)val1(i.a) >= (s8)val1(i.b); break;
		case Comparison::unsigned_less:           val1(i.d) = (u8)val1(i.a) <  (u8)val1(i.b); break;
		case Comparison::unsigned_greater:        val1(i.d) = (u8)val1(i.a) >  (u8)val1(i.b); break;
		case Comparison::unsigned_less_equals:    val1(i.d) = (u8)val1(i.a) <= (u8)val1(i.b); break;
		case Comparison::unsigned_greater_equals: val1(i.d) = (u8)val1(i.a) >= (u8)val1(i.b); break;
	}
}
void Interpreter::execute(Instruction::cmp2_t i) {
	switch (i.cmp) {
		case Comparison::equals:                  val1(i.d) =      val2(i.a) ==      val2(i.b); break;
		case Comparison::not_equals:              val1(i.d) =      val2(i.a) !=      val2(i.b); break;
		case Comparison::signed_less:             val1(i.d) = (s16)val2(i.a) <  (s16)val2(i.b); break;
		case Comparison::signed_greater:          val1(i.d) = (s16)val2(i.a) >  (s16)val2(i.b); break;
		case Comparison::signed_less_equals:      val1(i.d) = (s16)val2(i.a) <= (s16)val2(i.b); break;
		case Comparison::signed_greater_equals:   val1(i.d) = (s16)val2(i.a) >= (s16)val2(i.b); break;
		case Comparison::unsigned_less:           val1(i.d) = (u16)val2(i.a) <  (u16)val2(i.b); break;
		case Comparison::unsigned_greater:        val1(i.d) = (u16)val2(i.a) >  (u16)val2(i.b); break;
		case Comparison::unsigned_less_equals:    val1(i.d) = (u16)val2(i.a) <= (u16)val2(i.b); break;
		case Comparison::unsigned_greater_equals: val1(i.d) = (u16)val2(i.a) >= (u16)val2(i.b); break;
	}
}
void Interpreter::execute(Instruction::cmp4_t i) {
	switch (i.cmp) {
		case Comparison::equals:                  val1(i.d) =      val4(i.a) ==      val4(i.b); break;
		case Comparison::not_equals:              val1(i.d) =      val4(i.a) !=      val4(i.b); break;
		case Comparison::signed_less:             val1(i.d) = (s32)val4(i.a) <  (s32)val4(i.b); break;
		case Comparison::signed_greater:          val1(i.d) = (s32)val4(i.a) >  (s32)val4(i.b); break;
		case Comparison::signed_less_equals:      val1(i.d) = (s32)val4(i.a) <= (s32)val4(i.b); break;
		case Comparison::signed_greater_equals:   val1(i.d) = (s32)val4(i.a) >= (s32)val4(i.b); break;
		case Comparison::unsigned_less:           val1(i.d) = (u32)val4(i.a) <  (u32)val4(i.b); break;
		case Comparison::unsigned_greater:        val1(i.d) = (u32)val4(i.a) >  (u32)val4(i.b); break;
		case Comparison::unsigned_less_equals:    val1(i.d) = (u32)val4(i.a) <= (u32)val4(i.b); break;
		case Comparison::unsigned_greater_equals: val1(i.d) = (u32)val4(i.a) >= (u32)val4(i.b); break;
	}
}
void Interpreter::execute(Instruction::cmp8_t i) {
	switch (i.cmp) {
		case Comparison::equals:                  val1(i.d) =      val8(i.a) ==      val8(i.b); break;
		case Comparison::not_equals:              val1(i.d) =      val8(i.a) !=      val8(i.b); break;
		case Comparison::signed_less:             val1(i.d) = (s64)val8(i.a) <  (s64)val8(i.b); break;
		case Comparison::signed_greater:          val1(i.d) = (s64)val8(i.a) >  (s64)val8(i.b); break;
		case Comparison::signed_less_equals:      val1(i.d) = (s64)val8(i.a) <= (s64)val8(i.b); break;
		case Comparison::signed_greater_equals:   val1(i.d) = (s64)val8(i.a) >= (s64)val8(i.b); break;
		case Comparison::unsigned_less:           val1(i.d) = (u64)val8(i.a) <  (u64)val8(i.b); break;
		case Comparison::unsigned_greater:        val1(i.d) = (u64)val8(i.a) >  (u64)val8(i.b); break;
		case Comparison::unsigned_less_equals:    val1(i.d) = (u64)val8(i.a) <= (u64)val8(i.b); break;
		case Comparison::unsigned_greater_equals: val1(i.d) = (u64)val8(i.a) >= (u64)val8(i.b); break;
	}
}
void Interpreter::execute(Instruction::sex21_t i) { val8(i.d) = (s16)(s8)val1(i.a); }
void Interpreter::execute(Instruction::sex41_t i) { val8(i.d) = (s32)(s8)val1(i.a); }
void Interpreter::execute(Instruction::sex42_t i) { val8(i.d) = (s32)(s16)val2(i.a); }
void Interpreter::execute(Instruction::sex81_t i) { val8(i.d) = (s64)(s8)val1(i.a); }
void Interpreter::execute(Instruction::sex82_t i) { val8(i.d) = (s64)(s16)val2(i.a); }
void Interpreter::execute(Instruction::sex84_t i) { val8(i.d) = (s64)(s32)val4(i.a); }
void Interpreter::execute(Instruction::call_t i) {
	E(push, (s64)current_instruction_index);

	auto call_to = val8(i.d);
		
	debug_call_stack.add(call_to);

	// NOTE: offset by -1 because it will be incremented in the main loop
	current_instruction_index = call_to - 1;

	debug_stack.add(val8(Register::stack));
}
void Interpreter::execute(Instruction::callext_t i) {
	auto fn = load_extern_function(i.lib, i.name);

	s64 parameter_count = i.lambda->head.parameters_block.definition_list.count;

	s64 result = 0;

	#if 0
	system("cls");
	println("switch (parameter_count) {");
	for (int i = 0; i < 16; ++i) {
		println("    case {}: {{", i);
		for (int j = 0; j < i; ++j) {
			println("        s64 arg{} = val8(Address{{.base = Register::stack, .offset = {}}});", j, j*8);
		}
		print("        result = ((s64(*)(");
		for (int j = 0; j < i; ++j) {
			if (j) print(',');
			print("s64");
		}
		print("))fn)(");
		for (int j = 0; j < i; ++j) {
			if (j) print(',');
			print("arg{}", j);
		}
		println(");");
		println("        break;");
		println("    }");
	}
	println("    default: not_implemented();");
	println("}");
	exit(1);
	#endif
	switch (parameter_count) {
		case 0: {
			result = ((s64(*)())fn)();
			break;
		}
		case 1: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			result = ((s64(*)(s64))fn)(arg0);
			break;
		}
		case 2: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			result = ((s64(*)(s64,s64))fn)(arg0,arg1);
			break;
		}
		case 3: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			result = ((s64(*)(s64,s64,s64))fn)(arg0,arg1,arg2);
			break;
		}
		case 4: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			result = ((s64(*)(s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3);
			break;
		}
		case 5: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			result = ((s64(*)(s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4);
			break;
		}
		case 6: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5);
			break;
		}
		case 7: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
			break;
		}
		case 8: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
			break;
		}
		case 9: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
			s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
			break;
		}
		case 10: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
			s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
			s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
			break;
		}
		case 11: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
			s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
			s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
			s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
			break;
		}
		case 12: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
			s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
			s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
			s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
			s64 arg11 = val8(Address{.base = Register::stack, .offset = 88});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11);
			break;
		}
		case 13: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
			s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
			s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
			s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
			s64 arg11 = val8(Address{.base = Register::stack, .offset = 88});
			s64 arg12 = val8(Address{.base = Register::stack, .offset = 96});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12);
			break;
		}
		case 14: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
			s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
			s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
			s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
			s64 arg11 = val8(Address{.base = Register::stack, .offset = 88});
			s64 arg12 = val8(Address{.base = Register::stack, .offset = 96});
			s64 arg13 = val8(Address{.base = Register::stack, .offset = 104});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13);
			break;
		}
		case 15: {
			s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
			s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
			s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
			s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
			s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
			s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
			s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
			s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
			s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
			s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
			s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
			s64 arg11 = val8(Address{.base = Register::stack, .offset = 88});
			s64 arg12 = val8(Address{.base = Register::stack, .offset = 96});
			s64 arg13 = val8(Address{.base = Register::stack, .offset = 104});
			s64 arg14 = val8(Address{.base = Register::stack, .offset = 112});
			result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14);
			break;
		}
		default: not_implemented();
	}
	val8(Address{.base = Register::stack, .offset = 8 * parameter_count}) = result;
}
void Interpreter::execute(Instruction::copyext_t i) {
	val8(i.d) = (s64)load_extern_function(i.lib, i.name);
}
void Interpreter::execute(Instruction::ret_t i) {
	{
		auto expected = debug_stack.pop().value();
		auto actual = val8(Register::stack);
		assert(actual == expected);
	}
		
	debug_call_stack.pop();

	auto prev_instruction_index = current_instruction_index;
	current_instruction_index = val8(Address{.base = Register::stack});
	E(add8, .d = Register::stack, .a = Register::stack, .b = 8);
}
void Interpreter::execute(Instruction::jmp_t i) { current_instruction_index = val8(i.d) - 1; }
void Interpreter::execute(Instruction::jf_t i) { if (val1(i.s) == 0) current_instruction_index = val8(i.d) - 1; }
void Interpreter::execute(Instruction::jt_t i) { if (val1(i.s) != 0) current_instruction_index = val8(i.d) - 1; }
void Interpreter::execute(Instruction::intrinsic_t i) {
	switch (i.i) {
		#define x(name) case Intrinsic::name: execute_intrinsic_##name(i); break;
		ENUMERATE_INTRINSICS
		#undef x
		default: invalid_code_path();
	}
}
	
void Interpreter::execute_intrinsic_print_S64(Instruction::intrinsic_t i) {
	scoped_replace_if(current_printer, {
		[](String string, void *) { append(Interpreter::current_interpreter->output_builder, string); }
	}, context_base->run_interactive);

	print(val8(Address { .base = Register::stack }));
}
void Interpreter::execute_intrinsic_print_String(Instruction::intrinsic_t i) {
	auto data  = val8(Address { .base = Register::stack });
	auto count = val8(Address { .base = Register::stack, .offset = 8 });

	scoped_replace_if(current_printer, {
		[](String string, void *) { append(Interpreter::current_interpreter->output_builder, string); }
	}, context_base->run_interactive);

	print(String((utf8 *)data, count));
}
void Interpreter::execute_intrinsic_panic(Instruction::intrinsic_t i) {
	immediate_reporter.error("PANIC: {}", i.message);
	invalid_code_path();
}
void Interpreter::execute_intrinsic_debug_break(Instruction::intrinsic_t i) {
	debug_break();
}
void Interpreter::execute_intrinsic_assert(Instruction::intrinsic_t i) {
	if (!val1(Address{.base = Register::stack})) {
		immediate_reporter.error(i.message, "Assertion failed: {}", i.message);
		longjmp(stop_interpering_jmp_buf, 1);
	}
}

TargetPlatform Interpreter::target_platform() {
	return {
		.create_bridge = &create_bridge,
	};
}

u64 Interpreter::ffi_callback(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda) {
	auto &parameters = lambda->head.parameters_block.definition_list;
	auto ret_size = ceil(get_size(lambda->head.return_type), 8ull);
	assert(parameters.count == 4, "Expected exactly 4 arguments. TODO: implement others");
	auto arg0_size = get_size(parameters[0]->type);
	auto arg1_size = get_size(parameters[1]->type);
	auto arg2_size = get_size(parameters[2]->type);
	auto arg3_size = get_size(parameters[3]->type);
	E(sub8, Register::stack, Register::stack, (s64)(ret_size + lambda->head.total_parameters_size));
	E(copy, .d = Address{.base = Register::stack, .offset = (s64)parameters[0]->offset}, .s = (s64)arg0, .size = arg0_size);
	E(copy, .d = Address{.base = Register::stack, .offset = (s64)parameters[1]->offset}, .s = (s64)arg1, .size = arg1_size);
	E(copy, .d = Address{.base = Register::stack, .offset = (s64)parameters[2]->offset}, .s = (s64)arg2, .size = arg2_size);
	E(copy, .d = Address{.base = Register::stack, .offset = (s64)parameters[3]->offset}, .s = (s64)arg3, .size = arg3_size);
	E(call, (s64)lambda->first_instruction_index);
		
	// NOTE: `call` does an offset by -1 because the main loop always increments the index, but we are not in a loop yet, so offset the offset.
	++current_instruction_index;

	umm target_stack_count = debug_stack.count;
	run_while([&] { return debug_stack.count >= target_stack_count; });

	// NOTE: now current_instruction_index is one past `callext` instruction and will be incremented again at the end of main loop iteration, which will make it
	// skip one instruction, so offset it here as well.
	--current_instruction_index;

	E(add8, Register::stack, Register::stack, (s64)(ret_size + lambda->head.total_parameters_size));
		
	auto result = val8(Address{.base = Register::stack, .offset = -(s64)ret_size});
	return result;
}

u64 Interpreter::ffi_callback_static(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda) {
	return Interpreter::current_interpreter->ffi_callback(arg0, arg1, arg2, arg3, lambda);
}
void *Interpreter::create_bridge(Lambda *lambda) {
	// arg0 - rcx
	// arg1 - rdx
	// arg2 - r8
	// arg3 - r9
	assert(lambda->head.parameters_block.definition_list.count == 4, "Other count of arguments not implemented");
	List<u8> bytes;
	
	constexpr u8 stack_size = 40;
	// sub rsp, 40 // 8 bytes for lambda, 32 bytes for shadow space -----, 8 dummy bytes to keep stack aligned
	bytes.add({0x48, 0x83, 0xec, stack_size});

	// mov r10, lambda
	bytes.add({0x49, 0xba});
	bytes.add(value_as_bytes(lambda));

	// mov r11, callback
	bytes.add({0x49, 0xbb});
	bytes.add(value_as_bytes(&ffi_callback_static));
	
	// mov qword ptr[rsp+32], r10
	bytes.add({0x4c, 0x89, 0x54, 0x24, 32});

	// call r11
	bytes.add({0x41, 0xff, 0xd3});
	
	// add rsp, 40
	bytes.add({0x48, 0x83, 0xc4, stack_size});

	// ret
	bytes.add({0xc3});


	#if OS_WINDOWS
	void *page = VirtualAlloc(0, bytes.count, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	#elif OS_LINUX
	void *page = mmap(NULL, bytes.count, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	#endif

	memcpy(page, bytes.data, bytes.count);
	memset((char *)page + bytes.count, 0xcc, ceil(bytes.count, (umm)4096) - bytes.count);

	#if OS_WINDOWS
	DWORD old_protect;
	if (!VirtualProtect(page, bytes.count, PAGE_EXECUTE_READ, &old_protect)) {
		immediate_reporter.error(lambda->location, "FATAL: VirtualProtect failed: {}", win32_error());
		exit(-1);
	}
	#elif OS_LINUX
	if (mprotect(page, bytes.count, PROT_READ | PROT_EXEC) == -1) {
		immediate_reporter.error(lambda->location, "FATAL: mprotect failed: {}", strerror(errno));
	}
	#endif

	return {page};
}


#undef E

}
