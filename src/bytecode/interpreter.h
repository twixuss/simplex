#pragma once
#include "bytecode.h"
#include <conio.h>

using namespace tl;

namespace Bytecode {

struct Interpreter {
	inline static Interpreter *current_interpreter;
	Bytecode *bytecode = 0;
	bool interactive = false;

	struct DebugWindow {
		static constexpr int source    = 0x1;
		static constexpr int arguments = 0x2;
		static constexpr int locals    = 0x4;
		static constexpr int bytecode  = 0x8;
		static constexpr int registers = 0x10;
		static constexpr int stack     = 0x20;
	};

	int enabled_windows = ~0;

	struct Commands {
		static constexpr char next_instruction = 'n';
		static constexpr char redraw_window = 'r';
	};

	Optional<u64> run(Bytecode *bytecode, umm entry_index, bool interactive) {
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
					auto lambda = found->value;
					auto loc = get_source_location(lambda->location);
					println("{}:{}: {}", loc.file, loc.line_number, lambda->definition ? lambda->definition->name : u8"(unnamed)"s);
				} else {
					println("unknown at #{}", index);
				}
			}
			return {};
		}

		return val8(Address { .offset = (s64)((stack + stack_size) - 8)});
	}
	void run_while(auto predicate) {
		while (predicate()) {
			auto i = bytecode->instructions[current_instruction_index];

			if (interactive) {
			redraw:
				clear_console();
				auto column2 = []() {
					for (int i = 0; i < 80; ++i)
						print(' ');
				};
				
				auto header = [&](char const *name, int flag) {
					with(((enabled_windows & flag) ? ConsoleColor::cyan : ConsoleColor::gray), println("==== {} {} ====", log2(flag), name));
					return enabled_windows & flag;
				};

				column2();
				if (header("Bytecode", DebugWindow::bytecode)) {
					for (s64 o = -5; o <= 5; ++o) {
						scoped_if(ConsoleColor::yellow, o == 0);

						u64 instruction_index = current_instruction_index + o;
						if (instruction_index >= bytecode->instructions.count) {
							column2(); println("...");
						} else {
							column2(); print_instruction(instruction_index, bytecode->instructions[instruction_index]);
						}
					}
				}
				column2();
				if (header("Registers", DebugWindow::registers)) {
					auto print_register = [&] (Register r) {
						print("0x{}", FormatInt{.value = reg(r), .radix = 16, .leading_zero_count = 16});
					};

					column2(); print("base: "); print_register(Register::base); println();
					column2(); print("stack: "); print_register(Register::stack); println();
					for (int i = 0; i < 8; ++i) {
						column2(); print("r{}: ", i); print_register((Register)i); println();
					}
				}
				

				Lambda *lambda = 0;
				if (enabled_windows & (DebugWindow::locals | DebugWindow::arguments | DebugWindow::stack)) {
					u64 max_lambda_first_instruction = 0;
					if (current_instruction_index < bytecode->instructions.count - 3) { // Ignore initial instructions that don't belong to any lambda
						for (auto [first_instruction, some_lambda] : bytecode->first_instruction_to_lambda) {
							if (current_instruction_index >= first_instruction) {
								if (first_instruction > max_lambda_first_instruction) {
									max_lambda_first_instruction = first_instruction;
									lambda = some_lambda;
								}
							}
						}
					}
				}
				
				column2();
				if (header("Stack", DebugWindow::stack)) {
					if (lambda) {
						for (s64 i = lambda->stack_frame_size - 8; i >= 0; i -= 8) {
							s64 addr = reg(Register::stack) + i;
							column2(); 
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
				
				SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), {0, 0});

				if (header("Source", DebugWindow::source)) {
					if (i.source_location.count) {
						auto location = get_source_location(i.source_location);

						print_source_chunk(location, 0, ConsoleColor::yellow);
					} else {
						println("unknown source");
					}
				}
				if (header("Arguments", DebugWindow::arguments)) {
					if (lambda) {
						for (auto parameter : lambda->head.parameters_block.definition_list) {
							print("{}: ", parameter->name);
							Address address = {
								.base = Register::base,
								.offset = (s64)(16 + parameter->offset),
							};
							print_value(address, parameter->type);
							println();
						}
					}
				}
				if (header("Locals", DebugWindow::locals)) {
					if (lambda) {
						for (auto local : lambda->locals) {
							print("{}: ", local->name);
							Address address = {
								.base = Register::base,
								.offset = (s64)(-lambda->locals_size + local->offset),
							};
							print_value(address, local->type);
							println();
						}
					}
				}

				println();
				println();
				println("{} - next instruction", Commands::next_instruction);
				println("{} - redraw window", Commands::redraw_window);

			retry_char:
				int pressed_key = _getch();
				switch (pressed_key) {
					ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
						(int &)enabled_windows ^= 1 << (pressed_key - '0');
						goto redraw;
					}
					case Commands::next_instruction: {
						break;
					}
					case Commands::redraw_window: {
						goto redraw;
					}
					default: {
						goto retry_char;
					}
				}
			}

			switch (i.kind) {
#define x(name, fields) case InstructionKind::name: execute(i.v_##name); break;
				ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
			}
			++current_instruction_index;
		}
	}
	
	struct PrintValueOptions {
		bool hex = false;
	};

	int print_value(Address address, Type type, PrintValueOptions options = {}) {
		__try {
			auto print_int = [&]<class T>(auto address) {
				auto val = [&](Address address) {
					if constexpr (sizeof(T) == 1) return val1(address);
					if constexpr (sizeof(T) == 2) return val2(address);
					if constexpr (sizeof(T) == 4) return val4(address);
					if constexpr (sizeof(T) == 8) return val8(address);
				};

				auto v = FormatInt{(T)val(address)};
				if (options.hex) {
					v = format_hex(v.value); 
					print("0x");
				}
				return print(v);
			};

			if (types_match(type, BuiltinType::None)) return print("none");
			if (types_match(type, BuiltinType::Bool)) return print((bool)val1(address));
			if (types_match(type, BuiltinType::U8 )) { return print_int.operator()<u8 >(address); }
			if (types_match(type, BuiltinType::U16)) { return print_int.operator()<u16>(address); }
			if (types_match(type, BuiltinType::U32)) { return print_int.operator()<u32>(address); }
			if (types_match(type, BuiltinType::U64)) { return print_int.operator()<u64>(address); }
			if (types_match(type, BuiltinType::S8 )) { return print_int.operator()<s8 >(address); }
			if (types_match(type, BuiltinType::S16)) { return print_int.operator()<s16>(address); }
			if (types_match(type, BuiltinType::S32)) { return print_int.operator()<s32>(address); }
			if (types_match(type, BuiltinType::S64)) { return print_int.operator()<s64>(address); }

			if (auto array = as<ArrayType>(type)) {
				print("[");
				auto element_size = get_size(array->element_type);
				for (umm i = 0; i < array->count.value(); ++i) {
					if (i) print(", ");
					auto element_address = address;
					element_address.offset += i * element_size;
					print_value(element_address, array->element_type);
				}
				print("]");
				return 0;
			}

			return print("unsupported type");
		} __except (EXCEPTION_EXECUTE_HANDLER) {
			return print("???");
		}
	}

	umm current_instruction_index = (umm)-1;
	s64 registers[256] = {};
	u8 *stack = 0;
	List<u64> debug_stack;
	List<s64> debug_call_stack;

	struct Library {
		Dll dll;
		HashMap<String, void *> functions;
	};
	HashMap<String, Library> libraries;

	s64 &reg(Register r) {
		return registers[to_underlying(r)];
	}
	s64 &val8(Register r) {
		return registers[to_underlying(r)];
	}
	s64 &val8(Address a) {
		s64 x = a.offset;
		if (a.base) {
			x += reg(a.base.value());
		}
		if (a.element_size) {
			x += reg(a.element_index) * a.element_size;
		}

		return *(s64 *)x;
	}
	s64 &val8(Site s) {
		if (s.is_register()) {
			return reg(s.get_register());
		} else {
			return val8(s.get_address());
		}
	}
	s64 &val8(InputValue v) {
		if (v.is_register()) {
			return reg(v.get_register());
		} else if (v.is_address()) {
			return val8(v.get_address());
		} else {
			return v.get_constant();
		}
	}
	s32 &val4(Register   x) { return (s32 &)val8(x); }
	s32 &val4(Address    x) { return (s32 &)val8(x); }
	s32 &val4(Site       x) { return (s32 &)val8(x); }
	s32 &val4(InputValue x) { return (s32 &)val8(x); }
	s16 &val2(Register   x) { return (s16 &)val8(x); }
	s16 &val2(Address    x) { return (s16 &)val8(x); }
	s16 &val2(Site       x) { return (s16 &)val8(x); }
	s16 &val2(InputValue x) { return (s16 &)val8(x); }
	s8  &val1(Register   x) { return (s8  &)val8(x); }
	s8  &val1(Address    x) { return (s8  &)val8(x); }
	s8  &val1(Site       x) { return (s8  &)val8(x); }
	s8  &val1(InputValue x) { return (s8  &)val8(x); }

#define E(name, ...) execute(Instruction{.kind = InstructionKind::name, .v_##name = { __VA_ARGS__ }}.v_##name)

	void execute(Instruction::pop_t i) {
		E(copy, .d = i.d, .s = Address{.base = Register::stack}, .size = 8);
		E(add8, .d = Register::stack, .a = Register::stack, .b = 8);
	}

	void execute(Instruction::nop_t i) {}
	void execute(Instruction::push_t i) {
		E(sub8, .d = Register::stack, .a = Register::stack, .b = 8);
		E(copy, .d = Address{.base = Register::stack}, .s = i.s, .size = 8);
	}
	void execute(Instruction::copy_t i) {
		auto d = &val8(i.d);
		auto s = &val8(i.s);
		memcpy(d, s, i.size);
	}
	void execute(Instruction::set_t i) {
		auto d = &val8(i.d);
		memset(d, i.value, i.size);
	}
	void execute(Instruction::lea_t i) { val8(i.d) = (s64) &val8(i.s); }
	void execute(Instruction::add1_t i) { val1(i.d) = val1(i.a) + val1(i.b); }
	void execute(Instruction::sub1_t i) { val1(i.d) = val1(i.a) - val1(i.b); }
	void execute(Instruction::mul1_t i) { val1(i.d) = val1(i.a) * val1(i.b); }
	void execute(Instruction::div1_t i) { val1(i.d) = val1(i.a) / val1(i.b); }
	void execute(Instruction::mod1_t i) { val1(i.d) = val1(i.a) % val1(i.b); }
	void execute(Instruction::xor1_t i) { val1(i.d) = val1(i.a) ^ val1(i.b); }
	void execute(Instruction::and1_t i) { val1(i.d) = val1(i.a) & val1(i.b); }
	void execute(Instruction::or1_t i)  { val1(i.d) = val1(i.a) | val1(i.b); }
	void execute(Instruction::sll1_t i) { val1(i.d) = val1(i.a) << val1(i.b); }
	void execute(Instruction::srl1_t i) { val1(i.d) = (u8)val1(i.a) >> (u8)val1(i.b); }
	void execute(Instruction::sra1_t i) { val1(i.d) = (s8)val1(i.a) >> (s8)val1(i.b); }
	void execute(Instruction::cmp1_t i) {
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
	void execute(Instruction::add2_t i) { val2(i.d) = val2(i.a) + val2(i.b); }
	void execute(Instruction::sub2_t i) { val2(i.d) = val2(i.a) - val2(i.b); }
	void execute(Instruction::mul2_t i) { val2(i.d) = val2(i.a) * val2(i.b); }
	void execute(Instruction::div2_t i) { val2(i.d) = val2(i.a) / val2(i.b); }
	void execute(Instruction::mod2_t i) { val2(i.d) = val2(i.a) % val2(i.b); }
	void execute(Instruction::xor2_t i) { val2(i.d) = val2(i.a) ^ val2(i.b); }
	void execute(Instruction::and2_t i) { val2(i.d) = val2(i.a) & val2(i.b); }
	void execute(Instruction::or2_t i)  { val2(i.d) = val2(i.a) | val2(i.b); }
	void execute(Instruction::sll2_t i) { val2(i.d) = val2(i.a) << val2(i.b); }
	void execute(Instruction::srl2_t i) { val2(i.d) = (u16)val2(i.a) >> (u16)val2(i.b); }
	void execute(Instruction::sra2_t i) { val2(i.d) = (s16)val2(i.a) >> (s16)val2(i.b); }
	void execute(Instruction::cmp2_t i) {
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
	void execute(Instruction::add4_t i) { val4(i.d) = val4(i.a) + val4(i.b); }
	void execute(Instruction::sub4_t i) { val4(i.d) = val4(i.a) - val4(i.b); }
	void execute(Instruction::mul4_t i) { val4(i.d) = val4(i.a) * val4(i.b); }
	void execute(Instruction::div4_t i) { val4(i.d) = val4(i.a) / val4(i.b); }
	void execute(Instruction::mod4_t i) { val4(i.d) = val4(i.a) % val4(i.b); }
	void execute(Instruction::xor4_t i) { val4(i.d) = val4(i.a) ^ val4(i.b); }
	void execute(Instruction::and4_t i) { val4(i.d) = val4(i.a) & val4(i.b); }
	void execute(Instruction::or4_t i)  { val4(i.d) = val4(i.a) | val4(i.b); }
	void execute(Instruction::sll4_t i) { val4(i.d) = val4(i.a) << val4(i.b); }
	void execute(Instruction::srl4_t i) { val4(i.d) = (u32)val4(i.a) >> (u32)val4(i.b); }
	void execute(Instruction::sra4_t i) { val4(i.d) = (s32)val4(i.a) >> (s32)val4(i.b); }
	void execute(Instruction::cmp4_t i) {
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
	void execute(Instruction::add8_t i) { val8(i.d) = val8(i.a) + val8(i.b); }
	void execute(Instruction::sub8_t i) { val8(i.d) = val8(i.a) - val8(i.b); }
	void execute(Instruction::mul8_t i) { val8(i.d) = val8(i.a) * val8(i.b); }
	void execute(Instruction::div8_t i) { val8(i.d) = val8(i.a) / val8(i.b); }
	void execute(Instruction::mod8_t i) { val8(i.d) = val8(i.a) % val8(i.b); }
	void execute(Instruction::xor8_t i) { val8(i.d) = val8(i.a) ^ val8(i.b); }
	void execute(Instruction::and8_t i) { val8(i.d) = val8(i.a) & val8(i.b); }
	void execute(Instruction::or8_t i)  { val8(i.d) = val8(i.a) | val8(i.b); }
	void execute(Instruction::sll8_t i) { val8(i.d) = val8(i.a) << val8(i.b); }
	void execute(Instruction::srl8_t i) { val8(i.d) = (u64)val8(i.a) >> (u64)val8(i.b); }
	void execute(Instruction::sra8_t i) { val8(i.d) = (s64)val8(i.a) >> (s64)val8(i.b); }
	void execute(Instruction::cmp8_t i) {
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
	void execute(Instruction::sex21_t i) { val8(i.d) = (s16)(s8)val1(i.a); }
	void execute(Instruction::sex41_t i) { val8(i.d) = (s32)(s8)val1(i.a); }
	void execute(Instruction::sex42_t i) { val8(i.d) = (s32)(s16)val2(i.a); }
	void execute(Instruction::sex81_t i) { val8(i.d) = (s64)(s8)val1(i.a); }
	void execute(Instruction::sex82_t i) { val8(i.d) = (s64)(s16)val2(i.a); }
	void execute(Instruction::sex84_t i) { val8(i.d) = (s64)(s32)val4(i.a); }

	void execute(Instruction::call_t i) {
		E(push, (s64)current_instruction_index);

		auto call_to = val8(i.d);
		
		debug_call_stack.add(call_to);

		// NOTE: offset by -1 because it will be incremented in the main loop
		current_instruction_index = call_to - 1;

		debug_stack.add(val8(Register::stack));
	}
	void execute(Instruction::callext_t i) {
		auto &lib = libraries.get_or_insert(i.lib);
		if (!lib.dll) {
			auto lib_name = tformat(u8"{}{}"s, i.lib, dll_extension);
			lib.dll = load_dll(lib_name);
			assert(lib.dll);
		}

		auto &fn = lib.functions.get_or_insert(i.name);
		if (!fn) {
			fn = get_symbol(lib.dll, i.name);
			assert(fn, "{}.dll does not contain {}", i.lib, i.name);
		}

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
	void execute(Instruction::ret_t i) {
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
	void execute(Instruction::jmp_t i) { current_instruction_index = val8(i.d) - 1; }
	void execute(Instruction::jf_t i) { if (val1(i.s) == 0) current_instruction_index = val8(i.d) - 1; }
	void execute(Instruction::jt_t i) { if (val1(i.s) != 0) current_instruction_index = val8(i.d) - 1; }
	void execute(Instruction::intrinsic_t i) {
		switch (i.i) {
			case Intrinsic::println_S64: {
				println(val8(Address { .base = Register::stack }));
				break;
			}
			case Intrinsic::println_String: {
				auto data  = val8(Address { .base = Register::stack });
				auto count = val8(Address { .base = Register::stack, .offset = 8 });
				println(String((utf8 *)data, count));
				break;
			}
			case Intrinsic::panic: {
				immediate_reporter.error("PANIC");
				invalid_code_path();
				break;
			}
			case Intrinsic::debug_break: {
				debug_break();
				break;
			}
		}
	}

	u64 ffi_callback(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda) {
		auto ret_size = get_size(lambda->head.return_type);
		auto arg0_size = get_size(lambda->head.parameters_block.definition_list[0]->type);
		auto arg1_size = get_size(lambda->head.parameters_block.definition_list[1]->type);
		auto arg2_size = get_size(lambda->head.parameters_block.definition_list[2]->type);
		auto arg3_size = get_size(lambda->head.parameters_block.definition_list[3]->type);
		E(sub8, Register::stack, Register::stack, (s64)(ret_size + arg0_size + arg1_size + arg2_size + arg3_size));
		E(copy, .d = Address{.base = Register::stack, .offset = 0                                       }, .s = (s64)arg0, .size = arg0_size);
		E(copy, .d = Address{.base = Register::stack, .offset = (s64)(arg0_size                        )}, .s = (s64)arg1, .size = arg1_size);
		E(copy, .d = Address{.base = Register::stack, .offset = (s64)(arg0_size + arg1_size            )}, .s = (s64)arg2, .size = arg2_size);
		E(copy, .d = Address{.base = Register::stack, .offset = (s64)(arg0_size + arg1_size + arg2_size)}, .s = (s64)arg3, .size = arg3_size);
		E(call, (s64)lambda->first_instruction_index);
		
		// NOTE: `call` does an offset by -1 because the main loop always increments the index, but we are not in a loop yet, so offset the offset.
		++current_instruction_index;

		umm target_stack_count = debug_stack.count;
		run_while([&] { return debug_stack.count >= target_stack_count; });

		// NOTE: now current_instruction_index is one past `callext` instruction and will be incremented again at the end of main loop iteration, which will make it
		// skip one instruction, so offset it here as well.
		--current_instruction_index;

		E(add8, Register::stack, Register::stack, (s64)(ret_size + arg0_size + arg1_size + arg2_size + arg3_size));
		
		auto result = val8(Address{.base = Register::stack, .offset = -(s64)ret_size});
		return result;
	}

#undef E
};

}
