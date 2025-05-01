#define TL_IMPL
#include "../../src/common.h"
#include "../../src/bytecode/bytecode.h"
#include "../../src/x.h"
#include "../../src/reporter.h"

CompilerContext *context;

#include <tl/variant.h>

#include <Windows.h>

#pragma push_macro("assert")
#define X64W_IMPLEMENTATION
#define X64W_ENABLE_VALIDATION
#define X64W_ASSERT assert
#define X64W_NO_PREFIX
#include <x64write.h>
#pragma pop_macro("assert")

Array regnames8  { "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b", "", "", "", "", "spl", "bpl", "sil", "dil"};
Array regnames16 { "ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w", };
Array regnames32 { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d", };
Array regnames64 { "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", };

inline void append(StringBuilder &builder, Gpr8 r) { append(builder, regnames8[r.i]); }
inline void append(StringBuilder &builder, Gpr16 r) { append(builder, regnames16[r.i]); }
inline void append(StringBuilder &builder, Gpr32 r) { append(builder, regnames32[r.i]); }
inline void append(StringBuilder &builder, Gpr64 r) { append(builder, regnames64[r.i]); }

inline void append(StringBuilder &builder, Mem m) {
	append(builder, '[');
	if (m.base_scale) {
		if (m.size_override)
			append(builder, Gpr32{m.base});
		else
			append(builder, Gpr64{m.base});
	}
	if (m.index_scale) {
		if (m.base_scale)
			append(builder, '+');
		if (m.size_override)
			append(builder, Gpr32{m.index});
		else
			append(builder, Gpr64{m.index});
		append(builder, '*');
		append(builder, m.index_scale);
	}
	if (m.base_scale || m.index_scale) {
		if (m.displacement) {
			append(builder, '+');
			append(builder, m.displacement);
		}
	} else {
		append(builder, m.displacement);
	}
	append(builder, ']');
}

// Microsoft 64 bit calling convention - saved registers
// +-----+----------+
// | reg | volatile |
// +-----+----------+
// | rax |    +     |
// | rbx |          |
// | rcx |    +     |
// | rdx |    +     |
// | rsi |          |
// | rdi |          |
// | rsp |          |
// | rbp |          |
// | r8  |    +     |
// | r9  |    +     |
// | r10 |    +     |
// | r11 |    +     |
// | r12 |          |
// | r13 |          |
// | r14 |          |
// | r15 |          |
// +-----+----------+

#include <tl/process.h>

template <class T>
Span<T> same_start(Span<T> where, Span<T> what) {
	umm i = 0;
	for (; i < min(where.count, what.count); ++i) {
		if (what.data[i] != where.data[i]) {
			break;
		}
	}
	return where.subspan(0, i);
}

template <class T>
Span<T> find_most(Span<T> where, Span<T> what) {
	Span<T> result = where.subspan(0, 0);

	for (umm i = 0; i < where.count; ++i) {
		auto x = same_start(where.skip(i), what);
		if (x.count > result.count) {
			result = x;
		}
	}

	return result;

	if (what.count <= where.count) {
		for (umm i = 0; i < where.count - what.count + 1; ++i) {
			for (umm j = 0; j < what.count; ++j) {
				if (where.data[i + j] != what.data[j]) {
					if (j > result.count) {
						result = Span(where.data + i, j);
					}
					goto continue_outer;
				}
			}

			result = Span(where.data + i, what.count);
			break;
		continue_outer:;
		}
	}

	return result;
}
#pragma pack(push, 1) // Ensure no padding in structures

// COFF Header
typedef struct {
    uint16_t Machine;          // Machine type
    uint16_t NumberOfSections; // Number of sections
    uint32_t TimeDateStamp;    // Timestamp
    uint32_t PointerToSymbolTable; // Pointer to symbol table
    uint32_t NumberOfSymbols;  // Number of symbols
    uint16_t SizeOfOptionalHeader; // Size of optional header
    uint16_t Characteristics;   // Characteristics
} COFFHeader;

// Section Header
typedef struct {
    char Name[8];              // Section name
    // chatgpt is stupid uint32_t PhysicalAddress;  // Physical address
    uint32_t VirtualSize;      // Virtual size
    uint32_t VirtualAddress;    // Virtual address
    uint32_t SizeOfRawData;    // Size of raw data
    uint32_t PointerToRawData; // Pointer to raw data
    uint32_t PointerToRelocations; // Pointer to relocations
    uint32_t PointerToLinenumbers; // Pointer to line numbers
    uint16_t NumberOfRelocations; // Number of relocations
    uint16_t NumberOfLinenumbers; // Number of line numbers
    uint32_t Characteristics;   // Characteristics
} SectionHeader;

#pragma pack(pop)

void write_instructions_to_obj(Span<u8> instructions, Span<utf8> filename) {
	List<u8> bytes;

    // Create and write the COFF header
    COFFHeader coffHeader = {0};
    coffHeader.Machine = 0x8664; // x86-64
    coffHeader.NumberOfSections = 1;
    coffHeader.TimeDateStamp = 0; // Set to 0 for now
    coffHeader.PointerToSymbolTable = 0; // No symbols
    coffHeader.NumberOfSymbols = 0; // No symbols
    coffHeader.SizeOfOptionalHeader = 0; // No optional header
    coffHeader.Characteristics = 0 ; // Characteristics

    bytes.add(value_as_bytes(coffHeader));

    // Create and write the section header
	SectionHeader sectionHeader = {};
	memcpy(sectionHeader.Name, ".text$mn", 8);
    sectionHeader.VirtualSize = 0; // Size of the section
    sectionHeader.SizeOfRawData = instructions.count; // Size of raw data
    sectionHeader.PointerToRawData = sizeof(COFFHeader) + sizeof(SectionHeader); // Offset to raw data
    sectionHeader.Characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE;

    bytes.add(value_as_bytes(sectionHeader));
    bytes.add(instructions);

    write_entire_file(filename, bytes);
}

using Operand = Variant<Gpr8, Gpr16, Gpr32, Gpr64, Mem, s64>;
		
namespace b = Bytecode;

static u8 buf[65536 * 256] = {};

struct Emitter {
	u8 *c = buf;

	void emit(String target_executable_path, b::Bytecode bytecode) {

		for (auto i : bytecode.instructions) {
			emit_instruction(i);
		}
	}
	
	bool fits_in_1_byte (s64 x) { return ((x >>  8) + 1) <= 1; }
	bool fits_in_2_bytes(s64 x) { return ((x >> 16) + 1) <= 1; }
	bool fits_in_4_bytes(s64 x) { return ((x >> 32) + 1) <= 1; }
	u8 required_bytes(s64 x) {
		if (x == 0) return 0;
		if (fits_in_1_byte (x)) return 1;
		if (fits_in_2_bytes(x)) return 2;
		if (fits_in_4_bytes(x)) return 4;
		return 8;
	}

	inline static constexpr Array temp_regs = {rax};
	Optional<Gpr64> map(b::Register r) {
		switch (r) {
			case (b::Register)0: return rcx;
			case (b::Register)1: return rdx;
			case (b::Register)2: return r8;
			case (b::Register)3: return r9;
			case (b::Register)4: return r10;
			case (b::Register)5: return r11;
			case b::Register::stack: return rsp;
			case b::Register::base: return rbp;
		}
		return {};
	}
	Optional<Mem> map(b::Address a) {
		Mem result = {};
		if (a.base) {
			auto base = map(a.base.value());
			if (!base)
				return {};

			result.base = base.value().i;
			result.base_scale = 1;
		}
		auto index = map(a.element_index);
		if (!index)
			return {};
		result.index = index.value().i;
		switch (a.element_size) {
			case 0: break;
			case 1: result.index_scale = 1; break;
			case 2: result.index_scale = 2; break;
			case 4: result.index_scale = 4; break;
			case 8: result.index_scale = 8; break;
			default: return {};
		}

		assert(fits_in_32(a.offset), "Unmappable address: a.offset is bigger that 32 bit: {}", a.offset);

		result.displacement = (s32)a.offset;
		
		return result;
	}

	void emit_instruction(b::Instruction i) {
		switch (i.kind) {
			#define x(name, fields) case b::InstructionKind::name: return emit_instruction_impl(i.v_##name);
			ENUMERATE_BYTECODE_INSTRUCTION_KIND
			#undef x
		}
		invalid_code_path("Invalid instruction kind: {}", i.kind);
	}
	void emit_instruction_impl(b::Instruction::nop_t i) {}
	void emit_instruction_impl(b::Instruction::push_t i) {
		if (i.s.is_constant()) {
			assert(x64w_fits_in_32(i.s.get_constant()));
			push_i32(&c, i.s.get_constant());
		} else if (i.s.is_register()) {
			auto mapped = map(i.s.get_register());
			if (mapped) {
				push_r64(&c, mapped.value());
			} else {
				not_implemented();
			}
		} else {
			not_implemented();
		}
	}
	void emit_instruction_impl(b::Instruction::pop_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::copy_t i) {
		if (i.d.is_register()) {
			auto mapped_d = map(i.d.get_register());
			if (mapped_d) {
				if (i.s.is_constant()) {
					mov_ri64(&c, mapped_d.value(), i.s.get_constant());
				} else if (i.s.is_register()) {
					auto mapped_s = map(i.s.get_register());
					if (mapped_s) {
						mov_rr64(&c, mapped_d.value(), mapped_s.value());
					} else {
						not_implemented();
					}
				} else {
					not_implemented();
				}
			} else {
				not_implemented();
			}
		}
	}
	void emit_instruction_impl(b::Instruction::set_t i) {
		switch (i.size) {
			case 0: {
				break;
			}
			case 8: {
				auto mapped_d = map(i.d);
				if (mapped_d) {
					u64 x;
					memset(&x, i.value, sizeof(x));
					if (fits_in_32(x)) {
						mov_m64i32(&c, mapped_d.value(), x);
					} else {
						not_implemented();
					}
				} else {
					not_implemented();
				}
				break;
			}
			default: {
				not_implemented();
				break;
			}
		}
	}
	void emit_instruction_impl(b::Instruction::lea_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::add1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sub1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::mul1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::div1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::mod1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::xor1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::and1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::or1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sll1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::srl1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sra1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::cmp1_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::add2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sub2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::mul2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::div2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::mod2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::xor2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::and2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::or2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sll2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::srl2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sra2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::cmp2_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::add4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sub4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::mul4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::div4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::mod4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::xor4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::and4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::or4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sll4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::srl4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sra4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::cmp4_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::add8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sub8_t i) {
		if (i.d.is_register() && i.a.is_register() && (i.d.get_register() == i.a.get_register())) {
			auto mapped_d = map(i.d.get_register());
			auto mapped_a = map(i.a.get_register());

			if (mapped_d && mapped_a) {
				if (i.b.is_constant()) {
					auto b = -i.b.get_constant();
					if (fits_in_8(b)) {
						add_r64i8(&c, mapped_d.value(), b);
					} else {
						add_r64i32(&c, mapped_d.value(), b);
					}
				} else if (i.b.is_register()) {
					auto mapped_b = map(i.b.get_register());
					if (mapped_b) {
						add_rr64(&c, mapped_d.value(), mapped_b.value());
					} else {
						not_implemented();
					}
				} else {
					not_implemented();
				}
			} else {
				not_implemented();
			}
		} else {
			not_implemented();
		}
	}
	void emit_instruction_impl(b::Instruction::mul8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::div8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::mod8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::xor8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::and8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::or8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sll8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::srl8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sra8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::cmp8_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sex21_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sex41_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sex42_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sex81_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sex82_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::sex84_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::call_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::callext_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::copyext_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::ret_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::jmp_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::jf_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::jt_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::intrinsic_t i) { not_implemented(); }
};

extern "C" __declspec(dllexport)
void init(CompilerContext *c) {
	context = c;

	init_allocator();
	init_printer();
}

extern "C" __declspec(dllexport)
void convert_bytecode(b::Bytecode bytecode) {

	Emitter emitter;
	emitter.emit(tformat(u8"{}.exe", parse_path(context->input_source_path).path_without_extension()), bytecode);
}

// FIXME: Copied from main.cpp
void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, Span<char> message) {
	scoped(context->stdout_mutex);

	immediate_reporter.error("COMPILER ERROR: {} {} at {}:{} in function {}", cause_string, expression, file, line, function);
	if (message.count)
		println("Message: {}", message);

	println("Call stack:");
	println(resolve_names(get_call_stack().skip(1).skip(-7)));
}
