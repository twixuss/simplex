#include "x64.h"

#include <tl/variant.h>

#include <Windows.h>

#pragma push_macro("assert")
#define X64W_IMPLEMENTATION
#define X64W_ENABLE_VALIDATION
#define X64W_ASSERT assert
#define X64W_NO_PREFIX
#include "x64write.h"
#pragma pop_macro("assert")

Array regnames8  { "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b", "", "", "", "", "spl", "bpl", "sil", "dil"};
Array regnames16 { "ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w", };
Array regnames32 { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d", };
Array regnames64 { "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", };

inline umm append(StringBuilder &builder, Gpr8 r) { return append(builder, regnames8[r.i]); }
inline umm append(StringBuilder &builder, Gpr16 r) { return append(builder, regnames16[r.i]); }
inline umm append(StringBuilder &builder, Gpr32 r) { return append(builder, regnames32[r.i]); }
inline umm append(StringBuilder &builder, Gpr64 r) { return append(builder, regnames64[r.i]); }

inline umm append(StringBuilder &builder, Mem m) {
	umm result = 0;
	result += append(builder, '[');
	if (m.base_scale) {
		if (m.size_override)
			result += append(builder, Gpr32{m.base});
		else
			result += append(builder, Gpr64{m.base});
	}
	if (m.index_scale) {
		if (result != 1)
			result += append(builder, '+');
		if (m.size_override)
			result += append(builder, Gpr32{m.index});
		else
			result += append(builder, Gpr64{m.index});
		result += append(builder, '*');
		result += append(builder, m.index_scale);
	}
	if (m.base_scale || m.index_scale) {
		if (m.displacement) {
			result += append(builder, '+');
			result += append(builder, m.displacement);
		}
	} else {
		result += append(builder, m.displacement);
	}
	result += append(builder, ']');
	return result;
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
		
struct InstrDesc {
	String mnemonic;
	StaticList<Operand, 2> operands;
};
	
struct InstrInfo : InstrDesc {
	u8 *start;
	u8 size;
	u8 operand_size;
};

inline umm append(StringBuilder &builder, InstrInfo i) {
	append_format(builder, "{} ", i.mnemonic);
	for (auto &op : i.operands) {
		if (&op != i.operands.begin()) append(builder, ", ");
		append(builder, op);
	}
	return 0;
}

Optional<InstrDesc> parse_dumpbin_disasm_line(String line) {

	line = line.skip(20); // skip whitespace, index and colon
	
	if (!line.count)
		return {};

	auto first_space = find(line, u8' ');
	if (!first_space)
		return {};

	InstrDesc result;
	result.mnemonic = {line.begin(), first_space};

	line.set_begin(first_space);
	line = trim(line, [](auto x) { return is_whitespace((ascii)x); });

	auto parse_register = [&]() -> Variant<Empty, Gpr8, Gpr16, Gpr32, Gpr64> {
		#define PARSE(x) \
			else if (starts_with(line, u8###x##s)) { line.set_begin(line.begin() + u8###x##s.count); return x; }

		if (false) {}
		PARSE(al)
		PARSE(cl)
		PARSE(dl)
		PARSE(bl)
		PARSE(ah)
		PARSE(ch)
		PARSE(dh)
		PARSE(bh)
		PARSE(spl)
		PARSE(bpl)
		PARSE(sil)
		PARSE(dil)
		PARSE(r8b)
		PARSE(r9b)
		PARSE(r10b)
		PARSE(r11b)
		PARSE(r12b)
		PARSE(r13b)
		PARSE(r14b)
		PARSE(r15b)
		PARSE(ax)
		PARSE(cx)
		PARSE(dx)
		PARSE(bx)
		PARSE(sp)
		PARSE(bp)
		PARSE(si)
		PARSE(di)
		PARSE(r8w)
		PARSE(r9w)
		PARSE(r10w)
		PARSE(r11w)
		PARSE(r12w)
		PARSE(r13w)
		PARSE(r14w)
		PARSE(r15w)
		PARSE(eax)
		PARSE(ecx)
		PARSE(edx)
		PARSE(ebx)
		PARSE(esp)
		PARSE(ebp)
		PARSE(esi)
		PARSE(edi)
		PARSE(r8d)
		PARSE(r9d)
		PARSE(r10d)
		PARSE(r11d)
		PARSE(r12d)
		PARSE(r13d)
		PARSE(r14d)
		PARSE(r15d)
		PARSE(rax)
		PARSE(rcx)
		PARSE(rdx)
		PARSE(rbx)
		PARSE(rsp)
		PARSE(rbp)
		PARSE(rsi)
		PARSE(rdi)
		PARSE(r8)
		PARSE(r9)
		PARSE(r10)
		PARSE(r11)
		PARSE(r12)
		PARSE(r13)
		PARSE(r14)
		PARSE(r15)
		return {};

		#undef PARSE
	};

	int size = 8;

	while (line.count) {
		if (auto reg = parse_register(); !reg.is<Empty>()) {

			reg.visit(Combine {
				[&](auto x) { result.operands.add(x); },
				[&](Empty) {},
			});
			reg.visit(Combine {
				[&](auto x) {},
				[&](Gpr8) {size = 1;},
				[&](Gpr16) {size = 2;},
				[&](Gpr32) {size = 4;},
				[&](Gpr64) {size = 8;},
			});
		} else if (
			(starts_with(line, u8"byte ptr ["s) && (line.set_begin(line.begin() + 10), true)) ||
			(starts_with(line, u8"word ptr ["s) && (line.set_begin(line.begin() + 10), true)) ||
			(starts_with(line, u8"dword ptr ["s) && (line.set_begin(line.begin() + 11), true)) ||
			(starts_with(line, u8"qword ptr ["s) && (line.set_begin(line.begin() + 11), true))
		) {
			Mem m = {};

			bool has_regs = false;

			if (auto reg = parse_register(); !reg.is<Empty>()) {
				has_regs = true;

				if (reg.is<Gpr32>()) {
					m.size_override = 1;
				}

				m.base_scale = 1;
				reg.visit(Combine {
					[&](auto x) { m.base = x.i; },
					[&](Empty) {},
				});
			}

			if (line.front() == '*') {
				m.index = m.base;
				m.base = 0;
				m.base_scale = 0;
				line.set_begin(line.begin() + 1);
				if (false) {}
				else if (line.front() == '1') { m.index_scale = 1; }
				else if (line.front() == '2') { m.index_scale = 2; }
				else if (line.front() == '4') { m.index_scale = 4; }
				else if (line.front() == '8') { m.index_scale = 8; }
				line.set_begin(line.begin() + 1);
			} else if (line.front() == '+') {
				line.set_begin(line.begin() + 1);
				if (auto reg = parse_register(); !reg.is<Empty>()) {
					has_regs = true;
					reg.visit(Combine {
						[&](auto x) { m.index = x.i; },
						[&](Empty) {},
					});
					if (line.front() == '*') {
						line.set_begin(line.begin() + 1);
						if (false) {}
						else if (line.front() == '1') { m.index_scale = 1; }
						else if (line.front() == '2') { m.index_scale = 2; }
						else if (line.front() == '4') { m.index_scale = 4; }
						else if (line.front() == '8') { m.index_scale = 8; }
						line.set_begin(line.begin() + 1);
					} else {
						m.index_scale = 1;
					}
				} else {
					line.set_begin(line.begin() - 1);
				}
			}
						
			if ((line.front() == '+' && (line.set_begin(line.begin() + 1), true)) || !has_regs) {
				s64 disp = 0;
				while (is_hex_digit(line.front())) {
					disp = disp * 16 + hex_digit_to_int(line.front());
					line.set_begin(line.begin() + 1);
				}
							
				assert(line.front() == 'h');
				line.set_begin(line.begin() + 1);
				m.displacement = disp;
			}

			assert(line.front() == ']');
			line.set_begin(line.begin() + 1);

			result.operands.add(m);
		} else {
			s64 imm = 0;
			while (is_hex_digit(line.front())) {
				imm = imm * 16 + hex_digit_to_int(line.front());
				line.set_begin(line.begin() + 1);
			}
							
			assert(line.front() == 'h');
			line.set_begin(line.begin() + 1);
			switch (size) {
				case 1: result.operands.add((s64)(s8)imm); break;
				case 2: result.operands.add((s64)(s16)imm); break;
				case 4: result.operands.add((s64)(s32)imm); break;
				case 8: result.operands.add(imm); break;
			}
		}
				
		line = trim(line, [](auto x) { return is_whitespace((ascii)x); });
		if (line.count && line.front() == ',') line.set_begin(line.begin() + 1);
		line = trim(line, [](auto x) { return is_whitespace((ascii)x); });
	}

	return result;
}

namespace target_x64 {

static u8 buf[65536 * 256] = {};

struct Emitter {
	void emit(String target_executable_path, b::Bytecode bytecode) {
		{
			Array regs8  {al, cl, dl, bl, ah, ch, dh, bh, r8b,r9b,r10b,r11b,r12b,r13b,r14b,r15b,spl,bpl,sil,dil,};
			Array regs16 {ax, cx, dx, bx, sp, bp, si, di, r8w,r9w,r10w,r11w,r12w,r13w,r14w,r15w,};
			Array regs32 {eax,ecx,edx,ebx,esp,ebp,esi,edi,r8d,r9d,r10d,r11d,r12d,r13d,r14d,r15d,};
			Array regs64 {rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8, r9, r10, r11, r12, r13, r14, r15,};

			u8 *c = buf;
			
			List<InstrInfo> instr_infos;

			List<Mem> mems;
			
			mems.add(mem64_d(0x34));
			
			mems.add(mem64_d(0x3456));
			
			for (u8 i = 0; i < 16; ++i)
				mems.add(mem64_b((Gpr64{i})));
			for (u8 i = 0; i < 16; ++i)
				mems.add(mem64_bd((Gpr64{i}), 0x34));	
			for (u8 i = 0; i < 16; ++i)
				mems.add(mem64_bd((Gpr64{i}), 0x3456));	
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_i((Gpr64{i}), 1));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_i((Gpr64{i}), 2));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_i((Gpr64{i}), 4));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_i((Gpr64{i}), 8));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_id((Gpr64{i}), 1, 0x34));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_id((Gpr64{i}), 2, 0x34));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_id((Gpr64{i}), 4, 0x34));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_id((Gpr64{i}), 8, 0x34));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_id((Gpr64{i}), 1, 0x3456));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_id((Gpr64{i}), 2, 0x3456));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_id((Gpr64{i}), 4, 0x3456));
			for (u8 i = 0; i < 16; ++i)
				if (i != 4)
					mems.add(mem64_id((Gpr64{i}), 8, 0x3456));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bi((Gpr64{i}), (Gpr64{j}), 1));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bi((Gpr64{i}), (Gpr64{j}), 2));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bi((Gpr64{i}), (Gpr64{j}), 4));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bi((Gpr64{i}), (Gpr64{j}), 8));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bid((Gpr64{i}), (Gpr64{j}), 1, 0x34));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bid((Gpr64{i}), (Gpr64{j}), 2, 0x34));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bid((Gpr64{i}), (Gpr64{j}), 4, 0x34));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bid((Gpr64{i}), (Gpr64{j}), 8, 0x34));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bid((Gpr64{i}), (Gpr64{j}), 1, 0x3456));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bid((Gpr64{i}), (Gpr64{j}), 2, 0x3456));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bid((Gpr64{i}), (Gpr64{j}), 4, 0x3456));
			for (u8 i = 0; i < 16; ++i)
				for (u8 j = 0; j < 16; ++j)
					if (j != 4)
						mems.add(mem64_bid((Gpr64{i}), (Gpr64{j}), 8, 0x3456));
			
			{
				umm count = mems.count;
				for (umm i = 0; i < count; ++i) {
					if (mems[i].base_scale || mems[i].index_scale) {
						mems.add(mems[i] withx { it.size_override = 1; });
					}
				}
			}

			List<std::pair<Gpr8, Gpr8>> reg8_pairs;
			
			for (auto a : regs8) {
				for (auto b : regs8) {
					if (!gpr8_compatible_rr(a, b)) {
						continue;
					}
					reg8_pairs.add({a, b});
				}
			}
			
			StringBuilder tmp_builder;

			//append(asm_builder, ".code\nmain proc\n");

			auto test = [&](String name, int size, auto instr, auto ...args) {
				//tmp_builder.clear();
				//append(tmp_builder, "  ");
				//append(tmp_builder, format_hex((u64)(c - buf)));
				//append(tmp_builder, ": ");
				//append(tmp_builder, Format(name, align_left(12, ' ')));
				//auto append_int = [&](auto x) {
				//	auto str = to_string(FormatInt{
				//		.value = (std::make_unsigned_t<decltype(x)>)x,
				//		.radix = 16, 
				//	});
				//	if (!is_digit((ascii)str[0]))
				//		append(tmp_builder, '0');
				//	append(tmp_builder, str);
				//	append(tmp_builder, 'h');
				//};
				//auto append_arg = Combine {
				//	[&](auto x) { append(tmp_builder, x); },
				//	[&](OneOf<int8_t, int16_t, int32_t, int64_t> auto x) { 
				//		if (size == 64 && std::is_same_v<decltype(x), int32_t>) {
				//			append_int((s64)x);
				//		} else {
				//			append_int(x);
				//		}
				//	},
				//	[&](Mem m) {
				//		switch (size) {
				//			case  8: append(tmp_builder, "byte"); break;
				//			case 16: append(tmp_builder, "word"); break;
				//			case 32: append(tmp_builder, "dword"); break;
				//			case 64: append(tmp_builder, "qword"); break;
				//		}
				//		append(tmp_builder, " ptr [");
				//		if (m.base_scale) {
				//			if (m.size_override) {
				//				append(tmp_builder, Gpr32{m.base});
				//			} else {
				//				append(tmp_builder, Gpr64{m.base});
				//			}
				//		}
				//		if (m.index_scale) {
				//			if (m.base_scale) {
				//				append(tmp_builder, '+');
				//			}
				//			if (m.size_override) {
				//				append(tmp_builder, Gpr32{m.index});
				//			} else {
				//				append(tmp_builder, Gpr64{m.index});
				//			}
				//			append(tmp_builder, '*');
				//			append(tmp_builder, m.index_scale);
				//		}
				//		if ((m.base_scale || m.index_scale) && m.displacement) {
				//			append(tmp_builder, '+');
				//		}
				//
				//		if ((!m.base_scale && !m.index_scale) || m.displacement) {
				//			//switch (x64w_displacement_form(m)) {
				//			//	case x64w_df_8bit:  append_int((s8 )m.displacement); break;
				//			//	case x64w_df_32bit: append_int((s64)m.displacement); break;
				//			//}
				//			append_int((s64)m.displacement);
				//		}
				//
				//		append(tmp_builder, ']');
				//	},
				//};
				//auto append_args = Combine {
				//	[&]() {},
				//	[&](auto x) { append_arg(x); },
				//	[&](auto x, auto y) { append_arg(x); append(tmp_builder, ','); append_arg(y); },
				//};
				//append_args(args...);
				//
				//auto expected_line = as_utf8(to_string(tmp_builder));
				//expected_disasm.add(expected_line);

				auto start = c;
				
				InstrInfo i = {
					.start = start,
					.size = autocast (c - start),
				};
				i.mnemonic = name;
				i.operand_size = size;

				auto add_operand = [&](auto op) {
					if constexpr (OneOf<decltype(op), s8, s16, s32>) {
						i.operands.add((s64)op);
					} else {
						i.operands.add(op);
					}
				};

				(add_operand(args), ...);

				if (auto reason = instr(&c, args...)) {
					with(ConsoleColor::red, println("Failed to encode '{}'\nReason: {}", i, reason));
					exit(1);
				}

				instr_infos.add(i);
			};

			// D = R/M
			// V = R/M/I
			#define TEST_I8(name)  test(u8###name##s,  8, x64w_##name##8i , (int8_t )0x123456789abcdef);
			#define TEST_I16(name) test(u8###name##s, 16, x64w_##name##16i, (int16_t)0x123456789abcdef);
			#define TEST_I32(name) test(u8###name##s, 32, x64w_##name##32i, (int32_t)0x123456789abcdef);
			#define TEST_I64(name) test(u8###name##s, 64, x64w_##name##64i, (int64_t)0x123456789abcdef);
			#define TEST_R8(name)  for (auto r : regs8)  test(u8###name##s,  8, x64w_##name##8r , r);
			#define TEST_R16(name) for (auto r : regs16) test(u8###name##s, 16, x64w_##name##16r, r);
			#define TEST_R32(name) for (auto r : regs32) test(u8###name##s, 32, x64w_##name##32r, r);
			#define TEST_R64(name) for (auto r : regs64) test(u8###name##s, 64, x64w_##name##64r, r);
			#define TEST_M8(name)  for (auto m : mems)   test(u8###name##s,  8, x64w_##name##8m , m);
			#define TEST_M16(name) for (auto m : mems)   test(u8###name##s, 16, x64w_##name##16m, m);
			#define TEST_M32(name) for (auto m : mems)   test(u8###name##s, 32, x64w_##name##32m, m);
			#define TEST_M64(name) for (auto m : mems)   test(u8###name##s, 64, x64w_##name##64m, m);
			#define TEST_RI8(name)     for (auto r : regs8)  test(u8###name##s,  8, x64w_##name##8ri,  r, (int8_t )0x123456789abcdef);
			#define TEST_RI16(name)    for (auto r : regs16) test(u8###name##s, 16, x64w_##name##16ri, r, (int16_t)0x123456789abcdef);
			#define TEST_RI32(name)    for (auto r : regs32) test(u8###name##s, 32, x64w_##name##32ri, r, (int32_t)0x123456789abcdef);
			#define TEST_RI64(name)    for (auto r : regs64) test(u8###name##s, 64, x64w_##name##64ri, r, (int32_t)0x123456789abcdef);
			#define TEST_RI16_8(name)  for (auto r : regs16) test(u8###name##s, 16, x64w_##name##16r8i, r, (int8_t)0x123456789abcdef);
			#define TEST_RI32_8(name)  for (auto r : regs32) test(u8###name##s, 32, x64w_##name##32r8i, r, (int8_t)0x123456789abcdef);
			#define TEST_RI64_8(name)  for (auto r : regs64) test(u8###name##s, 64, x64w_##name##64r8i, r, (int8_t)0x123456789abcdef);
			#define TEST_RI64_64(name) for (auto r : regs64) test(u8###name##s, 64, x64w_##name##64ri, r, (int64_t)0x123456789abcdef);
			#define TEST_RR8(name)  for (auto a : regs8)  for (auto b : regs8) if (gpr8_compatible_rr(a, b)) test(u8###name##s,  8, x64w_##name##8rr,  a, b);
			#define TEST_RR16(name) for (auto a : regs16) for (auto b : regs16)                              test(u8###name##s, 16, x64w_##name##16rr, a, b);
			#define TEST_RR32(name) for (auto a : regs32) for (auto b : regs32)                              test(u8###name##s, 32, x64w_##name##32rr, a, b);
			#define TEST_RR64(name) for (auto a : regs64) for (auto b : regs64)                              test(u8###name##s, 64, x64w_##name##64rr, a, b);
			#define TEST_RM8(name)  for (auto a : regs8 ) for (auto b : mems) if (gpr8_compatible_rm(a, b)) test(u8###name##s,  8, x64w_##name##8rm,  a, b);
			#define TEST_RM16(name) for (auto a : regs16) for (auto b : mems)                               test(u8###name##s, 16, x64w_##name##16rm, a, b);
			#define TEST_RM32(name) for (auto a : regs32) for (auto b : mems)                               test(u8###name##s, 32, x64w_##name##32rm, a, b);
			#define TEST_RM64(name) for (auto a : regs64) for (auto b : mems)                               test(u8###name##s, 64, x64w_##name##64rm, a, b);
			#define TEST_MR8(name)  for (auto a : mems) for (auto b : regs8 ) if (gpr8_compatible_rm(b, a)) test(u8###name##s,  8, x64w_##name##8mr,  a, b);
			#define TEST_MR16(name) for (auto a : mems) for (auto b : regs16)                               test(u8###name##s, 16, x64w_##name##16mr, a, b);
			#define TEST_MR32(name) for (auto a : mems) for (auto b : regs32)                               test(u8###name##s, 32, x64w_##name##32mr, a, b);
			#define TEST_MR64(name) for (auto a : mems) for (auto b : regs64)                               test(u8###name##s, 64, x64w_##name##64mr, a, b);
			#define TEST_MI8(name)     for (auto a : mems)  test(u8###name##s,  8, x64w_##name##8mi,  a, (int8_t )0x123456789abcdef);
			#define TEST_MI16(name)    for (auto a : mems)  test(u8###name##s, 16, x64w_##name##16mi, a, (int16_t)0x123456789abcdef);
			#define TEST_MI32(name)    for (auto a : mems)  test(u8###name##s, 32, x64w_##name##32mi, a, (int32_t)0x123456789abcdef);
			#define TEST_MI64(name)    for (auto a : mems)  test(u8###name##s, 64, x64w_##name##64mi, a, (int32_t)0x123456789abcdef);
			#define TEST_MI16_8(name)    for (auto a : mems)  test(u8###name##s, 16, x64w_##name##16m8i, a, (int8_t)0x123456789abcdef);
			#define TEST_MI32_8(name)    for (auto a : mems)  test(u8###name##s, 32, x64w_##name##32m8i, a, (int8_t)0x123456789abcdef);
			#define TEST_MI64_8(name)    for (auto a : mems)  test(u8###name##s, 64, x64w_##name##64m8i, a, (int8_t)0x123456789abcdef);
			
			#define TEST_R(name) \
				TEST_R8(name) \
				TEST_R16(name) \
				TEST_R32(name) \
				TEST_R64(name) \

			#define TEST_M(name) \
				TEST_M8(name) \
				TEST_M16(name) \
				TEST_M32(name) \
				TEST_M64(name) \

			#define TEST_D(name) \
				TEST_R(name); \
				TEST_M(name); \

			#define TEST_RI(name) \
				TEST_RI8(name) \
				TEST_RI16(name) \
				TEST_RI32(name) \
				TEST_RI64(name) \

			#define TEST_RR(name) \
				TEST_RR8(name) \
				TEST_RR16(name) \
				TEST_RR32(name) \
				TEST_RR64(name) \

			#define TEST_RM(name) \
				TEST_RM8(name) \
				TEST_RM16(name) \
				TEST_RM32(name) \
				TEST_RM64(name) \

			#define TEST_MI(name) \
				TEST_MI8(name) \
				TEST_MI16(name) \
				TEST_MI32(name) \
				TEST_MI64(name) \

			#define TEST_MR(name) \
				TEST_MR8(name) \
				TEST_MR16(name) \
				TEST_MR32(name) \
				TEST_MR64(name) \

			//TEST_I8(push);
			//TEST_I32(push);
			//TEST_R16(push);
			//TEST_R64(push);
			//TEST_M16(push);
			//TEST_M64(push);
			//
			//TEST_R16(pop);
			//TEST_R64(pop);
			//TEST_M16(pop);
			//TEST_M64(pop);
			//
			//TEST_D(inc);
			//TEST_D(dec);
			//TEST_D(not);
			//TEST_D(neg);
			//
			//TEST_RI8(mov);
			//TEST_RI16(mov);
			//TEST_RI32(mov);
			//TEST_RI64_64(mov);
			//TEST_RR(mov);
			//TEST_RM(mov);
			//TEST_MR(mov);
			//TEST_MI(mov);

			TEST_RI(add);
			TEST_RR(add);
			TEST_RM(add);
			TEST_MI(add);
			TEST_MR(add);
			TEST_RI16_8(add);
			TEST_RI32_8(add);
			TEST_RI64_8(add);
			TEST_MI16_8(add);
			TEST_MI32_8(add);
			TEST_MI64_8(add);

			//test(mov64mi,  [&](auto a, auto b) { return format(u8"    mov QWORD PTR {}, DWORD PTR {}\n", a, b); }, mem64_d(0x3456), (int32_t)0x89abcdef);

			//expected_disasm.enable_reading();

			
			{
				StringBuilder builder;
				for (auto c : Span(buf, c)) {
					append(builder, format_hex(c));
				}
				auto hex_path = "f:\\projects\\simplex\\tmp\\check.txt"s;
				write_entire_file(hex_path, as_bytes(to_string(builder)));
			}

			//append(asm_builder, "main endp\nend");

			//auto asm_path = u8"F:\\projects\\simplex\\tmp\\check.asm"s;
			auto obj_path = u8"F:\\projects\\simplex\\tmp\\check.obj"s;
			//write_entire_file(asm_path, as_bytes(to_string(asm_builder)));
			
			write_instructions_to_obj(Span(buf, (umm)(c - buf)), obj_path);

			auto disasm_output = (String)start_process_and_get_output(tformat(u8"D:\\Programs\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.40.33807\\bin\\Hostx64\\x64\\dumpbin.exe /disasm:nobytes /nologo {}"s, obj_path));

			//for (auto line : expected_disasm)
			//	println(line);


			smm line_index = -5;
			split_by_seq(disasm_output, u8"\r\n"s, [&](String line) {
				defer{++line_index;};

				if (line_index < 0)
					return ForEach_continue;

				if (line_index >= instr_infos.count)
					return ForEach_break;

				auto i = instr_infos[line_index];

				if (line_index == 1370)
					;//debug_break();

				String reason = {};
				
				auto encoding_is_right = [&]() {
					auto parsed_ = parse_dumpbin_disasm_line(line);

					if (!parsed_) {
						reason = u8"Could not parse"s;
						return false;
					}

					auto parsed = parsed_.value();

					if (i.mnemonic != parsed.mnemonic) {
						reason = format(u8"invalid mnemonic. expected: {}, actual: {}"s, i.mnemonic, parsed.mnemonic);
						return false;
					}
					if (i.operands.count != parsed.operands.count) {
						reason = format(u8"operand count mismatch. expected: {}, actual: {}"s, i.operands.count, parsed.operands.count);
						return false;
					}
					for (int j = 0; j < i.operands.count; ++j) {
						auto normalize_mem = [&](Mem &m) {
							if (m.base_scale == 0 && m.index_scale == 1) {
								m.base_scale = 1;
								m.index_scale = 0;
								m.base = m.index;
								m.index = 0;
							}
						};
						i.operands[j].visit(Combine{
							[](auto &){},
							[&](Mem &m) {
								normalize_mem(m);
							}
						});
						parsed.operands[j].visit(Combine{
							[](auto &){},
							[&](Mem &m) {
								normalize_mem(m);
							}
						});

						bool ok = true;

						if (i.operands[j].is<s64>() && parsed.operands[j].is<s64>()) {
							switch (i.operand_size) {
								case 8: ok = (s8)i.operands[j].as<s64>().value() == (s8)parsed.operands[j].as<s64>().value(); break;
								case 16: ok = (s16)i.operands[j].as<s64>().value() == (s16)parsed.operands[j].as<s64>().value(); break;
								case 32: ok = (s32)i.operands[j].as<s64>().value() == (s32)parsed.operands[j].as<s64>().value(); break;
								case 64: ok = (s64)i.operands[j].as<s64>().value() == (s64)parsed.operands[j].as<s64>().value(); break;
							}
						} else {
							ok = i.operands[j] == parsed.operands[j];
						}

						if (!ok) {
							reason = format(u8"operand {} mismatch. expected: {}, actual: {}"s, j, i.operands[j], parsed.operands[j]);
							return false;
						}
					}
					return true;
				};

				if (!encoding_is_right()) {
					with(ConsoleColor::red, println("FAIL TO ENCODE"));
					println("Expected: {}", i);
					println("Actual:   {}", line);
					println("Reason:   {}", reason);
					
					println("My bytes: {}", format_hex(Span(i.start, (umm)i.size)));

					exit(1);
				}

				return ForEach_continue;
			});
			
			with(ConsoleColor::green, println("Successfully encoded {} bytes", c - buf));

			#if 0
			start_process(tformat(u8"D:\\Programs\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.40.33807\\bin\\Hostx64\\x64\\ml64.exe /Fo {} /c {}"s, obj_path, asm_path), [&](Span<u8> output) {
				print((String)output);
			});

			Buffer obj_data;
			for (int i = 0; i < 4; ++i ) {
				obj_data = read_entire_file(obj_path);
				if (obj_data.count)
					break;
			}
			if (!obj_data.count)
				exit(1);

			auto expected_bytecode = obj_data.skip(0x8c);

			auto found = same_start(expected_bytecode, Span(buf, c));
			if (found.count == c - buf) {
				with(ConsoleColor::green, println("Successfully encoded {} bytes", c - buf));
			} else {
				auto failed_byte_index = found.count;
				with(ConsoleColor::red, println("Encoding failed at byte {}", failed_byte_index));
				u8 *t = buf + failed_byte_index;
				for (umm i = 0; i < instr_starts.count; ++i) {
					if (instr_starts[i].c > t) {
						auto f = instr_starts[i - 1];
						println(f.s);
						println("My bytes: {}", format_hex(Span(f.c, (umm)15)));
						println("Expected: {}", format_hex(Span(f.c - buf + expected_bytecode.data, (umm)15)));
						break;
					}
				}
			}

			//write_entire_file("F:\\projects\\simplex\\tmp\\test.bin"s, Span(buf, c));

			//for (auto c : Span(buf, c)) {
			//	append(builder, format_hex(c));
			//}
			#endif
			exit(1);
		}



		//encoded_instructions = default_allocator.allocate<u8>(bytecode.instructions.count * x64::max_instruction_size);
		//c = encoded_instructions;
		//
		//for (auto &i : bytecode.instructions) {
	//		emit_instruction(i);
		//}
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
	#if 0
	x64::Register64 map(b::Register r) {
		switch (r) {
			case (b::Register)0: return x64::Register64::rcx;
			case (b::Register)1: return x64::Register64::rdx;
			case (b::Register)2: return x64::Register64::r8;
			case (b::Register)3: return x64::Register64::r9;
			case (b::Register)4: return x64::Register64::r10;
			case (b::Register)5: return x64::Register64::r11;
			case b::Register::stack: return x64::Register64::rsp;
			case b::Register::base: return x64::Register64::rbp;
		}
		invalid_code_path("Unmappable register {}", r);
	}
	x64::Address map(b::Address a) {
		x64::Address result = {};
		if (a.base) {
			result.r1 = map(a.base.value());
			result.r1_scale = 1;
		}
		result.r2 = map(a.element_index);
		switch (a.element_size) {
			case 0: break;
			case 1: result.r2_scale_index = 1; break;
			case 2: result.r2_scale_index = 2; break;
			case 4: result.r2_scale_index = 3; break;
			case 8: result.r2_scale_index = 4; break;
			default: invalid_code_path("Unmappable address: a.element_size is {}", a.element_size);
		}

		assert(fits_in_4_bytes(a.offset), "Unmappable address: a.offset is not 4 byte: {}", a.offset);

		result.c = (s32)a.offset;
		
		return result;
	}
	u8 *encoded_instructions = 0;
	u8 *c = 0;

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
		if (i.s.is_register()) {
			auto r = map(i.s.get_register());

			bool is_2_bytes = (int)r >> 3;

			*c = 0x41;
			*c += is_2_bytes;
			*c = 0x50 | ((int)r & 7);

			return;
		}
		invalid_code_path();
	}
	void emit_instruction_impl(b::Instruction::pop_t i) { not_implemented(); }
	void emit_instruction_impl(b::Instruction::copy_t i) {
		if (i.d.is_register()) {
			auto d = map(i.d.get_register());
			if (i.s.is_register()) {
				auto s = map(i.s.get_register());

				*c++ = 0x48 | ((int)s >> 3) | ((int)d >> 3 << 2);
				*c++ = 0x8b;
				*c++ = 0xc0 | ((int)s & 7) | (((int)d & 7) << 3);
				return;
			} else if (i.s.is_address()) {
				auto s = map(i.s.get_address());

				if (s.r1_scale) {
					switch (s.r2_scale_index) {
						case 0: {
							switch (required_bytes(s.c)) {
								case 0: {
									// [r1]
									*c++ = 0x48 | ((int)d >> 3);
									*c++ = 0x8b;
									*c++ = ((int)s.r1);
									return;
								}
								case 1: {
									// [r1 + 100]
									*c++ = 0x48 | ((int)d >> 3);
									*c++ = 0x8b;
									*c++ = 0x40 | ((int)s.r1);
									*c++ = s.c;
									return;
								}
								case 2:
								case 4: {
									// [r1 + 10000]
									*c++ = 0x48 | ((int)d >> 3);
									*c++ = 0x8b;
									*c++ = 0x80 | ((int)s.r1);
									*c++ = s.c;
									return;
								}
								default:
									invalid_code_path();
							}
							break;
						}
						case 1: {
							switch (required_bytes(s.c)) {
								case 0: {
									// [r1 + r2]
									*c++ = 0x48 | ((int)s.r2 >> 3 << 1);
									*c++ = 0x8b;
									*c++ = 0x04;
									*c++ = (((int)s.r2) << 3) | ((int)s.r1);
									return;
								}
								default:
									invalid_code_path();
							}
							break;
						}
					}
				}
			}
		}
		invalid_code_path();
	}
	#endif

	void emit_instruction_impl(b::Instruction::set_t i) { not_implemented(); }
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
	void emit_instruction_impl(b::Instruction::add8_t i) {
		not_implemented();
	}
	void emit_instruction_impl(b::Instruction::sub8_t i) { not_implemented(); }
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


void emit(String target_executable_path, b::Bytecode bytecode) {
	Emitter emitter;
	emitter.emit(target_executable_path, bytecode);
}

}
