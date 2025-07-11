#pragma once
#include "../common.h"
#include "../nodes.h"

using namespace tl;

struct Lambda;

namespace Bytecode {

#define ENUMERATE_1248 \
	x(1) \
	x(2) \
	x(4) \
	x(8) \

#define ENUMERATE_48 \
	x(4) \
	x(8) \

#define ENUMERATE_NAMED_BYTECODE_REGISTERS \
	x(base           , 248) /* NOTE: all registers before this one can be allocated. */ \
	x(stack          , 249) \
	x(returns        , 250) \
	x(arguments      , 251) \
	x(temporary      , 252) \
	x(locals         , 253) \
	x(global_readonly, 254) \
	x(global_mutable , 255) \

enum class Register : u8 {
#define x(name, value) name = value,
	ENUMERATE_NAMED_BYTECODE_REGISTERS
#undef x
};

inline void append(StringBuilder &builder, Register r) {
	switch (r) {
#define x(name, value) case Register::name: return append(builder, #name);
		ENUMERATE_NAMED_BYTECODE_REGISTERS
#undef x
	}
	return append_format(builder, "r{}", (u64)r);
}

struct Address {
	Optional<Register> base = {};
	Register element_index = {};
	u8 element_size = {};
	s64 offset = {};

	constexpr auto operator<=>(Address const &) const = default;
};

inline void append(StringBuilder &builder, Address a) {
	append(builder, '[');

	if (a.base) if (a.element_size) if (a.offset) append_format(builder, "{}+{}*{}{}{}", a.base.value(), a.element_index, a.element_size, a.offset < 0 ? "-" : "+", abs(a.offset));
	                                else          append_format(builder, "{}+{}*{}", a.base.value(), a.element_index, a.element_size);
	            else                if (a.offset) append_format(builder, "{}{}{}", a.base.value(), a.offset < 0 ? "-" : "+", abs(a.offset));
	                                else          append_format(builder, "{}", a.base.value());
	else        if (a.element_size) if (a.offset) append_format(builder, "{}*{}{}{}", a.element_index, a.element_size, a.offset < 0 ? "-" : "+", abs(a.offset));
	                                else          append_format(builder, "{}*{}", a.element_index, a.element_size);
	            else                if (a.offset) append_format(builder, "{}{}", a.offset < 0 ? "-" : "", abs(a.offset));
	                                else          append_format(builder, "0");

	append(builder, ']');
}

struct Site {
	Site() { memset(this, 0, sizeof(*this)); }
	Site(Register r) : _is_address(false), r(r) {}
	Site(Address a) : _is_address(true), a(a) {}
	~Site() {}

	bool is_register() { return !_is_address; }
	bool is_address() { return _is_address; }

	Register &get_register() { assert(!_is_address); return r; }
	Address &get_address() { assert(_is_address); return a; }
	
	bool operator==(Site const &that) const {
		if (_is_address != that._is_address) {
			return false;
		}
		switch (_is_address) {
			case false: return r == that.r;
			case true:  return a == that.a;
		}
		return false;
	}

private:
	bool _is_address;
	union {
		Register r;
		Address a;
	};
};

inline void append(StringBuilder &builder, Site s) {
	if (s.is_register())
		return append(builder, s.get_register());
	else
		return append(builder, s.get_address());
}

struct InputValue {
	InputValue() { memset(this, 0, sizeof(*this)); }
	InputValue(Register r) : kind(Kind::Register), r(r) {}
	InputValue(Address a) : kind(Kind::Address), a(a) {}
	InputValue(s64 c) : kind(Kind::Constant), c(c) {}
	InputValue(Site s) : kind(s.is_register() ? Kind::Register : Kind::Address) {
		if (s.is_register())
			r = s.get_register();
		else
			a = s.get_address();
	}
	~InputValue() {}

	bool is_register() { return kind == Kind::Register; }
	bool is_address() { return kind == Kind::Address; }
	bool is_constant() { return kind == Kind::Constant; }
	
	Register &get_register() { assert(is_register()); return r; }
	Address &get_address() { assert(is_address()); return a; }
	s64 &get_constant() { assert(is_constant()); return c; }

	Optional<Register> as_register() { if (is_register()) return r; return {}; }
	Optional<Address> as_address() { if (is_address()) return a; return {}; }
	Optional<s64> as_constant() { if (is_constant()) return c; return {}; }

	bool operator==(InputValue const &that) const {
		if (kind != that.kind) {
			return false;
		}
		switch (kind) {
			case Kind::Register: return r == that.r;
			case Kind::Address:  return a == that.a;
			case Kind::Constant: return c == that.c;
		}
		return false;
	}

private:
	enum class Kind {
		Register,
		Address,
		Constant,
	};

	Kind kind;
	union {
		Register r;
		Address a;
		s64 c;
	};
};

inline void append(StringBuilder &builder, InputValue v) {
	if (v.is_register())
		append(builder, v.get_register());
	else if (v.is_address())
		append(builder, v.get_address());
	else
		append(builder, v.get_constant());
}

// TODO: encode as exponents of two for compression?
struct ArrayLayout {
	u8 size  = 0; // size of one element
	u8 count = 0; // count of elements

	[[nodiscard]] inline constexpr auto operator<=>(ArrayLayout const &) const noexcept = default;
};

inline void append(StringBuilder &builder, ArrayLayout p) {
	append_format(builder, "{}x{}", p.size, p.count);
}

/*
#define x(name)
ENUMERATE_INTRINSICS
#undef x
*/
#define ENUMERATE_INTRINSICS \
	x(print_S64) \
	x(print_String) \
	x(panic) \
	x(debug_break) \
	x(assert) \

enum class Intrinsic : u8 {
#define x(name) name,
	ENUMERATE_INTRINSICS
#undef x
};

inline void append(StringBuilder &builder, Intrinsic i) {
	switch (i) {
		#define x(name) case Intrinsic::name: return append(builder, #name);
		#define y(name, value) x(name)
		ENUMERATE_INTRINSICS
		#undef y
		#undef x		

	}
	append_format(builder, "(unknown Intrinsic {})", (u64)i);
}

// NOTE: Site is always written to
//       InputValue is always read from
/*
#define y(type, name)
#define x(name, fields)
ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
#undef y
*/
#define ENUMERATE_BYTECODE_INSTRUCTION_KIND \
	x(nop,  ()) \
	x(push, (y(InputValue, s))) \
	x(pop,  (y(Site, d))) \
	x(copy, (y(Site, d) y(InputValue, s) y(u64, size))) \
	x(set,  (y(Address, d) y(u8, value) y(u64, size))) \
	x(lea,  (y(Site, d) y(Address, s))) \
	x(add1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(add2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(add4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(add8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(divu1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(divu2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(divu4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(divu8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(divs1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(divs2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(divs4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(divs8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(modu1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(modu2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(modu4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(modu8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mods1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mods2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mods4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mods8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or1,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or2,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or4,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or8,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp1, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(cmp2, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(cmp4, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(cmp8, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(sex21,  (y(Site, d) y(InputValue, a))) \
	x(sex41,  (y(Site, d) y(InputValue, a))) \
	x(sex42,  (y(Site, d) y(InputValue, a))) \
	x(sex81,  (y(Site, d) y(InputValue, a))) \
	x(sex82,  (y(Site, d) y(InputValue, a))) \
	x(sex84,  (y(Site, d) y(InputValue, a))) \
	x(neg1, (y(Site, d) y(InputValue, a))) \
	x(neg2, (y(Site, d) y(InputValue, a))) \
	x(neg4, (y(Site, d) y(InputValue, a))) \
	x(neg8, (y(Site, d) y(InputValue, a))) \
	x(movmsk, (y(Site, d) y(Address, s) y(ArrayLayout, layout))) \
	x(blend, (y(Address, d) y(Register, m) y(Address, a) y(Address, b) y(ArrayLayout, layout))) \
	x(fadd4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fadd8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fsub4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fsub8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fmul4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fmul8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fdiv4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fdiv8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fmod4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(fmod8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(f32_to_s32, (y(Site, d) y(InputValue, a))) \
	x(f32_to_f64, (y(Site, d) y(InputValue, a))) \
	x(f64_to_s64, (y(Site, d) y(InputValue, a))) \
	x(f64_to_f32, (y(Site, d) y(InputValue, a))) \
	x(call, (y(InputValue, d))) \
	x(callext, (y(Lambda *, lambda) y(String, lib) y(String, name))) \
	x(copyext, (y(Site, d) y(String, lib) y(String, name))) \
	x(ret,  ()) \
	x(jmp,  (y(s64, d) /* relative */)) \
	x(jf,   (y(InputValue, s) y(s64, d) /* relative */)) \
	x(jt,   (y(InputValue, s) y(s64, d) /* relative */)) \
	x(intrinsic, (y(Intrinsic, i) y(String, message))) \

enum class InstructionKind : u8 {
#define x(name, fields) name,
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
};

inline void append(StringBuilder &builder, InstructionKind i) {
	switch (i) {
		#define x(name, fields) case InstructionKind::name: return append(builder, #name);
		ENUMERATE_BYTECODE_INSTRUCTION_KIND
		#undef x		

	}
	append_format(builder, "(unknown InstructionKind {})", (u64)i);
}

struct Instruction {
	InstructionKind kind;

	// `Invalid` is here to prevent me from leaving an uninitialized member.
	#define y(type, name) type name = Invalid();
	#define x(name, fields)      \
		template <class Invalid> \
		struct name##_x {        \
			PASSTHROUGH fields   \
		};
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
	#undef x
	#undef y

	#define x(name, fields) using name##_t = name##_x<void>;
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
	#undef x

	#define x(name, fields) name##_t &name() { assert(kind == InstructionKind::name); return v_##name; }
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
	#undef x

	union {
		#define x(name, fields) name##_t v_##name;
		ENUMERATE_BYTECODE_INSTRUCTION_KIND
		#undef x
	};
	
	char const *file = 0;
	u16 line = 0;
	String source_location = {};

	~Instruction() {}
	
	void visit_registers(auto &&visitor) {
		switch (kind) {
			#define y(type, name) visit_register(i.name, visitor);
			#define x(name, fields)           \
				case InstructionKind::name: { \
					auto &i = v_##name;       \
					PASSTHROUGH fields;       \
					break;                    \
				}
			ENUMERATE_BYTECODE_INSTRUCTION_KIND
			#undef x
			#undef y
		}
	}
	
	void visit_addresses(auto &&visitor) {
		switch (kind) {
			#define y(type, name) visit_address(i.name, visitor);
			#define x(name, fields)           \
				case InstructionKind::name: { \
					auto &i = v_##name;       \
					PASSTHROUGH fields;       \
					break;                    \
				}
			ENUMERATE_BYTECODE_INSTRUCTION_KIND
			#undef x
			#undef y
		}
	}
	
	void visit_operands(this auto &&self, auto &&visitor) {
		switch (self.kind) {
			#define y(type, name) visitor(i.name);
			#define x(name, fields)           \
				case InstructionKind::name: { \
					auto &i = self.v_##name;  \
					PASSTHROUGH fields;       \
					break;                    \
				}
			ENUMERATE_BYTECODE_INSTRUCTION_KIND
			#undef x
			#undef y
		}
	}

	bool operator==(Instruction const &that) const {
		if (kind != that.kind)
			return false;
		
		switch (kind) {
			#define y(type, name) if (i.name != j.name) return false;
			#define x(name, fields)           \
				case InstructionKind::name: { \
					auto &i = v_##name;       \
					auto &j = that.v_##name;  \
					PASSTHROUGH fields;       \
					break;                    \
				}
			ENUMERATE_BYTECODE_INSTRUCTION_KIND
			#undef x
			#undef y
		}

		return true;
	}

private:
	static void visit_register(auto, auto &&visitor) {}
	static void visit_register(Register &r, auto &&visitor) { visitor(r); }
	static void visit_register(Site &s, auto &&visitor) { if (s.is_register()) visitor(s.get_register()); }
	static void visit_register(InputValue &v, auto &&visitor) { if (v.is_register()) visitor(v.get_register()); }
	static void visit_address(auto, auto &&visitor) {}
	static void visit_address(Address &a, auto &&visitor) { visitor(a); }
	static void visit_address(Site &s, auto &&visitor) { if (s.is_address()) visitor(s.get_address()); }
	static void visit_address(InputValue &v, auto &&visitor) { if (v.is_address()) visitor(v.get_address()); }
};

umm print_instruction(Instruction instruction);
void print_instruction(umm index, Instruction instruction);
void print_instructions(Span<Instruction> instructions);

struct Callback {
	void *start_address;
};

u64 ffi_callback(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda);

Callback generate_callback(Lambda *lambda);

struct Bytecode {
	List<Instruction> instructions;
	List<u8> global_readonly_data;
	List<u8> global_mutable_data;
	HashMap<u64, Lambda *> first_instruction_to_lambda;
	umm entry_point_instruction_index = -1;
};

#define y(type, name) \
	if (need_comma) { result += print(", "); } \
	need_comma = true; \
	result += print(i.name); \

#define x(name, fields) \
inline umm print_instruction(Instruction::name##_t i) { \
	umm result = 0; \
	result += print(InstructionKind::name); \
	result += print(' '); \
	bool need_comma = false; \
	PASSTHROUGH fields; \
	return result; \
}
ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
#undef y

inline umm print_instruction(Instruction i) {
	// Custom printers
	switch (i.kind) {
		case InstructionKind::callext: {
			auto kind = i.kind;
			REDECLARE_REF(i, i.v_callext);
			return print("{} {}", kind, i.lambda->link_name);
		}
	}
	// Default printers
	switch (i.kind) {
#define x(name, fields) case InstructionKind::name: return print_instruction(i.v_##name);
		ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
	}
	return print("(unknown InstructionKind {})", (u64)i.kind);
}

inline void print_instruction(umm index, Instruction instruction) {
	umm c = 0;
	c += print("{}: ", index);
	c += print_instruction(instruction);
	while (c <= 48) {
		print(' ');
		c++;
	}
	with(ConsoleColor::dark_gray, println("{}:{}", instruction.file, instruction.line));
}

inline void print_instructions(Span<Instruction> instructions) {
	foreach (it, instructions) {
		auto [index, instruction] = it.key_value();
		print_instruction(index, instruction);
	}
	println();
}

}