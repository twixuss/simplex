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

	static constexpr u8 max_element_size = 255;
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
	Site() {
		_is_address = false;
		r = {};
	}
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
	InputValue() {
		kind = Kind::Constant;
		c = 0;
	}
	InputValue(s64 c) : kind(Kind::Constant), c(c) {}
	InputValue(Register r) : kind(Kind::Register), r(r) {}
	InputValue(Address a) : kind(Kind::Address), a(a) {}
	InputValue(Site s) : kind(s.is_register() ? Kind::Register : Kind::Address) {
		if (s.is_register())
			r = s.get_register();
		else
			a = s.get_address();
	}
	~InputValue() {}
	
	bool is_constant() { return kind == Kind::Constant; }
	bool is_register() { return kind == Kind::Register; }
	bool is_address() { return kind == Kind::Address; }
	
	s64 &get_constant() { assert(is_constant()); return c; }
	Register &get_register() { assert(is_register()); return r; }
	Address &get_address() { assert(is_address()); return a; }

	Optional<s64> as_constant() { if (is_constant()) return c; return {}; }
	Optional<Register> as_register() { if (is_register()) return r; return {}; }
	Optional<Address> as_address() { if (is_address()) return a; return {}; }

	bool operator==(InputValue const &that) const {
		if (kind != that.kind) {
			return false;
		}
		switch (kind) {
			case Kind::Constant: return c == that.c;
			case Kind::Register: return r == that.r;
			case Kind::Address:  return a == that.a;
		}
		return false;
	}

private:
	enum class Kind {
		Constant,
		Register,
		Address,
	};

	Kind kind;
	union {
		s64 c;
		Register r;
		Address a;
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
	u8 count = 1; // count of elements

	[[nodiscard]] inline constexpr auto operator<=>(ArrayLayout const &) const noexcept = default;

	inline constexpr u16 total_size() {
		return size * count;
	}
};

inline constexpr ArrayLayout r8  = {1, 1};
inline constexpr ArrayLayout r16 = {2, 1};
inline constexpr ArrayLayout r32 = {4, 1};
inline constexpr ArrayLayout r64 = {8, 1};

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
	x(set,  (y(Address, d) y(s64, value) y(u64, size) y(u64, count))) \
	x(lea,  (y(Site, d) y(Address, s))) \
	x(add,  (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(sub,  (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(mul,  (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(divu, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(divs, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(modu, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(mods, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(bxor, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(band, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(bor,  (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(sll,  (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(srl,  (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(sra,  (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(cmp,  (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp) y(ArrayLayout, layout))) \
	x(sex2, (y(Site, d) y(InputValue, a) y(ArrayLayout, layout))) \
	x(sex4, (y(Site, d) y(InputValue, a) y(ArrayLayout, layout))) \
	x(sex8, (y(Site, d) y(InputValue, a) y(ArrayLayout, layout))) \
	x(neg,  (y(Site, d) y(InputValue, a) y(ArrayLayout, layout))) \
	x(movmsk, (y(Site, d) y(Address, s) y(ArrayLayout, layout))) \
	x(blend, (y(Address, d) y(Register, m) y(Address, a) y(Address, b) y(ArrayLayout, layout))) \
	x(fadd, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(fsub, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(fmul, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(fdiv, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(fmod, (y(Site, d) y(InputValue, a) y(InputValue, b) y(ArrayLayout, layout))) \
	x(f32_to_s32, (y(Site, d) y(InputValue, a) y(u64, count))) \
	x(f32_to_f64, (y(Site, d) y(InputValue, a) y(u64, count))) \
	x(f64_to_s64, (y(Site, d) y(InputValue, a) y(u64, count))) \
	x(f64_to_f32, (y(Site, d) y(InputValue, a) y(u64, count))) \
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

	#define x(name, fields) name##_t &name() { assert(kind == InstructionKind::name); return as.name; }
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
	#undef x

	union As {
		#define x(name, fields) name##_t name;
		ENUMERATE_BYTECODE_INSTRUCTION_KIND
		#undef x

		~As() {}
	} as;
	
	char const *file = 0;
	u16 line = 0;
	String source_location = {};

	~Instruction() {}
	
	void visit_registers(auto &&visitor) {
		switch (kind) {
			#define y(type, name) visit_register(i.name, visitor);
			#define x(name, fields)           \
				case InstructionKind::name: { \
					auto &i = as.name;       \
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
					auto &i = as.name;       \
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
					auto &i = self.as.name;   \
					PASSTHROUGH fields;       \
					(void)i;                  \
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
					auto &i = as.name;        \
					auto &j = that.as.name;   \
					PASSTHROUGH fields;       \
					(void)i;                  \
					(void)j;                  \
					break;                    \
				}
			ENUMERATE_BYTECODE_INSTRUCTION_KIND
			#undef x
			#undef y
		}

		return true;
	}

private:
	static void visit_register(auto, auto &&) {}
	static void visit_register(Register &r, auto &&visitor) { visitor(r); }
	static void visit_register(Site &s, auto &&visitor) { if (s.is_register()) visitor(s.get_register()); }
	static void visit_register(InputValue &v, auto &&visitor) { if (v.is_register()) visitor(v.get_register()); }
	static void visit_address(auto, auto &&) {}
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

#define y(type, name)                          \
	if (need_comma) { result += print(", "); } \
	need_comma = true;                         \
	result += print(i.name);                   \

#define x(name, fields)                                 \
inline umm print_instruction(Instruction::name##_t i) { \
	umm result = 0;                                     \
	result += print(InstructionKind::name);             \
	result += print(' ');                               \
	bool need_comma = false;                            \
	PASSTHROUGH fields;                                 \
	return result;                                      \
	(void)i;                                            \
	(void)need_comma;                                   \
}
ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
#undef y

inline umm print_instruction(Instruction i) {
	// Custom printers
	switch (i.kind) {
		case InstructionKind::callext: {
			auto kind = i.kind;
			REDECLARE_REF(i, i.as.callext);
			return print("{} {}", kind, i.lambda->link_name);
		}
		default: break;
	}
	// Default printers
	switch (i.kind) {
#define x(name, fields) case InstructionKind::name: return print_instruction(i.as.name);
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