#pragma once
#include "common.h"

using namespace tl;

namespace Bytecode {

#define ENUMERATE_1248 \
	x(1) \
	x(2) \
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

struct Address {
	Optional<Register> base = {};
	Register element_index = {};
	u8 element_size = {};
	s64 offset = {};
};

struct Site {
	Site() { memset(this, 0, sizeof(*this)); }
	Site(Register r) : _is_address(false), r(r) {}
	Site(Address a) : _is_address(true), a(a) {}
	~Site() {}

	bool is_register() { return !_is_address; }
	bool is_address() { return _is_address; }

	Register &get_register() { assert(!_is_address); return r; }
	Address &get_address() { assert(_is_address); return a; }

private:
	bool _is_address;
	union {
		Register r;
		Address a;
	};
};

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

#define ENUMERATE_INTRINSICS \
	x(println_S64) \
	x(println_String) \
	x(panic) \
	x(debug_break) \

enum class Intrinsic : u8 {
#define x(name) name,
	ENUMERATE_INTRINSICS
#undef x
};

inline umm append(StringBuilder &builder, Intrinsic i) {
	switch (i) {
		#define x(name) case Intrinsic::name: return append(builder, #name);
		#define y(name, value) x(name)
		ENUMERATE_INTRINSICS
		#undef y
		#undef x		

	}
	return append_format(builder, "(unknown Intrinsic {})", (u64)i);
}

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
	x(sub1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(div1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mod1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or1,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp1, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(add2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(div2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mod2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or2,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp2, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(add4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(div4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mod4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or4,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp4, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(add8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(div8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mod8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or8,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp8, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(sex21,  (y(Site, d) y(InputValue, a))) \
	x(sex41,  (y(Site, d) y(InputValue, a))) \
	x(sex42,  (y(Site, d) y(InputValue, a))) \
	x(sex81,  (y(Site, d) y(InputValue, a))) \
	x(sex82,  (y(Site, d) y(InputValue, a))) \
	x(sex84,  (y(Site, d) y(InputValue, a))) \
	x(call, (y(InputValue, d))) \
	x(callext, (y(Lambda *, lambda) y(String, lib) y(String, name))) \
	x(ret,  ()) \
	x(jmp,  (y(InputValue, d))) \
	x(jf,   (y(Site, s) y(InputValue, d))) \
	x(jt,   (y(Site, s) y(InputValue, d))) \
	x(intrinsic, (y(Intrinsic, i))) \

enum class InstructionKind : u8 {
#define x(name, fields) name,
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
};

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
	
	void visit_operands(auto &&visitor) {
		switch (kind) {
			#define y(type, name) visitor(i.name, visitor);
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

private:
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

Callback generate_callback(Lambda *lambda) {
	// arg0 - rcx
	// arg1 - rdx
	// arg2 - r8
	// arg3 - r9
	assert(lambda->head.parameters_block.definition_list.count == 4, "Other count of arguments not implemented");
	List<u8> bytes;
	
	// sub rsp, 40 // 8 bytes for lambda and 32 bytes for shadow space
	bytes.add({0x48, 0x83, 0xec, 40});

	// mov r10, lambda
	bytes.add({0x49, 0xba});
	bytes.add(value_as_bytes(lambda));

	// mov r11, ffi_callback
	bytes.add({0x49, 0xbb});
	bytes.add(value_as_bytes(&ffi_callback));
	
	// mov qword ptr[rsp+32], r10
	bytes.add({0x4c, 0x89, 0x54, 0x24, 32});

	// call r11
	bytes.add({0x41, 0xff, 0xd3});
	
	// add rsp, 40
	bytes.add({0x48, 0x83, 0xc4, 40});

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


struct Bytecode {
	List<Instruction> instructions;
	List<u8> global_readonly_data;
	List<u8> global_mutable_data;
	HashMap<u64, Lambda *> first_instruction_to_lambda;
};

}