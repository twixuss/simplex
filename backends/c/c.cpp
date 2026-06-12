#define TL_IMPL
#include "../../src/common.h"
#include "../../src/nodes.h"
#include "../../src/x.h"
#include "../../src/reporter.h"
#include "../../src/visit.h"
#include "../../src/type.h"
#include "../../src/compiler_context.h"
#include "../../src/cmd_args.h"

#include <tl/process.h>
#include <tl/linear_set.h>
#include <tl/variant.h>
#include <tl/precise_time.h>


u32 start_process_and_inherit_standard_streams(Span<utf8> command_line8) {
	scoped(temporary_storage_checkpoint);
	utf16 const *command_line = to_utf16<TemporaryAllocator>(command_line8, true).data;
	SECURITY_ATTRIBUTES saAttr;
	saAttr.nLength = sizeof(saAttr);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL;

	PROCESS_INFORMATION piProcInfo = {};

	STARTUPINFOW siStartInfo = {};
	siStartInfo.cb = sizeof(siStartInfo);
	siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	siStartInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	siStartInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
	siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

	if (!CreateProcessW(NULL,
		(wchar_t *)command_line,
		NULL,          // process security attributes
		NULL,          // primary thread security attributes
		TRUE,          // handles are inherited
		0,             // creation flags
		NULL,          // use parent's environment
		NULL,          // use parent's current directory
		&siStartInfo,  // STARTUPINFO pointer
		&piProcInfo)   // receives PROCESS_INFORMATION
	) {
		standard_error_printer.writeln(u8"Could not start process. Make sure it is in your PATH."s);
		return 1;
	}

	CloseHandle(piProcInfo.hThread);

	WaitForSingleObject((HANDLE)piProcInfo.hProcess, INFINITE);

	DWORD exit_code;
	GetExitCodeProcess((HANDLE)piProcInfo.hProcess, &exit_code);
	return (u32)exit_code;
}
bool run(String cmd) {
	standard_error_printer.writeln(cmd);

	u32 code = start_process_and_inherit_standard_streams(cmd);

	if (code) {
		standard_error_printer.writeln(u8"Process failed."s);
		return false;
	}
	return true;
}

#if COMPILER_MSVC
#define CFLAGS ""
#else
#define CFLAGS "-Werror=implicit-function-declaration -fmax-errors=1"
#endif

// TODO: select cl/gcc/clang/whatever through command line arguments instead of this way
bool compile_intrinsics(String intrinsics_c_path, String intrinsics_obj_path) {
	#if COMPILER_MSVC
	if (!run(tformat(u8"cl "CFLAGS" /c {} /Fo:\"{}\" /FS /nologo", intrinsics_c_path, intrinsics_obj_path)))
		return false;
	#else
	if (!run(tformat(u8"gcc "CFLAGS" -c {} -o \"{}\"", intrinsics_c_path, intrinsics_obj_path)))
		return false;
	#endif

	return true;
}
bool compile_input(String path_base, String intrinsics_obj_path) {
	#if COMPILER_MSVC
	if (!run(tformat(u8"cl "CFLAGS" {}.c /Fo:\"{}.obj\" {} /Zi /FS /JMC /nologo /link /out:{}.exe", path_base, path_base, intrinsics_obj_path, path_base)))
		return false;
	#else
	if (!run(tformat(u8"gcc "CFLAGS" {}.c {} -o {}.exe", path_base, intrinsics_obj_path, path_base)))
		return false;
	#endif

	return true;
}

CompilerContext *context;

Node *debug_current_node;

#define tto_string to_string<TemporaryAllocator>


// An extra string to append to labels.
// Appending a node multiple times might introduce name collisions.
// Use this to make unique names.
GList<utf8> label_extension;

#define scoped_label_extension(str)          \
	auto prev_count = label_extension.count; \
	label_extension.add(str);                \
	defer { label_extension.count = prev_count; }

template <class T>
struct ExtLabel {
	T value;
};

template <class T>
inline void append(StringBuilder &builder, ExtLabel<T> const &id) {
	append_format(builder, "{}{}", id.value, label_extension);
}



template <class T>
struct CName {
	T value;
};

void append(StringBuilder &builder, CName<String> name) {
	for (auto c : name.value) {
		if (c == '_' ||
			('0' <= c && c <= '9') ||
			('a' <= c && c <= 'z') ||
			('A' <= c && c <= 'Z')
		) {
			append(builder, c);
		} else {
			append(builder, '_');
		}
	}
}

template <CNode Node>
void append(StringBuilder &builder, CName<Node *> node) {
	return append_format(builder, "_{}", node.value->uid);
}

void append(StringBuilder &builder, CName<Lambda *> lambda) {
	if (lambda.value->link_name.count) {
		append(builder, lambda.value->link_name);
		return;
	}
	
	if (lambda.value->is_extern) {
		append(builder, lambda.value->definition ? lambda.value->definition->name : lambda.value->link_name ? lambda.value->link_name : tformat(u8"UNKNOWN_EXTERN_LAMBDA_{}"s, lambda.value->uid));
		return;
	}

	if (lambda.value->is_intrinsic) {
		append(builder, lambda.value->definition ? lambda.value->definition->name : lambda.value->link_name ? lambda.value->link_name : tformat(u8"UNKNOWN_INTRINSIC_LAMBDA_{}"s, lambda.value->uid));
		return;
	}

	append(builder, "fn_");
	append(builder, lambda.value->uid);
}
void append(StringBuilder &builder, CName<Struct *> t) {
	if (t.value->definition) {
		append(builder, CName{t.value->definition->name});
	}
	append(builder, '_');
	append(builder, t.value->uid);
}
void append(StringBuilder &builder, CName<Definition *> t) {
	return append_format(builder, "{}_{}", t.value->name, t.value->uid);
}
void append(StringBuilder &builder, CName<Name *> t) {
	visit_one(direct(t.value), Combine{
		[&](Lambda *node) { append(builder, CName{node}); },
		[&](Struct *node) { append(builder, CName{node}); },
		//[&](Definition *node) { append(builder, CName{node}); },
		[&](auto *node) { append_format(builder, "_{}", node->uid); },
	});
}
void append(StringBuilder &builder, CName<Node *> t) {
	switch (t.value->kind) {
		#define x(name) case NodeKind::name: return append(builder, CName{(name *)t.value});
		ENUMERATE_NODE_KIND(x)
		#undef x
	}
	invalid_code_path();
}
void append(StringBuilder &builder, CName<Expression *> t) {
	switch (t.value->kind) {
		#define x(name) case NodeKind::name: return append(builder, CName{(name *)t.value});
		ENUMERATE_EXPRESSION_KIND(x)
		#undef x
	}
	invalid_code_path();
}
void append(StringBuilder &builder, CName<Statement *> t) {
	switch (t.value->kind) {
		#define x(name) case NodeKind::name: return append(builder, CName{(name *)t.value});
		ENUMERATE_STATEMENT_KIND(x)
		#undef x
	}
	invalid_code_path();
}



struct CType {
	Type type;
};

CType ctype(Type type) {
	return {type};
}

struct ArrayDesc {
	Type innermost_element_type;
	StaticList<u32, 8> counts;

	ArrayDesc(ArrayType *array) {
		while (array) {
			innermost_element_type = direct(array->element_type);
			counts.add(array->count.value());
			array = as<ArrayType>(innermost_element_type);
		}
	}

	bool operator==(ArrayDesc const &that) const {
		return counts.span() == that.counts.span() && types_match(innermost_element_type, that.innermost_element_type);
	}
};

template <>
u64 get_hash(ArrayDesc const &a) {
	return get_hash(a.counts.span()) ^ (u64)a.innermost_element_type.expression;
}


GHashMap<ArrayDesc, u32> built_array_types;

GLinearSet<LambdaHead *> went_over_heads;

u32 get_first_matching_head_uid(LambdaHead *head) {
	for (auto other : went_over_heads) {
		if (head->parameters_block.definition_list.count != other->parameters_block.definition_list.count)
			goto next_head;

		for (umm i = 0; i < head->parameters_block.definition_list.count; ++i) {
			if (!types_match(head->parameters_block.definition_list[i]->type, other->parameters_block.definition_list[i]->type)) {
				goto next_head;
			}
		}

		if (!types_match(head->return_type, other->return_type)) {
			goto next_head;
		}

		// Signature matches with previous.
		return other->uid;

	next_head:;
	}

	went_over_heads.add(head);
	return head->uid;
}

void append(StringBuilder &builder, CType type) {
	auto t = (Expression *)type.type;
	visit_one(t, Combine{
		[&](auto *t) {
			append_format(builder, "UnknownType_{}", t->kind);
		},
		[&](BuiltinTypeName *t) {
			switch (t->type_kind) {
				case BuiltinType::Bool: append(builder, "bool"); break;
				case BuiltinType::U8:  append(builder, "uint8_t"); break;
				case BuiltinType::U16: append(builder, "uint16_t"); break;
				case BuiltinType::U32: append(builder, "uint32_t"); break;
				case BuiltinType::U64: append(builder, "uint64_t"); break;
				case BuiltinType::S8:  append(builder, "int8_t"); break;
				case BuiltinType::S16: append(builder, "int16_t"); break;
				case BuiltinType::S32: append(builder, "int32_t"); break;
				case BuiltinType::S64: append(builder, "int64_t"); break;
				case BuiltinType::F32: append(builder, "float"); break;
				case BuiltinType::F64: append(builder, "double"); break;
				case BuiltinType::UnsizedInteger: append(builder, "int64_t"); break;
				case BuiltinType::UnsizedFloat: append(builder, "float64_t"); break;
				case BuiltinType::None: append(builder, "void"); break;
				default: append_format(builder, "UnknownBuiltinType_{}", t->type_kind); break;
			}
		},
		[&](Unary *t) {
			switch (t->operation) {
				case UnaryOperation::pointer: {
					append(builder, ctype(t->expression));
					append(builder, '*');
					break;
				}
				default: append_format(builder, "UnknownUnary_{}", t->operation); break;
			}
		},
		[&](Name *t) {
			append(builder, ctype(t->definition()->initial_value));
		},
		[&](Struct *t) {
			append_format(builder, "_{}", t->uid);
		},
		[&](Enum *t) {
			append_format(builder, "_{}", t->uid);
		},
		[&](LambdaHead *head) {
			append_format(builder, "_{}", head->uid);
		},
		[&](ArrayType *array) {
			if (auto found = built_array_types.find(array)) {
				append_format(builder, "_{}", *found.value);
			} else {
				append_format(builder, "_ARRAY_OF_{}_WHICH_IS_DECLARED_AFTER", direct(array->element_type)->uid);
			}
		},
	});
}

struct CUnaryOperation { UnaryOperation op; };
void append(StringBuilder &builder, CUnaryOperation op) {
	switch (op.op) {
		case UnaryOperation::plus: return append(builder, "+");
		case UnaryOperation::minus: return append(builder, "-");
		case UnaryOperation::lnot: return append(builder, "!");
		case UnaryOperation::pointer: return append(builder, "*");
		case UnaryOperation::dereference: return append(builder, "*");
		case UnaryOperation::addr: return append(builder, "&");
		case UnaryOperation::bnot: return append(builder, "~");
		default: return append_format(builder, "(invalid unop {})", op.op);
	}
}

struct CBinaryOperation { BinaryOperation op; };
void append(StringBuilder &builder, CBinaryOperation op) {
	switch (op.op) {
		case BinaryOperation::dot: return append(builder, ".");
		case BinaryOperation::mul: return append(builder, "*");
		case BinaryOperation::div: return append(builder, "/");
		case BinaryOperation::mod: return append(builder, "%");
		case BinaryOperation::add: return append(builder, "+");
		case BinaryOperation::sub: return append(builder, "-");
		case BinaryOperation::bor: return append(builder, "|");
		case BinaryOperation::ban: return append(builder, "&");
		case BinaryOperation::bxo: return append(builder, "^");
		case BinaryOperation::bsl: return append(builder, "<<");
		case BinaryOperation::bsr: return append(builder, ">>");
		case BinaryOperation::equ: return append(builder, "==");
		case BinaryOperation::neq: return append(builder, "!=");
		case BinaryOperation::les: return append(builder, "<");
		case BinaryOperation::leq: return append(builder, "<=");
		case BinaryOperation::grt: return append(builder, ">");
		case BinaryOperation::grq: return append(builder, ">=");
		case BinaryOperation::lan: return append(builder, "&&");
		case BinaryOperation::lor: return append(builder, "||");
		case BinaryOperation::ass: return append(builder, "=");
		case BinaryOperation::addass: return append(builder, "+=");
		case BinaryOperation::subass: return append(builder, "-=");
		case BinaryOperation::mulass: return append(builder, "*=");
		case BinaryOperation::divass: return append(builder, "/=");
		case BinaryOperation::modass: return append(builder, "%=");
		case BinaryOperation::borass: return append(builder, "|=");
		case BinaryOperation::banass: return append(builder, "&=");
		case BinaryOperation::bxoass: return append(builder, "^=");
		case BinaryOperation::bslass: return append(builder, "<<=");
		case BinaryOperation::bsrass: return append(builder, ">>=");
		default: return append_format(builder, "(invalid binop {})", op.op);
	}
}

struct Tabs { 
	u32 value;
	
	void operator()(StringBuilder &builder) {
		append(builder, Repeat{"    ", value});
	}
	void operator()() {
		standard_error_printer.write(Repeat{"    ", value});
	}
	void operator++() {++value;}
	void operator--() {--value;}
} tabs;

#define tabbed \
	++tabs;    \
	defer { --tabs; }

#define tabbed_block            \
	append_line(code, "{");     \
	++tabs;                     \
	defer {                     \
		--tabs;                 \
		append_line(code, "}"); \
	}

bool generate_readable_code = false;

void append_line(StringBuilder &builder, auto const &...args) {
	if (generate_readable_code) {
		tabs(builder);
	}
	if constexpr (sizeof...(args) == 1) {
		append(builder, args...);
	} else {
		append_format(builder, args...);
	}
	if (generate_readable_code) {
		append(builder, '\n');
	}
}

// Format:
// Values    - "_{id}"
// Addresses - "a{id}"

void append_node(StringBuilder &code, Node *node, bool define = true);

void append_address(StringBuilder &code, Node *node) {
	scoped_replace(debug_current_node, node);
	
	visit_one(node, Combine{
		[&](auto *node) {
			append_line(code, "a{} = &(unknown_node {});", node->uid, node->kind);
		},
		[&](NoneLiteral *literal) {
			append_line(code, "void* a{} = 0;", node->uid);
		},
		//[&](StringLiteral *literal) {
		//	append_line(code, "address_of_string_literal");
		//},
		[&](Definition *definition) {
			if (definition->initial_value) {
				append_node(code, definition->initial_value);
				append_line(code, "{} _{} = _{};", ctype(definition->type), node->uid, definition->initial_value->uid);
			} else {
				append_line(code, "{} _{} = {{0}};", ctype(definition->type), node->uid);
			}
			append_line(code, "{} *a{} = &_{};", ctype(definition->type), node->uid, definition->uid);
		},
		[&](Name *name) {
			append_line(code, "{} *a{} = &_{};", ctype(name->type), node->uid, name->definition()->uid);
		},
		[&](Binary *binary) {
			if (binary->operation == BinaryOperation::dot) {
				if (as_pointer(binary->left->type)) {
					append_node(code, binary->left);
					append_line(code, "{} *a{} = &_{}->_{};", ctype(binary->type), node->uid, binary->left->uid, as<Name>(binary->right)->definition()->uid);
				} else {
					append_address(code, binary->left);
					append_line(code, "{} *a{} = &a{}->_{};", ctype(binary->type), node->uid, binary->left->uid, as<Name>(binary->right)->definition()->uid);
				}
				return;
			}
			append_line(code, "a{} = &(unknown_binary {});", node->uid, binary->operation);
		},
		[&](Unary *unary) {
			if (unary->operation == UnaryOperation::dereference) {
				append_node(code, unary->expression);
				append_line(code, "{} *a{} = _{};", ctype(unary->type), node->uid, unary->expression->uid);
				return;
			}
			append_line(code, "{} *a{} = &(unknown_unary {});", ctype(unary->type), node->uid, unary->operation);
		},
		[&](Block *block) {
			append_line(code, "{} *a{} = 0;", ctype(block->type), node->uid);

			for (auto child : block->children.skip(-1)) {
				append_node(code, child);
			}
			append_address(code, block->children.back());
			append_line(code, "a{} = a{};", node->uid, block->children.back()->uid);
		},
		[&](Subscript *subscript) {
			append_address(code, subscript->subscriptable);
			append_node(code, subscript->index);
			append_line(code, "{} *a{} = &a{}->data[_{}];", ctype(subscript->type), node->uid, subscript->subscriptable->uid, subscript->index->uid);
		},
	});
}

String debug_info_last_file;
u32 debug_info_last_line;

void append_debug_info(StringBuilder &builder, String location) {
	if (generate_readable_code)
		return;

	if (location) {
		auto sloc = get_source_location(location);
		if (debug_info_last_line != sloc.lines_start_number || debug_info_last_file != sloc.file) {
			append_format(builder, "\n#line {} \"{}\"\n", sloc.lines_start_number, EscapedCString{sloc.file});
		}
		debug_info_last_line = sloc.lines_start_number;
		debug_info_last_file = sloc.file;
	}
}

// if `define` is false, definition for resulting variable will be omitted.
// one defer body can be appended multiple times, which will create duplicate definitions.
// `define` is not propagated.
void append_node(StringBuilder &code, Node *node, bool define) {
	auto prev_node = debug_current_node;
	scoped_replace(debug_current_node, node);

	append_debug_info(code, node->location);

	//defer{
	//	if (prev_node && prev_node->location) {
	//		auto sloc = get_source_location(prev_node->location);
	//		if (debug_info_last_line != sloc.lines_start_number - 1 || debug_info_last_file != sloc.file) {
	//			append_format(code, "#line {} \"{}\"\n", sloc.lines_start_number-1, EscapedCString{sloc.file});
	//		}
	//		debug_info_last_line = sloc.lines_start_number - 1;
	//		debug_info_last_file = sloc.file;
	//	}
	//};

	if (generate_readable_code) {
		append_line(code, "/* {} */", node->location);
	}

	if (auto expression = as<Expression>(node)) {
		if (!types_match(expression->type, BuiltinType::None)) {
			if (define) {
				append_line(code, "{} _{} = {{0}};", ctype(expression->type), node->uid);
			}
		}
	}

	visit_one(node, Combine{
		[&](Node *node) {
			append_line(code, "_{} = (unknown_node {});", node->uid, node->kind);
		},
		[&](Expression *expression) {
			append_line(code, "_{} = (unknown_expression {});", node->uid, expression->kind);
		},
		[&](Lambda *lambda) {
			append_line(code, "_{} = {};", node->uid, CName{lambda});
		},
		[&](Defer *Defer) {
		},
		[&](Block *block) {
			{tabbed_block;
				for (auto child : block->children) {
					append_node(code, child);
				}

				if (!types_match(block->type, BuiltinType::None)) {
					if (auto last_expr = as<Expression>(block->children.back()); last_expr && !types_match(last_expr->type, BuiltinType::None)) {
						append_line(code, "_{} = _{};", node->uid, last_expr->uid);
					}
				}
			
				append_line(code, "end_{}:;", ExtLabel{block->uid});

				for (auto Defer : block->defers) {
					{tabbed_block;
						append_node(code, Defer->body, false);
					}
				}
			}
		},
		[&](Definition *definition) {
			if (definition->initial_value) {
				append_node(code, definition->initial_value);
				append_line(code, "_{} = _{};", node->uid, definition->initial_value->uid);
			} else {
				if (auto Struct = direct_as<::Struct>(definition->type)) {
					for (auto member : Struct->member_list) {
						if (member->initial_value) {
							append_node(code, member->initial_value);
							append_line(code, "_{}._{} = _{};", node->uid, member->uid, member->initial_value->uid);
						}
					}
				}
			}
		},
		[&](ZeroInitialized *zero_initialized) {
			append_line(code, "memset(&_{}, 0, sizeof(_{}));", node->uid, node->uid);
		},
		[&](Name *name) {
			append_line(code, "_{} = {};", node->uid, CName{name});
		},
		[&](BuiltinTypeName *name) {
			append_format(code, "BuiltinTypeName({})", name->type_kind);
		},
		[&](NoneLiteral *literal) {
			append_line(code, "int64_t _{} = 0;", node->uid);
		},
		[&](BooleanLiteral *literal) {
			append_line(code, "_{} = {};", node->uid, literal->value);
		},
		[&](IntegerLiteral *literal) {
			auto type = direct(literal->type);
			if (auto Enum = as<::Enum>(type)) {
				type = direct(Enum->underlying_type);
			}
			auto builtin_type = as<BuiltinTypeName>(type);
			if (!builtin_type) {
				immediate_reporter.error(literal->location, "backends/c: append_node: Unexpected integer literal type");
				invalid_code_path();
			}
			auto underlying_builtin_type = builtin_type->type_kind;
			switch (underlying_builtin_type) {
				case BuiltinType::U8 : append_line(code, "_{} = {}u;",   node->uid, (u32)literal->value); break;
				case BuiltinType::U16: append_line(code, "_{} = {}u;",   node->uid, (u32)literal->value); break;
				case BuiltinType::U32: append_line(code, "_{} = {}u;",   node->uid, (u32)literal->value); break;
				case BuiltinType::U64: append_line(code, "_{} = {}ull;", node->uid, (u64)literal->value); break;
				case BuiltinType::S8 : append_line(code, "_{} = {};",    node->uid, (s32)literal->value); break;
				case BuiltinType::S16: append_line(code, "_{} = {};",    node->uid, (s32)literal->value); break;
				case BuiltinType::S32: append_line(code, "_{} = {};",    node->uid, (s32)literal->value); break;
				case BuiltinType::S64: append_line(code, "_{} = {}ll;",  node->uid, (s64)literal->value); break;

				case BuiltinType::UnsizedInteger:
					// TODO: UnsizedInteger constants must be split into 4:
					//       1, 2, 4 and 8 byte ones. The right one should
					//       be picked at usage site, depending on the destination type.
					//       Same should be done with UnsizedFloats.
					//       In the meantime treat them as S64
					append_line(code, "_{} = {}ll;", node->uid, (s64)literal->value);
					break;
				default: 
					immediate_reporter.error(literal->location, "backends/c: append_node: IntegerLiteral: unhandled type {}", underlying_builtin_type);
					invalid_code_path();
					break;
			}
		},
		[&](FloatLiteral *literal) {
			append_line(code, "_{} = {};", node->uid, (f64)literal->value);
		},
		[&](StringLiteral *literal) {
			append_line(code, "_{} = __make_string(\"{}\");", node->uid, EscapedCString{literal->value});
		},
		[&](Unary *unary) {
			if (unary->operation == UnaryOperation::addr) {
				append_address(code, unary->expression);
				append_line(code, "_{} = a{};", node->uid, unary->expression->uid);
				return;
			}
			append_node(code, unary->expression);
			append_line(code, "_{} = {}_{};", node->uid, CUnaryOperation{unary->operation}, unary->expression->uid);
		},
		[&](Binary *binary) {
			if (binary->operation == BinaryOperation::dot) {
				append_node(code, binary->left);

				auto dot_or_arrow = as_pointer(binary->left->type) ? u8"->"s : u8"."s;

				append_line(code, "_{} = _{}{}_{};", node->uid, binary->left->uid, dot_or_arrow, as<Name>(binary->right)->definition()->uid);
				return;
			}

			if (is_ass(binary->operation)) {
				append_address(code, binary->left);
				append_node(code, binary->right);
				append_line(code, "*a{} {} _{};", binary->left->uid, CBinaryOperation{binary->operation}, binary->right->uid);
				return;
			}

			if (binary->operation == BinaryOperation::as) {
				append_node(code, binary->left);
				if (!types_match(binary->right, get_builtin_type(BuiltinType::None))) {
					append_line(code, "_{} = ({})(_{});", node->uid, ctype(binary->right), binary->left->uid);
				}
				return;
			}
			
			if (as_pointer(binary->left->type)) {
				append_node(code, binary->left);
				append_node(code, binary->right);
				append_line(code, "_{} = _{} {} _{};", node->uid, binary->left->uid, CBinaryOperation{binary->operation}, binary->right->uid);
				return;
			}

			if (as_pointer(binary->right->type)) {
				append_node(code, binary->left);
				append_node(code, binary->right);
				append_line(code, "_{} = _{} {} (char *)_{};", node->uid, binary->left->uid, CBinaryOperation{binary->operation}, binary->right->uid);
				return;
			}

			if (is_concrete_integer(binary->right->type) && is_concrete_integer(binary->left->type) && binary->operation == BinaryOperation::mod) {
				assert(types_match(binary->left->type, binary->right->type));
				append_node(code, binary->left);
				append_node(code, binary->right);
				append_line(code, "_{} = __modulo_{}(_{}, _{});", node->uid, binary->left->type, binary->left->uid, binary->right->uid);
				return;
			}

			if (is_concrete_float(binary->right->type) && is_concrete_float(binary->left->type) && binary->operation == BinaryOperation::mod) {
				append_node(code, binary->left);
				append_node(code, binary->right);

				// a % b
				// = frac(a / b) * b
				// = (a / b - floor(a / b)) * b
				// 
				// d = a / b
				// (d - floor(d)) * b
				
				auto temp_type_str = get_size(binary->right->type) == 4 ? "float" : "double";
				auto floor_str = get_size(binary->right->type) == 4 ? "floorf" : "floor";

				append_line(code, "{} d{} = _{} / _{};", temp_type_str, node->uid, binary->left->uid, binary->right->uid);
				append_line(code, "_{} = (d{} - {}(d{})) * _{};", node->uid, node->uid, floor_str, node->uid, binary->right->uid);
				return;
			}

			append_node(code, binary->left);
			append_node(code, binary->right);
			append_line(code, "_{} = ({})(_{} {} _{});", node->uid, ctype(binary->type), binary->left->uid, CBinaryOperation{binary->operation}, binary->right->uid);
		},
		[&](Return *ret) {
			if (ret->value) {
				append_node(code, ret->value);
			}

			for (auto Defer : ret->defers) {
				{tabbed_block;
					append_node(code, Defer->body, false);
				}
			}

			if (ret->value) {
				append_line(code, "return _{};", ret->value->uid);
			} else {
				append_line(code, "return;");
			}
		},
		[&](Call *call) {
			switch (call->call_kind) {
				case CallKind::lambda: {
					append_node(code, call->callable);

					for (auto arg : call->arguments) {
						append_node(code, arg.expression);
					}
			
					tabs(code);
					if (!types_match(call->type, BuiltinType::None)) {
						append_format(code, "_{} = ", node->uid);
					}
					append_format(code, "_{}(", call->callable->uid);
					for (auto &arg : call->arguments) {
						if (&arg != &call->arguments[0]) {
							append(code, ", ");
						}
						append_format(code, "_{}", arg.expression->uid);
					}
					append_format(code, ");");
					break;
				}
				case CallKind::constructor: {
					auto s = direct_as<Struct>(call->type);

					for (auto arg : call->arguments) {
						append_node(code, arg.expression);
					}
					
					tabs(code);
					append_format(code, "_{} = ({}){{", node->uid, ctype(call->type));
					for (umm i = 0; i < call->arguments.count; ++i) {
						auto member = s->member_list[i];
						append_format(code, "._{} = _{},", member->uid, call->arguments[i].expression->uid);
					}
					append(code, "};");

					break;
				}
				default: {
					append_format(code, "_{} = UnknownCallKind({})", node->uid, call->call_kind);
					break;
				}
			}
		},
		[&](Match *match) {
			bool is_expression = !types_match(match->type, BuiltinType::None);
			append_node(code, match->expression);
			{tabbed_block;
				for (auto c : match->cases) {
					if (!c.froms)
						continue;

					for (auto from : c.froms) {
						append_node(code, from);
						append_line(code, "if (_{} == _{}) goto take_case_{};", match->expression->uid, from->uid, c.to->uid);
					}

					append_line(code, "goto skip_case_{};", ExtLabel{c.to->uid});
					append_line(code, "take_case_{}:;", ExtLabel{c.to->uid});
					{tabbed_block;
						append_node(code, c.to);
						if (is_expression) {
							append_line(code, "_{} = _{};", node->uid, c.to->uid);
						}
						append_line(code, "goto endmatch{};", ExtLabel{match->uid});
					}
					append_line(code, "skip_case_{}:;", ExtLabel{c.to->uid});
				}
				if (is_expression) {
					if (match->default_case) {
						append_node(code, match->default_case->to);
						append_line(code, "_{} = _{};", node->uid, match->default_case->to->uid);
					} else {
						append_line(code, "panic(__make_string(\"incomplete match\"));");
					}
				} else {
					if (match->default_case) {
						append_node(code, match->default_case->to);
					}
				}
				append_line(code, "endmatch{}:;", ExtLabel{match->uid});
			}
		},
		[&](While *While) {
			append_line(code, "while (true)");
			{tabbed_block;
				append_node(code, While->condition);
				append_line(code, "if (!_{}) break;", While->condition->uid);

				append_node(code, While->body);
				append_line(code, "continue_{}:;", ExtLabel{While->uid});
			}
			append_line(code, "break_{}:;", ExtLabel{While->uid});
		},
		[&](Continue *Continue) {
			for (auto Defer : Continue->defers) {
				{tabbed_block;
					append_node(code, Defer->body, false);
				}
			}
			append_line(code, "goto continue_{};", ExtLabel{Continue->loop->uid});
		},
		[&](Break *Break) {
			if (Break->loop) {
				for (auto Defer : Break->defers) {
					{tabbed_block;
						append_node(code, Defer->body, false);
					}
				}
				append_line(code, "goto break_{};", ExtLabel{Break->loop->uid});
			} else {
				assert(Break->tag_block);
				append_node(code, Break->value);
				append_line(code, "{} _{} = _{};", ctype(Break->value->type), node->uid, Break->value->uid);
				append_line(code, "_{} = _{};", Break->tag_block->uid, node->uid);
				for (auto Defer : Break->defers) {
					{tabbed_block;
						append_node(code, Defer->body, false);
					}
				}
				append_line(code, "goto end_{};", ExtLabel{Break->tag_block->uid});
			}
		},
		[&](IfStatement *If) {
			append_node(code, If->condition);
			append_line(code, "if (_{})", If->condition->uid);
			{tabbed_block;
				append_node(code, If->true_branch);
			}
			if (If->false_branch) {
				append_line(code, "else");
				{tabbed_block;
					append_node(code, If->false_branch);
				}
			}
		},
		[&](IfExpression *If) {
			if (If->is_array) {
				append_node(code, If->condition);
				auto arr = as<ArrayType>(If->true_branch->type);
				auto count = arr->count.value();
				append_line(code, "uint64_t _msk{} = __movmsk(&_{}, {}, {});", If->uid, If->condition->uid, 1, count);
				append_line(code, "if (_msk{} == {})", If->uid, slln(1, count) - 1);
				{tabbed_block;
					append_node(code, If->true_branch);
					if (!types_match(If->type, BuiltinType::None)) {
						append_line(code, "_{} = _{};", node->uid, If->true_branch->uid);
					}
				}
				append_line(code, "else if (_msk{} == 0)", If->uid);
				{tabbed_block;
					append_node(code, If->false_branch);
					if (!types_match(If->type, BuiltinType::None)) {
						append_line(code, "_{} = _{};", node->uid, If->false_branch->uid);
					}
				}
				append_line(code, "else");
				{tabbed_block;
					scoped_label_extension(tto_string(If->uid));

					append_node(code, If->true_branch);
					append_node(code, If->false_branch);
					append_line(code, "__blend(&_{}, _msk{}, &_{}, &_{}, {}, {});", node->uid, If->uid, If->true_branch->uid, If->false_branch->uid, get_size(arr->element_type), count);
				}
			} else {
				append_node(code, If->condition);
				append_line(code, "if (_{})", If->condition->uid);
				{tabbed_block;
					append_node(code, If->true_branch);
					if (!types_match(If->type, BuiltinType::None)) {
						append_line(code, "_{} = _{};", node->uid, If->true_branch->uid);
					}
				}
				if (If->false_branch) {
					append_line(code, "else");
					{tabbed_block;
						append_node(code, If->false_branch);
						if (!types_match(If->type, BuiltinType::None)) {
							append_line(code, "_{} = _{};", node->uid, If->false_branch->uid);
						}
					}
				}
			}
		},
		[&](Subscript *subscript) {
			append_address(code, subscript->subscriptable);
			append_node(code, subscript->index);
			append_line(code, "_{} = a{}->data[_{}];", node->uid, subscript->subscriptable->uid, subscript->index->uid);
		},
		[&](ArrayConstructor *constructor) {
			for (auto e : constructor->elements) {
				append_node(code, e);
			}

			tabs(code);
			append_format(code, "_{} = ({}){{", node->uid, ctype(constructor->type));
			for (auto e : constructor->elements) {
				append_format(code, "_{}, ", e->uid);
			}
			append_line(code, "};");
		},
		[&](Use *Use) {
			// nothing to do
		},
	});
}

CmdArg args_handlers[] = {
	{"-readable", +[] { generate_readable_code = true; }},
};

extern "C" __declspec(dllexport)
void init(CompilerContext *c, Span<String> args) {
	context = c;

	init_allocator();
	init_printer();

	for (umm i = 0; i < args.count; ++i) {

		for (auto handler : args_handlers) {
			auto cmd = args[i];
			if (args[i] == handler.key) {
				handler.run.visit(Combine {
					[&](void (*run)()) {
						run();
					},
					[&](void (*run)(u64 x)) {
						if (++i < args.count) {
							if (auto number = parse_u64(args[i])) {
								run(number.value());
							} else {
								immediate_reporter.error("Could not parse number after {}. Ignoring.", cmd);
							}
						} else {
							immediate_reporter.error("Expected a number after {}.", cmd);
						}
					},
					[&](void (*run)(String x)) {
						if (++i < args.count) {
							run(args[i]);
						} else {
							immediate_reporter.error("Expected a string after {}.", cmd);
						}
					},
				});
				goto next_arg;
			}
		}
		immediate_reporter.warning("Unknown command line parameter for c backend: {}", args[i]);
	next_arg:;
	}
}

struct DirectExpression {
	Expression *expression = 0;
	DirectExpression(Expression *expression) : expression(direct(expression)) {}
	inline constexpr auto operator<=>(DirectExpression const &) const noexcept = default;
};

#define PRELUDE \
"#include <stdint.h>            \n" \
"#include <stdbool.h>           \n" \
"                               \n" \
"typedef bool Bool;             \n" \
"typedef uint8_t U8;            \n" \
"typedef uint16_t U16;          \n" \
"typedef uint32_t U32;          \n" \
"typedef uint64_t U64;          \n" \
"typedef int8_t S8;             \n" \
"typedef int16_t S16;           \n" \
"typedef int32_t S32;           \n" \
"typedef int64_t S64;           \n" \
"                               \n" \
"typedef U64 UInt;              \n" \
"typedef S64 SInt;              \n" \
"typedef SInt Int;              \n" \

void add_dependencies(Expression* root, LinearSet<DirectExpression> &types_to_declare) {
	if (find(types_to_declare.span(), DirectExpression{root})) {
		return;
	}
	visit(root, Combine {
		[&](auto) {},
		[&](Lambda *lambda) {
			if (lambda->head.is_template)
				return ForEach_dont_recurse;
			return ForEach_continue;
		},
		[&](Expression *expression) {
			if (auto array = direct_as<ArrayType>(expression)) {
				types_to_declare.add(array);
			} else if (auto array = direct_as<ArrayType>(expression->type)) {
				types_to_declare.add(array);
			}
		},
		[&](ArrayType *array) {
			add_dependencies(array->element_type, types_to_declare);
			types_to_declare.add(array);
		},
		[&](Struct *s) {
			int dummy = 443;
			for (auto member : s->member_list) {
				add_dependencies(member, types_to_declare);
			}
			types_to_declare.add(s);
			return ForEach_dont_recurse;
		},
		[&](LambdaHead *head) {
			add_dependencies(head->return_type, types_to_declare);
			for (auto &parameter : head->parameters_block.definition_list) {
				add_dependencies(parameter->type, types_to_declare);
			}
			types_to_declare.add(head);
		},
		[&](Enum *Enum) {
			add_dependencies(Enum->underlying_type, types_to_declare);
			types_to_declare.add(Enum);
		},
		[&](Name *name) {
			add_dependencies(name->definition(), types_to_declare);
			add_dependencies(name->definition()->type, types_to_declare);
		}
	});
}
	
extern "C" __declspec(dllexport)
bool convert_ast(Block *global_block, Lambda *main_lambda, Definition *main_lambda_definition) {
	(void)main_lambda_definition;
	StringBuilder builder;
	StringBuilder code;
	
	append(builder, PRELUDE R"(
#include <stdio.h>
#include <string.h>
#include <math.h>

#undef assert

#pragma warning(push, 0)

)");
	
	LinearSet<String> extern_libraries;
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Lambda *lambda) {
			if (lambda->is_extern) {
				extern_libraries.add(lambda->extern_library);
			}
		},
	});
	for (auto lib : extern_libraries) {
		append_format(builder, "#pragma comment(lib, \"{}\")\n", lib);
	}


	LinearSet<DirectExpression> types_to_declare;

	add_dependencies(global_block, types_to_declare);

	append(builder, R"(
//
// Struct declarations
//
)");
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Struct *s) {
			append_format(builder, "typedef struct _{} _{};\n", s->uid, s->uid);
		},
	});

	append(builder, R"(
//
// Type definitions
//
)");
	
	for (auto type : types_to_declare) {
		visit_one(type.expression, Combine {
			[&] (auto) {},
			[&] (Struct *s) {
				append_format(builder, "struct _{} {{ // {}\n", s->uid, s->definition ? s->definition->name : to_string(get_source_location(s->location)));
				++tabs;
				if (s->member_list.count) {
					for (auto member : s->member_list) {
						append_line(builder, "{} _{};", ctype(member->type), member->uid);
					}
				} else {
					append_line(builder, "char dummy;");
				}
				--tabs;
				append(builder, "};\n");
			},
			[&] (ArrayType *array){
				auto desc = ArrayDesc(array);

				if (built_array_types.find(desc))
					return;

				append_format(builder, 
R"(// {}   hash {}   innermost {}  counts {}
typedef struct {{
	{} data[{}];
}} _{};
)", array, format_hex(get_hash(desc)), format_hex((u64)(Node *)desc.innermost_element_type), desc.counts.span(), ctype(array->element_type), array->count.value(), array->uid);

				built_array_types.get_or_insert(array) = array->uid;
			},
			[&] (LambdaHead *head) {
				append_line(builder, "/* {} */", head->location);
				if (!head->is_template) {
					append_format(builder, "typedef {} (*_{})(", ctype(head->return_type), head->uid);
					for (umm i = 0; i < head->parameters_block.definition_list.count; ++i) {
						auto param = head->parameters_block.definition_list[i];

						if (i) {
							append(builder, ", ");
						}

						append(builder, ctype(param->type));
					}
					append(builder, ");\n");
				}
			},
			[&](Enum *Enum) {
				append_format(builder, "typedef {} _{};\n", ctype(Enum->underlying_type), Enum->uid);
			},
		});
	}
	
	append_format(builder, R"(
//
// Intrinsics declarations
//
_{} __make_string(char const *str);
void print_String(_{} x);
void print_S64(int64_t v);
uint64_t __movmsk(void const *msk, uint64_t size, uint64_t count);
void __blend(void *dest, uint64_t msk, void const *a, void const *b, uint64_t size, uint64_t count);
U8  __modulo_U8 (U8 , U8 );
U16 __modulo_U16(U16, U16);
U32 __modulo_U32(U32, U32);
U64 __modulo_U64(U64, U64);
S8  __modulo_S8 (S8 , S8 );
S16 __modulo_S16(S16, S16);
S32 __modulo_S32(S32, S32);
S64 __modulo_S64(S64, S64);
)", context->builtin_structs.String->uid, context->builtin_structs.String->uid);
	
	append(builder, R"(
//
// Global variables
//
)");

	for (auto global : global_block->children) {
		if (auto definition = as<Definition>(global)) {
			if (definition->initial_value) {
				if (is_type(definition->initial_value)) {
					continue;
				}
				if (direct_as<Lambda>(definition->initial_value)) {
					continue;
				}
			}

			append_line(builder, "{} _{};", ctype(definition->type), definition->uid);
		}
	}

	append(builder, R"(
//
// Function prototypes/declarations
//
)");
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Lambda *lambda) {
			append_line(builder, "/* {} */", lambda->location);
			if (!lambda->head.is_template) {
				if (!lambda->body) {
					append(builder, "extern ");
				}
				append_format(builder, "{} ", ctype(lambda->head.return_type));
				if (!lambda->body) {
					append(builder, "__stdcall ");
				}
				append_format(builder, "{}(", CName{lambda});
				for (umm i = 0; i < lambda->head.parameters_block.definition_list.count; ++i) {
					auto param = lambda->head.parameters_block.definition_list[i];

					if (i) {
						append(builder, ", ");
					}

					append(builder, ctype(param->type));
				}
				append(builder, ");\n");
			}
		},
	});
	
	append(builder, R"(
//
// Function implementations/definitions
//
)");
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Lambda *lambda) {
			append_debug_info(builder, lambda->location);
			append_line(builder, "/* {} */", lambda->location);
			if (lambda->body && !lambda->head.is_template) {
				append_format(builder, "{} {}(", ctype(lambda->head.return_type), CName{lambda});
				for (umm i = 0; i < lambda->head.parameters_block.definition_list.count; ++i) {
					auto param = lambda->head.parameters_block.definition_list[i];

					if (i) {
						append(builder, ", ");
					}

					append_format(builder, "{} _{}", ctype(param->type), param->uid);
				}
				append(builder, ") {");

				++tabs;
				code.clear();

				append_node(code, lambda->body);

				append     (builder, code);
				append     (builder, "retlabel:");
				if (types_match(lambda->body->type, BuiltinType::None)) {
					if (types_match(lambda->head.return_type, BuiltinType::None)) {
						append_line(builder, "return;");
					} else {
						append_line(builder, "panic(__make_string(\"Should have returned something by that time but didn't\"));");
					}
				} else {
					append_line(builder, "return _{};", lambda->body->uid);
				}
				append     (builder, "}\n");
				--tabs;
			}
		},
	});
	
	auto path_base = parse_path(context_base->input_source_path).path_without_extension();

	{
		u32 lines = 0;
		builder.for_each_block([&](StringBuilder::Block *block) {
			for (auto c : *block) {
				lines += c == '\n';
			}
		});
		if (!generate_readable_code) {
			append_format(builder, R"(
#line {} "{}.c"
)", lines + 3, EscapedCString{path_base});
			}
		}

	append_format(builder, R"(
int main() {{
)");

	++tabs;
	append(builder, R"(
    //
    // Global variable initialization
    //
)");

	for (auto global : global_block->children) {
		if (auto definition = as<Definition>(global)) {
			if (definition->initial_value) {
				if (is_type(definition->initial_value)) {
					continue;
				}
				if (direct_as<Lambda>(definition->initial_value)) {
					continue;
				}
				
				append_node(builder, definition->initial_value);
				append_line(builder, "_{} = _{};", definition->uid, definition->initial_value->uid);
			}
		}
	}
	--tabs;

		append_format(builder, "\n#line 999999 \"{}.c\"\n", path_base);

	if (types_match(main_lambda->head.return_type, BuiltinType::None)) {
		append_format(builder, R"(
	{}();
	return 0;
}}
)", CName{main_lambda});
	} else {
		append_format(builder, R"(
	return {}();
}}
)", CName{main_lambda});
	}

	write_entire_file(tformat(u8"{}.c", path_base), to_string(builder));
	
	

	auto intrinsics_c_path = tformat(u8"{}\\intrinsics.c", context_base->compiler_bin_directory);
	auto intrinsics_obj_path = tformat(u8"{}\\intrinsics.obj", context_base->compiler_bin_directory);

	if (get_file_write_time(tformat("{}\\targets\\c.dll", context_base->compiler_bin_directory)).value_or(0) > get_file_write_time(intrinsics_obj_path).value_or(0)) {
		builder.clear();

		append(builder, PRELUDE R"(
#pragma warning(push, 0)

//
// Intrinsics implementation
//

typedef struct {
	U8 *data;
	UInt count;
} String;

bool __stdcall WriteFile(void *hFile, void const *lpBuffer, unsigned nNumberOfBytesToWrite, unsigned *lpNumberOfBytesWritten, void *lpOverlapped);
void *__stdcall GetStdHandle(int);

size_t strlen(char const *);
void *memcpy(void *, void const *, size_t);

String __make_string(char const *str) {
	return (String){(U8 *)str, strlen(str)};
}

// stdio replaces \n with \r\n ...
// thats why winapi
void print_String(String x) {
	WriteFile(GetStdHandle(-11), x.data, x.count, 0, 0);
}
void print_S64(int64_t v) {
	int radix = 10;
	char buf[64];
	char const *digits = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	char *c = buf + sizeof(buf);
	if (v == 0) {
		WriteFile(GetStdHandle(-11), "0", 1, 0, 0);
		return;
	}
	bool neg = v < 0;
	v = neg ? -v : v;

	do {
		*--c = digits[v % radix];
	} while (v /= radix);

	if (neg) {
		*--c = '-';
	}
	WriteFile(GetStdHandle(-11), c, buf + sizeof(buf) - c, 0, 0);

}

U64 __movmsk(U8 const *msk, U64 size, U64 count) {
	U64 result = 0;
	for (U64 i = 0; i < count; ++i)
		result |= (U64)(msk[i * size] & 1) << i;
	return result;
}
void __blend(U8 *d, U64 msk, U8 const *a, U8 const *b, U64 size, U64 count) {
	for (U64 i = 0; i < count; ++i)
		memcpy(d + i*size, ((msk >> i) & 1 ? a : b) + i*size, size);
}


U8  __modulo_U8 (U8  v, U8  s) { return v % s; }
U16 __modulo_U16(U16 v, U16 s) { return v % s; }
U32 __modulo_U32(U32 v, U32 s) { return v % s; }
U64 __modulo_U64(U64 v, U64 s) { return v % s; }
S8  __modulo_S8 (S8  v, S8  s) { return v < 0 ? (v + 1) % s + s - 1 : v % s; }
S16 __modulo_S16(S16 v, S16 s) { return v < 0 ? (v + 1) % s + s - 1 : v % s; }
S32 __modulo_S32(S32 v, S32 s) { return v < 0 ? (v + 1) % s + s - 1 : v % s; }
S64 __modulo_S64(S64 v, S64 s) { return v < 0 ? (v + 1) % s + s - 1 : v % s; }

void debug_break() {
	__debugbreak();
}
void panic(String reason) {
	print_String(reason);
	__debugbreak();
}
void assert(Bool x) {
	if (!x)
		debug_break();
}

)");

		write_entire_file(intrinsics_c_path, to_string(builder));
		if (!compile_intrinsics(intrinsics_c_path, intrinsics_obj_path))
			return false;
	}
	
	if (!compile_input(path_base, intrinsics_obj_path))
		return false;
	
	if (!context_base->keep_build_artifacts) {
		delete_file(tformat(u8"{}.c", path_base));
		delete_file(tformat(u8"{}.obj", path_base));
		delete_file(tformat(u8"{}.ilk", path_base));
	}

	return true;
}

extern "C" __declspec(dllexport)
u32 run() {
	return start_process_and_inherit_standard_streams(tformat(u8"{}.exe", parse_path(context->input_source_path).path_without_extension()));
}

// FIXME: Copied from main.cpp
void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, Span<char> message) {
	scoped(context_base->stdout_mutex);

	immediate_reporter.error(debug_current_node ? debug_current_node->location : String{}, "COMPILER ERROR: {} {} at {}:{} in function {}", cause_string, expression, file, line, function);
	if (message.count)
		println("Message: {}", message);

	println("Call stack:");
	println(resolve_names(get_call_stack()));
}
