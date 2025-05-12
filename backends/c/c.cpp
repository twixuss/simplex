#define TL_IMPL
#include "../../src/common.h"
#include "../../src/nodes.h"
#include "../../src/x.h"
#include "../../src/reporter.h"
#include "../../src/visit.h"
#include "../../src/type.h"
#include "../../src/compiler_context.h"

#include <tl/process.h>
#include <tl/linear_set.h>

CompilerContext *context;

Node *debug_current_node;

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

void append(StringBuilder &builder, CName<Lambda *> lambda) {
	if (lambda.value->link_name.count) {
		append(builder, lambda.value->link_name);
		return;
	}

	if (lambda.value->is_extern) {
		append(builder, lambda.value->definition ? lambda.value->definition->name : lambda.value->link_name ? lambda.value->link_name : tformat(u8"UNKNOWN_EXTERN_LAMBDA_{}"s, lambda.value->uid));
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
void append(StringBuilder &builder, CName<Name *> t) {
	visit_one(direct(t.value), Combine{
		[&](Lambda *node) { append(builder, CName{node}); },
		[&](Struct *node) { append(builder, CName{node}); },
		[&](auto *node) { append_format(builder, "_{}", node->uid); },
	});
}

struct CType {
	Type type;
};

CType ctype(Type type) {
	return {type};
}

struct ArrayDesc {
	Type type;
	u32 count;

	ArrayDesc(ArrayType *array) {
		type = direct(array->element_type);
		count = array->count.value();
	}

	bool operator==(ArrayDesc const &that) const {
		return count == that.count && types_match(type, that.type);
	}
};

template <>
u64 get_hash(ArrayDesc const &a) {
	return a.count ^ (u64)a.type.expression;
}


GHashMap<ArrayDesc, u32> built_array_types;

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
				case BuiltinType::UnsizedInteger: append(builder, "int64_t"); break;
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
		[&](LambdaHead *head) {
			append_format(builder, "_{}", head->uid);
		},
		[&](ArrayType *array) {
			append_format(builder, "_{}", *built_array_types.find(array).value);
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

void append_line(StringBuilder &builder, auto const &...args) {
	tabs(builder);
	if constexpr (sizeof...(args) == 1) {
		append(builder, args...);
	} else {
		append_format(builder, args...);
	}
	append(builder, '\n');
}

void append_node(StringBuilder &code, Node *node, bool define = true);

void append_address(StringBuilder &code, Node *node) {
	scoped_replace(debug_current_node, node);
	
	if (node->uid == 291) {
		int x = 2;
	}

	visit_one(node, Combine{
		[&](auto *node) {
			append_line(code, "_{} = &(unknown_node {});", node->uid, node->kind);
		},
		[&](NoneLiteral *literal) {
			append_line(code, "void* _{} = 0;", node->uid);
		},
		//[&](StringLiteral *literal) {
		//	append_line(code, "address_of_string_literal");
		//},
		[&](Name *name) {
			append_line(code, "{} *_{} = &_{};", ctype(name->type), node->uid, name->definition()->uid);
		},
		[&](Binary *binary) {
			if (binary->operation == BinaryOperation::dot) {
				if (as_pointer(binary->left->type)) {
					append_node(code, binary->left);
				} else {
					append_address(code, binary->left);
				}
				append_line(code, "{} *_{} = &_{}->_{};", ctype(binary->type), node->uid, binary->left->uid, as<Name>(binary->right)->definition()->uid);
				return;
			}
			append_line(code, "_{} = &(unknown_binary {});", node->uid, binary->operation);
		},
		[&](Unary *unary) {
			if (unary->operation == UnaryOperation::dereference) {
				append_node(code, unary->expression);
				append_line(code, "{} *_{} = _{};", ctype(unary->type), node->uid, unary->expression->uid);
				return;
			}
			append_line(code, "{} *_{} = &(unknown_unary {});", ctype(unary->type), node->uid, unary->operation);
		},
		[&](Block *block) {
			append_line(code, "{} *_{} = 0;", ctype(block->type), node->uid);

			for (auto child : block->children.skip(-1)) {
				append_node(code, child);
			}
			append_address(code, block->children.back());
			append_line(code, "_{} = _{};", node->uid, block->children.back()->uid);
		},
		[&](Subscript *subscript) {
			append_address(code, subscript->subscriptable);
			append_node(code, subscript->index);
			append_line(code, "{} *_{} = &_{}->data[_{}];", ctype(subscript->type), node->uid, subscript->subscriptable->uid, subscript->index->uid);
		},
	});
}

// if `define` is false, definition for resulting variable will be omitted.
// one defer body can be appended multiple times, which will create duplicate definitions.
// `define` is not propagated.
void append_node(StringBuilder &code, Node *node, bool define) {
	scoped_replace(debug_current_node, node);

	if (node->uid == 291) {
		int x = 2;
	}

	append_line(code, "/* {} */", node->location);
	if (auto expression = as<Expression>(node); expression && !types_match(expression->type, BuiltinType::None) && define) {
		append_line(code, "{} _{};", ctype(expression->type), node->uid);
		append_line(code, "memset(&_{}, 0, sizeof(_{}));", node->uid, node->uid);
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
			append_line(code, "{");
			++tabs;
			for (auto child : block->children) {
				append_node(code, child);
			}

			if (!types_match(block->type, BuiltinType::None)) {
				if (auto last_expr = as<Expression>(block->children.back()); last_expr && !types_match(last_expr->type, BuiltinType::None)) {
					append_line(code, "_{} = _{};", node->uid, last_expr->uid);
				}
			}
			
			append_line(code, "end_{}:;", block->uid);

			for (auto Defer : block->defers) {
				append_node(code, Defer->body, false);
			}
			--tabs;
			append_line(code, "}");
		},
		[&](Definition *definition) {
			if (definition->initial_value) {
				append_node(code, definition->initial_value);
				append_line(code, "_{} = _{};", node->uid, definition->initial_value->uid);
			} else {
				append_line(code, "memset(&_{}, 0, sizeof(_{}));", node->uid, node->uid);
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
			append_line(code, "_{} = {};", node->uid, (u64)literal->value);
		},
		[&](StringLiteral *literal) {
			append_line(code, "_{} = __make_string(\"{}\");", node->uid, EscapedCString{literal->value});
		},
		[&](Unary *unary) {
			if (unary->operation == UnaryOperation::addr) {
				append_address(code, unary->expression);
				append_line(code, "_{} = _{};", node->uid, unary->expression->uid);
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
				append_line(code, "*_{} {} _{};", binary->left->uid, CBinaryOperation{binary->operation}, binary->right->uid);
				return;
			}

			if (binary->operation == BinaryOperation::as) {
				append_node(code, binary->left);
				append_line(code, "_{} = ({})(_{});", node->uid, ctype(binary->right), binary->left->uid);
				return;
			}

			append_node(code, binary->left);
			append_node(code, binary->right);
			append_line(code, "_{} = _{} {} _{};", node->uid, binary->left->uid, CBinaryOperation{binary->operation}, binary->right->uid);
		},
		[&](Return *ret) {
			if (ret->value) {
				append_node(code, ret->value);
			}

			for (auto Defer : ret->defers) {
				append_node(code, Defer->body, false);
			}

			if (ret->value) {
				append_line(code, "return _{};", ret->value->uid);
			} else {
				append_line(code, "return;");
			}
		},
		[&](Call *call) {
			append_line(code, "{");
			++tabs;
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
					append_format(code, ");\n");
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
						auto member = s->members[i];
						append_format(code, "._{} = _{},", member->uid, call->arguments[i].expression->uid);
					}
					append(code, "};\n");

					break;
				}
				default: {
					append_format(code, "_{} = UnknownCallKind({})", node->uid, call->call_kind);
					break;
				}
			}
			--tabs;
			append_line(code, "}");
		},
		[&](Match *match) {
			if (types_match(match->type, BuiltinType::None)) {
				// Statement match
				append_node(code, match->expression);
				append_line(code, "{");
				++tabs;
				for (auto c : match->cases) {
					if (!c.from)
						continue;
					append_node(code, c.from);
					append_line(code, "if (_{} == _{}) {{", match->expression->uid, c.from->uid);
					++tabs;
					append_node(code, c.to);
					append_line(code, "goto endmatch{};", match->uid);
					--tabs;
					append_line(code, "}");
				}
				if (match->default_case) {
					append_node(code, match->default_case);
				}
				append_line(code, "endmatch{}:;", match->uid);
				--tabs;
				append_line(code, "}");
			} else {
				// Expression match
				append_node(code, match->expression);
				append_line(code, "{");
				++tabs;
				for (auto c : match->cases) {
					if (!c.from)
						continue;
					append_node(code, c.from);
					append_line(code, "if (_{} == _{}) {{", match->expression->uid, c.from->uid);
					++tabs;
					append_node(code, c.to);
					append_line(code, "_{} = _{};", node->uid, c.to->uid);
					append_line(code, "goto endmatch{};", match->uid);
					--tabs;
					append_line(code, "}");
				}
				if (match->default_case) {
					append_node(code, match->default_case);
					append_line(code, "_{} = _{};", node->uid, match->default_case->uid);
				} else {
					append_line(code, "*(int *)0 = 0; // incomplete match");
				}
				append_line(code, "endmatch{}:;", match->uid);
				--tabs;
				append_line(code, "}");
			}
		},
		[&](While *While) {
			append_line(code, "while (true) {");
			++tabs;
			append_node(code, While->condition);
			append_line(code, "if (!_{}) break;", While->condition->uid);

			append_node(code, While->body);
			append_line(code, "continue_{}:;", While->uid);
			--tabs;
			append_line(code, "}");
			append_line(code, "break_{}:;", While->uid);
		},
		[&](Continue *Continue) {
			for (auto Defer : Continue->defers) {
				append_node(code, Defer->body, false);
			}
			append_line(code, "goto continue_{};", Continue->loop->uid);
		},
		[&](Break *Break) {
			if (Break->loop) {
				for (auto Defer : Break->defers) {
					append_node(code, Defer->body, false);
				}
				append_line(code, "goto break_{};", Break->loop->uid);
			} else {
				assert(Break->tag_block);
				append_node(code, Break->value);
				append_line(code, "{} _{} = _{};", ctype(Break->value->type), node->uid, Break->value->uid);
				append_line(code, "_{} = _{};", Break->tag_block->uid, node->uid);
				for (auto Defer : Break->defers) {
					append_node(code, Defer->body, false);
				}
				append_line(code, "goto end_{};", Break->tag_block->uid);
			}
		},
		[&](IfStatement *If) {
			append_node(code, If->condition);
			append_line(code, "if (_{}) {{", If->condition->uid);
			++tabs;
			append_node(code, If->true_branch);
			--tabs;
			if (If->false_branch) {
				append_line(code, "} else {");
				++tabs;
				append_node(code, If->false_branch);
				--tabs;
			}
			append_line(code, "}");
		},
		[&](IfExpression *If) {
			append_node(code, If->condition);
			append_line(code, "if (_{}) {{", If->condition->uid);
			++tabs;
			append_node(code, If->true_branch);
			append_line(code, "_{} = _{};", node->uid, If->true_branch->uid);
			--tabs;
			if (If->false_branch) {
				append_line(code, "} else {");
				++tabs;
				append_node(code, If->false_branch);
				append_line(code, "_{} = _{};", node->uid, If->false_branch->uid);
				--tabs;
			}
			append_line(code, "}");
		},
		[&](Subscript *subscript) {
			append_address(code, subscript->subscriptable);
			append_node(code, subscript->index);
			append_line(code, "_{} = _{}->data[_{}];", node->uid, subscript->subscriptable->uid, subscript->index->uid);
		},
		[&](ArrayConstructor *constructor) {
			for (auto e : constructor->elements) {
				append_node(code, e);
			}

			tabs();
			append_format(code, "_{} = ({}){{", node->uid, ctype(constructor->type));
			for (auto e : constructor->elements) {
				append_format(code, "_{}, ", e->uid);
			}
			append(code, "};\n");
		},
	});
}

extern "C" __declspec(dllexport)
void init(CompilerContext *c) {
	context = c;

	init_allocator();
	init_printer();
}

extern "C" __declspec(dllexport)
bool convert_ast(Block *global_block, Lambda *main_lambda, Definition *main_lambda_definition) {
	StringBuilder builder;
	StringBuilder code;
	
	append(builder, R"(
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

typedef bool Bool;
typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef uint64_t U64;
typedef int8_t S8;
typedef int16_t S16;
typedef int32_t S32;
typedef int64_t S64;

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
// Struct definitions
// TODO: order dependencies
//
)");
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Struct *s) {
			append_format(builder, "struct _{} {{\n", s->uid);
			++tabs;
			for (auto member : s->members) {
				append_line(builder, "{} _{};", ctype(member->type), member->uid);
			}
			--tabs;
			append(builder, "};\n");
		},
	});
	
	append(builder, R"(
//
// Array definitions
//
)");
	auto append_array_struct = [&] (ArrayType *array){
		if (built_array_types.find(array))
			return;

		append_format(builder, 
R"(typedef struct {{
	{} data[{}];
}} _{};
)", ctype(array->element_type), array->count.value(), array->uid);

		built_array_types.get_or_insert(array) = array->uid;
	};

	visit(global_block, Combine{
		// NOTE: `this auto &&self` is required in all overloads apparently.
		// NOTE2: Originally this was using `this auto &&self`, but msvc is buggy and for some reason passes `builder` by copy or something. addresses are different.
		//        Using outer lambda as a workaround. I tried reproducing this on godbolt, it works as expected there.
		[&](Node *node) {},
		[&](Lambda *lambda) {
			if (lambda->head.is_template)
				return ForEach_dont_recurse;
			return ForEach_continue;
		},
		[&](Expression *expression) {
			if (auto array = as<ArrayType>(expression)) {
				append_array_struct(array);
			} else if (auto array = as<ArrayType>(expression->type)) {
				append_array_struct(array);
			}
		},
		[&](ArrayType *array) {
			append_array_struct(array);
		},
	});

	append_format(builder, R"(
//
// Intrinsics declarations
//
_{} __make_string(char const *str);
void print_String(_{} x);
void print_S64(int64_t v);
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
	
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Lambda *lambda) {
			append_line(builder, "/* {} */", lambda->location);
			if (!lambda->head.is_template) {
				append_format(builder, "typedef {} (*_{})(", ctype(lambda->head.return_type), lambda->head.uid);
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
				append(builder, ") {\n");

				++tabs;
				code.clear();

				append_node(code, lambda->body);

				append     (builder, code);
				append     (builder, "retlabel:\n");
				if (types_match(lambda->body->type, BuiltinType::None)) {
					append_line(builder, "return;");
				} else {
					append_line(builder, "return _{};", lambda->body->uid);
				}
				append     (builder, "}\n");
				--tabs;
			}
		},
	});

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
	append_format(builder, R"(
    return {}();
}}
)", CName{main_lambda});

	
	append_format(builder, R"(
//
// Intrinsics implementation
//

bool __stdcall WriteFile(void *hFile, void const *lpBuffer, unsigned nNumberOfBytesToWrite, unsigned *lpNumberOfBytesWritten, void *lpOverlapped);

_{} __make_string(char const *str) {{
	return (_{}){{str, strlen(str)}};
}}

// stdio replaces \n with \r\n ...
// thats why winapi
void print_String(_{} x) {{
	char *data = (char *)(((void **)&x)[0]);
	unsigned count = (unsigned)(((void **)&x)[1]);
	WriteFile(GetStdHandle(-11), data, count, 0, 0);
}}
void print_S64(int64_t v) {{
	int radix = 10;
	char buf[64];
	char const *digits = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	char *c = buf + sizeof(buf);
	if (v == 0) {{
		WriteFile(GetStdHandle(-11), "0", 1, 0, 0);
		return;
	}}
	bool neg = v < 0;
	v = neg ? -v : v;

	do {{
		*--c = digits[v % radix];
	}} while (v /= radix);

	if (neg) {{
		*--c = '-';
	}}
	WriteFile(GetStdHandle(-11), c, buf + sizeof(buf) - c, 0, 0);

}}

)", context->builtin_structs.String->uid, context->builtin_structs.String->uid, context->builtin_structs.String->uid);
	
	auto path_base = parse_path(context_base->input_source_path).path_without_extension();

	write_entire_file(tformat(u8"{}.c", path_base), to_string(builder));

	auto cmd = tformat(u8"cl {}.c /Zi /FS /nologo /link /out:{}.exe", path_base, path_base);
	standard_error_printer.writeln(cmd);

	auto ret = start_process(cmd, [](auto x) {standard_error_printer.write(x); });
	
	if (!ret) {
		standard_error_printer.writeln(u8"Could not start `cl` process. Make sure it is in your PATH."s);
		return false;
	}

	if (ret.value()) {
		standard_error_printer.writeln(u8"C compiler failed."s);
		return false;
	}

	if (!context_base->keep_build_artifacts) {
		delete_file(tformat(u8"{}.c", path_base));
		delete_file(tformat(u8"{}.ilk", path_base));
	}

	return true;
}

extern "C" __declspec(dllexport)
u32 run() {
	auto code = start_process(tformat(u8"{}.exe", parse_path(context->input_source_path).path_without_extension()), [](auto s){standard_error_printer.write(s);});
	return code.value_or(0);
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
