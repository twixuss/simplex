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

	if (lambda.value->definition) {
		append(builder, CName{lambda.value->definition->name});
	}
	if (lambda.value->body) {
		append(builder, '_');
		append(builder, lambda.value->uid);
	}
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
		[&](auto *node) { append_format(builder, "{}_{}", CName{t.value->name}, node->uid); },
	});
}

struct CType {
	Type type;
};

CType ctype(Type type) {
	return {type};
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
			append(builder, CName{t});
		},
		[&](LambdaHead *head) {
			append_format(builder, "F_{}", head->uid);
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

using ValueId = u32;

ValueId value_id_counter = 0;
GHashMap<Definition *, ValueId> definition_ids;

struct Tabs { 
	u32 value;
	
	void operator()(StringBuilder &builder) {
		append(builder, Repeat{"    ", value});
	}
	void operator()() {
		standard_error_printer.write(Repeat{"    ", value});
	}
} tabs;

template <>
struct tl::Scoped<Tabs> {
	void enter(Tabs) {
		++tabs.value;
	}
	void exit() {
		--tabs.value;
	}
};

void append_line(StringBuilder &builder, auto const &...args) {
	tabs(builder);
	if constexpr (sizeof...(args) == 1) {
		append(builder, args...);
	} else {
		append_format(builder, args...);
	}
	append(builder, '\n');
}

ValueId append_node(StringBuilder &code, Node *node);

ValueId append_address(StringBuilder &code, Node *node) {
	ValueId id = value_id_counter++;

	scoped_replace(debug_current_node, node);

	visit_one(node, Combine{
		[&](auto *node) {
			append_line(code, "_{} = &(unknown_node {});", id, node->kind);
		},
		[&](NoneLiteral *literal) {
			append_line(code, "void* _{} = 0;", id);
		},
		//[&](StringLiteral *literal) {
		//	append_line(code, "address_of_string_literal");
		//},
		[&](Name *name) {
			auto found_def = definition_ids.find(name->definition()).value;
			if (found_def) {
				append_line(code, "{} *_{} = &_{};", ctype(name->type), id, *found_def);
			} else {
				append_line(code, "{} *_{} = &{};", ctype(name->type), id, name->name);
			}
		},
		[&](Binary *binary) {
			if (binary->operation == BinaryOperation::dot) {
				auto left_id = append_address(code, binary->left);
				append_line(code, "{} *_{} = &_{}->{};", ctype(binary->type), id, left_id, as<Name>(binary->right)->name);
				return;
			}
			append_line(code, "_{} = &(unknown_binary {});", id, binary->operation);
		},
		[&](Unary *unary) {
			if (unary->operation == UnaryOperation::dereference) {
				auto left_id = append_node(code, unary->expression);
				append_line(code, "{} *_{} = _{};", ctype(unary->type), id, left_id);
				return;
			}
			append_line(code, "{} *_{} = &(unknown_unary {});", ctype(unary->type), id, unary->operation);
		},
		[&](Block *block) {
			for (auto child : block->children.skip(-1)) {
				append_node(code, child);
			}
			id = append_address(code, block->children.back());
		},
	});

	return id;
}

ValueId append_node(StringBuilder &code, Node *node) {
	ValueId id = value_id_counter++;

	scoped_replace(debug_current_node, node);

	visit_one(node, Combine{
		[&](Node *node) {
			append_line(code, "_{} = (unknown_node {});", id, node->kind);
		},
		[&](Expression *expression) {
			append_line(code, "{} _{} = (unknown_expression {});", ctype(expression->type), id, expression->kind);
		},
		[&](Lambda *lambda) {
			append_line(code, "{} _{} = {};", ctype(&lambda->head), id, CName{lambda});
		},
		[&](Block *block) {
			ValueId last_id;
			for (auto child : block->children) {
				last_id = append_node(code, child);
			}

			if (!types_match(block->type, BuiltinType::None)) {
				// Could return last_id and save a copy.
				append_line(code, "{} _{} = _{};", ctype(block->type), id, last_id);
				return;
			}

			// no value
			id = -1;
		},
		[&](Definition *definition) {
			definition_ids.insert(definition, id);
			if (definition->initial_value) {
				auto init_id = append_node(code, definition->initial_value);
				append_line(code, "{} _{} = _{};", ctype(definition->type), id, init_id);
			} else {
				append_line(code, "{} _{};", ctype(definition->type), id);
				append_line(code, "memset(&_{}, 0, sizeof(_{}));", id, id);
			}
		},
		[&](Name *name) {
			auto found_def = definition_ids.find(name->definition()).value;
			if (found_def) {
				append_line(code, "{} _{} = _{};", ctype(name->type), id, *found_def);
			} else {
				append_line(code, "{} _{} = {};", ctype(name->type), id, CName{name});
			}
		},
		[&](BuiltinTypeName *name) {
			append_format(code, "BuiltinTypeName({})", name->type_kind);
		},
		[&](NoneLiteral *literal) {
			append_line(code, "int64_t _{} = 0;", id);
		},
		[&](BooleanLiteral *literal) {
			append_line(code, "{} _{} = {};", ctype(literal->type), id, literal->value);
		},
		[&](IntegerLiteral *literal) {
			append_line(code, "{} _{} = {};", ctype(literal->type), id, (u64)literal->value);
		},
		[&](StringLiteral *literal) {
			tabs(code);
			append_format(code, "{} _{} = __make_string(\"", ctype(literal->type), id);
			escape_c_string(literal->value, [&](String s) { append(code, s); });
			append(code, "\");\n");
		},
		[&](Unary *unary) {
			if (unary->operation == UnaryOperation::addr) {
				auto expr_id = append_address(code, unary->expression);
				append_line(code, "{} _{} = _{};", ctype(unary->type), id, expr_id);
				return;
			}
			auto expr_id = append_node(code, unary->expression);
			append_line(code, "{} _{} = {}_{};", ctype(unary->type), id, CUnaryOperation{unary->operation}, expr_id);
		},
		[&](Binary *binary) {
			if (binary->operation == BinaryOperation::dot) {
				auto left_id = append_node(code, binary->left);

				auto dot_or_arrow = as_pointer(binary->left->type) ? u8"->"s : u8"."s;

				append_line(code, "{} _{} = _{}{}{};", ctype(binary->type), id, left_id, dot_or_arrow, as<Name>(binary->right)->name);
				return;
			}

			if (is_ass(binary->operation)) {
				auto left_id = append_address(code, binary->left);
				auto right_id = append_node(code, binary->right);
				append_line(code, "*_{} {} _{};", left_id, CBinaryOperation{binary->operation}, right_id);
				return;
			}

			if (binary->operation == BinaryOperation::as) {
				auto left_id = append_node(code, binary->left);
				append_line(code, "{} _{} = ({})(_{});", ctype(binary->type), id, ctype(binary->right), left_id);
				return;
			}

			auto left_id = append_node(code, binary->left);
			auto right_id = append_node(code, binary->right);
			append_line(code, "{} _{} = _{} {} _{};", ctype(binary->type), id, left_id, CBinaryOperation{binary->operation}, right_id);
		},
		[&](Return *ret) {
			auto value_id = append_node(code, ret->value);
			append_line(code, "// TODO: defers");
			append_line(code, "return _{};", value_id);
		},
		[&](Call *call) {
			auto callable_id = append_node(code, call->callable);
			
			static GList<ValueId> arg_ids;

			arg_ids.clear();
			for (auto arg : call->arguments) {
				auto arg_id = append_node(code, arg.expression);
				arg_ids.add(arg_id);
			}
			
			tabs(code);
			if (!types_match(call->type, BuiltinType::None)) {
				append_format(code, "{} _{} = ", ctype(call->type), id);
			}
			append_format(code, "_{}(", callable_id);
			for (auto &arg_id : arg_ids) {
				if (&arg_id != &arg_ids[0]) {
					append(code, ", ");
				}
				append_format(code, "_{}", arg_id);
			}
			append_format(code, ");\n");
		},
		[&](Match *match) {
			if (types_match(match->type, BuiltinType::None)) {
				// Statement match
				auto expr_id = append_node(code, match->expression);
				append_line(code, "{");
				withs(tabs) {
					for (auto c : match->cases) {
						auto case_id = append_node(code, c.from);
						append_line(code, "if (_{} == _{}) {{", expr_id, case_id);
						withs(tabs) {
							auto value_id = append_node(code, c.to);
							append_line(code, "goto endmatch{};", match->uid);
						};
						append_line(code, "}");
					}
					if (match->default_case) {
						append_node(code, match->default_case);
					}
					append_line(code, "endmatch{}:;", match->uid);
				};
				append_line(code, "}");
			} else {
				// Expression match
				auto expr_id = append_node(code, match->expression);
				append_line(code, "{} _{};", ctype(match->type), id);
				append_line(code, "{");
				withs(tabs) {
					for (auto c : match->cases) {
						auto case_id = append_node(code, c.from);
						append_line(code, "if (_{} == _{}) {{", expr_id, case_id);
						withs(tabs) {
							auto value_id = append_node(code, c.to);
							append_line(code, "_{} = _{};", id, value_id);
							append_line(code, "goto endmatch{};", match->uid);
						};
						append_line(code, "}");
					}
					if (match->default_case) {
						auto value_id = append_node(code, match->default_case);
						append_line(code, "_{} = _{};", id, value_id);
					} else {
						append_line(code, "*(int *)0 = 0; // incomplete match");
					}
					append_line(code, "endmatch{}:;", match->uid);
				};
				append_line(code, "}");
			}
		},
		[&](While *While) {
			append_line(code, "while (true) {");
			withs(tabs) {
				auto cond_id = append_node(code, While->condition);
				append_line(code, "if (!_{}) break;", cond_id);

				append_node(code, While->body);
			};
			append_line(code, "}");
		},
		[&](IfStatement *If) {
			auto cond_id = append_node(code, If->condition);
			append_line(code, "if (_{}) {{", cond_id);
			withs(tabs) {
				append_node(code, If->true_branch);
			};
			if (If->false_branch) {
				append_line(code, "} else {");
				withs(tabs) {
					append_node(code, If->false_branch);
				};
			}
			append_line(code, "}");
		},
		[&](IfExpression *If) {
			auto cond_id = append_node(code, If->condition);
			append_line(code, "{} _{};", ctype(If->type), id);
			append_line(code, "if (_{}) {{", cond_id);
			withs(tabs) {
				auto branch_id = append_node(code, If->true_branch);
				append_line(code, "_{} = _{};", id, branch_id);
			};
			if (If->false_branch) {
				append_line(code, "} else {");
				withs(tabs) {
					auto branch_id = append_node(code, If->false_branch);
					append_line(code, "_{} = _{};", id, branch_id);
				};
			}
			append_line(code, "}");
		},
	});

	return id;
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
#include <stdint.h>
#include <stdbool.h>

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
			append_format(builder, "typedef struct {} {};\n", CName{s}, CName{s});
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
			append_format(builder, "struct {} {{\n", CName{s});
			withs(tabs) {
				for (auto member : s->members) {
					append_line(builder, "{} {};", ctype(member->type), member->name);
				}
			};
			append(builder, "};\n");
		},
	});
	
	append_format(builder, R"(
{} __make_string(char const *str) {{
	return ({}){{str, strlen(str)}};
}}

void print_S64(int64_t x) {{ printf("%d", x); }}
void print_String({} x) {{ printf("%.*s", x.count, x.data); }}

)", CName{context->builtin_structs.String}, CName{context->builtin_structs.String}, CName{context->builtin_structs.String});

	append(builder, R"(
//
// Function prototypes/declarations
//
)");
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Lambda *lambda) {
			if (!lambda->head.is_template) {
				if (!lambda->body) {
					append(builder, "extern ");
				}
				append_format(builder, "{} {}(", ctype(lambda->head.return_type), CName{lambda});
				for (umm i = 0; i < lambda->head.parameters_block.definition_list.count; ++i) {
					auto param = lambda->head.parameters_block.definition_list[i];

					if (i) {
						append(builder, ", ");
					}

					append(builder, ctype(param->type));
					append(builder, ' ');
					append(builder, CName{param->name});
				}
				append(builder, ");\n");
			}
		},
	});
	
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Lambda *lambda) {
			if (!lambda->head.is_template) {
				append_format(builder, "typedef {} (*F_{})(", ctype(lambda->head.return_type), lambda->head.uid);
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
			if (lambda->body && !lambda->head.is_template) {
				append_format(builder, "{} {}(", ctype(lambda->head.return_type), CName{lambda});
				for (umm i = 0; i < lambda->head.parameters_block.definition_list.count; ++i) {
					auto param = lambda->head.parameters_block.definition_list[i];

					if (i) {
						append(builder, ", ");
					}

					append_format(builder, "{} {}_{}", ctype(param->type), CName{param->name}, param->uid);
				}
				append(builder, ") {\n");

				withs(tabs) {
					code.clear();
					definition_ids.clear();
					value_id_counter = 0;

					auto id = append_node(code, lambda->body);

					append     (builder, code);
					append_line(builder, "// TODO: defers");
					append     (builder, "retlabel:\n");
					if (id == -1) {
						append_line(builder, "return;");
					} else {
						append_line(builder, "return _{};", id);
					}
					append     (builder, "}\n");
				};
			}
		},
	});

	append_format(builder, R"(
int main() {{
	return {}();
}}
)", CName{main_lambda});

	auto path_base = parse_path(context_base->input_source_path).path_without_extension();

	write_entire_file(tformat(u8"{}.c", path_base), to_string(builder));

	auto cmd = tformat(u8"cl {}.c /Zi /FS /link /out:{}.exe", path_base, path_base);
	standard_error_printer.writeln(cmd);

	auto ret = start_process(cmd, [](auto x) {standard_error_printer.write(x); });
	
	if (!ret) {
		standard_error_printer.writeln(u8"Could not start `cl` process. Make sure it is in your PATH."s);
		return false;
	}

	if (ret.value()) {
		standard_error_printer.writeln(u8"C compiler failed. Resulting C code:"s);
		standard_error_printer.write(builder);
		return false;
	}

	if (!context_base->keep_build_artifacts) {
		delete_file(tformat(u8"{}.c", path_base));
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
