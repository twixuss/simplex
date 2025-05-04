#define TL_IMPL
#include "../../src/common.h"
#include "../../src/nodes.h"
#include "../../src/x.h"
#include "../../src/reporter.h"
#include "../../src/visit.h"
#include "../../src/type.h"
#include "../../src/compiler_context.h"

CompilerContext *context;

Node *debug_current_node;

struct CName {
	String string;
};

void append(StringBuilder &builder, CName name) {
	for (auto c : name.string) {
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

void append_name(StringBuilder &builder, Lambda *lambda) {
	if (lambda->definition) {
		append(builder, CName{lambda->definition->name});
	} else {
		append(builder, "__lambda_");
		append(builder, lambda->uid);
	}
}
void append_name(StringBuilder &builder, Struct *t) {
	if (t->definition) {
		append(builder, CName{t->definition->name});
	} else {
		append(builder, "__struct_");
		append(builder, t->uid);
	}
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
			append(builder, "UnknownType");
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
				default: append(builder, "UnknownType");
			}
		},
		[&](Unary *t) {
			switch (t->operation) {
				case UnaryOperation::pointer: {
					append(builder, ctype(t->expression));
					append(builder, '*');
					break;
				}
				default: append(builder, "UnknownType");
			}
		},
		[&](Name *t) {
			append(builder, ctype(t->definition()->initial_value));
		},
		[&](Struct *t) {
			append_name(builder, t);
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
		print(Repeat{"    ", value});
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
				append_line(code, "{} *_{} = &_{}.{};", ctype(binary->type), id, left_id, as<Name>(binary->right)->name);
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
		}
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
		[&](Block *block) {
			ValueId last_id;
			for (auto child : block->children) {
				last_id = append_node(code, child);
			}

			if (block->children.count) {
				if (auto expression = as<Expression>(block->children.back())) {
					if (!types_match(block->type, BuiltinType::None)) {
						// Could return last_id and save a copy.
						append_line(code, "{} _{} = _{};", ctype(block->type), id, last_id);
						return;
					}
				}
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
			}
		},
		[&](Name *name) {
			auto found_def = definition_ids.find(name->definition()).value;
			if (found_def) {
				append_line(code, "{} _{} = _{};", ctype(name->type), id, *found_def);
			} else {
				append_line(code, "{} _{} = {};", ctype(name->type), id, name->name);
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
				append_line(code, "{} _{} = _{}.{};", ctype(binary->type), id, left_id, as<Name>(binary->right)->name);
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
			static GList<ValueId> arg_ids;

			arg_ids.clear();
			for (auto arg : call->arguments) {
				auto arg_id = append_node(code, arg.expression);
				arg_ids.add(arg_id);
			}
			
			tabs(code);
			append_format(code, "{} _{} = {}(", ctype(call->type), id, as<Name>(call->callable)->name);
			for (auto &arg_id : arg_ids) {
				if (&arg_id != &arg_ids[0]) {
					append(code, ", ");
				}
				append_format(code, "_{}", arg_id);
			}
			append_format(code, ");\n");
		},
		[&](Match *match) {
			if (types_match(match, BuiltinType::None)) {
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
					append_node(code, match->default_case);
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
void convert_ast(Block *global_block, Lambda *main_lambda, Definition *main_lambda_definition) {
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
	
	append(builder, R"(
//
// Struct declarations
//
)");
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Struct *s) {
			append(builder, "typedef struct ");
			append_name(builder, s);
			append(builder, " ");
			append_name(builder, s);
			append(builder, ";\n");
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
			append(builder, "struct ");
			append_name(builder, s);
			append(builder, " {\n");
			withs(tabs) {
				for (auto member : s->members) {
					append_line(builder, "{} {};", ctype(member->type), member->name);
				}
			};
			append(builder, "};\n");
		},
	});
	
	append(builder, R"(
String __make_string(char const *str) {
	return (String){str, strlen(str)};
}
)");

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
				append(builder, ctype(lambda->head.return_type));
				append(builder, ' ');
				append_name(builder, lambda);
				append(builder, '(');
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
	
	append(builder, R"(
//
// Function implementations/definitions
//
)");
	visit(global_block, Combine {
		[&] (auto) {},
		[&] (Lambda *lambda) {
			if (lambda->body && !lambda->head.is_template) {
				append(builder, ctype(lambda->head.return_type));
				append(builder, ' ');
				append_name(builder, lambda);
				append(builder, '(');
				for (umm i = 0; i < lambda->head.parameters_block.definition_list.count; ++i) {
					auto param = lambda->head.parameters_block.definition_list[i];

					if (i) {
						append(builder, ", ");
					}

					append(builder, ctype(param->type));
					append(builder, ' ');
					append(builder, CName{param->name});
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

	println("Resulting C code:\n");
	println(builder);
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
