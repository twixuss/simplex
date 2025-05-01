#define TL_IMPL
#include "../../src/common.h"
#include "../../src/nodes.h"
#include "../../src/x.h"
#include "../../src/reporter.h"
#include "../../src/visit.h"
#include "../../src/type.h"

CompilerContext *context;

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

void append(StringBuilder &builder, CType ctype) {
	auto t = ctype.type;
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
					append(builder, CType{Type{t->expression}});
					append(builder, '*');
					break;
				}
				default: append(builder, "UnknownType");
			}
		},
		[&](Name *t) {
			append(builder, CType{Type{t->definition()->initial_value}});
		},
		[&](Struct *t) {
			append_name(builder, t);
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
void convert_ast(Block *global_block, Lambda *main_lambda, Definition *main_lambda_definition) {
	StringBuilder builder;
	
	append(builder, R"(
#include <stdint.h>
#include <stdbool.h>

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
				append(builder, CType{Type{lambda->head.return_type}});
				append(builder, ' ');
				append_name(builder, lambda);
				append(builder, '(');
				for (umm i = 0; i < lambda->head.parameters_block.definition_list.count; ++i) {
					auto param = lambda->head.parameters_block.definition_list[i];

					if (i) {
						append(builder, ", ");
					}

					append(builder, CType{Type{param->type}});
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
				append(builder, CType{Type{lambda->head.return_type}});
				append(builder, ' ');
				append_name(builder, lambda);
				append(builder, '(');
				for (umm i = 0; i < lambda->head.parameters_block.definition_list.count; ++i) {
					auto param = lambda->head.parameters_block.definition_list[i];

					if (i) {
						append(builder, ", ");
					}

					append(builder, CType{Type{param->type}});
					append(builder, ' ');
					append(builder, CName{param->name});
				}
				append(builder, ") {\n");
				append(builder, "    // body\n");
				append(builder, "}\n");
			}
		},
	});

	println("Resulting C code:\n");
	println(builder);
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
