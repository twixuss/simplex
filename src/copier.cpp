#include "copier.h"
#include "nodes.h"

#define COPY(x) to->x = from->x
#define DEEP_COPY(x) to->x = deep_copy(from->x)
#define DEEP_COPY_INPLACE(x) deep_copy(&from->x, &to->x)
#define LOOKUP_COPY(x)                               \
	if (auto found = copied_nodes.find(from->x)) {   \
		assert((*found.value)->kind == from->x->kind);  \
		to->x = autocast *found.value;               \
	} else {                                         \
		to->x = from->x;                             \
	}
#define COPY_LIST(x, COPY_MODE)                     \
	to->x.resize(from->x.count);                    \
	for (umm __i = 0; __i < from->x.count; ++__i) { \
		COPY_MODE(x[__i]);                          \
	}
	
Node *Copier::deep_copy(Node *from) {
	switch (from->kind) {
		#define x(name) case NodeKind::name: return deep_copy((name *)from);
		ENUMERATE_NODE_KIND(x)
		#undef x
	}
	invalid_code_path();
}
Expression *Copier::deep_copy(Expression *from) {
	switch (from->kind) {
		#define x(name) case NodeKind::name: return deep_copy((name *)from);
		ENUMERATE_EXPRESSION_KIND(x)
		#undef x
	}
	invalid_code_path();
}
Statement *Copier::deep_copy(Statement *from) {
	switch (from->kind) {
		#define x(name) case NodeKind::name: return deep_copy((name *)from);
		ENUMERATE_STATEMENT_KIND(x)
		#undef x
	}
	invalid_code_path();
}
Call::Argument Copier::deep_copy(Call::Argument from) {
	Call::Argument to = from;
	deep_copy_impl(&from, &to);
	return to;
}

void Copier::deep_copy_impl(Block *from, Block *to) {
	LOOKUP_COPY(parent);
	LOOKUP_COPY(container);
	
	// NOTE: Block::add populates all necessary lists. COPY_LIST is not sufficient here.
	for (auto from_child : from->children) {
		auto to_child = deep_copy(from_child);
		to->add(to_child);
	}

	COPY_LIST(defers, LOOKUP_COPY);
	LOOKUP_COPY(type);
} 
void Copier::deep_copy_impl(Call *from, Call *to) {
	DEEP_COPY(callable);
	COPY_LIST(arguments, DEEP_COPY);
	COPY(inline_status);
	COPY(call_kind);
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(Call::Argument *from, Call::Argument *to) {
	DEEP_COPY(expression);
	LOOKUP_COPY(parameter);
	to->name = from->name;
}
void Copier::deep_copy_impl(Definition *from, Definition *to) {
	COPY(name);
	if (from->parsed_type)
		DEEP_COPY(parsed_type);
	if (from->initial_value)
		DEEP_COPY(initial_value);

	COPY(is_parameter);
	COPY(is_template_parameter);
	COPY(use);
	COPY(mutability);

	LOOKUP_COPY(container);
	LOOKUP_COPY(type);
} 
void Copier::deep_copy_impl(IntegerLiteral *from, IntegerLiteral *to) {
	COPY(value);
	COPY(type); // builtin, no need to look up
} 
void Copier::deep_copy_impl(BooleanLiteral *from, BooleanLiteral *to) {
	COPY(value);
	COPY(type); // builtin, no need to look up
} 
void Copier::deep_copy_impl(NoneLiteral *from, NoneLiteral *to) {
	COPY(type); // builtin, no need to look up
} 
void Copier::deep_copy_impl(StringLiteral *from, StringLiteral *to) {
	COPY(value);
	COPY(type); // builtin, no need to look up
} 
void Copier::deep_copy_impl(Lambda *from, Lambda *to) {
	COPY(inline_status);
	COPY(is_intrinsic);
	DEEP_COPY_INPLACE(head);
	DEEP_COPY(body);
	LOOKUP_COPY(definition);

	// returns will be updated by deep_copy_impl(Return)
		
	LOOKUP_COPY(type);
} 
void Copier::deep_copy_impl(LambdaHead *from, LambdaHead *to) {
	DEEP_COPY_INPLACE(template_parameters_block);
	DEEP_COPY_INPLACE(parameters_block);
	if (from->parsed_return_type) {
		DEEP_COPY(parsed_return_type);
	}
	LOOKUP_COPY(return_type);
	COPY(is_template);
	COPY(type); // builtin, no need to look up
} 
void Copier::deep_copy_impl(Name *from, Name *to) {
	COPY(name);
	COPY_LIST(possible_definitions, LOOKUP_COPY);
	LOOKUP_COPY(type);
} 
void Copier::deep_copy_impl(IfStatement *from, IfStatement *to) {
	DEEP_COPY(condition);
	DEEP_COPY(true_branch);
	if (from->false_branch)
		DEEP_COPY(false_branch);
} 
void Copier::deep_copy_impl(IfExpression *from, IfExpression *to) {
	DEEP_COPY(condition);
	DEEP_COPY(true_branch);
	DEEP_COPY(false_branch);
	LOOKUP_COPY(type);
} 
void Copier::deep_copy_impl(BuiltinTypeName *from, BuiltinTypeName *to) {
	COPY(type_kind);
	COPY(type); // builtin, no need to look up
} 
void Copier::deep_copy_impl(Binary *from, Binary *to) {
	DEEP_COPY(left);
	DEEP_COPY(right);
	COPY(operation);
	LOOKUP_COPY(type);
} 
void Copier::deep_copy_impl(Match *from, Match *to) {
	DEEP_COPY(expression);
	to->cases.resize(from->cases.count);
	for (umm i = 0; i < to->cases.count; ++i) {
		COPY_LIST(cases[i].froms, DEEP_COPY);
		DEEP_COPY(cases[i].to);
	}
	LOOKUP_COPY(type);
} 
void Copier::deep_copy_impl(Unary *from, Unary *to) {
	DEEP_COPY(expression);
	COPY(operation);
	COPY(mutability);
	LOOKUP_COPY(type);
} 
void Copier::deep_copy_impl(Return *from, Return *to) {
	if (from->value) {
		DEEP_COPY(value);
	}
	LOOKUP_COPY(lambda);
	COPY_LIST(defers, LOOKUP_COPY);
	to->lambda->returns.add(to);
}
void Copier::deep_copy_impl(While *from, While *to) {
	DEEP_COPY(condition);
	DEEP_COPY(body);
} 
void Copier::deep_copy_impl(Continue *from, Continue *to) {
	LOOKUP_COPY(loop);
	COPY_LIST(defers, LOOKUP_COPY);
} 
void Copier::deep_copy_impl(Break *from, Break *to) {
	LOOKUP_COPY(tag_block);
	LOOKUP_COPY(loop);
	if (from->value) {
		DEEP_COPY(value);
	}
	COPY_LIST(defers, LOOKUP_COPY);
}
void Copier::deep_copy_impl(Struct *from, Struct *to) {
	DEEP_COPY_INPLACE(template_parameters_block);
	LOOKUP_COPY(definition);
	COPY_LIST(member_list, DEEP_COPY);
	for (auto &member : to->member_list) {
		to->member_map.insert(member->name, member);
	}
	COPY(size);
	COPY(must_be_fully_initialized);
	COPY(is_template);
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(Enum *from, Enum *to) {
	DEEP_COPY_INPLACE(block);
	LOOKUP_COPY(definition);
	if (from->parsed_underlying_type)
		DEEP_COPY(parsed_underlying_type);
	LOOKUP_COPY(underlying_type);
	COPY(allow_from_int);
	COPY(allow_to_int);
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(ArrayType *from, ArrayType *to) {
	if (from->parsed_element_type) {
		DEEP_COPY(parsed_element_type);
	}
	LOOKUP_COPY(element_type);
	DEEP_COPY(count_expression);
	COPY(count);
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(Subscript *from, Subscript *to) {
	DEEP_COPY(subscriptable);
	DEEP_COPY(index);
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(ArrayConstructor *from, ArrayConstructor *to) {
	COPY_LIST(elements, DEEP_COPY);
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(Import *from, Import *to) {
	COPY(path);
}
void Copier::deep_copy_impl(Defer *from, Defer *to) {
	DEEP_COPY(body);
}
void Copier::deep_copy_impl(ZeroInitialized *from, ZeroInitialized *to) {
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(CallerLocation *from, CallerLocation *to) {
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(CallerArgumentString *from, CallerArgumentString *to) {
	COPY(parameter_name);
	LOOKUP_COPY(type);
}
void Copier::deep_copy_impl(For *from, For *to) {
	COPY(it_name);
	DEEP_COPY(range);
	DEEP_COPY(body);
}

void Copier::deep_copy_impl(Use *from, Use *to) {
	DEEP_COPY_INPLACE(name);
}

#undef LOOKUP_COPY
#undef DEEP_COPY
#undef COPY
