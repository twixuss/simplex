#include "type.h"
#include "nodes.h"
#include "builtin_structs.h"

umm append(StringBuilder &builder, BuiltinType type_kind) {
	switch (type_kind) {
		#define x(name) case BuiltinType::name: return append(builder, #name);
		#define y(name, value) x(name)
		ENUMERATE_BUILTIN_TYPES(x)
		#undef y
		#undef x
	}
	return append_format(builder, "(unknown BuiltinType {})", (u32)type_kind);
}

BuiltinType to_builtin_type_kind(TokenKind kind) {
	switch (kind) {
#define x(name) case Token_##name: return BuiltinType::name;
		ENUMERATE_CONCRETE_BUILTIN_TYPES(x);
#undef x
	}

	invalid_code_path();
	return {};
}

umm append(StringBuilder &builder, Type type) {
	return append(builder, (Node *)type);
}

BuiltinTypeName builtin_types[(u32)BuiltinType::count];

BuiltinTypeName *get_builtin_type(BuiltinType kind) {
	return &builtin_types[(u32)kind];
}

Expression *direct(Expression *node) {
	while (true) {
		if (auto name = as<Name>(node)) {
			if (name->possible_definitions.count == 1) {
				auto definition = name->definition();
				assert(definition);
				if (definition->mutability == Mutability::constant) {
					node = definition->initial_value;
					continue;
				} else {
					return definition;
				}
			} else {
				return name;
			}
		}
		break;
	}
	if (auto bt = as<BuiltinTypeName>(node)) {
		return get_builtin_type(bt->type_kind);
	}
	return node;
}

#define TYPES_MUST_MATCH(a, b)          \
	if (auto _ = types_match(a, b); !_) \
		return _

CheckResult2 types_match(Type a, Type b) {
	assert(a);
	assert(b);
	a = direct(a);
	b = direct(b);
	assert(a);
	assert(b);

	if (a->kind == b->kind) {
		switch (a->kind) {
			case NodeKind::BuiltinTypeName: {
				REDECLARE_VAL(a, (BuiltinTypeName *)a);
				REDECLARE_VAL(b, (BuiltinTypeName *)b);

				if (a->type_kind == b->type_kind) {
					return true;
				}
				break;
			}
			case NodeKind::LambdaHead: {
				REDECLARE_VAL(a, (LambdaHead *)a);
				REDECLARE_VAL(b, (LambdaHead *)b);

				if (a->parameters_block.definition_list.count == b->parameters_block.definition_list.count) {
					TYPES_MUST_MATCH(a->return_type, b->return_type);

					for (umm i = 0; i < a->parameters_block.definition_list.count; ++i) {
						TYPES_MUST_MATCH(a->parameters_block.definition_list[i]->type, b->parameters_block.definition_list[i]->type);
					}
					return true;
				}
				break;
			}
			case NodeKind::Unary: {
				REDECLARE_VAL(a, (Unary *)a);
				REDECLARE_VAL(b, (Unary *)b);

				if (a->operation == b->operation) {
					assert(a->operation == UnaryOperation::pointer);

					TYPES_MUST_MATCH(a->expression, b->expression);

					if (a->mutability == b->mutability) {
						return true;
					}
				}
				break;
			}
			case NodeKind::Struct: {
				REDECLARE_VAL(a, (Struct *)a);
				REDECLARE_VAL(b, (Struct *)b);

				if (a == b) {
					return true;
				}
				break;
			}
			case NodeKind::ArrayType: {
				REDECLARE_VAL(a, (ArrayType *)a);
				REDECLARE_VAL(b, (ArrayType *)b);

				TYPES_MUST_MATCH(a->element_type, b->element_type);
				assert(a->count);
				assert(b->count);

				if (a->count.value() == b->count.value()) {
					return true;
				}

				break;
			}
			default:
				invalid_code_path("invalid node kind {} in types_match", a->kind);
		}
	}
	return {false, a, b};
}

CheckResult2 types_match(Expression *a, BuiltinType b) {
	assert(a);
	a = direct(a);
	assert(a);

	if (auto ab = as<BuiltinTypeName>(a)) {
		if (ab->type_kind == b) {
			return true;
		}
	}

	return {false, a, get_builtin_type(b)};
}
CheckResult2 types_match(BuiltinType a, Expression *b) {
	return types_match(b, a);
}

bool is_type(Expression *expression) {
	return types_match(expression->type, BuiltinType::Type);
}

bool is_concrete_integer(Type type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::U8:
			case BuiltinType::U16:
			case BuiltinType::U32:
			case BuiltinType::U64:
			case BuiltinType::S8:
			case BuiltinType::S16:
			case BuiltinType::S32:
			case BuiltinType::S64:
				return true;
		}
	}

	return false;
}
bool is_signed_integer(Type type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::S8:
			case BuiltinType::S16:
			case BuiltinType::S32:
			case BuiltinType::S64:
				return true;
		}
	}

	return false;
}
bool is_unsigned_integer(Type type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::U8:
			case BuiltinType::U16:
			case BuiltinType::U32:
			case BuiltinType::U64:
				return true;
		}
	}

	return false;
}

bool is_concrete(Type type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::UnsizedInteger:
				return false;
		}
	}

	return true;
}

void propagate_concrete_type(Expression *expression, Type type) {
	switch (expression->kind) {
		case NodeKind::IntegerLiteral: 
		case NodeKind::Name:
			expression->type = type;
			break;
		case NodeKind::Unary: {
			auto unary = (Unary *)expression;
			unary->type = type;
			propagate_concrete_type(unary->expression, type);
			break;
		}
		case NodeKind::IfExpression: {
			auto If = (::IfExpression *)expression;
			If->type = type;
			propagate_concrete_type(If->true_branch, type);
			propagate_concrete_type(If->false_branch, type);
			break;
		}
		case NodeKind::Block: {
			auto block = (Block *)expression;
			if (block->children.count) {
				if (auto last_expr = as<Expression>(block->children.back())) {
					block->type = type;
					propagate_concrete_type(last_expr, type);
				}
			}
			break;
		}
		default: invalid_code_path(); break;
	}
}

void make_concrete(Expression *expression) {
	auto type = direct(expression->type);

	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::UnsizedInteger: {
				propagate_concrete_type(expression, get_builtin_type(BuiltinType::S64));
				return;
			}
		}
	}
}

u64 get_size(BuiltinType type_kind) {
	switch (type_kind) {
		case BuiltinType::None:   return 0;
		case BuiltinType::U8:     return 1;
		case BuiltinType::U16:    return 2;
		case BuiltinType::U32:    return 4;
		case BuiltinType::U64:    return 8;
		case BuiltinType::S8:     return 1;
		case BuiltinType::S16:    return 2;
		case BuiltinType::S32:    return 4;
		case BuiltinType::S64:    return 8;
		case BuiltinType::Bool:   return 1;
		case BuiltinType::Type:   return 8;
		default: invalid_code_path("Invalid BuiltinType {}", type_kind);
	}
}

u64 get_size_impl(LambdaHead *node) {
	return 8;
}
u64 get_size_impl(BuiltinTypeName *node) {
	return get_size(node->type_kind);
}
u64 get_size_impl(Unary *node) {
	if (node->operation == UnaryOperation::pointer)
		return 8;
	invalid_code_path("get_size(Unary {}) is invalid", node->operation);
}
u64 get_size_impl(Struct *Struct) {
	assert(Struct->size != -1);
	return Struct->size;
}
u64 get_size_impl(ArrayType *arr) {
	return get_size(arr->element_type) * arr->count.value();
}

u64 get_size(Type type) {
	type = direct(type);
	switch (type->kind) {
#define x(name) case NodeKind::name: return get_size_impl((name *)type);
		ENUMERATE_TYPE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path("get_size({}) is invalid", type->kind);
}

Sign get_sign(BuiltinType type_kind) {
	switch (type_kind) {
		case BuiltinType::U8: 
		case BuiltinType::U16:
		case BuiltinType::U32:
		case BuiltinType::U64: 
			return Sign::Unsigned;
		case BuiltinType::S8:  
		case BuiltinType::S16: 
		case BuiltinType::S32: 
		case BuiltinType::S64: 
			return Sign::Signed;
		default: invalid_code_path("Invalid BuiltinType {}", type_kind);
	}
}

Unary *as_pointer(Type type) {
	if (auto unary = as<Unary>(type); unary && unary->operation == UnaryOperation::pointer) {
		return unary;
	}
	return 0;
}

Expression *is_pointer_to_none_comparison(Expression *left, Expression *right) {
	auto dleft  = direct(left->type);
	auto dright = direct(right->type);
	if (auto left_pointer = as_pointer(dleft)) {
		if (auto right_builtin = as<BuiltinTypeName>(dright); right_builtin && right_builtin->type_kind == BuiltinType::None) {
			return left;
		}
	}
	if (auto left_builtin = as<BuiltinTypeName>(dleft); left_builtin && left_builtin->type_kind == BuiltinType::None) {
		if (auto right_pointer = as_pointer(dright)) {
			return right;
		}
	}
	return 0;
}

std::pair<Lambda *, LambdaHead *> get_lambda_and_head(Expression *expression) {
	auto directed = direct(expression);
	auto lambda = as<Lambda>(directed);
	LambdaHead *head = 0;
	if (lambda) {
		head = &lambda->head;
	} else {
		if (auto definition = as<Definition>(directed)) {
			head = direct_as<LambdaHead>(definition->type);
		}
	}
	return {lambda, head};
}
std::tuple<Lambda *, LambdaHead *, Struct *> get_lambda_and_head_or_struct(Expression *expression) {
	auto directed = direct(expression);
	auto lambda = as<Lambda>(directed);
	auto struct_ = as<Struct>(directed);
	LambdaHead *head = 0;
	if (lambda) {
		head = &lambda->head;
	} else {
		head = direct_as<LambdaHead>(expression->type);
	}
	return {lambda, head, struct_};
}
