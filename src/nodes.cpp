#include "nodes.h"

Node::Node() {
	if (uid == 3674) {
		int x = 4;
	}
}

void append(StringBuilder &builder, Node *node) {
	switch (node->kind) {
		case NodeKind::Name: {
			append(builder, ((Name *)node)->name);
			break;
		}
		case NodeKind::BuiltinTypeName: {
			auto type_kind = ((BuiltinTypeName *)node)->type_kind;
			switch (type_kind) {
				#define x(name) case BuiltinType::name: return append(builder, #name);
				ENUMERATE_BUILTIN_TYPES(x)
				#undef x
			}
			append_format(builder, "BuiltinTypeName({})", (u64)type_kind);
			break;
		}
		case NodeKind::LambdaHead: {
			auto head = (LambdaHead *)node;

			append(builder, '(');
			for (auto &parameter : head->parameters_block.definition_list) {
				if (&parameter != head->parameters_block.definition_list.data) {
					append(builder, ", ");
				}

				append(builder, parameter->name);
				append(builder, ": ");
				append(builder, parameter->type);
			}
			append(builder, ") ");
			append(builder, head->return_type);
			break;
		}
		case NodeKind::Unary: {
			auto unary = (Unary *)node;
			if (unary->operation == UnaryOperation::pointer) {
				append_format(builder, "*{} {}", unary->mutability, unary->expression);
			}
			break;
		}
		case NodeKind::Struct: {
			auto Struct = (::Struct *)node;
			if (Struct->definition) {
				append(builder, Struct->definition->name);
			} else {
				append(builder, "struct");
			}
			break;
		}
		case NodeKind::Enum: {
			auto Enum = (::Enum *)node;
			if (Enum->definition) {
				append(builder, Enum->definition->name);
			} else {
				append(builder, "enum");
			}
			break;
		}
		case NodeKind::ArrayType: {
			auto arr = (ArrayType *)node;
			
			append(builder, '[');
			append(builder, arr->count.value());
			append(builder, ']');
			append(builder, arr->element_type);

			break;
		}
		default: {
			append(builder, "(unknown)");
			break;
		}
	}
}

bool is_substitutable(Block *block) {
	return block->children.count == 1 && block->breaks.count == 0;
}

bool is_addressable(Expression *expression) {
	switch (expression->kind) {
		case NodeKind::Name:
		case NodeKind::Definition:
			return true;
		case NodeKind::Binary: {
			auto binary = (Binary *)expression;
			switch (binary->operation) {
				case BinaryOperation::dot:
					return true;
			}
			break;
		}
		case NodeKind::Unary: {
			auto unary = (Unary *)expression;
			switch (unary->operation) {
				case UnaryOperation::dereference:
					return true;
			}
			break;
		}
		case NodeKind::Block: {
			auto block = (Block *)expression;
			if (auto last = as<Expression>(block->children.back())) {
				return is_addressable(last);
			}
			break;
		}
		case NodeKind::Subscript: {
			auto subscript = (Subscript *)expression;
			return is_addressable(subscript->subscriptable);
		}
	}

	return false;
}

#if ENABLE_NOTE_LEAK

GList<String> leaks;

void note_leak(String expression, Node *node, String message = {}, std::source_location location = std::source_location::current()) {
	if (message)
		leaks.add(format(u8"{} ({}) at {}:{} - {}. {}", expression, node->kind, location.file_name(), location.line(), location.function_name(), message));
	else 
		leaks.add(format(u8"{} ({}) at {}:{} - {}", expression, node->kind, location.file_name(), location.line(), location.function_name()));
}

#endif