#include "nodes.h"

umm append(StringBuilder &builder, Node *node) {
	switch (node->kind) {
		case NodeKind::Name: {
			return append(builder, ((Name *)node)->name);
		}
		case NodeKind::BuiltinTypeName: {
			switch (((BuiltinTypeName *)node)->type_kind) {
				#define x(name) case BuiltinType::name: return append(builder, #name);
				ENUMERATE_BUILTIN_TYPES(x)
				#undef x
			}
			return append(builder, "(unknown BuiltinTypeName)");
		}
		case NodeKind::LambdaHead: {
			auto head = (LambdaHead *)node;

			umm result = 0;
			auto write = [&] (auto &&...args) {
				result += append(builder, args...);
			};

			write('(');
			for (auto &parameter : head->parameters_block.definition_list) {
				if (&parameter != head->parameters_block.definition_list.data) {
					write(", ");
				}

				write(parameter->name);
				write(": ");
				write(parameter->type);
			}
			write(") ");
			write(head->return_type);

			return result;
		}
		case NodeKind::Unary: {
			auto unary = (Unary *)node;
			if (unary->operation == UnaryOperation::pointer) {
				return append_format(builder, "*{} {}", unary->mutability, unary->expression);
			}
			break;
		}
		case NodeKind::Struct: {
			auto Struct = (::Struct *)node;
			if (Struct->definition)
				return append(builder, Struct->definition->name);
			else
				return append(builder, "struct");
			break;
		}
		case NodeKind::ArrayType: {
			auto arr = (ArrayType *)node;
			
			return 
				append(builder, '[') +
				append(builder, arr->count.value()) +
				append(builder, ']') +
				append(builder, arr->element_type);

			break;
		}
	}
	return append(builder, "(unknown)");
}

bool is_substitutable(Block *block) {
	return block->children.count == 1 && block->breaks.count == 0;
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