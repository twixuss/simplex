int tabs = 0;
void print_tabs() {
	for (int i = 0; i < tabs; ++i)
		print("    ");
}

#define tabbed ++tabs; defer { --tabs; }

static constexpr ConsoleColor untypechecked_color = ConsoleColor::red;

void print_ast(Node *node);

void print_if_type(Node *type, Node *fallback) {
	if (type) {
		print_ast(type);
	} else {
		with(untypechecked_color, print_ast(fallback));
	}
}

void print_ast(Value value) {
	switch (value.kind) {
		case ValueKind::lambda: print_ast(value.lambda); break;
		case ValueKind::Type: print_ast(value.Type); break;
		default: print(value); break;
	}
}
void print_ast_impl(Block *block) {
	print("{");
	if (block->breaks.count) {
		print(" :{}", block->tag);
	}
	print('\n');
	{ tabbed;
		for (auto child : block->children) {
			print_tabs();
			print_ast(child);
			println();
		}
	}
	print_tabs();
	println("}");
}
void print_ast_impl(Call *call) {
	print_ast(call->callable);
	print('(');
	{ tabbed;
		for (auto &argument : call->arguments) {
			if (&argument != call->arguments.data) {
				print(", ");
			}
			print_ast(argument.expression);
		}
	}
	print(')');
}
void print_ast_impl(Definition *definition) {
	switch (definition->mutability) {
		case Mutability::constant: print("const"); break;
		case Mutability::readonly: print("let"); break;
		case Mutability::variable: print("var"); break;
		default: invalid_code_path();
	}
	print(' ');
	print(definition->name);
	if (context->print_uids) {
		print('_');
		print(definition->uid);
	}
	print(": ");
	print_if_type(definition->type, definition->parsed_type);
	if (definition->constant_value) {
		print(" = ");
		print_ast(definition->constant_value.value());
	} else if (definition->initial_value) {
		print(" = ");
		print_ast(definition->initial_value);
	}
}
void print_ast_impl(IntegerLiteral *literal) {
	print('{');
	{ tabbed;
		print(literal->value);
		print(" as ");
		print_ast(literal->type);
	}
	print('}');
}
void print_ast_impl(BooleanLiteral *literal) {
	print(literal->value);
}
void print_ast_impl(NoneLiteral *) {
	print("none");
}
void print_ast_impl(StringLiteral *literal) {
	print("\"{}\"", EscapedString{literal->value});
}
void print_ast_impl(LambdaHead *head, bool print_braces = true) {
	if (print_braces) {
		print("{");
		++tabs;
	}
	print("fn (");
	{ tabbed;
	#if 1
		// Individual parameters
		foreach (it, head->parameters_block.definition_list) {
			auto [index, parameter] = it.key_value();

			if (index)
				print(", ");

			print(parameter->name);
			print(": ");
			print_if_type(parameter->type, parameter->parsed_type);
		}
	#else
		// Grouped parameters

		List<List<Definition *>> grouped_parameters;

		for (auto parameter : head->parameters_block.definition_list) {
			if (grouped_parameters.count && types_match(grouped_parameters.back().front()->type, parameter->parsed_type)) {
				grouped_parameters.back().add(parameter);
			} else {
				List<Definition *> group;
				group.add(parameter);
				grouped_parameters.add(group);
			}
		}

		for (auto &group : grouped_parameters) {
			if (&group != grouped_parameters.begin())
				print(", ");

			for (auto &parameter : group) {
				if (&parameter != group.begin())
					print(", ");

				print(parameter->name);
			}
			print(": ");
			print_ast(group[0]->type);
		}
	#endif
	}
	print("): ");
	print_if_type(head->return_type, head->parsed_return_type);
	if (print_braces) {
		--tabs;
		print("}");
	}
}
void print_ast_impl(Lambda *lambda) {
	print("{");
	{ tabbed;
		switch (lambda->inline_status) {
			case InlineStatus::always: print("inline "); break;
			case InlineStatus::never: print("noinline "); break;
		}
		print_ast_impl(&lambda->head, false);
		print(" => ");
		if (lambda->is_intrinsic) 
			print("#intrinsic");
		if (lambda->is_extern) 
			print("#extern \"{}\"", EscapedString(lambda->extern_library));
		if (lambda->body) {
			print_ast(lambda->body);
		}
	}
	print_tabs();
	print("}");
}
void print_ast_impl(Name *name) {
	print(name->name);
	if (context->print_uids) {
		print('_');
		assert(name->definition());
		print(name->definition()->uid);
	}
}
void print_ast_impl(Return *return_) {
	print("return");
	if (return_->value) {
		print(' ');
		print_ast(return_->value);
	}
}
void print_ast_impl(IfStatement *If) {
	print("if ");
	print_ast(If->condition);
	print(" then ");
	print_ast(If->true_branch);
	if (If->false_branch) {
		print(" else ");
		print_ast(If->false_branch);
	}
}
void print_ast_impl(IfExpression *If) {
	print("if ");
	print_ast(If->condition);
	print(" then ");
	print_ast(If->true_branch);
	print(" else ");
	print_ast(If->false_branch);
}
void print_ast_impl(While *While) {
	print("while ");
	print_ast(While->condition);
	print(" then ");
	print_ast(While->body);
}
void print_ast_impl(BuiltinTypeName *type) {
	switch (type->type_kind) {
		#define x(name) case BuiltinType::name: print(#name); return;
		#define y(name, value) x(name)
		ENUMERATE_BUILTIN_TYPES(x)
		#undef y
		#undef x
	}
	invalid_code_path();
}
void print_ast_impl(Continue *) { print("continue"); }
void print_ast_impl(Break *Break) {
	print("break");
	if (Break->value) {
		print(" :{} ", Break->tag_block->tag);
		print_ast(Break->value);
	}
}
void print_ast_impl(Binary *binary) {
	print('{');
	{ tabbed;
		print_ast(binary->left);
		print(' ');
		print(binary->operation);
		print(' ');
		print_ast(binary->right);
	}
	print('}');
}
void print_ast_impl(Match *match) {
	print("match ");
	print_ast(match->expression);
	print(" {\n");
	{ tabbed;
		for (auto Case : match->cases) {
			print_tabs();
			if (Case.froms) {
				for (auto &from : Case.froms) {
					if (&from != Case.froms.data)
						print(" or ");
					print_ast(from);
				}
			} else {
				print("else");
			}
			print(" => ");
			print_ast(Case.to);
			print("\n");
		}
	}
	print_tabs();
	print("}");
}
void print_ast_impl(Unary *unary) {
	print('{');
	{ tabbed;
		print(unary->operation);
		print_ast(unary->expression);
	}
	print('}');
}
void print_ast_impl(Struct *Struct) {
	print("struct {\n");
	{ tabbed;
		for (auto member : Struct->members) {
			print_ast(member);
		}
	}
	print("}");
}
void print_ast_impl(Enum *Enum) {
	print("enum : ");
	print_if_type(Enum->underlying_type, Enum->parsed_underlying_type);
	print(" {\n");
	{ tabbed;
		for (auto element : Enum->block.definition_list) {
			print_tabs();
			print_ast(element);
			println();
		}
	}
	print_tabs();
	print("}");
}
void print_ast_impl(ArrayType *Array) {
	print("{[");
	print(Array->count.value());
	print("]");
	print_ast(Array->element_type);
	print("}");
}
void print_ast_impl(Subscript *Subscript) {
	print("{");
	print_ast(Subscript->subscriptable);
	print("[");
	print_ast(Subscript->index);
	print("]}");
}
void print_ast_impl(ArrayConstructor *arr) {
	print(".[");
	if (arr->elements.count)
		print_ast(arr->elements[0]);
	for (auto element : arr->elements.skip(1)) {
		print(", ");
		print_ast(element);
	}
	print("]");
}
void print_ast_impl(Import *import) {
	print("import \"{}\"", EscapedString{import->path});
}
void print_ast_impl(Defer *defer_) {
	print("defer {\n");
	{ tabbed;
		print_tabs();
		print_ast(defer_->body);
		println();
	}
	print_tabs();
	print("}");
}
void print_ast_impl(ZeroInitialized *zi) {
	print("{none as ");
	print_ast(zi->type);
	print("}");
}
void print_ast_impl(CallerLocation *cl) {
	print("#caller_location");
}
void print_ast_impl(CallerArgumentString *cas) {
	print("#argument_string {}", cas->parameter_name);
}
void print_ast(Node *node) {
	if (!node) {
		print("<NULL>");
		return;
	}
	switch (node->kind) {
#define x(name) case NodeKind::name: return print_ast_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
}
