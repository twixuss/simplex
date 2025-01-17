#pragma once
#include "common.h"

struct Parser {
	enum class YieldResult {
		parsed_node,
		success,
		fail,
	};

	Lexer lexer;
	Token token;
	Token previous_token;
	Block *current_block = &global_block;
	While *current_loop = 0;
	Expression *current_container = 0;
	Reporter reporter;
	Node *last_parsed_node = 0;
	ReusableFiber fiber = {};
	Fiber parent_fiber = {};
	Node *result_node;
	YieldResult last_yield_result = YieldResult::fail;
	List<utf8> extern_library = {};

	// For better error reporting
	String currently_parsing_if_condition_location;
	String currently_parsing_while_condition_location;

	List<utf8> unescape_string(String string) {
		if (auto result = ::unescape_string(string.skip(1).skip(-1))) {
			return result.string;
		} else {
			reporter.error(result.failed_at, "Failed to unespace this string: {}", result.fail_reason);
			yield(YieldResult::fail);
			return {};
		}
	}

	template <class T>
	T finish_node(T node) {
		last_parsed_node = node;
		return node;
	}

	void report_last_parsed_node() {
		if (last_parsed_node) {
			reporter.info(last_parsed_node->location, "Last parsed node is {}", last_parsed_node->kind);
		}
	}

	// Parses parse_expression_2 with binary operators and definitions.
	Expression *parse_expression(bool whitespace_is_skippable_before_binary_operator = false, int right_precedence = 0) {
		switch (token.kind) {
			case Token_var:
			case Token_let:
			case Token_const: {
				auto definition = Definition::create();
				definition->mutability = to_mutability(token.kind).value();

				definition->container = current_container;
				if (current_container) {
					if (auto lambda = as<Lambda>(current_container)) {
						lambda->locals.add(definition);
					}
				}
				
				next();
				skip_lines();

				expect(Token_name);

				definition->name = token.string;
				definition->location = token.string;

				next();
				skip_lines();

				expect({':', '='});

				if (token.kind == ':') {
					next();
					skip_lines();

					definition->parsed_type = parse_expression_2(); // NOTE: don't parse '='

					switch (definition->parsed_type->kind) {
						case NodeKind::Name:
						case NodeKind::BuiltinTypeName:
						case NodeKind::Unary:
						case NodeKind::ArrayType:
							break;
						default:
							reporter.error(definition->parsed_type->location, "{} is not allowed in type context.", definition->parsed_type->kind);
							yield(YieldResult::fail);
					}
				}

				if (token.kind == '=') {
					next();
					skip_lines();

					definition->initial_value = parse_expression();

					if (definition->mutability == Mutability::constant) {
						if (auto lambda = as<Lambda>(definition->initial_value)) {
							lambda->definition = definition;
						} else if (auto Struct = as<::Struct>(definition->initial_value)) {
							Struct->definition = definition;
						}
					}
				} else {
					//if (definition->mutability != Mutability::variable) {
					//	reporter.error(definition->location, "Definitions can't be marked as {} and have no initial expression.", definition->mutability);
					//	reporter.help(definition->location, "You can either change {} to {}, or provide an initial expression.", definition->mutability, Mutability::variable);
					//	yield(YieldResult::fail);
					//}

					expect({Token_eol, Token_eof});
				}

				return finish_node(definition);
			}
		}


		//null denotation
		auto left = parse_expression_2();

		// left binding power
		Optional<BinaryOperation> operation;
		while (1) {

			if (whitespace_is_skippable_before_binary_operator) {
				skip_lines();
			}

			constexpr bool enable_custom_infix = false;

			if (enable_custom_infix && token.kind == Token_name) {

				// Custom binary operator

				if (right_precedence < custom_precedence) {
					auto location = token.string;

					next();
					skip_lines();

					auto right = parse_expression(whitespace_is_skippable_before_binary_operator, custom_precedence);

					auto name = Name::create();
					name->name = location;
					name->location = location;

					auto call = Call::create();
					call->location = {left->location.begin(), right->location.end()};
					call->callable = name;
					call->arguments.add({{.expression = left}, {.expression = right}});
					left = call;
					continue;
				}
			} else {
				operation = as_binary_operation(token.kind);
				if (operation && right_precedence < get_precedence(operation.value())) {
					auto binop = Binary::create();
					binop->location = token.string;
					binop->left = left;
					binop->operation = operation.value();

					next();
					skip_lines();

					binop->right = parse_expression(whitespace_is_skippable_before_binary_operator, get_precedence(binop->operation) - is_right_associative(operation.value()));
					binop->location = { binop->left->location.begin(), binop->right->location.end() };

					left = binop;
					continue;
				}
			}
			break;
		}

		return finish_node(left);
	}
	// Parses parse_expression_1 plus parentheses or brackets after, e.g. calls, subscripts.
	Expression *parse_expression_2() {
		auto node = parse_expression_1();

		while (token.kind == '(' || token.kind == '[') {
			switch (token.kind) {
				case '(': {
					auto call = Call::create();
					call->callable = node;
					call->location = node->location;

					next();
					skip_lines();
					if (token.kind != ')') {
						while (true) {
							auto argument = parse_expression();
							String argument_name = {};

							if (auto binary = as<Binary>(argument)) {
								if (binary->operation == BinaryOperation::ass) {
									if (auto name = as<Name>(binary->left)) {
										argument_name = name->name;
										argument = binary->right;
									}
								}
							}
							
							call->arguments.add({
								.name = argument_name,
								.expression = argument,
							});

							skip_lines();
							if (token.kind == ',') {
								next();
								skip_lines();
								if (token.kind == ')') {
									break;
								}
								continue;
							}
							if (token.kind == ')') {
								break;
							}

							reporter.error(token.string, "Unexpected token `{}` when parsing call argument list. Expected `,` or `)`.", token.string);
							yield(YieldResult::fail);
						}
					}

					call->location = {call->location.begin(), token.string.end()};

					next();
					node = call;
					break;
				}
				case '[': {
					auto subscript = Subscript::create();
					subscript->subscriptable = node;
					subscript->location = node->location;

					next();
					skip_lines();

					subscript->index = parse_expression();

					skip_lines();
					expect(']');

					subscript->location = {subscript->location.begin(), token.string.end()};

					next();
					node = subscript;
					break;
				}
				default: invalid_code_path("unreachable");
			}
		}
		return finish_node(node);
	}
	// Parses parse_expression_0 plus member access.
	Expression *parse_expression_1() {
		auto expression = parse_expression_0();
		while (token.kind == '.') {
			auto binary = Binary::create();
			binary->location = token.string;
			binary->operation = BinaryOperation::dot;
			binary->left = expression;
			next();
			binary->right = parse_expression_0();
			if (!as<Name>(binary->right)) {
				reporter.error(binary->right->location, "Only names can follow a dot.");
				yield(YieldResult::fail);
			}
			binary->location = {binary->left->location.begin(), binary->right->location.end()};
			expression = binary;
		}
		return finish_node(expression);
	}
	// Parses single-part expressions
	Expression *parse_expression_0() {
		switch (token.kind) {
			case Token_fn: {
				auto lambda = Lambda::create();
				lambda->location = token.string;
				lambda->head.parameters_block.parent = current_block;

				scoped_replace(current_block, &lambda->head.parameters_block);
				scoped_replace(current_loop, 0);
				scoped_replace(current_container, lambda);
				assert(current_container->kind == NodeKind::Lambda);

				next();
				skip_lines();

				expect('(');

				next();
				skip_lines();

				if (token.kind != ')') {
					while (true) {
						expect({Token_name, Token_var, Token_let, Token_const});

						auto mutability = Mutability::readonly;

						switch (token.kind) {
							case Token_var:
							case Token_let:
							case Token_const:
								mutability = to_mutability(token.kind).value();
								next();
								skip_lines();
								expect(Token_name);
								break;
						}

						List<Definition *> parameter_group;

						auto create_and_add_parameter = [&] {
							auto parameter = Definition::create();
							parameter->name = token.string;
							parameter->location = token.string;
							parameter_group.add(parameter);
						};

						create_and_add_parameter();

						next();
						skip_lines();

						while (token.kind == ',') {
							next();
							skip_lines();
							expect(Token_name);
							create_and_add_parameter();
							next();
						}
						
						if (token.kind != ':') {
							reporter.error(token.string, "Expected {}, but got {}", (TokenKind)':', token);
							reporter.help(lambda->location, "We are currently parsing a lambda, because only lambdas start with `(`. If you want to wrap an operation, do that with a block `{}`");
							yield(YieldResult::fail);
						}

						next();
						skip_lines();

						auto parsed_type = parse_expression();

						for (auto parameter : parameter_group) {
							parameter->container = lambda;
							parameter->parsed_type = parsed_type;
							parameter->is_parameter = true;
							parameter->mutability = mutability;

							if (auto found = lambda->head.parameters_block.definition_map.find(parameter->name); found && found->value.count) {
								reporter.error(parameter->location, "Redefinition of parameter '{}'", parameter->name);
								reporter.info(found->value[0]->location, "First definition here:");
								yield(YieldResult::fail);
							}

							lambda->head.parameters_block.add(parameter);
						}

						skip_lines();
						if (token.kind == ',') {
							next();
							skip_lines();
							if (token.kind == ')') {
								break;
							}
							continue;
						}
						if (token.kind == ')') {
							break;
						}
					}
				}
				
				next();
				skip_lines();

				bool body_required = true;

				if (token.kind == ':') {
					next();
					skip_lines();

					lambda->head.parsed_return_type = parse_expression_0();
					body_required = false;
				}
				
				lambda->head.location = lambda->location = { lambda->location.begin(), previous_token.string.end() };

				constexpr auto arrow = const_string_to_token_kind("=>"s);
				if (lambda->head.parsed_return_type) {
					if (token.kind == arrow) {
						next();
						body_required = true;
					}
				} else {
					if (token.kind != arrow) {
						reporter.error(token.string, "Expected : or => after )");

						reporter.help("Functions are written like this:\n\n    (a: Type1, b: Type2): ReturnType => BodyExpression\n\nReturnType can be omitted:\n\n    (a: Type1, b: Type2) => BodyExpression");
						yield(YieldResult::fail);
					}
					next();
					body_required = true;
				}

				if (body_required) {
					skip_lines();

					while (token.kind == Token_directive) {
						if (token.string == u8"#intrinsic"s) {
							lambda->is_intrinsic = true;
						} else if (token.string == u8"#extern"s) {
							lambda->is_extern = true;
							lambda->extern_library = extern_library;
						} else {
							reporter.error(token.string, "Unknown lambda directive '{}'.", token.string);
							yield(YieldResult::fail);
						}
						next();
					}

					if (lambda->is_intrinsic || lambda->is_extern) {
						return finish_node(lambda);
					}

					lambda->body = parse_expression();

					return finish_node(lambda);
				}

				NOTE_LEAK(lambda, u8"the rest of the lambda is unused.can't just free lambda because head is in it"s);
		
				return finish_node(&lambda->head);
			}
			case '(': {
				next();
				skip_lines();

				auto expression = parse_expression();

				skip_lines();

				expect(')');
				next();

				return expression;
			}
			case '{': {
				auto block = Block::create();
				block->location = token.string;

				block->parent = current_block;
				scoped_replace(current_block, block);

				block->container = current_container;

				next();

				if (token.kind == ':') {
					next();
					expect(Token_name);

					block->tag = token.string;
					next();
				}

				while (true) {
					skip_lines();
					while (token.kind == ';') {
						next();
						skip_lines();
					}

					if (token.kind == '}') {
						break;
					}

					auto child = parse_statement();

					block->add(child);
				}
				block->location = {block->location.begin(), token.string.end()};
				next();

				for (auto child : block->children.skip(-1)) {
					ensure_allowed_in_statement_context(child);
				}

				if (is_substitutable(block)) {
					if (auto expression = as<Expression>(block->children[0])) {
						block->free();
						return finish_node(expression);
					}
				}

				return finish_node(block);
			}
			case '[': {
				auto Array = ArrayType::create();
				Array->location = token.string;
				next();
				Array->count_expression = parse_expression();
				expect(']');
				next();
				Array->element_type = parse_expression_0();
				Array->location = {Array->location.begin(), Array->element_type->location.end()};
				return finish_node(Array);
			}
			case '.': {
				auto constructor = ArrayConstructor::create();
				constructor->location = token.string;
				next();
				expect('[');
				next();
				while (true) {
					skip_lines();
					constructor->elements.add(parse_expression());
					skip_lines();

					expect({',', ']'});

					if (token.kind == ']') {
						break;
					}
					next();
				}
				constructor->location = {constructor->location.begin(), token.string.end()};
				next();
				return finish_node(constructor);
			}
			case Token_none: {
				auto none = NoneLiteral::create();
				none->location = token.string;
				next();
				return finish_node(none);
			}
			case Token_name: {
				auto name = Name::create();
				name->location = token.string;
				name->name = token.string;

				next();
				if (token.kind == Token_name) {
					reporter.error({previous_token.string.begin(), token.string.end()}, "Two consecutive names is invalid syntax.");
					if (currently_parsing_if_condition_location.count) {
						reporter.help(currently_parsing_if_condition_location, "We are parsing `if` condition right now. If you want to have it on the same line as the branch, you need to use `then` keyword between them");
					}
					if (currently_parsing_while_condition_location.count) {
						reporter.help(currently_parsing_while_condition_location, "We are parsing `while` condition right now. If you want to have it on the same line as the body, you need to use `do` keyword between them");
					}
					yield(YieldResult::fail);
				}

				return finish_node(name);
			}
			case Token_number: {
				auto literal = IntegerLiteral::create();
				literal->location = token.string;

				auto hex_digit_to_int = [](utf8 c) -> u8 {
					u32 u = c;
					if (u - '0' < 10) {
						return u - '0';
					}
					if (u - 'A' < 6) {
						return u - 'A' + 10;
					}
					if (u - 'a' < 6) {
						return u - 'a' + 10;
					}
					invalid_code_path();
				};

				if (token.string.count >= 2 && token.string.data[1] == 'x') {
					u64 result = 0;
					for (int i = 2; i < token.string.count; ++i) {
						result = (result << 4) | hex_digit_to_int(token.string.data[i]);
					}
					literal->value = result;
				} else {
					auto parsed = parse_u64(token.string);
					if (!parsed) {
						reporter.error(token.string, "Could not parse number {}", token.string);
						yield(YieldResult::fail);
					}

					literal->value = parsed.value();
				}

				next();

				return finish_node(literal);
			}
			case Token_false:
			case Token_true: {
				auto literal = BooleanLiteral::create();
				literal->location = token.string;
				literal->value = token.kind == Token_true;
				next();
				return finish_node(literal);
			}
			case Token_string: {
				auto literal = StringLiteral::create();
				literal->location = token.string;
				literal->value = unescape_string(token.string);
				next();
				return finish_node(literal);
			}
			case Token_if: {
				auto If = IfExpression::create();
				If->location = token.string;
				next();
				skip_lines();

				{
					scoped_replace(currently_parsing_if_condition_location, If->location);
					If->condition = parse_expression();
				}

				skip_lines();
				if (token.kind == Token_then) {
					next();
					skip_lines();
				}

				If->true_branch = parse_expression();

				skip_lines();
				if (token.kind == ';') {
					next();
					skip_lines();
				}
				
				if (token.kind != Token_else) {
					reporter.error(token.string, "Expected {}, but got {}", Token_else, token);
					reporter.help(If->location, "We are parsing `if` *expression*, which must have both branches. You can add `else` branch, or put `if` in a block, making it a statement.");
					yield(YieldResult::fail);
				}
				next();
				skip_lines();

				If->false_branch = parse_expression();
					
				If->location = {If->location.begin(), If->false_branch->location.end()};
				return finish_node(If);
			}
			case Token_match: {
				auto match = Match::create();
				match->location = token.string;
				next();
				skip_lines();

				match->expression = parse_expression();

				skip_lines();
				expect('{');

				next();
				skip_lines();

				while (true) {
					Expression *from = 0;
					if (token.kind == Token_else) {
						next();
					} else {
						from = parse_expression();
					}

					skip_lines();
					expect(const_string_to_token_kind("=>"s));
					next();
					skip_lines();

					auto to = parse_expression();

					auto &Case = match->cases.add({from, to});
					if (!from) {
						if (match->default_case) {
							reporter.error(to->location, "Match expression can not have multiple default cases.");
							yield(YieldResult::fail);
						}
						match->default_case = &Case;
					}

					skip_lines();
					while (token.kind == ';') {
						next();
						skip_lines();
					}
					if (token.kind == '}')
						break;
				}
				next();

				return finish_node(match);
			}
			case Token_inline:
			case Token_noinline: {
				auto inline_token = token;
				auto status = token.kind == Token_inline ? Inline::always : Inline::never;
				next();
				auto expr = parse_expression_2();
				if (auto lambda = as<Lambda>(expr)) {
					lambda->inline_status = status;
					return finish_node(lambda);
				} else if (auto call = as<Call>(expr)) {
					call->inline_status = status;
					return finish_node(call);
				}

				reporter.error(inline_token.string, "{} keyword must precede a lambda or a call, not a {}", inline_token.string, expr->kind);
				yield(YieldResult::fail);
				return 0;
			}
			case Token_struct: {
				auto Struct = Struct::create();
				scoped_replace(current_container, Struct);

				Struct->location = token.string;
				next();
				expect('{');
				next();
				skip_lines();

				while (token.kind != '}') {
					expect(Token_name);
					auto name = token.string;
					next();

					expect(':');
					next();
					auto type = parse_expression();

					expect('\n');
					skip_lines();

					auto definition = Definition::create();
					definition->location = name;
					definition->name = name;
					definition->container = Struct;
					definition->mutability = Mutability::variable;
					definition->parsed_type = type;
					Struct->members.add(definition);
				}
				next();

				return finish_node(Struct);
			}
#define x(name) case Token_##name:
			ENUMERATE_CONCRETE_BUILTIN_TYPES(x)
#undef x
			{
				auto type = BuiltinTypeName::create();
				type->location = token.string;
				type->type_kind = to_builtin_type_kind(token.kind);
				next();
				return finish_node(type);
			}

			default: {
				if (auto operation = as_unary_operation(token.kind)) {
					auto unop = Unary::create();
					unop->operation = operation.value();
					unop->location = token.string;
					next();
					skip_lines();

					if (operation.value() == UnaryOperation::star) {
						switch (token.kind) {
							case Token_var: 
							case Token_let: 
							case Token_const: {
								unop->mutability = to_mutability(token.kind).value();
								next();
								skip_lines();
								break;
							}
						}
					}

					unop->expression = parse_expression_2();
					unop->location = {unop->location.begin(), unop->expression->location.end()};
					return finish_node(unop);
				}

				reporter.error(token.string, "Unexpected token {} when parsing expression.", token.kind);
				report_last_parsed_node();
				yield(YieldResult::fail);
			}
		}

		invalid_code_path("node was not returned");
	}
	Node *parse_statement() {
		switch (token.kind) {
			case Token_return: {
				auto return_ = Return::create();
				return_->location = token.string;

				if (!current_container) {
					reporter.error(return_->location, "Return statement can not appear outside of a lambda.");
					yield(YieldResult::fail);
				}

				if (auto lambda = as<Lambda>(current_container)) {
					return_->lambda = lambda;
					lambda->returns.add(return_);
				} else {
					reporter.error(return_->location, "Return statement can only appear in a lambda. But current container is {}.", current_container->kind);
					yield(YieldResult::fail);
				}

				next();

				if (token.kind != '\n') {
					return_->value = parse_expression();
				}

				return finish_node(return_);
			}
			case Token_while: {
				auto While = While::create();
				While->location = token.string;
				next();

				scoped_replace(current_loop, While);
				
				{
					scoped_replace(currently_parsing_while_condition_location, While->location);
					While->condition = parse_expression();
				}

				skip_lines();
				if (token.kind == Token_then) {
					next();
					skip_lines();
				}

				While->body = parse_statement();

				return finish_node(While);
			}
			case Token_continue: {
				if (!current_loop) {
					reporter.error(token.string, "`continue` must be inside a loop.");
					yield(YieldResult::fail);
				}

				auto Continue = Continue::create();
				Continue->location = token.string;
				next();
				return finish_node(Continue);
			}
			case Token_break: {
				auto Break = Break::create();
				Break->location = token.string;
				next();

				if (!current_block) {
					reporter.error(Break->location, "`break` must be inside a block.");
					yield(YieldResult::fail);
				}

				if (token.kind == ':') {
					next();
					expect(Token_name);

					auto tag = token.string;
					next();
					Break->value = parse_expression();

					bool blocks_are_valid = true;

					auto block = current_block;
					while (1) {
						if (block->tag == tag) {
							if (blocks_are_valid) {
								Break->tag_block = block;
								Break->tag_block->breaks.add(Break);
							} else {
								reporter.error(Break->location, "Block with name {} is outside of current container.", tag);
								reporter.info(block->location, "Here is the block:");
								reporter.info(current_container->location, "Here is current container:");
								yield(YieldResult::fail);
							}
							break;
						}

						block = block->parent;
						if (block) {
							if (block->container != current_container) {
								blocks_are_valid = false;
							}
						}

						if (!block) {
							reporter.error(Break->location, "Could not find block with name {}.", tag);
							yield(YieldResult::fail);
						}
					}
				} else {
					if (!current_loop) {
						reporter.error(Break->location, "Empty `break` must be inside a loop.");
						yield(YieldResult::fail);
					}
				}


				return finish_node(Break);
			}
			case Token_directive: {
				if (token.string == "#extern") {
					auto extern_location = token.string;
					next();
					expect(Token_string);
					// Don't free extern_library. Lambdas point to it.
					extern_library = unescape_string(token.string);
					next();
					skip_lines();
					if (token.kind == Token_eof) {
						reporter.error(extern_location, "At least one definition must follow `extern` directive.");
						yield(YieldResult::fail);
					}
					return parse_statement();
				}

				reporter.error(token.string, "Unknown directive.");
				yield(YieldResult::fail);
				break;
			}
			case Token_if: {
				auto location = token.string;
				next();
				skip_lines();

				Expression *condition;
				{
					scoped_replace(currently_parsing_if_condition_location, location);
					condition = parse_expression();
				}

				skip_lines();
				if (token.kind == Token_then) {
					next();
					skip_lines();
				}

				auto true_branch = parse_statement();
				Node *false_branch = 0;

				skip_lines();
				if (token.kind == ';') {
					next();
					skip_lines();
				}
				if (token.kind == Token_else) {
					next();
					skip_lines();

					false_branch = parse_statement();

					location = {location.begin(), false_branch->location.end()};
				} else {
					location = {location.begin(), true_branch->location.end()};
				}

				if (false_branch) {
					if (auto true_expression = as<Expression>(true_branch)) {
						if (auto false_expression = as<Expression>(false_branch)) {
							auto If = IfExpression::create();
							If->location = location;
							If->condition = condition;
							If->true_branch = true_expression;
							If->false_branch = false_expression;
							return finish_node(If);
						}
					}
				}

				auto If = IfStatement::create();
				If->location = location;
				If->condition = condition;
				If->true_branch = true_branch;
				If->false_branch = false_branch;
				return finish_node(If);
			}
			case Token_import: {
				next();

				expect(Token_string);
				
				auto import = Import::create();
				import->path = unescape_string(token.string);
				
				next();
	
				auto full_path = normalize_path(make_absolute_path(format(u8"{}\\import\\{}.sp", compiler_root_directory, import->path)));
				locked_use(imports) {
					imports.add_file({.path = full_path, .location = import->location});
				};

				return finish_node(import);
			}
		}

		auto expression = parse_expression();

		return expression;
	}

	void yield(YieldResult result) {
		last_yield_result = result;
		switch (result) {
			case YieldResult::parsed_node:
				tl::yield(parent_fiber);
				break;
			case YieldResult::success:
			case YieldResult::fail:
				tl::yield_reuse(parent_fiber, fiber);
				break;
			default:
				invalid_code_path();
		}
	}
	void main() {
		scoped_replace(debug_current_location, {});
	
		token = lexer.next_token();

		while (true) {
			skip_lines();
			while (token.kind == ';') {
				next();
				skip_lines();
			}

			if (token.kind == Token_eof) {
				break;
			}

			auto child = parse_statement();

			switch (child->kind) {
				case NodeKind::Block: {
					reporter.error(child->location, "Blocks are not allowed in global scope.");
					yield(YieldResult::fail);
					return;
				}
			}

			ensure_allowed_in_statement_context(child);

			result_node = child;
			yield(YieldResult::parsed_node);
		}

		yield(YieldResult::success);
	}

	Node *parse_next_node() {
		result_node = 0;
		tl::yield(fiber);
		return result_node;
	}

	void ensure_allowed_in_statement_context(Node *node) {
		String is_global = current_block == &global_block ? u8"global "s : u8""s;

		switch (node->kind) {
			#define x(name) case NodeKind::name:
			ENUMERATE_STATEMENT_KIND(x)
			#undef x
			case NodeKind::Definition:
			case NodeKind::Block:
			case NodeKind::Call:
			case NodeKind::IfExpression:
			case NodeKind::Match:
				return;
			case NodeKind::Binary: {
				if (current_block != &global_block) {
					auto binary = (Binary *)node;
					switch (binary->operation) {
						case BinaryOperation::ass:
						case BinaryOperation::addass:
						case BinaryOperation::subass:
						case BinaryOperation::mulass:
						case BinaryOperation::divass:
						case BinaryOperation::modass:
						case BinaryOperation::borass:
						case BinaryOperation::banass:
						case BinaryOperation::bxoass:
						case BinaryOperation::bslass:
						case BinaryOperation::bsrass:
							return;
					}

					reporter.error(node->location, "Binary {} is not allowed in {}statement context.", binary->operation, is_global);
					yield(YieldResult::fail);
				}
				break;
			}
		}

		reporter.error(node->location, "{} is not allowed in {}statement context.", node->kind, is_global);
		yield(YieldResult::fail);
	}
	bool next() {
		previous_token = token;
		token = lexer.next_token();
		debug_current_location = token.string;
		return token.kind != Token_eof;
	}
	void expect(std::underlying_type_t<TokenKind> expected_kind) {
		if (token.kind != expected_kind) {
			reporter.error(token.string, "Expected {}, but got {}", (TokenKind)expected_kind, token);
			yield(YieldResult::fail);
		}
	}
	void expect_not(std::underlying_type_t<TokenKind> unexpected_kind) {
		if (token.kind == unexpected_kind) {
			reporter.error(token.string, "Unexpected {}", (TokenKind)unexpected_kind);
			yield(YieldResult::fail);
		}
	}
	void expect(std::initializer_list<std::underlying_type_t<TokenKind>> expected_kinds) {
		for (auto expected_kind : expected_kinds) {
			if (token.kind == expected_kind) {
				return;
			}
		}

		StringBuilder builder;
		append(builder, "Expected ");
		for (auto expected_kind : expected_kinds) {
			append_format(builder, "{} or ", (TokenKind)expected_kind);
		}
		append_format(builder, "but got {}.\0"s, token);
		reporter.error(token.string, (char *)to_string(builder).data);
		yield(YieldResult::fail);
	}
	void expect_not(std::initializer_list<std::underlying_type_t<TokenKind>> unexpected_kinds) {
		for (auto unexpected_kind : unexpected_kinds) {
			expect_not(unexpected_kind);
		}
	}
	void skip_lines() {
		while (token.kind == '\n') {
			previous_token = token;
			token = lexer.next_token();
		}
		debug_current_location = token.string;
	}

	void init(String source) {
		this->parent_fiber = init_or_get_current_fiber();
		lexer = Lexer::create(source);
		fiber = get_new_fiber();
		set_start(fiber, [] (void *param) {
			((Parser *)param)->main();
		}, this);
	}

	void free() {
		add_fiber_to_reuse(fiber);
		fiber = {};
	}
};
