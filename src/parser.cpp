#pragma once
#include "parser.h"
#include "nodes.h"
#include "escape.h"
#include "mutability.h"
#include "capitalized.h"
#include "fiber.h"
#include "debug.h"
#include "compiler_context.h"

struct LockIfBlockIsGlobal {
	Block *block;
};

template <>
struct Scoped<LockIfBlockIsGlobal> {
	LockIfBlockIsGlobal x;

	void enter(LockIfBlockIsGlobal x) {
		this->x = x;
		enter();
	}

	void enter() {
		if (x.block == &context->global_block.unprotected) {
			lock(context->global_block._lock);
		}
	}
	void exit() {
		if (x.block == &context->global_block.unprotected) {
			unlock(context->global_block._lock);
		}
	}
};

LockProtected<Imports, SpinLock> imports;

List<utf8> Parser::unescape_string_or_fail(String string) {
	assert(string.count >= 2);
	assert(string.front() == '"');
	assert(string.back()  == '"');
	if (auto result = ::unescape_string(string.skip(1).skip(-1))) {
		return result.string;
	} else {
		reporter.error(result.failed_at, "Failed to unespace this string: {}", result.fail_reason);
		yield(YieldResult::fail);
		return {};
	}
}

void Parser::report_last_parsed_node() {
	if (last_parsed_node) {
		reporter.info(last_parsed_node->location, "Last parsed node is {}", last_parsed_node->kind);
	}
}

bool Parser::parse_template_parameter_list(Expression *parent, Block *template_parameters_block) {
	return parse_list('[', ',', ']', {}, [&] {
		List<Definition *> template_parameter_group;

		auto parse_name_and_add_to_group = [&] {
			auto template_parameter = Definition::create();
			parse_name(&template_parameter->location, &template_parameter->name);
			template_parameter_group.add(template_parameter);
		};

		parse_name_and_add_to_group();

		skip_lines();

		while (token.kind == ',') {
			next();
			skip_lines();
			parse_name_and_add_to_group();
		}
						
		if (token.kind != ':') {
			reporter.error(token.string, "Expected {}, but got {}", (TokenKind)':', token);
			yield(YieldResult::fail);
		}

		next();
		skip_lines();

		auto parsed_type = parse_expression();

		for (auto template_parameter : template_parameter_group) {
			template_parameter->container = parent;
			template_parameter->parsed_type = parsed_type;
			template_parameter->is_template_parameter = true;
			template_parameter->mutability = Mutability::constant;

			if (auto found = template_parameters_block->definition_map.find(template_parameter->name); found && found.value->count) {
				reporter.error(template_parameter->location, "Redefinition of template_parameter '{}'", template_parameter->name);
				reporter.info((*found.value)[0]->location, "First definition here:");
				yield(YieldResult::fail);
			}

			template_parameters_block->add(template_parameter);
		}
	});

}

Parser::NamedLambda Parser::parse_lambda() {
	auto lambda = Lambda::create();
	lambda->location = token.string;
	lambda->head.template_parameters_block.parent = current_block;

	scoped_replace(current_block, &lambda->head.parameters_block);
	scoped_replace(current_loop, 0);
	scoped_replace(current_container, lambda);
	assert(current_container->kind == NodeKind::Lambda);

	String lambda_name = {};

	next();
	skip_lines();

	if (token.kind == Token_name) {
		String dummy_location;

		parse_name(&dummy_location, &lambda_name);

		skip_lines();
	}
	
	if (lambda_name == "set_sum") {
		int x = 5;
	}
		

	lambda->head.is_template = parse_template_parameter_list(lambda, &lambda->head.template_parameters_block);

	expect('(');
	parse_list('(', ',', ')', {}, [&] {
		auto parameter = Definition::create();

		expect({Token_name, Token_var, Token_let, Token_const, Token_use});

		if (token.kind == Token_use) {
			parameter->use = true;

			next();
		}

		parameter->mutability = Mutability::readonly;
		switch (token.kind) {
			case Token_var:
			case Token_let:
			case Token_const:
				parameter->mutability = to_mutability(token.kind).value();
				next();
				skip_lines();
				expect(Token_name);
				break;
		}
		
		parameter->name = token.string;
		parameter->location = token.string;

		next();
		skip_lines();

		if (token.kind != ':') {
			reporter.error(token.string, "Expected {}, but got {}", (TokenKind)':', token);
			reporter.help(lambda->location, "We are currently parsing a lambda, because only lambdas start with `(`. If you want to wrap an operation, do that with a block `{}`");
			yield(YieldResult::fail);
		}

		next();
		skip_lines();

		Expression *parsed_type = 0;
		if (token.kind != '=') {
			parsed_type = parse_expression_1(); // NOTE: don't parse default value
			skip_lines();
		}

		if (token.kind == '=') {
			next();
			skip_lines();

			if (token.kind == Token_directive) {
				if (token.string == "#caller_location") {
					parameter->initial_value = CallerLocation::create();
					next();
				} else if (token.string == "#argument_string") {
					auto cas = CallerArgumentString::create();
					next();
					skip_lines();

					expect(Token_name);
					String name_location;
					parse_name(&name_location, &cas->parameter_name);

					parameter->initial_value = cas;
				}
			} 

			if (!parameter->initial_value) {
				parameter->initial_value = parse_expression();
			}
		}

		parameter->container = &lambda->head;
		parameter->parsed_type = parsed_type;
		parameter->is_parameter = true;

		if (auto found = lambda->head.parameters_block.definition_map.find(parameter->name); found && found.value->count) {
			reporter.error(parameter->location, "Redefinition of parameter '{}'", parameter->name);
			reporter.info((*found.value)[0]->location, "First definition here:");
			yield(YieldResult::fail);
		}

		lambda->head.parameters_block.add(parameter);
	});

	for (auto parameter : lambda->head.parameters_block.definition_list) {
		if (parameter->initial_value) {
			if (auto cas = as<CallerArgumentString>(parameter->initial_value)) {
				auto found = lambda->head.parameters_block.definition_map.find(cas->parameter_name);
				if (found && found.value->count == 1) {
					cas->parameter = found.value->data[0];
				} else {
					reporter.error(cas->location, "No parameter named {} to take expression string of", cas->parameter);
					yield(YieldResult::fail);
				}
			}
		}
	}

	bool body_required = true;
	bool should_expect_arrow = true;

	if (token.kind == ':') {
		next();
		skip_lines();

		lambda->head.parsed_return_type = parse_expression_0();
		body_required = false;
	} else if (token.kind == '{') {
		body_required = true;
		should_expect_arrow = false;
	}
				
	lambda->head.location = lambda->location = { lambda->location.begin(), previous_token.string.end() };

	if (should_expect_arrow) {
		if (lambda->head.parsed_return_type) {
			if (token.kind == '=>') {
				next();
				body_required = true;
			}
		} else {
			if (token.kind != '=>') {
				reporter.error(token.string, "Expected : or => after )");

				reporter.help("Functions are written like this:\n\n    (a: Type1, b: Type2): ReturnType => BodyExpression\n\nReturnType can be omitted:\n\n    (a: Type1, b: Type2) => BodyExpression");
				yield(YieldResult::fail);
			}
			next();
			body_required = true;
		}
	}

	if (body_required) {
		skip_lines();

		while (token.kind == Token_directive) {
			if (token.string == u8"#intrinsic"s) {
				lambda->is_intrinsic = true;
			} else if (token.string == u8"#extern"s) {
				lambda->is_extern = true;
				lambda->extern_library = extern_library;
			} else if (token.string == u8"#linkname"s) {
				next();
				expect(Token_string);
				lambda->link_name = copy(lexer.string_value);
			} else if (token.string == u8"#print_bytecode"s) {
				lambda->print_bytecode = true;
			} else {
				reporter.error(token.string, "Unknown lambda directive '{}'.", token.string);
				yield(YieldResult::fail);
			}
			next();
		}

		if (lambda->is_intrinsic || lambda->is_extern) {
			if (lambda->head.is_template) {
				reporter.error(lambda->location, "Templated lambdas can't be intrinsic or extern and must have a body");
				yield(YieldResult::fail);
			}

			return {finish_node(lambda), lambda_name};
		}

		lambda->body = parse_expression();
		
		if (auto definition = as<Definition>(lambda->body)) {
			reporter.error(definition->location, "You can't have a definition as a lambda's body");
			yield(YieldResult::fail);
		}

		return {finish_node(lambda), lambda_name};
	}

	NOTE_LEAK(lambda, u8"the rest of the lambda is unused.can't just free lambda because head is in it"s);
		
	if (lambda->head.is_template) {
		reporter.error(lambda->location, "Using templated lambdas as types is not supported");
		yield(YieldResult::fail);
	}

	return {finish_node(&lambda->head), lambda_name};
}

Parser::NamedStruct Parser::parse_struct() {
	auto Struct = Struct::create();
	scoped_replace(current_container, Struct);

	Struct->location = token.string;
	next();
	skip_lines();
	String name_location, name;
	if (token.kind == Token_name) {
		parse_name(&name_location, &name);
		skip_lines();
	}
	if (token.kind == Token_directive) {
		if (token.string == u8"#must_be_fully_initialized"s) {
			Struct->must_be_fully_initialized = true;
		} else {
			reporter.error(token.string, "Unknown directive for struct");
			yield(YieldResult::fail);
		}
		next();
		skip_lines();
	}

	Struct->is_template = parse_template_parameter_list(Struct, &Struct->template_parameters_block);

	expect('{');
	next();
	skip_lines();

	while (token.kind != '}') {
		auto definition = Definition::create();

		parse_name(&definition->location, &definition->name);

		expect(':');
		next();
		definition->parsed_type = parse_expression_1(); // Don't parse = as binop

		if (token.kind == '=') {
			next();
			definition->initial_value = parse_expression();
		}

		skip_lines();
		while (token.kind == ';') {
			next();
			skip_lines();
		}

		definition->container = Struct;
		definition->mutability = Mutability::variable;

		auto &entry = Struct->member_map.get_or_insert(definition->name);

		if (entry) {
			reporter.error(definition->location, "Redefinition of {}", definition->name);
			reporter.info(entry->location, "First declared here:");
			yield(YieldResult::fail);
		} else {
			entry = definition;
			Struct->member_list.add(definition);
		}
	}
	next();

	return {
		.Struct = finish_node(Struct),
		.name = name,
	};
}

Parser::NamedEnum Parser::parse_enum() {
	auto Enum = ::Enum::create();

	next();
	skip_lines();
	
	String name_location, name;
	if (token.kind == Token_name) {
		parse_name(&name_location, &name);
		skip_lines();
	}

	if (token.kind == ':') {
		next();
		skip_lines();
		Enum->parsed_underlying_type = parse_expression();
		skip_lines();
	}

	while (1) {
		if (token.kind == Token_directive) {
			if (token.string == "#allow_from_int") {
				Enum->allow_from_int = true;
			} else if (token.string == "#allow_to_int") {
				Enum->allow_to_int = true;
			} else {
				reporter.error(token.string, "Unknown directive for enum: {}", token.string);
				yield(YieldResult::fail);
			}
			next();
		} else {
			break;
		}
	}

	expect('{');
	next();
			
	scoped_replace(current_container, Enum);
	scoped_replace(current_block, &Enum->block);

	while (1) {
		skip_lines();
				
		if (token.kind == '}') {
			next();
			break;
		}
				
		if (token.kind == ';') {
			next();
			continue;
		}
				
		auto element = Definition::create();

		element->mutability = Mutability::constant;

		expect(Token_name);
		element->name = token.string;
		element->location = token.string;
		next();

		if (token.kind == '=') {
			next();
			element->initial_value = parse_expression();
		}
				
		for (auto other : Enum->block.definition_list) {
			if (element->name == other->name) {
				reporter.error(element->location, "Redefinition of enum value {}", element->name);
				reporter.info(other->location, "Previously defined here");
				yield(YieldResult::fail);
			}
		}
		Enum->block.add(element);

		expect({';', '\n'});
		next();
	}

	return {
		.Enum = finish_node(Enum),
		.name = name,
	};
}

bool Parser::fail_due_to_unseparated_ambiguous_expression(Expression *expression, String separator) {
	if (auto root_binary = as<Binary>(expression)) {
		if (is_ass(root_binary->operation)) {
			if (auto left_binary = as<Binary>(root_binary->left)) {
				if (could_be_unary(left_binary->operation)) {
					reporter.error(expression->location, "This failed to parse. You most likely didn't separate the condition from the body. Use `{}` or a new-line between them.", separator);

					utf8 *line_begin = expression->location.begin();
					while (1) {
						--line_begin;
						if (*line_begin == '\0' || *line_begin == '\n') {
							++line_begin;
							break;
						}
					}

					utf8 *line_end = expression->location.end();
					while (1) {
						if (*line_end == '\0' || *line_end == '\n') {
							break;
						}
						++line_end;
					}

					StringBuilder builder;
					append(builder, Span(line_begin, left_binary->left->location.end()));
					append(builder, ' ');
					append(builder, separator);
					append(builder, ' ');
					append(builder, left_binary->operation);
					append(builder, Span(left_binary->right->location.begin(), line_end));
					reporter.help("Use `{}` keyword like this:", separator);
					reporter.help("{}", to_string(builder));
					return false;
				}
			}
		}
	}
	return false;
}

bool Parser::is_valid_name_part(TokenKind kind) {
	switch (kind) {
		#define x(name) case Token_##name:
		ENUMERATE_CONCRETE_BUILTIN_TYPES(x)
		ENUMERATE_BUILTIN_STRUCTS(x)
		#undef x
		case Token_name:
			return true;
	}
	return false;
}

void Parser::ensure_signature_validity_for_operators(Definition *definition) {
	bool is_operator = definition->name == definition_name_for_implicit_as || definition->name == definition_name_for_explicit_as;

	if (is_operator) {
		if (definition->mutability != Mutability::constant) {
			reporter.error(definition->location, "Operator definitions must be constant");
			yield(YieldResult::fail);
		}

		auto lambda = definition->initial_value ? as<Lambda>(definition->initial_value) : 0;

		if (!lambda) {
			reporter.error(definition->location, "Operator definitions must have a lambda as initial value");
			yield(YieldResult::fail);
		}

		if (lambda->head.parameters_block.definition_list.count != 1) {
			reporter.error(definition->location, "Cast operators must accept exactly one argument");
			yield(YieldResult::fail);
		}

		if (!lambda->head.parsed_return_type) {
			reporter.error(lambda->head.location, "Cast operators must explicitly specify the return type");
			yield(YieldResult::fail);
		}
	}
}

void Parser::parse_name(String *location, String *name) {
	if (!is_valid_name_part(token.kind)) {
		reporter.error(token.string, "Expected a name, but got {}", token.kind);
		yield(YieldResult::fail);
	}

	*location = token.string;
	*name = token.string;

	next();

	auto can_be_merged = [&] {
		return is_valid_name_part(token.kind) && token.string.begin() - location->end() == 1;
	};

	if (can_be_merged()) {
		do {
			location->set_end(token.string.end());
			next();
		} while (can_be_merged());

		*name = *location;
	}
}

void add_definition_to_block(Definition *definition, Block *block) {
	scoped(LockIfBlockIsGlobal{block});
	block->definition_list.add(definition);
	block->definition_map.get_or_insert(definition->name).add(definition);
}

Expression *Parser::parse_expression(bool whitespace_is_skippable_before_binary_operator, u32 right_precedence) {
	//null denotation
	auto left = parse_expression_1();

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

// Parses parse_expression_0 plus member access, calls and subscripts.
Expression *Parser::parse_expression_1() {
	auto expression = parse_expression_0();
	
	while (1) {
		switch (token.kind) {
			case '.': {
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
				break;
			}
			case '(': {
				auto call = Call::create();
				call->callable = expression;
				call->location = expression->location;

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
				expression = call;
				break;
			}
			case '[': {
				auto subscript = Subscript::create();
				subscript->subscriptable = expression;
				subscript->location = expression->location;

				next();
				skip_lines();

				subscript->index = parse_expression();

				skip_lines();
				expect(']');

				subscript->location = {subscript->location.begin(), token.string.end()};

				next();
				expression = subscript;
				break;
			}
			default:
				return finish_node(expression);
		}
	}
}
// Parses single-part expressions
Expression *Parser::parse_expression_0() {
	bool parsed_use = false;
	switch (token.kind) {
		case Token_use: {
			next();
			parsed_use = true;

			goto parse_definition;
		}
		case Token_var:
		case Token_let:
		case Token_const: {
		parse_definition:
			auto definition = Definition::create();
			definition->mutability = to_mutability(token.kind).value();

			definition->container = current_container;
			definition->use = parsed_use;
			
			next();
			skip_lines();
			
			parse_name(&definition->location, &definition->name);

			if (definition->name == "file_size") {
				int x = 2;
			}

			skip_lines();

			expect({':', '='});

			if (token.kind == ':') {
				next();
				skip_lines();

				definition->parsed_type = parse_expression_1(); // NOTE: don't parse '='

				switch (definition->parsed_type->kind) {
					case NodeKind::Name:
					case NodeKind::BuiltinTypeName:
					case NodeKind::Unary:
					case NodeKind::ArrayType:
					case NodeKind::LambdaHead:
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
					link_constant_definition_to_initial_value(definition);
				}
			}
			
			ensure_signature_validity_for_operators(definition);

			add_definition_to_block(definition, current_block);

			return finish_node(definition);
		}
		case Token_fn: return parse_lambda().lambda_or_head;
		case '(': {
			auto open = token.string;

			next();
			skip_lines();

			auto expression = parse_expression();

			skip_lines();

			expect(')');
			auto close = token.string;
			next();

			expression->location = {open.begin(), close.end()};

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

				// :MULTIWORD BLOCK NAME:
				// Can't name a block with multiple words because theres no delimiter.
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

				block->children.add(child);
			}
			block->location = {block->location.begin(), token.string.end()};
			next();

			for (auto child : block->children.skip(-1)) {
				ensure_allowed_in_statement_context(child);
			}

			if (is_substitutable(block)) {
				//assert(block->children.count == 1);

				//if (auto definition = as<Definition>(block->children[0])) {
				//	if (definition->initial_value) {
				//		return finish_node(definition->initial_value);
				//	} else {
				//		reporter.error(definition->location, "What the hell is this?");
				//		yield(YieldResult::fail);
				//	}
				//}

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
			Array->parsed_element_type = parse_expression_0();
			Array->location = {Array->location.begin(), Array->parsed_element_type->location.end()};
			return finish_node(Array);
		}
		case '.': {
			auto dot_token = token.string;
			next();
			switch (token.kind) {
				case '[': {
					auto constructor = ArrayConstructor::create();
					constructor->location = dot_token;
					parse_list('[', ',', ']', {.call_next_after_finishing = false}, [&] {
						constructor->elements.add(parse_expression());
					});

					constructor->location = {constructor->location.begin(), token.string.end()};

					next();

					return finish_node(constructor);
				}
				case Token_name: {
					auto unary = Unary::create();
					auto name = Name::create();
					parse_name(&name->location, &name->name);
					unary->expression = name;
					unary->operation = UnaryOperation::dot;
					unary->location = {dot_token.begin(), name->location.end()};
					next();
					return finish_node(unary);
				}
				default: {
					goto unexpected_token;
				}
			}
		}
		case Token_none: {
			auto none = NoneLiteral::create();
			none->location = token.string;
			next();
			return finish_node(none);
		}

		#define x(name) case Token_##name:
		ENUMERATE_BUILTIN_STRUCTS(x)
		#undef x
		case Token_name: {
			auto name = Name::create();
			parse_name(&name->location, &name->name);
			if (name->name == "bytes_remaining") {
				int x = 4;
			}
			return finish_node(name);
		}
		case Token_number: {
			auto literal = IntegerLiteral::create();
			literal->location = token.string;
			literal->value = lexer.int_value;
			next();
			return finish_node(literal);
		}
		case Token_float: {
			auto literal = FloatLiteral::create();
			literal->location = token.string;
			literal->value = lexer.float_value;
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
			literal->value = copy(lexer.string_value);
			next();
			return finish_node(literal);
		}
		case Token_if: {
			auto If = IfExpression::create();
			If->location = token.string;
			next();
			skip_lines();

			If->condition = parse_expression();
					
			if (fail_due_to_unseparated_ambiguous_expression(If->condition, u8"then"s)) {
				yield(YieldResult::fail);
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
				auto &Case = match->cases.add();
				if (token.kind == Token_else) {
					next();
					skip_lines();
					expect('=>');
					next();
				} else {
					while (1) {
						Case.froms.add(parse_expression());
						skip_lines();
						if (token.kind == 'or') {
							next();
							skip_lines();
						} else if (token.kind == '=>') {
							Case.arrow_location = token.string;
							next();
							break;
						}
					}
				}

				skip_lines();

				Case.to = parse_statement();

				if (!Case.froms) {
					if (match->default_case) {
						reporter.error(Case.to->location, "Match expression can not have multiple default cases.");
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
			auto status = token.kind == Token_inline ? InlineStatus::always : InlineStatus::never;
			next();
			auto expr = parse_expression_1();
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
			return parse_struct().Struct;
		}
		case Token_enum: {
			return parse_enum().Enum;
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

				unop->expression = parse_expression_1();
				unop->location = {unop->location.begin(), unop->expression->location.end()};
				return finish_node(unop);
			}

		unexpected_token:
			reporter.error(token.string, "Unexpected token {} when parsing expression.", token.kind);
			// report_last_parsed_node();
			yield(YieldResult::fail);
		}
	}

	invalid_code_path("node was not returned");
}
Node *Parser::parse_statement() {
	InlineStatus inline_status = InlineStatus::unspecified;

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

			if (token.kind != '\n' && token.kind != ';') {
				return_->value = parse_expression();
			}

			return finish_node(return_);
		}
		case Token_while: {
			auto While = While::create();
			While->location = token.string;
			next();

			scoped_replace(current_loop, While);
				
			While->condition = parse_expression();

			skip_lines();
			if (token.kind == Token_then) {
				next();
				skip_lines();
			}

			While->body = parse_statement();

			return finish_node(While);
		}
		case Token_for: {
			auto For = ::For::create();
			For->location = token.string;
			next();
			
			expect(Token_name);
			String name_location;
			parse_name(&name_location, &For->it_name);

			if (token.kind == ':') {
				next();
				For->it_parsed_type = parse_expression();
			}

			expect(Token_in);
			next();

			// NOTE: `reverse` is contextual keyword.
			if (token.kind == Token_name && token.string == "reverse") {
				For->reverse = true;
				next();
			}

			For->range = parse_expression();

			skip_lines();
			if (token.kind == Token_then) {
				next();
				skip_lines();
			}

			For->body = parse_statement();

			return finish_node(For);
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

				// :MULTIWORD BLOCK NAME:
				// Multi-word names not supported here. Need a delimiter. E.g. `with` Like `break foo with 42`;
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
				extern_library = copy(lexer.string_value);
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

			auto condition = parse_expression();

			if (fail_due_to_unseparated_ambiguous_expression(condition, u8"then"s)) {
				yield(YieldResult::fail);
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
			import->path = copy(lexer.string_value);
				
			next();
	
			auto full_path = normalize_path(make_absolute_path(format(u8"{}\\import\\{}.sp", context_base->compiler_root_directory, import->path)));
			locked_use(imports) {
				imports.add_file({.path = full_path, .location = import->location});
			};

			return finish_node(import);
		}
		case Token_defer: {
			auto defer_ = Defer::create();
			defer_->location = token.string;

			next();
			skip_lines();

			defer_->body = parse_statement();

			return defer_;
		}
		case Token_inline: {
			next();
			expect(Token_fn);
			inline_status = InlineStatus::always;
			goto parse_lambda_label;
		}
		case Token_noinline: {
			next();
			expect(Token_fn);
			inline_status = InlineStatus::never;
			goto parse_lambda_label;
		}
		case Token_fn: {
		parse_lambda_label:
			auto parsed = parse_lambda();

			if (!parsed.name.count) {
				return parsed.lambda_or_head;
			}

			if (parsed.lambda_or_head->kind == NodeKind::LambdaHead) {
				reporter.error(parsed.lambda_or_head->location, "This is a type, but an actual lambda was expected.");
				reporter.help(parsed.lambda_or_head->location, "Sorry if this is confusing. Maybe you missed => after return type?");
				yield(YieldResult::fail);
			}
			assert(parsed.lambda_or_head->kind == NodeKind::Lambda);
			
			auto lambda = as<Lambda>(parsed.lambda_or_head);
			lambda->inline_status = inline_status;

			auto definition = Definition::create();
			definition->name = parsed.name;
			definition->initial_value = parsed.lambda_or_head;
			definition->location = parsed.name;
			definition->mutability = Mutability::constant;
			link_constant_definition_to_initial_value(definition);
			ensure_signature_validity_for_operators(definition);
			add_definition_to_block(definition, current_block);
			return definition;
		}
		case Token_struct: {
			auto parsed = parse_struct();
				
			if (!parsed.name.count) {
				return parsed.Struct;
			}

			auto definition = Definition::create();
			definition->name = parsed.name;
			definition->initial_value = parsed.Struct;
			definition->location = parsed.name;
			definition->mutability = Mutability::constant;
			link_constant_definition_to_initial_value(definition);
			add_definition_to_block(definition, current_block);
			return definition;
		}
		case Token_enum: {
			auto parsed = parse_enum();
				
			if (!parsed.name.count) {
				return parsed.Enum;
			}

			auto definition = Definition::create();
			definition->name = parsed.name;
			definition->initial_value = parsed.Enum;
			definition->location = parsed.name;
			definition->mutability = Mutability::constant;
			link_constant_definition_to_initial_value(definition);
			add_definition_to_block(definition, current_block);
			return definition;
		}
		case Token_use: {
			auto use = Use::create();

			use->location = token.string;
			next();

			String dummy;
			parse_name(&dummy, &use->name.name);

			return use;
		}
	}

	auto expression = parse_expression();

	return expression;
}
Node *Parser::parse_statement_with_attributes() {
	not_implemented();

	GList<Expression *> attributes;
	while (1) {
		if (token.kind == '#') {
			next();
			expect('[');
			next();
			attributes.add(parse_expression());
			expect(']');
			next();
			skip_lines();
		} else {
			break;
		}
	}

	auto result = parse_statement();

	if (!as<Statement>(result)) {

	}

	return result;
}

void Parser::yield(YieldResult result) {
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
void Parser::main() {
	scoped_replace(debug_current_location, {});
	
	token = lexer.next_token();
	all_tokens.add(token);

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

Node *Parser::parse_next_node() {
	result_node = 0;
	tl::yield(fiber);
	return result_node;
}

void Parser::link_constant_definition_to_initial_value(Definition *definition) {
	if (auto lambda = as<Lambda>(definition->initial_value)) {
		lambda->definition = definition;
		if ((lambda->is_extern || lambda->is_intrinsic) && !lambda->link_name) {
			lambda->link_name = definition->name;
		}
	} else if (auto Struct = as<::Struct>(definition->initial_value)) {
		Struct->definition = definition;
	} else if (auto Enum = as<::Enum>(definition->initial_value)) {
		Enum->definition = definition;
	}
}

void Parser::ensure_allowed_in_statement_context(Node *node) {
	String is_global = current_block == get_global_block_unprotected() ? u8"global "s : u8""s;

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
			if (current_block != get_global_block_unprotected()) {
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
bool Parser::next() {
	previous_token = token;
	token = lexer.next_token();
	all_tokens.add(token);
	debug_current_location = token.string;
	return token.kind != Token_eof;
}
void Parser::expect(std::underlying_type_t<TokenKind> expected_kind) {
	if (token.kind != expected_kind) {
		reporter.error(token.string, "Expected {}, but got {}", (TokenKind)expected_kind, token);
		yield(YieldResult::fail);
	}
}
void Parser::expect_not(std::underlying_type_t<TokenKind> unexpected_kind) {
	if (token.kind == unexpected_kind) {
		reporter.error(token.string, "Unexpected {}", (TokenKind)unexpected_kind);
		yield(YieldResult::fail);
	}
}
void Parser::expect(std::initializer_list<std::underlying_type_t<TokenKind>> expected_kinds) {
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
void Parser::expect_not(std::initializer_list<std::underlying_type_t<TokenKind>> unexpected_kinds) {
	for (auto unexpected_kind : unexpected_kinds) {
		expect_not(unexpected_kind);
	}
}
void Parser::skip_lines() {
	while (token.kind == '\n') {
		previous_token = token;
		token = lexer.next_token();
		all_tokens.add(token);
	}
	debug_current_location = token.string;
}

void Parser::init(String source) {
	this->parent_fiber = init_or_get_current_fiber();
	fiber = get_new_fiber();
	lexer = Lexer::create(source, &reporter, parent_fiber, fiber);
	set_start(fiber, [] (void *param) {
		((Parser *)param)->main();
	}, this);
}

void Parser::free() {
	if (context_base->print_tokens) {
		withs(context_base->stdout_mutex) {
			for (auto token : all_tokens) {
				println(token);
			}
		};
	}
	add_fiber_to_reuse(fiber);
	fiber = {};
}

bool read_file_and_parse_into_global_block(String import_location, String path) {
	timed_function();

	if (!file_exists(path)) {
		immediate_reporter.error(import_location, "File {} does not exist", path);
		return false;
	}

	// Will be used after function exits, don't free.
	auto source_buffer = read_entire_file(path, {.extra_space_before = LEXER_PADDING_SIZE, .extra_space_after = LEXER_PADDING_SIZE});

	auto source = (String)source_buffer.skip(LEXER_PADDING_SIZE).skip(-LEXER_PADDING_SIZE);
	
	locked_use_expr(content_start_to_file_name, context_base->content_start_to_file_name) {
		content_start_to_file_name.get_or_insert(source.data) = path;
	};

	bool success = parse_source(source, [&](Node *node) {
		locked_use_expr(global_block, context->global_block) {
			global_block.children.add(node);
		};
	});
	
	if (!success) {
		LOG_ERROR_PATH("Failed to parse this file: {}", path);
		return false;
	}

	return true;
}
