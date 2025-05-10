#define TYPECHECKER_GLOBAL
#include "typechecker.h"
#include "value.h"
#include "nodes.h"
#include "make_node.h"
#include "copier.h"
#include "visit.h"
#include "node_interpreter.h"
#include "parser.h"
#include "capitalized.h"
#include "nameable.h"
#include "builtin_structs.h"
#include "is_constant.h"
#include "is_mutable.h"
#include "do_all_paths_return.h"
#include "get_constant_value.h"
#include "compiler_context.h"

#define ENABLE_TYPECHECKER_REUSE 0

Typechecker *Typechecker::create(Node *node) {
	assert(node);

	Typechecker *typechecker = 0;
		
	#if ENABLE_TYPECHECKER_REUSE
	if (auto popped = locked_use_it(retired_typecheckers, it.pop())) {
		typechecker = popped.value();
		// immediate_reporter.info("created cached typechecker {} for node {}", typechecker->uid, node->location);

		assert(typechecker->debug_stopped);
		assert(typechecker->yield_result == YieldResult{});
		assert(typechecker->reporter.reports.count == 0);
		assert(typechecker->initial_node == 0);
		assert(typechecker->current_block == 0);
	} else
	#endif
	{
		typechecker = default_allocator.allocate<Typechecker>();
		typechecker->fiber = get_new_fiber();
		set_start(typechecker->fiber, [](void *param) {
			((Typechecker *)param)->fiber_main(); 
		}, typechecker);

		// immediate_reporter.info("created new typechecker {} for node {}", typechecker->uid, node->location);
	}

	typechecker->reporter.reports.clear();
	typechecker->current_block = &context->global_block.unprotected;
	typechecker->initial_node = node;
	typechecker->debug_stopped = true;
	return typechecker;
}

Typechecker::YieldResult Typechecker::continue_typechecking(TypecheckEntry *entry) {
	assert(debug_thread_id == 0);
	debug_thread_id = get_current_thread_id();
	defer { debug_thread_id = 0; };

	debug_start();

	{
		scoped_replace(this->parent_fiber, init_or_get_current_fiber());
		scoped_replace(this->entry, entry);

		tl::yield(fiber);
	}

	if (yield_result != YieldResult::wait) {
		locked_use(deferred_reports) {
			{
				scoped_replace(reporter.indentation, reporter.indentation + 1);
				for (auto frame : template_instantiation_stack_for_reports) {
					StringBuilder builder;
					append_format(builder, "While instantiating {} with ", frame.original_lambda->definition ? frame.original_lambda->definition->name : u8"unnamed lambda"s);
					for (auto definition : frame.instantiated_lambda->head.template_parameters_block.definition_list) {
						append_format(builder, "{} = {}; ", definition->name, definition->initial_value);
					}
					reporter.info(frame.original_lambda->location, "{}", to_string(builder));
				}
			}

			deferred_reports.add(reporter.reports);
		};
	}
	return yield_result;
}

void Typechecker::stop() {
	debug_stop();
}
void Typechecker::retire() {
	parent_fiber = {};
	yield_result = {};
	reporter.reports.clear();
	reporter.indentation = 0;
	initial_node = 0;
	current_block = 0;
	current_container = 0;
	current_loop = 0;
	node_stack.clear();
	entry = 0;
	main_loop_unwind_point = {};
	current_unwind_point = {};
	can_generate_vectorized_lambdas = false;
	template_instantiation_stack_for_reports.clear();
	fail_strategy = FailStrategy::yield;
	add_fiber_to_reuse(fiber);
	fiber = {};

	locked_use_it(retired_typecheckers, it.add(this));
}

void Typechecker::debug_start() {
	assert(debug_stopped, "attempt to start already started typechecker {}", uid);
	debug_stopped = false;
	// println("started typechecker {}", uid);
}
void Typechecker::debug_stop() {
	assert(!debug_stopped, "attempt to stop already stopped typechecker {}", uid);
	debug_stopped = true;
	// println("stopped typechecker {}", uid);
}

void Typechecker::fail_impl() {
	if (fail_strategy == FailStrategy::yield) {
		yield(YieldResult::fail);
	} else {
		jmp_buf zero = {};
		if (memcmp(&zero, &current_unwind_point, sizeof(jmp_buf)) == 0) {
			immediate_reporter.error(debug_current_location, "INTERNAL ERROR: current_unwind_point is zero");
			invalid_code_path();
		}
		longjmp(current_unwind_point.buf, fail_unwind_tag);
	}
}

#define fail()               \
	do {                     \
		fail_impl();         \
		invalid_code_path(); \
	} while (0)

void Typechecker::yield(Typechecker::YieldResult result) {
	yield_result = result;
	scoped_replace(debug_current_location, {});
	tl::yield_reuse(parent_fiber, fiber);
	if (result == YieldResult::fail) {
		longjmp(main_loop_unwind_point.buf, 1);
	}
}

void Typechecker::fiber_main() {
	while (1) {
		setjmp(main_loop_unwind_point.buf);
		assert(initial_node);
		can_generate_vectorized_lambdas = true;
		auto result = typecheck(initial_node, true);
		assert(result == initial_node);
		yield(YieldResult::success);
	}
}

bool Typechecker::implicitly_cast(Expression **_expression, Expression *target_type, Reporter *reporter, bool apply) {
	auto expression = *_expression;
	defer {
		*_expression = expression;
	};

	auto source_type = expression->type;
	auto direct_source_type = direct(source_type);
	auto direct_target_type = direct(target_type);

	// Equal types do not need implicit cast
	if (types_match(direct_source_type, direct_target_type)) {
		return true;
	}

	// Unsized integer to concrete
	if (types_match(direct_source_type, BuiltinType::UnsizedInteger)) {
		if (::is_concrete_integer(direct_target_type)) {
			if (apply) {
				propagate_concrete_type(expression, target_type);
			}
			return true;
		}
	}

	// Integer to Integer
	if (auto src_builtin_type = as<BuiltinTypeName>(direct_source_type)) {
		switch (src_builtin_type->type_kind) {
			case BuiltinType::U8:
			case BuiltinType::U16:
			case BuiltinType::U32:
			case BuiltinType::U64:
			case BuiltinType::S8:
			case BuiltinType::S16:
			case BuiltinType::S32:
			case BuiltinType::S64: {
				if (auto dst_builtin_type = as<BuiltinTypeName>(direct_target_type)) {
					switch (dst_builtin_type->type_kind) {
						case BuiltinType::U8:
						case BuiltinType::U16:
						case BuiltinType::U32:
						case BuiltinType::U64:
						case BuiltinType::S8:
						case BuiltinType::S16:
						case BuiltinType::S32:
						case BuiltinType::S64: {
							auto src_size = get_size(src_builtin_type->type_kind);
							auto dst_size = get_size(dst_builtin_type->type_kind);
							auto src_sign = get_sign(src_builtin_type->type_kind);
							auto dst_sign = get_sign(dst_builtin_type->type_kind);

							if (src_sign == dst_sign) {
								if (src_size <= dst_size) {
									if (apply)
										expression = make_cast(expression, target_type);
									return true;
								} else {
									if (reporter)
										reporter->error(expression->location, "Can't implicitly convert {} to {}, because source is bigger than destination, meaning that there could be information loss.", source_type, target_type);
									return false;
								}
							} else {
								if (src_size <= dst_size) {
									if (reporter)
										reporter->error(expression->location, "Can't implicitly convert {} to {}, because the signs don't match.", source_type, target_type);
									return false;
								} else {
									if (reporter)
										reporter->error(expression->location, "Can't implicitly convert {} to {}, because source is bigger than destination, meaning that there could be information loss, and the signs don't match.", source_type, target_type);
									return false;
								}
							}

							break;
						}
					}
				}
				break;
			}
		}
	}

	// Auto dereference
	if (auto pointer = as_pointer(direct_source_type)) {
		if (types_match(pointer->expression, direct_target_type)) {
			if (apply) {
				auto dereference = Unary::create();
				dereference->operation = UnaryOperation::dereference;
				dereference->expression = expression;
				dereference->location = expression->location;
				dereference->type = target_type;
				expression = dereference;
			}
			return true;
		}
	}

	// None -> pointer
	if (auto none = as<NoneLiteral>(expression)) {
		if (auto target_pointer = as_pointer(direct_target_type)) {
			if (apply) {
				expression = make_cast(expression, target_type);
			}
			return true;
		}
	}

	// Anything -> None
	if (types_match(direct_target_type, BuiltinType::None)) {
		if (apply) {
			expression = make_cast(expression, target_type);
		}
		return true;
	}

	// Pointer -> Pointer
	if (auto source_pointer = as_pointer(direct_source_type)) {
		if (auto target_pointer = as_pointer(direct_target_type)) {
			if (target_pointer->mutability == Mutability::readonly) {
				if (source_pointer->mutability == Mutability::variable) {
					if (apply) {
						expression = make_cast(expression, target_type);
					}
					return true;
				}
			}
		}
	}

	if (reporter) {
		reporter->error(expression->location, "Expression of type `{}` is not implicitly convertible to `{}`.", source_type, target_type);
		if (auto match = as<Match>(expression)) {
			if (!match->default_case) {
				reporter->help(expression->location, "This match has type None because there is no default case.");
			}
		}
	}
	return false;
}

void Typechecker::why_is_this_immutable(Expression *expr) {
	if (auto unary = as<Unary>(expr)) {
		if (unary->operation == UnaryOperation::dereference) {
			if (auto name = as<Name>(unary->expression)) {
				auto definition = name->definition();
				assert(definition);
				reporter.info(definition->location, "Because {} is a pointer to {}.", name->name, Meaning(definition->mutability));
				if (definition->initial_value) {
					why_is_this_immutable(definition->initial_value);
				}
			}
		} else if (unary->operation == UnaryOperation::addr) {
			if (auto name = as<Name>(unary->expression)) {
				auto definition = name->definition();
				assert(definition);
				reporter.info(definition->location, "Because {} is marked as {}. Mark it with `var` instead to make it mutable.", name->name, definition->mutability);
			}
		}
	} else if (auto name = as<Name>(expr)) {
		auto definition = name->definition();
		assert(definition);
		reporter.info(definition->location, "Because {} is {}.", name->name, Meaning(definition->mutability));
		why_is_this_immutable(definition->initial_value);
	}
}

Expression *Typechecker::inline_body(Call *call, Lambda *lambda) {
	if (!yield_while_null(call->location, &lambda->body->type)) {
		reporter.error(lambda->location, "Could not wait for lambda's body to typecheck for inlining");
		fail();
	}
		
	auto copied_lambda = as<Lambda>(Copier{}.deep_copy(lambda));
	NOTE_LEAK(copied_lambda);
	assert(copied_lambda);

	auto result_block = Block::create();
	result_block->location = call->location;

	assert(lambda->head.parameters_block.definition_list.count == call->arguments.count);
	for (umm i = 0; i < call->arguments.count; ++i) {
		auto argument = call->arguments[i];
		auto parameter = copied_lambda->head.parameters_block.definition_list[i];
		parameter->initial_value = argument.expression;
		parameter->is_parameter = false;
		parameter->container = current_container;
		result_block->add(parameter);
	}

	if (auto body_block = as<Block>(copied_lambda->body)) {
		result_block->tag = format(u8"_{}", result_block->uid);
		visit(body_block, Combine{
			[&](Node *) {},
			[&](Return *ret) -> Statement * {
				if (ret->lambda == copied_lambda) {
					auto Break = Break::create();
					Break->value = ret->value;
					Break->tag_block = result_block;
					Break->defers = ret->defers;
					ret->defers = {};
					result_block->breaks.add(Break);
					NOTE_LEAK(ret);
					return Break;
				}
				return ret;
			},
		});
	}
		
	result_block->add(copied_lambda->body);

	result_block->type = lambda->head.return_type;

	return result_block;
}

Node *Typechecker::get_last_child_recursive(Node *node) {
	while (node->kind == NodeKind::Block) {
		auto block = (Block *)node;
		if (block->children.count == 0) {
			return 0;
		}

		node = block->children.back();
	}
	return node;
}

Value Typechecker::execute(Node *node) {
	auto context = NodeInterpreter::create(node);
	while (true) {
		auto result = context->run();
		if (result.is_value()) {
			return result.value();
		}

		switch (result.error()) {
			case NodeInterpreter::YieldResult::fail: {
				reporter.error(node->location, "Failed to evaluate value.");
				yield(YieldResult::fail);
				break;
			}
			case NodeInterpreter::YieldResult::wait: {
				yield(YieldResult::wait);
				break;
			}
		}
	}
}

VectorizedLambda Typechecker::get_or_instantiate_vectorized_lambda(Lambda *original_lambda, u64 vector_size, String instantiation_location) {
	return locked_use(vectorized_lambdas) {
		VectorizedLambda vectorized = {};
		auto found = vectorized_lambdas.find({ original_lambda, vector_size });
		if (found) {
			vectorized = *found.value;
		} else {
			assert(original_lambda->definition, "Lambda requires a name to be vectorizable. Assign it to a definition.");

			String lambda_name = {};
			if (original_lambda->definition) {
				lambda_name = format(u8"__v_{}_{}"s, original_lambda->definition->name, vector_size);
			} else {
				auto location = get_source_location(original_lambda->location);
				lambda_name = format(u8"__v_{}_{}_{}_{}"s, Nameable(location.file), location.location_line_number, original_lambda->uid, vector_size);
			}

			StringBuilder source_builder;
			append_format(source_builder, "\0const {} = fn ("s, lambda_name);
			foreach (it, original_lambda->head.parameters_block.definition_list) {
				auto [i, parameter] = it.key_value();
				if (i) {
					append(source_builder, ", ");
				}

				append_format(source_builder, "{}: [{}]{{{}}}", parameter->name, vector_size, parameter->type);
			}
			append_format(source_builder, "): [{}]{{{}}} => {{\n"
				"	var i: S64\n"
				"	var c: [{}]{{{}}}\n"
				"	while i < {} {{\n"
				"		c[i] = {}("
				, vector_size, original_lambda->head.return_type, vector_size, original_lambda->head.return_type, vector_size, original_lambda->definition->name
			);
				
			foreach (it, original_lambda->head.parameters_block.definition_list) {
				auto [i, parameter] = it.key_value();
				if (i) {
					append(source_builder, ", ");
				}
				append_format(source_builder, "{}[i]", parameter->name);
			}
				
			append_format(source_builder, ")\n"
				//"		println(x[i])\n"
				//"		println(c[i])\n"
				"		i = i + 1\n"
				"	}}\n"
				"	c\n"
				"}}\n"
			);
			append(source_builder, '\0');

			auto source = (Span<utf8>)to_string(source_builder);
			source = source.subspan(1, source.count - 2);
				
			auto path = format(u8"{}\\{}.sp", context_base->generated_source_directory, lambda_name);

			auto &content_start_to_file_name = context_base->content_start_to_file_name;
			locked_use(content_start_to_file_name) {
				content_start_to_file_name.get_or_insert(source.data) = path;
			};

			Node *definition_node = 0;

			bool success = parse_source(source, [&](Node *node) {
				assert(!definition_node, "Only one node expected");
				definition_node = node;
				locked_use_expr(global_block, context->global_block) {
					global_block.add(node);
				};
			});

			can_generate_vectorized_lambdas = false;
			success &= with_unwind_strategy([&] {
				scoped_replace(current_block, &context->global_block.unprotected);
				scoped_replace(current_container, 0);
				scoped_replace(current_loop, 0);
				return typecheck(&definition_node);
			});
			can_generate_vectorized_lambdas = true;

			{
				with(temporary_storage_checkpoint);
				write_entire_file(path, as_bytes(source));

				if (!success) {
					immediate_reporter.error(instantiation_location, "INTERNAL ERROR: Failed to instantiate vectorized lambda for this operation. Generated source code is saved at {}", path);
					fail();
				}
			}
							
			vectorized.instantiated_definition = as<Definition>(definition_node);
			assert(vectorized.instantiated_definition);
			vectorized.instantiated_lambda = as<Lambda>(vectorized.instantiated_definition->initial_value);
			assert(vectorized.instantiated_lambda);
		}

		return vectorized;







		#if 0
		if (auto found = vectorized_lambdas.find({ original_lambda, vector_size })) {
			return *found.value;
		}

		auto original_param = original_lambda->head.parameters_block.definition_list[0];

		auto new_lambda_definition = Definition::create();
		auto new_lambda = Lambda::create();
		auto new_lambda_param = Definition::create();
		auto array = ArrayConstructor::create();

		new_lambda_param->name = original_param->name;
		new_lambda_param->type = make_array_type(original_param->type, vector_size);
		new_lambda_param->mutability = original_param->mutability;
		new_lambda_param->container = new_lambda;
		new_lambda_param->is_parameter = true;
		new_lambda_param->location = original_param->location;

		array->location = original_lambda->location;
		array->elements.reserve(vector_size);
		for (umm i = 0; i < vector_size; ++i) {
			auto index = IntegerLiteral::create();
			index->value = i;
			index->type = get_builtin_type(BuiltinType::U64);

			auto param_name = Name::create();
			param_name->location = original_lambda->location;
			param_name->name = new_lambda_param->name;
			param_name->possible_definitions.set(new_lambda_param);
			param_name->type = new_lambda_param->type;

			auto subscript = Subscript::create();
			subscript->location = original_lambda->location;
			subscript->subscriptable = param_name;
			subscript->index = index;
			subscript->type = original_param->type;

			auto callable = Name::create();
			callable->location = original_lambda->location;
			assert(original_lambda->definition);
			callable->name = original_lambda->definition->name;
			callable->possible_definitions.set(original_lambda->definition);
			callable->type = original_lambda->definition->type;

			auto call = Call::create();
			call->location = original_lambda->location;
			call->callable = callable;
			call->arguments.set({
				.expression = subscript,
				.parameter = new_lambda_param,
			});
			call->call_kind = CallKind::lambda;

			array->elements.add(call);
		}
		array->type = make_array_type(original_param->type, vector_size);

		new_lambda->head.parameters_block.add(new_lambda_param);
		new_lambda->head.return_type = array->type;
		new_lambda->head.type = get_builtin_type(BuiltinType::Type);
		new_lambda->definition = new_lambda_definition;
		new_lambda->location = original_lambda->location;
		new_lambda->type = &new_lambda->head;
		new_lambda->body = array;

		new_lambda_definition->initial_value = new_lambda;
		if (original_lambda->definition) {
			new_lambda_definition->name = format(u8"__v{}_{}", vector_size, original_lambda->definition->name);
		} else {
			new_lambda_definition->name = format(u8"__v{}_{}", vector_size, original_lambda->uid);
		}
		new_lambda_definition->location = original_lambda->location;
		new_lambda_definition->mutability = Mutability::constant;
		new_lambda_definition->type = new_lambda->type;

		withs(context->global_block_lock) {
			context->global_block.add(new_lambda_definition);
		};

		vectorized_lambdas.insert({ original_lambda, vector_size }, new_lambda_definition);
		return new_lambda_definition;
		#endif
	};
}

void Typechecker::sort_arguments(GList<Call::Argument> &arguments, GList<Definition *> &parameters, String call_location, Node *lambda_head_or_struct, Definition *lambda_or_struct_definition, SortArgumentOptions options) {
	scoped(temporary_storage_checkpoint);
	List<Call::Argument, TemporaryAllocator> sorted_arguments;
	sorted_arguments.resize(parameters.count);

	String definition_element_name = u8"parameter"s;
	NodeKind lambda_or_struct_kind = lambda_head_or_struct->kind;
	switch (lambda_head_or_struct->kind) {
		case NodeKind::LambdaHead: 
			definition_element_name = u8"parameter"s; 
			lambda_or_struct_kind = NodeKind::Lambda;
			break;
		case NodeKind::Struct: 
			definition_element_name = u8"member"s; 
			lambda_or_struct_kind = NodeKind::Struct;
			break;
	}

	for (umm i = 0; i < arguments.count; ++i) {
		auto &argument = arguments[i];
		if (argument.name.count) {
			auto parameter_index = find_index_of_if(parameters, [&](Definition *parameter) { return parameter->name == argument.name; });
			if (parameter_index >= parameters.count) {
				if (lambda_or_struct_definition) {
					reporter.error(argument.name, "{} \"{}\" does not have {} named {}", lambda_or_struct_kind, lambda_or_struct_definition->name, definition_element_name, argument.name);
				} else {
					reporter.error(argument.name, "{} does not have {} named {}", lambda_or_struct_kind, definition_element_name, argument.name);
				}
				reporter.info(lambda_head_or_struct->location, "Here's the {} list:", definition_element_name);
				fail();
			}

			auto parameter = parameters[parameter_index];
			argument.parameter = parameter;

			if (sorted_arguments[parameter_index].expression) {
				reporter.error(argument.name, "{} \"{}\" was already assigned", Capitalized{definition_element_name}, argument.name);
				reporter.info(sorted_arguments[parameter_index].name, "Here is first assignment:");
				reporter.info(lambda_head_or_struct->location, "Here's the {} list:", definition_element_name);
				fail();
			}
			sorted_arguments[parameter_index] = argument;
		} else {
			auto found_index = find_index_of_if(sorted_arguments, [&](Call::Argument arg) { return arg.expression == 0; });
			if (found_index >= sorted_arguments.count) {
				reporter.error(call_location, "Too many arguments. Expected {}, but got {}.", parameters.count, arguments.count);
				reporter.info(lambda_head_or_struct->location, "Here's the {} list:", definition_element_name);
				fail();
			}
			argument.parameter = parameters[found_index];
			sorted_arguments[found_index] = argument;
		}
	}

	for (umm i = 0; i < sorted_arguments.count; ++i) {
		auto &argument = sorted_arguments[i];
		auto &parameter = parameters[i];

		if (!argument.expression) {
			if (parameter->initial_value) {
				argument.expression = Copier{}.deep_copy(parameter->initial_value);
			}
		}

		if (!options.allow_missing) {
			if (!argument.expression) {
				reporter.error(call_location, "Too few arguments. Value for {} was not provided.", parameter->name);
				reporter.info(lambda_head_or_struct->location, "Here's the {} list:", definition_element_name);
				fail();
			}
		}
	}

	arguments.set(sorted_arguments);
}
	
bool Typechecker::match_one_template_parameter_type(Type expression_type, Type parameter_type, Block *template_parameters) {
	if (auto pname = as<Name>(parameter_type)) {
		// :performance: linear search
		if (auto found = find(template_parameters->definition_list, pname->definition())) {
			auto template_parameter = *found;
			template_parameter->initial_value = expression_type;
			template_parameter->type = expression_type->type;
			template_parameter->constant_value = Value(Type(expression_type));
			return true;
		}
	}

	return false;
}

Expression *Typechecker::instantiate_lambda_template(Call *original_call, Lambda *original_lambda) {
	// Keep passed in Call unmodified
	auto call = Copier{}.deep_copy(original_call);
	auto new_callable_name = Name::create();
	auto instantiated_lambda_definition = Definition::create();
	auto instantiated_lambda = Copier{}.deep_copy(original_lambda);

	auto &arguments = call->arguments;
	auto &parameters = instantiated_lambda->head.parameters_block.definition_list;
	auto &template_parameters = instantiated_lambda->head.template_parameters_block.definition_list;
		
	instantiated_lambda->definition = instantiated_lambda_definition;
	instantiated_lambda->head.is_template = false;
		
	// Link names in parameters_block to definitions in template_parameters_block to match them later.
	typecheck(instantiated_lambda->head.parameters_block);

	sort_arguments(arguments, parameters, call->location, &instantiated_lambda->head, instantiated_lambda->definition);

	for (umm i = 0; i < arguments.count; ++i) {
		auto &argument = arguments[i];
		auto &parameter = parameters[i];
		make_concrete(argument.expression);
		match_one_template_parameter_type(argument.expression->type, parameter->type, &instantiated_lambda->head.template_parameters_block);
	}

	// Matching is done


	template_instantiation_stack_for_reports.add({
		.original_lambda = original_lambda,
		.instantiated_lambda = instantiated_lambda,
		.template_parameters = template_parameters,
	});

	// Retypecheck.
	instantiated_lambda->head.type = 0;
	instantiated_lambda->type = 0;
	call->type = 0;
	visit(&instantiated_lambda->head.parameters_block, Combine {
		[](Node *node) {},
		[](Expression *expr) {
			expr->type = 0;
		},
	});

	typecheck(&instantiated_lambda);
		
	template_instantiation_stack_for_reports.pop();

	StringBuilder name_builder;
	defer { free(name_builder); };
	append(name_builder, "__");
	append(name_builder, original_lambda->definition ? original_lambda->definition->name : to_string(original_lambda->uid));
	for (auto template_parameter : template_parameters) {
		append(name_builder, "_");
		append(name_builder, Nameable{template_parameter->initial_value});
	}

	instantiated_lambda_definition->location = original_lambda->location;
	instantiated_lambda_definition->mutability = Mutability::constant;
	instantiated_lambda_definition->initial_value = instantiated_lambda;
	instantiated_lambda_definition->name = (String)to_string(name_builder);
	instantiated_lambda_definition->type = &instantiated_lambda->head;
	locked_use_expr(global_block, context->global_block) {
		global_block.add(instantiated_lambda_definition);
	};

	new_callable_name->name = instantiated_lambda_definition->name;
	new_callable_name->possible_definitions.set(instantiated_lambda_definition);
	new_callable_name->type = instantiated_lambda_definition->type;
	call->callable = new_callable_name;

	return typecheck_lambda_call(call, instantiated_lambda, &instantiated_lambda->head, true);
}

bool Typechecker::should_inline(Call *call, Lambda *lambda) {
	if (call->inline_status != InlineStatus::unspecified) {
		return call->inline_status == InlineStatus::always;
	}
		
	if (lambda->inline_status != InlineStatus::unspecified) {
		return lambda->inline_status == InlineStatus::always;
	}

	if (context_base->should_inline_unspecified_lambdas) {
		return !as<Block>(lambda->body);
	}

	return false;
}
	
Struct *Typechecker::get_struct_template_instantiation(Struct *template_struct, Expression *argument) {
	assert(template_struct->template_parameters_block.definition_list.count == 1, "Not implemented");

	auto instantiated_struct = Copier{}.deep_copy(template_struct);

	instantiated_struct->is_template = false;
	instantiated_struct->template_parameters_block.definition_list[0]->initial_value = argument;

	return instantiated_struct;
}

Expression *Typechecker::typecheck_lambda_call(Call *call, Lambda *lambda, LambdaHead *head, bool apply) {

	auto &arguments = call->arguments;
	auto &callable = call->callable;

	if (lambda && lambda->head.is_template) {
		immediate_reporter.warning(lambda->location, "TODO: implement template instantiation caching");
		return instantiate_lambda_template(call, lambda);
	}

	if (!yield_while_null(call->location, &head->return_type)) {
		reporter.error(call->location, "INTERNAL ERROR: Lambda `{}` was not properly typechecked. Its return type is not set.", call->callable->location);
		reporter.info(head->location, "That lambda is here:");
		fail();
	}

	auto &parameters = head->parameters_block.definition_list;

	sort_arguments(arguments, parameters, call->location, head, lambda ? lambda->definition : 0);

	if (can_generate_vectorized_lambdas) {
		if (lambda) {
			if (arguments.count == 1) {
				if (auto array = as<ArrayType>(arguments[0].expression->type)) {
					if (types_match(array->element_type, parameters[0]->type)) {
						auto vectorized_lambda = get_or_instantiate_vectorized_lambda(lambda, array->count.value(), call->location);

						auto name = Name::create();
						name->location = callable->location;
						name->name = vectorized_lambda.instantiated_definition->name;
						name->possible_definitions.set(vectorized_lambda.instantiated_definition);
						name->type = vectorized_lambda.instantiated_definition->type;

						callable = name;
						lambda = vectorized_lambda.instantiated_lambda;
						head = &lambda->head;
					}
				}
			}
		}
	}


	for (umm i = 0; i < arguments.count; ++i) {
		auto &argument = arguments[i];
		auto &parameter = head->parameters_block.definition_list[i];

		argument.parameter = parameter;

		if (!implicitly_cast(&argument.expression, parameter->type, &reporter, apply)) {
			reporter.info(parameter->location, "Parameter declared here:");
			fail();
		}
	}

	if (apply) {
		call->call_kind = CallKind::lambda;
		call->type = head->return_type;
	}

	if (lambda) {
		if (apply && should_inline(call, lambda)) {
			return inline_body(call, lambda);
		} else {
			return call;
		}
	}
	return call;
};
Expression *Typechecker::typecheck_constructor(Call *call, Struct *Struct) {
	call->call_kind = CallKind::constructor;

	auto &arguments = call->arguments;
	auto &members = Struct->members;

	sort_arguments(arguments, members, call->location, Struct, Struct->definition, {.allow_missing = true});

	for (umm i = 0; i < arguments.count; ++i) {
		auto &argument = arguments[i];
		auto &member = members[i];

		argument.parameter = member;

		if (argument.expression) {
			if (!implicitly_cast(&argument.expression, member->type, true)) {
				fail();
			}
		} else {
			if (Struct->must_be_fully_initialized) {
				reporter.error(call->location, "Member `{}` must be initialized", member->name);
				reporter.info(Struct->location, "Struct marked with #must_be_fully_initialized");
				fail();
			}
			argument.expression = ZeroInitialized::create();
			argument.expression->type = member->type;
			argument.expression->location = call->location;
		}
	}

	call->type = call->callable;
	return call;
};

Expression *Typechecker::typecheck_binary_dot(Binary *binary, Reporter &reporter) {
	typecheck(&binary->left);
	auto struct_ = direct_as<Struct>(binary->left->type);
	if (!struct_) {
		if (auto pointer = direct_as<Unary>(binary->left->type); pointer && pointer->operation == UnaryOperation::pointer) {
			struct_ = direct_as<Struct>(pointer->expression);
		}
	}

	if (!struct_) {
		if (types_match(binary->left->type, context->builtin_structs.String)) {
			auto member_name = as<Name>(binary->right);
			if (!member_name) {
				reporter.error(binary->right->location, "Expression after dot must be a name.");
				fail();
			}

			if (member_name->name == "data") {
				auto type = make_pointer(make_name(BuiltinType::U8, member_name->location), Mutability::variable);
				member_name->type = type;
				return make_cast(binary->left, type);
			} else if (member_name->name == "count") {
				auto type = make_name(BuiltinType::U64, member_name->location);
				member_name->type = type;
				return make_cast(binary->left, type);
			} else {
				reporter.error(binary->right->location, "Type `String` does not have member named `{}`.", member_name->name);
				fail();
			}
		}
	}

	if (!struct_) {
		reporter.error(binary->left->location, "Left of the dot must have struct or pointer to struct type.");
		fail();
	}

	auto member_name = as<Name>(binary->right);
	if (!member_name) {
		reporter.error(binary->right->location, "Expression after dot must be a name.");
		fail();
	}

	if (auto found = find_if(struct_->members, [&](auto member) { return member->name == member_name->name; })) {
		auto definition = *found;
		member_name->possible_definitions.set(definition);
		member_name->type = definition->type;
		binary->type = definition->type;
	} else {
		reporter.error(binary->right->location, "Struct {} does not contain member named {}.", struct_, member_name->name);
		fail();
	}

	return binary;
}

bool Typechecker::ensure_not_overloaded(Name *name) {
	if (name->possible_definitions.count > 1) {
		reporter.error(name->location, "`{}` was declared multiple times and is ambiguous.", name->name);
		for (umm i = 0; i < name->possible_definitions.count; ++i) {
			auto definition = name->possible_definitions[i];
			reporter.info(definition->location, "Definition #{}:", i);
		}
		fail();
	}
	return true;
}
	
bool Typechecker::ensure_not_overloaded(Expression *expression) {
	if (auto name = as<Name>(expression)) {
		return ensure_not_overloaded(name);
	}
	return true;
}

void Typechecker::add_defers(GList<Defer *> &defers) {
	auto lambda = as<Lambda>(current_container);
	assert(lambda);
	for (auto block = current_block; block && block->container == lambda; block = block->parent) {
		for (auto Defer : reversed(block->defers)) {
			defers.add(Defer);
		}
	}
}

Node             *Typechecker::typecheck(Node *node, bool can_substitute) {
	++progress;
	defer { ++progress; };

	//if (node->location == "-1 / 2")
	//	debug_break();

	if (auto expression = as<Expression>(node)) {
		if (expression->type) {
			return expression;
		}
	}

	scoped_replace(debug_current_location, node->location);

	node_stack.add(node);
	defer { node_stack.pop(); };

	Node *new_node = 0;

	switch (node->kind) {
#define x(name) case NodeKind::name: new_node = typecheck_impl((name *)node, can_substitute); break;
		ENUMERATE_NODE_KIND(x)
#undef x
		default:
			invalid_code_path();
	}

	if (fail_strategy != FailStrategy::unwind) {
		assert(new_node);

		if (!can_substitute) {
			assert(new_node == node, "Attempt to substitute a node which can't be substituted.");
		}

		if (auto expression = as<Expression>(new_node)) {
			if (!expression->type) {
				reporter.error(expression->location, "Could not compute the type of this expression.");
				fail();
			}
			if (!expression->type->type) {
				reporter.error(expression->location, "Type of type of this expression was not computed.");
				fail();
			}
		}
	}

	return new_node;
}
Expression       *Typechecker::typecheck_impl(Block *block, bool can_substitute) {
	scoped_replace(current_block, block);
	for (auto &old_child : block->children) {
		auto new_child = typecheck(old_child, true);

		// A child was substituted with a different node. Update `children` with new node.
		// No need to update `definition_list` and `definition_map` because definition nodes should not be replaced.
		if (old_child != new_child) {
			assert(old_child->kind != NodeKind::Definition, "Attempt to replace definition in a block with a different node.");
			old_child = new_child;
		}
	}

	if (block->children.count) {
		if (auto last_expression = as<Expression>(block->children.back())) {
			block->type = last_expression->type;
			if (can_substitute && is_substitutable(block)) {
				NOTE_LEAK(block);
				return last_expression;
			}
		}
	}

	if (block->breaks.count) {
		List<Expression **> break_values;


		for (auto &Break : block->breaks) {
			if (Break->value) {
				break_values.add(&Break->value);
			}
		}

		if (as<Expression>(block->children.back())) {
			break_values.add((Expression **)&block->children.back());
		}

		if (break_values.count != 0) {
			List<Expression **> concrete_break_values;
			List<Expression **> inconcrete_break_values;

			for (auto &return_value : break_values) {
				if (is_concrete((*return_value)->type)) {
					concrete_break_values.add(return_value);
				} else {
					inconcrete_break_values.add(return_value);
				}
			}

			List<Expression **> break_values_to_cast;
			Expression *picked_value = 0;

			if (inconcrete_break_values.count == 0) {
				picked_value = *concrete_break_values[0];
				break_values_to_cast.set(concrete_break_values.skip(1));
			} else if (concrete_break_values.count == 0) {
				picked_value = *inconcrete_break_values[0];
				make_concrete(picked_value);
				break_values_to_cast.set(inconcrete_break_values.skip(1));
			} else {
				picked_value = *concrete_break_values[0];
				break_values_to_cast.set(concrete_break_values.skip(1));
				break_values_to_cast.add(inconcrete_break_values);
			}

			for (auto other : break_values_to_cast) {
				Reporter cast_reporter;
				if (!implicitly_cast(other, picked_value->type, &cast_reporter, true)) {
					reporter.error((*other)->location, "Value of type {} can't be converted to {}", (*other)->type, picked_value->type);
					reporter.info(get_last_child_recursive(picked_value)->location, "Block's type {} was deduced from this expression:", picked_value->type);
					reporter.info("Here's the conversion attempt report:");
					reporter.reports.add(cast_reporter.reports);
					fail();
				}
			}

			for (auto &Break : block->breaks) {
				if (Break->value) {
					assert(types_match(Break->value->type, picked_value->type));
				}
			}

			block->type = picked_value->type;
		}
	}

	if (!block->type) {
		block->type = get_builtin_type(BuiltinType::None);
	}

	return block;
}
Definition       *Typechecker::typecheck_impl(Definition *definition, bool can_substitute) {
	if (definition->parsed_type) {
		typecheck(&definition->parsed_type);
	} else {
		assert(definition->initial_value);
	}

	if (definition->initial_value) {
		typecheck(&definition->initial_value);

		if (definition->mutability == Mutability::constant) {

			if (definition->parsed_type) {
				if (!implicitly_cast(&definition->initial_value, definition->parsed_type, true)) {
					fail();
				}
			}

			// 
			//if (auto builtin_type = direct_as<BuiltinTypeName>(definition->type)) {
			//	switch (builtin_type->type_kind) {
			//		case BuiltinType::S8: definition->constant_value = Value((s8)definition->constant_value.value().S8); break;
			//	}
			//}
		} else {
			if (definition->parsed_type) {
				if (!implicitly_cast(&definition->initial_value, definition->parsed_type, true)) {
					fail();
				}
			} else {
				make_concrete(definition->initial_value);
			}
		}
			
		if (current_block == &context->global_block.unprotected || definition->mutability == Mutability::constant) {
			auto constant_check = is_constant(definition->initial_value);
			if (!constant_check) {
				if (definition->mutability == Mutability::constant) {
					reporter.error(definition->location, "Initial value is not constant.");
				} else {
					reporter.error(definition->location, "Initial value of global variables must be constant.");
				}
				reporter.info(constant_check.error()->location, "Because this is not constant.");
				fail();
			}

			// NOTE:
			// Maybe this should not be an error, I just don't wanna deal with it rigt now.
			if (types_match(definition->initial_value->type, BuiltinType::None)) {
				reporter.error(definition->location, "Definitions with type None can't exist.");
				fail();
			}

			bool should_execute = true;

			if (auto Struct_ = as<Struct>(definition->initial_value)) {
				if (Struct_->is_template) {
					should_execute = false;
				}
			}

			if (should_execute) {
				definition->constant_value = execute(definition->initial_value);
			}
		}
	} else {
		if (definition->is_parameter) {
			// Parameters can be immutable and have no initial expression
		} else if (definition->is_template_parameter) {
			// Template parameters are constant and may have no initial expression
		} else if (definition->container && definition->container->kind == NodeKind::Struct) {
			// Struct members can be immutable and have no initial expression
		} else {
			if (definition->mutability != Mutability::variable) {
				reporter.error(definition->location, "You can't omit initialization of immutable definitions.");
				fail();
			}
		}
	}

	// NOTE: definition->type might be set by lambda
	if (!definition->type) {
		if (definition->parsed_type) {
			definition->type = definition->parsed_type;
		} else {
			definition->type = definition->initial_value->type;
		}
	}

	if (!current_container || !as<Struct>(current_container)) {
		assert(find(current_block->children, definition));
		assert(find(current_block->definition_list, definition));
		assert(current_block->definition_map.find(definition->name));
	}

	return definition;
}
IntegerLiteral   *Typechecker::typecheck_impl(IntegerLiteral *literal, bool can_substitute) {
	literal->type = get_builtin_type(BuiltinType::UnsizedInteger);
	return literal;
}
BooleanLiteral   *Typechecker::typecheck_impl(BooleanLiteral *literal, bool can_substitute) {
	literal->type = get_builtin_type(BuiltinType::Bool);
	return literal;
}
NoneLiteral      *Typechecker::typecheck_impl(NoneLiteral *literal, bool can_substitute) {
	literal->type = get_builtin_type(BuiltinType::None);
	return literal;
}
StringLiteral    *Typechecker::typecheck_impl(StringLiteral *literal, bool can_substitute) {
	literal->type = make_name(context->builtin_structs.String->definition);
	return literal;
}
LambdaHead       *Typechecker::typecheck_impl(LambdaHead *head, bool can_substitute) {
	if (head->is_template) {
		typecheck(head->template_parameters_block);
	} else {
		scoped_replace(current_block, &head->template_parameters_block);
		typecheck(head->parameters_block);
		
		u64 total_parameters_size = 0;
		for (auto parameter : head->parameters_block.definition_list) {
			parameter->offset = total_parameters_size;
			auto parameter_size = get_size(parameter->type);
			parameter_size = max((u64)1, parameter_size);
			total_parameters_size += parameter_size;
			total_parameters_size = ceil(total_parameters_size, (u64)8);
		}
		head->total_parameters_size = total_parameters_size;

		if (head->parsed_return_type) {
			typecheck(&head->parsed_return_type);
			head->return_type = head->parsed_return_type;
		}
	}
	head->type = get_builtin_type(BuiltinType::Type);
	return head;
}
Lambda           *Typechecker::typecheck_impl(Lambda *lambda, bool can_substitute) {
	if (lambda->is_intrinsic) {
		if (lambda->inline_status != InlineStatus::unspecified) {
			reporter.warning(lambda->location, "Inline specifiers for intrinsic lambda are meaningless.");
		}
	}
	typecheck(lambda->head);

	scoped_replace(current_block, &lambda->head.template_parameters_block);
	if (lambda->head.is_template) {
		lambda->type = get_builtin_type(BuiltinType::Template);
	} else {
		lambda->type = &lambda->head;

		if (lambda->head.return_type) {
			if (lambda->definition) {
				lambda->definition->type = lambda->type;
			}
		}

		bool all_paths_return = false;

		if (lambda->body) {
			scoped_replace(current_container, lambda);
			scoped_replace(current_block, &lambda->head.parameters_block);

			typecheck(&lambda->body);

			all_paths_return = do_all_paths_return(lambda->body);
		}

		if (lambda->head.return_type) {
			for (auto ret : lambda->returns) {
				if (!implicitly_cast(&ret->value, lambda->head.return_type, true)) {
					reporter.info(lambda->head.return_type->location, "Return type specified here:");
					fail();
				}
			}

			if (!all_paths_return) {
				if (lambda->body) {
					if (!implicitly_cast(&lambda->body, lambda->head.return_type, true)) {
						fail();
					}
				}
			}
		} else {
			if (lambda->body) {
				List<Expression **> return_values;
				List<Return *> empty_returns;

				for (auto &ret : lambda->returns) {
					String mixed_return_value_presence_location = {};
					if (ret->value) {
						return_values.add(&ret->value);
						if (empty_returns.count) {
							mixed_return_value_presence_location = empty_returns.back()->location;
						}
					} else {
						empty_returns.add(ret);
						if (return_values.count) {
							mixed_return_value_presence_location = (*return_values.back())->location;
						}
					}
					if (mixed_return_value_presence_location.count) {
						reporter.error(ret->location, "Right now you are not allowed to mix return statements with values and without.");
						reporter.info(mixed_return_value_presence_location, "Here's the other return statement:");
						fail();
					}
				}

				if (!types_match(lambda->body->type, BuiltinType::None))
					return_values.add(&lambda->body);

				if (return_values.count == 0) {
					lambda->head.return_type = get_builtin_type(BuiltinType::None);
				} else {
					List<Expression **> concrete_return_values;
					List<Expression **> inconcrete_return_values;

					for (auto &return_value : return_values) {
						if (is_concrete((*return_value)->type)) {
							concrete_return_values.add(return_value);
						} else {
							inconcrete_return_values.add(return_value);
						}
					}

					List<Expression **> return_values_to_cast;
					Expression *picked_value = 0;

					if (inconcrete_return_values.count == 0) {
						picked_value = *concrete_return_values[0];
						return_values_to_cast.set(concrete_return_values.skip(1));
					} else if (concrete_return_values.count == 0) {
						picked_value = *inconcrete_return_values[0];
						make_concrete(picked_value);
						return_values_to_cast.set(inconcrete_return_values.skip(1));
					} else {
						picked_value = *concrete_return_values[0];
						return_values_to_cast.set(concrete_return_values.skip(1));
						return_values_to_cast.add(inconcrete_return_values);
					}

					for (auto other : return_values_to_cast) {
						Reporter cast_reporter;
						if (!implicitly_cast(other, picked_value->type, &cast_reporter, true)) {
							reporter.error((*other)->location, "Return value of type {} can't be converted to {}", (*other)->type, picked_value->type);
							reporter.info(get_last_child_recursive(picked_value)->location, "Return type {} was deduced from this expression:", picked_value->type);
							reporter.info("Here's the conversion attempt report:");
							reporter.reports.add(cast_reporter.reports);
							fail();
						}
					}

					if (lambda->returns.count && lambda->returns.count == empty_returns.count) {
						if (!types_match(lambda->body->type, BuiltinType::None)) {
							reporter.error(lambda->location, "All return statements in this lambda do not provide a value, but lambda's body has a type {}", lambda->body->type);
							reporter.info(get_last_child_recursive(lambda->body)->location, "Here's the expression that is implicitly returned");
							fail();
						}
					}

					if (!all_paths_return) {
						if (!types_match(lambda->body->type, picked_value->type)) {
							reporter.error(lambda->location, "Not all paths return a value.");
							fail();
						}
					}
					for (auto &ret : lambda->returns) {
						if (ret->value) {
							assert(types_match(ret->value->type, picked_value->type));
						}
					}

					lambda->head.return_type = picked_value->type;
				}

				/*
				auto body_type = lambda->body->type;
				if (lambda->returns.count) {
					List<Expression *> concrete_return_types;
					List<Return *> empty_returns;

					for (auto ret : lambda->returns) {
						if (!ret->value) {
							empty_returns.add(ret);
						} else {
							if (is_concrete(ret->value->type)) {
								concrete_return_types.add(ret->value->type);
							}
						}
					}

					if (empty_returns.count > lambda->returns.count) {
						reporter.error(empty_returns[0]->location, "TODO: Using both valued and empty return statement in a single function is not yet implemented.");
						fail();
					}

					if (concrete_return_types.count) {
						lambda->head.return_type = concrete_return_types[0];
					} else if (empty_returns.count) {
						lambda->head.return_type = get_builtin_type(BuiltinType::None);
					} else {
						make_concrete(lambda->returns[0]->value);
						for (auto ret : lambda->returns.skip(1)) {
							if (!implicitly_cast(&ret->value, lambda->returns[0]->value->type, true)) {
								fail();
							}
						}
						lambda->head.return_type = lambda->returns[0]->value->type;
					}

					for (auto ret : lambda->returns) {
						if (ret->value) {
							if (!types_match(ret->value->type, lambda->head.return_type)) {
								reporter.error(ret->location, "Type {} does not match previously deduced return type {}.", ret->value->type, lambda->head.return_type);
								reporter.info(lambda->returns[0]->location, "First deduced here:");
								fail();
							}
						}
					}
				} else {
					make_concrete(lambda->body);
					lambda->head.return_type = lambda->body->type;
				}
				*/
			} else {
				lambda->head.return_type = get_builtin_type(BuiltinType::None);
			}
		}

		assert(lambda->head.return_type);
	}
	return lambda;
}
Expression       *Typechecker::typecheck_impl(Name *name, bool can_substitute) {
	name->possible_definitions.clear();

	for (auto block = current_block; block; block = block->parent) {
		if (auto found_definitions = block->definition_map.find(name->name)) {
			auto definitions = *found_definitions.value;
				
			if (definitions.count == 0) {
				continue;
			}

			for (auto definition : definitions) {
				auto definition_index = find_index_of(block->children, definition);
				assert_less(definition_index, block->children.count);

				if (block->container && as<Lambda>(block->container)) {
					// Find our parent node in found definition's block
					for (auto node : reversed(node_stack)) {
						auto parent_index = find_index_of(block->children, node);
						if (parent_index < block->children.count) {
							if (parent_index < definition_index) {
								// Can't access definition because it is declared after. Skip it.
								goto next_definition;
							}
							break;
						}
					}
				}
					
				name->possible_definitions.add(definition);


				if (block == &context->global_block.unprotected) {
					for (auto &typecheck_entry : typecheck_entries) {
						if (typecheck_entry.node == definition) {
							entry->dependency = &typecheck_entry;
							break;
						}
					}
				}

				if (!yield_while_null(name->location, &definition->type)) {
					// Sometimes this error is meaningless and noisy because is is caused by another error.
					// But other times compiler fails with only this error, which is not printed in case
					// print_wait_failures is false.

					//if (print_wait_failures) {
						reporter.error(name->location, "Definition referenced by this name was not properly typechecked.");
						reporter.info(definition->location, "Here is the bad definition:");
					//}
					fail();
				}

				entry->dependency = 0;

			next_definition:;
			}

			if (auto definition = name->definition()) {
				name->type = definition->type;

				if (context_base->constant_name_inlining) {
					if (definition->mutability == Mutability::constant) {
						// NOTE: Even though definition is constant, definition->initial_value can be null
						// if it is an unresolved template parameter.
						if (definition->initial_value) {
							switch (definition->initial_value->kind) {
								case NodeKind::Lambda:
								case NodeKind::Struct:
									break;
								default:
									auto result = to_node(definition->constant_value.value());
									result->location = name->location;
									if (!types_match(result->type, definition->type)) {
										immediate_reporter.warning(name->location, "INTERNAL: constant name inlining resulted in a literal with different type, {} instead of {}. TODO FIXME", result->type, definition->type);
										result->type = definition->type;
									}
									return result;
							}
						}
					}
				}
			} else {
				if (!name->allow_overload) {
					ensure_not_overloaded(name);
				}
				name->type = get_builtin_type(BuiltinType::Overload);
			}

			return name;
		}
	}
	reporter.error(name->location, "`{}` was not declared.", name->name);
	fail();
	return 0;
}
Expression       *Typechecker::typecheck_impl(Call *call, bool can_substitute) {
	defer { assert(call->callable->type != 0); };
	if (auto binary = as<Binary>(call->callable)) {
		if (binary->operation == BinaryOperation::dot) {
			if (auto lambda_name = as<Name>(binary->right)) {
				Reporter reporter2;
				if (with_unwind_strategy([&] {
					if (auto subst = typecheck_binary_dot(binary, reporter2)) {
						call->callable = subst;
						return true;
					}
					return false;
				})) {
					goto typecheck_dot_succeeded;
				} else {
					constexpr bool old_dotcall = true;

					if constexpr (old_dotcall) {
						//Definition *lambda_definition = 0;
						//for (auto block = current_block; block; block = block->parent) {
						//	if (auto found_definitions = block->definition_map.find(lambda_name->name)) {
						//		auto [name, definitions] = *found_definitions;
						//		assert(definitions.count != 0);
						//		if (definitions.count > 1) {
						//			reporter.error(binary->right->location, "Function overloading for dot calls is not implemented yet.");
						//			fail();
						//		}
						//		lambda_definition = definitions[0];
						//		break;
						//	}
						//}
						//
						//auto lambda = lambda_definition->initial_value ? as<Lambda>(lambda_definition->initial_value) : 0;
						//if (!lambda) {
						//	reporter.error(binary->right->location, "This is not a lambda.");
						//	fail();
						//}


						call->callable = binary->right;
						
						// Attempt passing `this` as follows (return on first successful attempt):
						//     1. As-is.
						//     2. By pointer.

						call->arguments.insert_at({.expression = binary->left}, 0);

						Reporter as_is_reporter;
						{
							scoped_exchange(reporter, as_is_reporter);
							auto result = with_unwind_strategy([&] { return typecheck_impl(call, can_substitute); });
							if (result) {
								as_is_reporter.reports.add(reporter.reports);
								return result;
							}
						}
						
						Reporter by_pointer_reporter;
						{
							scoped_exchange(reporter, by_pointer_reporter);

							auto first_argument_address = Unary::create();
							first_argument_address->location = binary->left->location;
							first_argument_address->expression = binary->left;
							first_argument_address->operation = UnaryOperation::addr;
							call->arguments[0].expression = first_argument_address;

							auto result = with_unwind_strategy([&] { return typecheck_impl(call, can_substitute); });
							if (result) {
								by_pointer_reporter.reports.add(reporter.reports);
								return result;
							}
						}

						reporter.error(call->location, "Unable to pass `this` argument. Here are attempt results:");
						reporter.info("Attempt to pass `this` as-is:");
						for (auto &r : as_is_reporter.reports) {
							++r.indentation;
						}
						reporter.reports.add(as_is_reporter.reports);
						reporter.info("Attempt to pass `this` by pointer:");
						for (auto &r : by_pointer_reporter.reports) {
							++r.indentation;
						}
						reporter.reports.add(by_pointer_reporter.reports);
						fail();
					} else {
					}
				}
			}
		}
	}


	if (auto name = as<Name>(call->callable)) {
		name->allow_overload = true;
	}
	typecheck(&call->callable);

typecheck_dot_succeeded:
	auto &arguments = call->arguments;
	for (auto &argument : arguments) {
		typecheck(&argument.expression);
	}

	auto directed_callable = direct(call->callable);

	if (auto struct_ = as<Struct>(directed_callable)) {
		return typecheck_constructor(call, struct_);
	} else if (auto lambda = as<Lambda>(directed_callable)) {
		return typecheck_lambda_call(call, lambda, &lambda->head);
	} else if (auto definition = as<Definition>(directed_callable)) {
		if (auto lambda_head = direct_as<LambdaHead>(definition->type)) {
			return typecheck_lambda_call(call, 0, lambda_head);
		} else {
			reporter.error(directed_callable->location, "This is not a lambda nor a struct.");
			fail();
			return 0;
		}
	} else if (auto name = as<Name>(directed_callable)) {
		assert(name->possible_definitions.count > 1);


		struct Overload {
			Definition *definition = 0;
			Lambda *lambda = 0;
			LambdaHead *lambda_head = 0;
			Struct *struct_ = 0;
			Reporter reporter;
		};

		List<Overload, TemporaryAllocator> overloads;

		overloads.reserve(name->possible_definitions.count);

		for (auto definition : name->possible_definitions) {
			Overload overload;

			overload.definition = definition;

			auto directed_type = direct(definition->type);
			if (auto lambda_head = as<LambdaHead>(directed_type)) {
				overload.lambda_head = lambda_head;
			}

			if (definition->initial_value) {
				if (auto lambda = as<Lambda>(definition->initial_value)) {
					overload.lambda = lambda;
				} else if (auto struct_ = as<Struct>(definition->initial_value)) {
					overload.struct_ = struct_;
				}
			}

			overloads.add(overload);
		}

		List<Overload *, TemporaryAllocator> matching_overloads;

		for (auto &overload : overloads) {
			scoped_exchange(reporter, overload.reporter);
			with_unwind_strategy([&] {
				if (typecheck_lambda_call(call, overload.lambda, overload.lambda_head, false)) {
					matching_overloads.add(&overload);
				}
			});
		}

		if (matching_overloads.count == 1) {
			auto matching_overload = matching_overloads[0];
			name->possible_definitions.set(matching_overload->definition);
			name->type = name->definition()->type;
			return typecheck_lambda_call(call, matching_overload->lambda, matching_overload->lambda_head);
		}
		if (matching_overloads.count == 0) {
			reporter.error(call->location, "No matching overload was found.");
			foreach (it, overloads) {
				auto [i, overload] = it.key_value();
				scoped_replace(reporter.indentation, reporter.indentation + 1);
				reporter.info("Overload #{}:", i);
				scoped_replace(reporter.indentation, reporter.indentation + 1);
				for (auto report : overload.reporter.reports) {
					reporter.add(report);
				}
			}
			fail();
		}

		reporter.error(call->location, "Multiple matching overload were found:");
		foreach (it, matching_overloads) {
			auto [i, overload] = it.key_value();
			reporter.info(overload->definition->location, "Overload #{}:", i);
		}
		fail();
	} else if (auto head = as<LambdaHead>(directed_callable->type)) {
		return typecheck_lambda_call(call, 0, head, true);
	}

	reporter.error(call->callable->location, "Expression of type {} can't be called", call->callable->type);
	fail();
	return 0;
}
Node             *Typechecker::typecheck_impl(IfStatement *If, bool can_substitute) {
	typecheck(&If->condition);

	typecheck(&If->true_branch);

	if (If->false_branch) {
		typecheck(&If->false_branch);
	}

	if (auto value_ = get_constant_value(If->condition)) {

		NOTE_LEAK(If);

		auto value = value_.value();
		assert(value.kind == ValueKind::Bool);
		if (value.Bool) {
			return If->true_branch;
		} else {
			if (If->false_branch) {
				return If->false_branch;
			} else {
				auto empty_block = Block::create();
				empty_block->location = If->location;
				empty_block->parent = current_block;
				empty_block->container = current_container;
				empty_block->type = get_builtin_type(BuiltinType::None);
				return empty_block;
			}
		}
	}

	return If;
}
Expression       *Typechecker::typecheck_impl(IfExpression *If, bool can_substitute) {
	typecheck(&If->condition);

	typecheck(&If->true_branch);
	typecheck(&If->false_branch);

	if (types_match(If->true_branch->type, If->false_branch->type)) {
		If->type = If->true_branch->type;
	} else {
		defer {
			If->true_branch = If->true_branch;
			If->false_branch = If->false_branch;
		};

		Reporter cast_reporter;
		cast_reporter.reports.allocator = current_temporary_allocator;
		auto t2f = implicitly_cast(&If->true_branch, If->false_branch->type, &cast_reporter, false);
		auto f2t = implicitly_cast(&If->false_branch, If->true_branch->type, &cast_reporter, false);

		if (!t2f && !f2t) {
			reporter.error(If->location, "Branch types {} and {} don't match in any way.", If->true_branch->type, If->false_branch->type);
			reporter.reports.add(cast_reporter.reports);
			cast_reporter.reports.clear();
			fail();
		} else if (t2f && f2t) {
			reporter.error(If->location, "Branch types {} and {} are both implicitly convertible to each other.", If->true_branch->type, If->false_branch->type);
			reporter.reports.add(cast_reporter.reports);
			cast_reporter.reports.clear();
			fail();
		} else if (t2f) {
			assert_always(implicitly_cast(&If->true_branch, If->false_branch->type, &cast_reporter, true));
			If->type = If->true_branch->type;
		} else {
			assert_always(implicitly_cast(&If->false_branch, If->true_branch->type, &cast_reporter, true));
			If->type = If->false_branch->type;
		}
	}

	if (auto value_ = get_constant_value(If->condition)) {

		NOTE_LEAK(If);

		auto value = value_.value();
		assert(value.kind == ValueKind::Bool);
		return value.Bool ? If->true_branch : If->false_branch;
	}

	if (!If->type)
		If->type = get_builtin_type(BuiltinType::None);

	return If;
}
BuiltinTypeName  *Typechecker::typecheck_impl(BuiltinTypeName *type, bool can_substitute) { 
	type->type = get_builtin_type(BuiltinType::Type);
	return type;
}
Expression       *Typechecker::typecheck_impl(Binary *binary, bool can_substitute) {
	if (binary->location == "v.x + v.y") {
		int x = 4;
	}
	if (binary->operation == BinaryOperation::dot) {
		Expression *result = binary;
		if (!with_unwind_strategy([&] {
			if (auto subst = typecheck_binary_dot(binary, reporter)) {
				result = subst;
				return true;
			}
			return false;
		})) {
			fail();
		}
		return result;
	} else {
		typecheck(&binary->left);
		typecheck(&binary->right);

		switch (binary->operation) {
			case BinaryOperation::ass: {
				auto result = is_mutable(binary->left);
				if (!result) {
					reporter.error(binary->left->location, "This expression can not be modified.");
					reporter.help(result.error()->location, "Because this is not mutable.");
					why_is_this_immutable(binary->left);

					fail();
				}
				if (!implicitly_cast(&binary->right, binary->left->type, true)) {
					fail();
				}
				binary->type = get_builtin_type(BuiltinType::None);
				return binary;
			}
			case BinaryOperation::as: {
				if (implicitly_cast(&binary->left, binary->right, 0, false)) {
					implicitly_cast(&binary->left, binary->right, &reporter, true);
					binary->type = binary->right;
					return binary;
				}

				auto source_type = direct(binary->left->type);
				auto target_type = direct(binary->right);

				// From lambda
				if (auto left_lambda_head = as<LambdaHead>(source_type)) {
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						binary->type = binary->right;
						return binary;
					}
				}

				// From pointer
				if (auto left_pointer = as_pointer(source_type)) {
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						binary->type = binary->right;
						return binary;
					}

					// To integer
					if (is_concrete_integer(target_type)) {
						binary->type = binary->right;
						return binary;
					}
				}

				// From integer
				if (is_concrete_integer(source_type)) {
					// To integer
					if (is_concrete_integer(target_type)) {
						binary->type = binary->right;
						return binary;
					}
						
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						binary->type = binary->right;
						return binary;
					}
				}


				reporter.error(binary->location, "No conversion from {} to {} is available.", binary->left->type, binary->right);
				fail();
				return 0;
			}
		}

		auto dleft  = direct(binary->left->type);
		auto dright = direct(binary->right->type);

		if (auto found = binary_typecheckers.find({ dleft, dright, binary->operation })) {
			return (this->*(*found.value))(binary);
		}
		if (auto left_array = as<ArrayType>(dleft)) {
			if (auto right_array = as<ArrayType>(dright)) {
				auto equals = []<class Value>(Optional<Value> a, Optional<Value> b) {
					if (a.has_value() && b.has_value()) {
						return a.value() == b.value();
					}
					return false;
				};

				auto left_count_result = left_array->count;
				auto right_count_result = right_array->count;

				if (equals(left_count_result, right_count_result)) {
					// TODO: make this work with arrays of arrays
					auto dleft_element = direct(left_array->element_type);
					auto dright_element = direct(right_array->element_type);
					auto element_count = left_count_result.value();
					if (auto found = binary_typecheckers.find({ dleft_element, dright_element, binary->operation })) {
						VectorizedBinaryValue vectorized = {};

						locked_use(vectorized_binarys) {
							auto found = vectorized_binarys.find({ dleft_element, dright_element, binary->operation, element_count });
							if (found) {
								vectorized = *found.value;
							} else {
								auto lambda_name = format(u8"__v_{}_{}_{}_{}"s, Nameable(binary->operation), element_count, Nameable(dleft_element), Nameable(dright_element));
								auto source_list = format(u8"\0" R"(
const {} = fn (a: {}, b: {}) => {{
var i: S64
var c: {}
while i < {} {{
	c[i] = a[i] {} b[i]
	i = i + 1
}}
c
}}
)" "\0"s, lambda_name, left_array, right_array, left_array, element_count, binary->operation);
							
								auto source = source_list.subspan(1, source_list.count - 2);
									
								auto path = format(u8"{}\\{}.sp", context_base->generated_source_directory, lambda_name);

								auto &content_start_to_file_name = context_base->content_start_to_file_name;
								locked_use(content_start_to_file_name) {
									content_start_to_file_name.get_or_insert(source.data) = path;
								};

								Node *definition_node = 0;

								bool success = parse_source(source, [&](Node *node) {
									assert(!definition_node, "Only one node expected");
									definition_node = node;
									locked_use_expr(global_block, context->global_block) {
										global_block.add(node);
									};
								});

								success &= with_unwind_strategy([&] {
									scoped_replace(current_block, &context->global_block.unprotected);
									scoped_replace(current_container, 0);
									scoped_replace(current_loop, 0);
									return typecheck(&definition_node);
								});
									
								{
									with(temporary_storage_checkpoint);
									write_entire_file(path, as_bytes(source));

									if (!success) {
										immediate_reporter.error(binary->location, "INTERNAL ERROR: Failed to instantiate vectorized lambda for this operation. Generated source code is saved at {}", path);
										fail();
									}
								}
				
								vectorized.definition = as<Definition>(definition_node);
								assert(vectorized.definition);
								vectorized.lambda = as<Lambda>(vectorized.definition->initial_value);
								assert(vectorized.lambda);
							}
							return 0;
						};


						auto name = Name::create();
						name->name = vectorized.definition->name;
						name->location = binary->location;
						name->type = vectorized.definition->type;
						name->possible_definitions.add(vectorized.definition);

						auto call = Call::create();
						call->location = binary->location;
						call->callable = name;
						call->arguments.add({.expression = binary->left, .parameter = vectorized.lambda->head.parameters_block.definition_list[0]});
						call->arguments.add({.expression = binary->right, .parameter = vectorized.lambda->head.parameters_block.definition_list[1]});
						call->type = vectorized.lambda->head.return_type;
						call->call_kind = CallKind::lambda;
							
						NOTE_LEAK(binary);
						binary->left = 0;
						binary->right = 0;

						return call;
					}
				}
			}
		}

		switch (binary->operation) {
			case BinaryOperation::equ:
			case BinaryOperation::neq: {
				if (is_pointer_to_none_comparison(binary->left, binary->right)) {
					binary->type = get_builtin_type(BuiltinType::Bool);
					return binary;
				}
				break;
			}
		}

	no_binop:
		reporter.error(binary->location, "No binary operation {} defined for types {} and {}.", binary->operation, binary->left->type, binary->right->type);
		fail();
		return 0;
	}
}
Match            *Typechecker::typecheck_impl(Match *match, bool can_substitute) {
	typecheck(&match->expression);

	make_concrete(match->expression);

	for (auto &Case : match->cases) {
		if (Case.from) {
			typecheck(&Case.from);

			if (!is_constant(Case.from)) {
				reporter.error(Case.from->location, "Match case expression must be constant.");
				fail();
			}

			if (!implicitly_cast(&Case.from, match->expression->type, true))
				fail();
		}

		typecheck(&Case.to);
	}

	if (match->default_case) {
		for (auto &Case : match->cases) {
			if (is_concrete(Case.to->type)) {
				match->type = Case.to->type;
				break;
			}
		}

		if (!match->type) {
			make_concrete(match->cases[0].to);
			match->type = match->cases[0].to->type;
		}

		for (auto &Case : match->cases) {
			if (!implicitly_cast(&Case.to, match->type, true)) {
				fail();
			}
		}
	} else {

		for (auto &Case : match->cases) {
			make_concrete(Case.to);
		}

		match->type = get_builtin_type(BuiltinType::None);
	}

	return match;
}
Expression       *Typechecker::typecheck_impl(Unary *unary, bool can_substitute) {
	typecheck(&unary->expression);
	auto constant = get_constant_value(unary->expression);
	switch (unary->operation) {
		case UnaryOperation::star: {
			if (types_match(unary->expression->type, BuiltinType::Type)) {
				unary->operation = UnaryOperation::pointer;
				unary->type = get_builtin_type(BuiltinType::Type);
			} else if (auto pointer = as_pointer(unary->expression->type)) {
				unary->operation = UnaryOperation::dereference;
				unary->type = pointer->expression;
			} else {
				reporter.error(unary->location, "Star is used to create pointer types and to dereference pointer values, but this expression is not a type nor a pointer.");
				reporter.info(unary->expression->location, "Type of this expression is {}.", unary->expression->type);
				fail();
			}
			break;
		}
		case UnaryOperation::addr: {
			if (auto name = as<Name>(get_last_child_recursive(unary->expression))) {
				auto definition = name->definition();
				assert(definition);
				unary->type = make_pointer(unary->expression->type, definition->mutability);
			} else {
				reporter.error(unary->location, "You can only take address of names, or blocks that end with a name.");
				fail();
			}
			break;
		}
		case UnaryOperation::plus: {
			if (auto builtin = as<BuiltinTypeName>(unary->expression->type)) {
				switch (builtin->type_kind) {
					case BuiltinType::U8:
					case BuiltinType::U16:
					case BuiltinType::U32:
					case BuiltinType::U64:
					case BuiltinType::S8:
					case BuiltinType::S16:
					case BuiltinType::S32:
					case BuiltinType::S64:
						unary->type = unary->expression->type;
						break;
				}
			}

			if (!unary->type) {
				reporter.error(unary->location, "Unary plus can't be applied to expression of type {}", unary->expression->type);
				fail();
			}

			NOTE_LEAK(unary);
			return unary->expression;
		}
		case UnaryOperation::minus: {
			if (auto literal = as<IntegerLiteral>(unary->expression)) {
				literal->value = -literal->value;
				return literal;
			}
			if (auto builtin = as<BuiltinTypeName>(unary->expression->type)) {
				switch (builtin->type_kind) {
					case BuiltinType::S8:
					case BuiltinType::S16:
					case BuiltinType::S32:
					case BuiltinType::S64:
						unary->type = unary->expression->type;
						break;
				}
			}

			if (!unary->type) {
				reporter.error(unary->location, "Unary minus can't be applied to expression of type {}", unary->expression->type);
				fail();
			}
			break;
		}
		case UnaryOperation::typeof: {
			make_concrete(unary->expression);
			unary->type = get_builtin_type(BuiltinType::Type);

			if (auto builtin_type = direct_as<BuiltinTypeName>(unary->expression->type)) {
				// NOTE: must copy to set location
				auto copied = Copier{}.deep_copy(builtin_type);
				copied->location = unary->location;
				NOTE_LEAK(unary);
				return copied;
			}
			break;
		}
		case UnaryOperation::lnot: {
			if (!implicitly_cast(&unary->expression, get_builtin_type(BuiltinType::Bool), true)) {
				fail();
			}
			unary->type = unary->expression->type;
			break;
		}
		default:
			not_implemented();
			break;
	}

	return unary;
}
Return           *Typechecker::typecheck_impl(Return *Return, bool can_substitute) {
	if (Return->value)
		typecheck(&Return->value);

	add_defers(Return->defers);

	return Return;
}
While            *Typechecker::typecheck_impl(While *While, bool can_substitute) {
	typecheck(&While->condition);

	scoped_replace(current_loop, While);

	if (auto builtin_type = direct_as<BuiltinTypeName>(While->condition->type); !builtin_type || builtin_type->type_kind != BuiltinType::Bool) {
		reporter.error(While->condition->location, "Condition type must be Bool.");
		fail();
	}

	typecheck(&While->body);

	return While;
}
Continue         *Typechecker::typecheck_impl(Continue *Continue, bool can_substitute) {
	assert(current_loop);
	Continue->loop = current_loop;
	
	add_defers(Continue->defers);
	
	return Continue;
}
Break            *Typechecker::typecheck_impl(Break *Break, bool can_substitute) {
	if (Break->value) {
		typecheck(&Break->value);
	} else {
		assert(current_loop);
		Break->loop = current_loop;
	}

	add_defers(Break->defers);

	return Break;
}
Struct           *Typechecker::typecheck_impl(Struct *Struct, bool can_substitute) {
	if (Struct->is_template) {
		Struct->type = get_builtin_type(BuiltinType::Template);
		return Struct;
	} else {
		defer {
			assert(Struct->size != -1);
		};

		scoped_replace(current_container, Struct);

		s64 struct_size = 0;
		for (auto &member : Struct->members) {
			typecheck(&member);
			if (!is_type(member->type) || !is_concrete(member->type)) {
				reporter.error(member->location, "Struct members must have concrete type. This type is `{}` which is not concrete.", member->type);
				fail();
			}
			member->offset = struct_size;
			struct_size += get_size(member->type);
		}
		Struct->type = get_builtin_type(BuiltinType::Type);
		Struct->size = struct_size;
		return Struct;
	}
}
ArrayType        *Typechecker::typecheck_impl(ArrayType *arr, bool can_substitute) {
	typecheck(&arr->count_expression);
	if (auto maybe_count = get_constant_value(arr->count_expression)) {
		auto count_value = maybe_count.value();
		s64 count = 0;
		switch (count_value.kind) {
			case ValueKind::U8: count = count_value.U8; break;
			case ValueKind::U16: count = count_value.U16; break;
			case ValueKind::U32: count = count_value.U32; break;
			case ValueKind::U64: count = count_value.U64; break;
			case ValueKind::S8: count = count_value.S8; break;
			case ValueKind::S16: count = count_value.S16; break;
			case ValueKind::S32: count = count_value.S32; break;
			case ValueKind::S64: count = count_value.S64; break;
			case ValueKind::UnsizedInteger: count = (s64)count_value.UnsizedInteger; break;
			default: {
				reporter.error(arr->count_expression->location, "Count expression must be an integer.");
				fail();
				break;
			}
		}

		if (count <= 0) {
			reporter.error(arr->count_expression->location, "Arrays of 0 elements or less are not allowed.");
			fail();
		}
		arr->count = (u64)count;
	} else {
		reporter.error(arr->count_expression->location, "Count expression must be constant.");
		fail();
	}
		
	typecheck(&arr->parsed_element_type);
	if (!is_type(arr->parsed_element_type)) {
		reporter.error(arr->parsed_element_type->location, "This must be a type.");
		reporter.info(arr->location, "Because this is an array.");
		fail();
	}

	arr->element_type = arr->parsed_element_type;
	arr->type = get_builtin_type(BuiltinType::Type);
	return arr;
}
Expression       *Typechecker::typecheck_impl(Subscript *Subscript, bool can_substitute) {
	typecheck(&Subscript->subscriptable);

	if (auto array_type = direct_as<ArrayType>(Subscript->subscriptable->type)) {
		typecheck(&Subscript->index);
		make_concrete(Subscript->index);
		if (!::is_concrete_integer(Subscript->index->type)) {
			reporter.error(Subscript->index->location, "This must be an integer.");
			fail();
		}

		Subscript->type = array_type->element_type;

		if (can_substitute) {
			if (auto index_value = get_constant_value(Subscript->index)) {
				if (auto array = direct_as<ArrayConstructor>(Subscript->subscriptable)) {
					NOTE_LEAK(Subscript);
					auto index = index_value.value();
					switch (index.kind) {
						case ValueKind::U8: return array->elements[index.U8];
						case ValueKind::U16: return array->elements[index.U16];
						case ValueKind::U32: return array->elements[index.U32];
						case ValueKind::U64: return array->elements[index.U64];
						case ValueKind::S8: return array->elements[index.S8];
						case ValueKind::S16: return array->elements[index.S16];
						case ValueKind::S32: return array->elements[index.S32];
						case ValueKind::S64: return array->elements[index.S64];
						default: invalid_code_path("invalid index kind: {}", index.kind);
					}
				}
			}
		}

		return Subscript;
	} else if (auto Struct_ = direct_as<Struct>(Subscript->subscriptable->type); Struct_ && Struct_->is_template) {
		return get_struct_template_instantiation(Struct_, Subscript->index);
	} else {
		reporter.error(Subscript->subscriptable->location, "This expression is not subscriptable.");
		reporter.help("Subscriptable expressions are arrays and templates.");
		fail();
	}
}
ArrayConstructor *Typechecker::typecheck_impl(ArrayConstructor *arr, bool can_substitute) {
	for (auto &element : arr->elements) {
		typecheck(&element);
	}

	make_concrete(arr->elements[0]);
	for (auto &element : arr->elements.skip(1)) {
		if (!implicitly_cast(&element, arr->elements[0]->type, true)) {
			fail();
		}
	}

	arr->type = make_array_type(arr->elements[0]->type, arr->elements.count);

	return arr;
}
Import           *Typechecker::typecheck_impl(Import *import, bool can_substitute) {
	return import;
}
Defer            *Typechecker::typecheck_impl(Defer *Defer, bool can_substitute) {
	typecheck(&Defer->body);
	current_block->defers.add(Defer);
	return Defer;
}
ZeroInitialized  *Typechecker::typecheck_impl(ZeroInitialized *zi, bool can_substitute) {
	invalid_code_path("ZeroInitialized cannot be typechecked.");
}

Expression *Typechecker::bt_take_left(Binary *binary) {
	binary->type = binary->left->type;
	return binary;
};
Expression *Typechecker::bt_set_bool(Binary *binary) {
	binary->type = get_builtin_type(BuiltinType::Bool);
	return binary;
};
Expression *Typechecker::bt_unsized_int_and_sized_int_math(Binary *binary) {
	auto sized = binary->left;
	auto unsized = binary->right;

	if (is_concrete(unsized->type)) {
		Swap(sized, unsized);
	}

	propagate_concrete_type(unsized, sized->type);

	binary->type = sized->type;
	return binary;
};
Expression *Typechecker::bt_unsized_int_and_sized_int_comp(Binary *binary) {
	auto sized = binary->left;
	auto unsized = binary->right;

	if (is_concrete(unsized->type)) {
		Swap(sized, unsized);
	}

	propagate_concrete_type(unsized, sized->type);
	binary->type = get_builtin_type(BuiltinType::Bool);
	return binary;
};
Expression *Typechecker::bt_unsized_int(Binary *binary) {
	auto l = get_constant_value(binary->left).value();
	auto r = get_constant_value(binary->right).value();
	assert(l.kind == ValueKind::UnsizedInteger);
	assert(r.kind == ValueKind::UnsizedInteger);
	switch (binary->operation) {
		case BinaryOperation::add: return make_integer(l.UnsizedInteger + r.UnsizedInteger, binary->location);
		case BinaryOperation::sub: return make_integer(l.UnsizedInteger - r.UnsizedInteger, binary->location);
		case BinaryOperation::mul: return make_integer(l.UnsizedInteger * r.UnsizedInteger, binary->location);
		case BinaryOperation::div: return make_integer(l.UnsizedInteger / r.UnsizedInteger, binary->location);
		case BinaryOperation::mod: return make_integer(l.UnsizedInteger % r.UnsizedInteger, binary->location);
		case BinaryOperation::bxo: return make_integer(l.UnsizedInteger ^ r.UnsizedInteger, binary->location);
		case BinaryOperation::ban: return make_integer(l.UnsizedInteger & r.UnsizedInteger, binary->location);
		case BinaryOperation::bor: return make_integer(l.UnsizedInteger | r.UnsizedInteger, binary->location);
		case BinaryOperation::bsl: return make_integer(l.UnsizedInteger << r.UnsizedInteger, binary->location);
		case BinaryOperation::bsr: return make_integer(l.UnsizedInteger >> r.UnsizedInteger, binary->location);
		case BinaryOperation::equ: return make_boolean(l.UnsizedInteger == r.UnsizedInteger, binary->location);
		case BinaryOperation::neq: return make_boolean(l.UnsizedInteger != r.UnsizedInteger, binary->location);
		case BinaryOperation::les: return make_boolean(l.UnsizedInteger < r.UnsizedInteger, binary->location);
		case BinaryOperation::grt: return make_boolean(l.UnsizedInteger > r.UnsizedInteger, binary->location);
		case BinaryOperation::leq: return make_boolean(l.UnsizedInteger <= r.UnsizedInteger, binary->location);
		case BinaryOperation::grq: return make_boolean(l.UnsizedInteger >= r.UnsizedInteger, binary->location);
	}
	invalid_code_path("Attempt to evaluate binary {} on unsized integers. This is not supported/implemented", binary->operation);
};

template <bool invert>
Expression *Typechecker::bt_comp_Type(Binary *binary) {
	return make_boolean(invert ^ types_match(binary->left, binary->right));
}

template <auto operation>
Expression *Typechecker::bt_math_opt(Binary *binary) {
	binary->type = binary->left->type;
	if (auto left = get_constant_value(binary->left)) {
		if (auto right = get_constant_value(binary->right)) {
			auto l = left.value();
			auto r = right.value();
			auto result = operation(l, r);
			result->location = binary->location;
			return result;
		}
	}
	return binary;
};

void Typechecker::init_binary_typecheckers() {
	construct(binary_typecheckers);

#define y(left, right, operation) binary_typecheckers.get_or_insert({ get_builtin_type(left), get_builtin_type(right), operation })
#define x(left, right, operation) y(BuiltinType::left, BuiltinType::right, BinaryOperation::operation)

	//
	// Every type is equatable
	// 
	for (u32 i = 0; i < (u32)BuiltinType::count; ++i) {
		y((BuiltinType)i, (BuiltinType)i, BinaryOperation::equ) = &bt_set_bool;
		y((BuiltinType)i, (BuiltinType)i, BinaryOperation::neq) = &bt_set_bool;
	}

	x(Type, Type, equ) = &bt_comp_Type<false>;
	x(Type, Type, neq) = &bt_comp_Type<true>;

#define ORDERABLE(type) \
x(type, type, les) = &bt_set_bool; \
x(type, type, leq) = &bt_set_bool; \
x(type, type, grt) = &bt_set_bool; \
x(type, type, grq) = &bt_set_bool

#define MATHABLE_INTEGER(type) \
x(type, type, add) = &bt_math_opt<[](Value l, Value r){ return make_integer(l.type + r.type, get_builtin_type(BuiltinType::type)); }>; \
x(type, type, sub) = &bt_math_opt<[](Value l, Value r){ return make_integer(l.type - r.type, get_builtin_type(BuiltinType::type)); }>; \
x(type, type, mul) = &bt_math_opt<[](Value l, Value r){ return make_integer(l.type * r.type, get_builtin_type(BuiltinType::type)); }>; \
x(type, type, div) = &bt_math_opt<[](Value l, Value r){ return make_integer(l.type / r.type, get_builtin_type(BuiltinType::type)); }>; \
x(type, type, mod) = &bt_math_opt<[](Value l, Value r){ return make_integer(l.type % r.type, get_builtin_type(BuiltinType::type)); }>;

#define BITWISE(type) \
x(type, type, bxo) = &bt_take_left; \
x(type, type, ban) = &bt_take_left; \
x(type, type, bor) = &bt_take_left; \
x(type, type, bsl) = &bt_take_left; \
x(type, type, bsr) = &bt_take_left; \

#define SYMMETRIC(a, b, op) x(a, b, op) = x(b, a, op)

#define UNSIZED_INT_AND_SIZED_INT(t) \
SYMMETRIC(t, UnsizedInteger, add) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, sub) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, mul) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, div) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, mod) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, bor) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, ban) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, bxo) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, bsl) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, bsr) = &bt_unsized_int_and_sized_int_math; \
SYMMETRIC(t, UnsizedInteger, equ) = &bt_unsized_int_and_sized_int_comp; \
SYMMETRIC(t, UnsizedInteger, neq) = &bt_unsized_int_and_sized_int_comp; \
SYMMETRIC(t, UnsizedInteger, les) = &bt_unsized_int_and_sized_int_comp; \
SYMMETRIC(t, UnsizedInteger, leq) = &bt_unsized_int_and_sized_int_comp; \
SYMMETRIC(t, UnsizedInteger, grt) = &bt_unsized_int_and_sized_int_comp; \
SYMMETRIC(t, UnsizedInteger, grq) = &bt_unsized_int_and_sized_int_comp

	ORDERABLE(Bool);
	ORDERABLE(U8);
	ORDERABLE(U16);
	ORDERABLE(U32);
	ORDERABLE(U64);
	ORDERABLE(S8);
	ORDERABLE(S16);
	ORDERABLE(S32);
	ORDERABLE(S64);

	MATHABLE_INTEGER(U8);
	MATHABLE_INTEGER(U16);
	MATHABLE_INTEGER(U32);
	MATHABLE_INTEGER(U64);
	MATHABLE_INTEGER(S8);
	MATHABLE_INTEGER(S16);
	MATHABLE_INTEGER(S32);
	MATHABLE_INTEGER(S64);

	BITWISE(U8);
	BITWISE(U16);
	BITWISE(U32);
	BITWISE(U64);
	BITWISE(S8);
	BITWISE(S16);
	BITWISE(S32);
	BITWISE(S64);

	UNSIZED_INT_AND_SIZED_INT(U8);
	UNSIZED_INT_AND_SIZED_INT(U16);
	UNSIZED_INT_AND_SIZED_INT(U32);
	UNSIZED_INT_AND_SIZED_INT(U64);
	UNSIZED_INT_AND_SIZED_INT(S8);
	UNSIZED_INT_AND_SIZED_INT(S16);
	UNSIZED_INT_AND_SIZED_INT(S32);
	UNSIZED_INT_AND_SIZED_INT(S64);

	x(UnsizedInteger, UnsizedInteger, add) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, sub) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, mul) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, div) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, mod) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, bxo) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, ban) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, bor) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, bsl) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, bsr) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, equ) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, neq) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, les) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, leq) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, grt) = &bt_unsized_int;
	x(UnsizedInteger, UnsizedInteger, grq) = &bt_unsized_int;

	x(Bool, Bool, lan) = &bt_set_bool;
	x(Bool, Bool, lor) = &bt_set_bool;

#undef UNSIZED_INT_AND_SIZED_INT
#undef SYMMETRIC
#undef MATHABLE
#undef ORDERABLE
#undef BITWISE
#undef x
#undef y
}
	
#undef fail
#undef with_unwind_strategy

u64 get_typechecking_progress() {
	u64 result = 0;
	for (auto &entry : typecheck_entries) {
		if (entry.typechecker)
			result += entry.typechecker->progress;
	}
	return result;
}
