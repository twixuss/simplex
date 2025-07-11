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
#include "print_ast.h"

#define ENABLE_TYPECHECKER_REUSE 0

#define scoped_lock_if_block_is_global(block)              \
	if (block == &context->global_block.unprotected) {     \
		lock(context->global_block._lock);                 \
		++global_block_locks_count;                        \
	}                                                      \
	defer {                                                \
		if (block == &context->global_block.unprotected) { \
			unlock(context->global_block._lock);           \
			--global_block_locks_count;                    \
		}                                                  \
	};

#define coalesce(a, b) ([&]{ auto _a = a; if (_a) return _a; return b; }())

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

void Typechecker::fail() {
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
	invalid_code_path();
}

void Typechecker::yield(Typechecker::YieldResult result) {
	yield_result = result;
	scoped_replace(debug_current_location, {});
	
	for (u32 i = 0; i < global_block_locks_count; ++i)
		unlock(context->global_block._lock);


	tl::yield_reuse(parent_fiber, fiber);
	if (result == YieldResult::fail) {
		longjmp(main_loop_unwind_point.buf, 1);
	}

	for (u32 i = 0; i < global_block_locks_count; ++i)
		lock(context->global_block._lock);
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

LowBinaryOperation integer_extension_low_op(umm source_size, bool source_signed, umm destination_size) {
	if (source_size == destination_size) {
		return LowBinaryOperation::left;
	}
	if (source_signed) {
		switch (destination_size) {
			case 2: switch (source_size) {
				case 1: return LowBinaryOperation::sex8to16;
			}
			case 4:	switch (source_size) {
				case 1: return LowBinaryOperation::sex8to32;
				case 2: return LowBinaryOperation::sex16to32;
			}
			case 8: switch (source_size) {
				case 1: return LowBinaryOperation::sex8to64;
				case 2: return LowBinaryOperation::sex16to64;
				case 4: return LowBinaryOperation::sex32to64;
			}
		}
	} else {
		switch (destination_size) {
			case 2: switch (source_size) {
				case 1: return LowBinaryOperation::zex8to16;
			}
			case 4:	switch (source_size) {
				case 1: return LowBinaryOperation::zex8to32;
				case 2: return LowBinaryOperation::zex16to32;
			}
			case 8: switch (source_size) {
				case 1: return LowBinaryOperation::zex8to64;
				case 2: return LowBinaryOperation::zex16to64;
				case 4: return LowBinaryOperation::zex32to64;
			}
		}
	}
	invalid_code_path("can't produce LowBinaryOperation for integer extension: {} {} to {}", source_signed ? "signed" : "unsigned", source_size, destination_size);
}

constexpr Array<Array<LowBinaryOperation, (int)BuiltinType::count>, (int)BuiltinType::count> builtin_type_conversion_low_op_table = [&](){
	Array<Array<LowBinaryOperation, (int)BuiltinType::count>, (int)BuiltinType::count> result = {};

	#define x(s, d, op) result[(int)BuiltinType::s][(int)BuiltinType::d] = LowBinaryOperation::op
	
	x(U8, U8,  left);
	x(U8, U16, zex8to16);
	x(U8, U32, zex8to32);
	x(U8, U64, zex8to64);
	x(U8, S8,  left);
	x(U8, S16, zex8to16);
	x(U8, S32, zex8to32);
	x(U8, S64, zex8to64);
	x(U8, F32, u8_to_f32);
	x(U8, F64, u8_to_f64);

	x(U16, U8,  left);
	x(U16, U16, left);
	x(U16, U32, zex16to32);
	x(U16, U64, zex16to64);
	x(U16, S8,  left);
	x(U16, S16, left);
	x(U16, S32, zex16to32);
	x(U16, S64, zex16to64);
	x(U16, F32, u16_to_f32);
	x(U16, F64, u16_to_f64);

	x(U32, U8,  left);
	x(U32, U16, left);
	x(U32, U32, left);
	x(U32, U64, zex32to64);
	x(U32, S8,  left);
	x(U32, S16, left);
	x(U32, S32, left);
	x(U32, S64, zex32to64);
	x(U32, F32, u32_to_f32);
	x(U32, F64, u32_to_f64);

	x(U64, U8,  left);
	x(U64, U16, left);
	x(U64, U32, left);
	x(U64, U64, left);
	x(U64, S8,  left);
	x(U64, S16, left);
	x(U64, S32, left);
	x(U64, S64, left);
	x(U64, F32, u64_to_f32);
	x(U64, F64, u64_to_f64);

	x(U8, U8,  left);
	x(U8, U16, sex8to16);
	x(U8, U32, sex8to32);
	x(U8, U64, sex8to64);
	x(U8, S8,  left);
	x(U8, S16, sex8to16);
	x(U8, S32, sex8to32);
	x(U8, S64, sex8to64);
	x(U8, F32, u8_to_f32);
	x(U8, F64, u8_to_f64);

	x(U16, U8,  left);
	x(U16, U16, left);
	x(U16, U32, sex16to32);
	x(U16, U64, sex16to64);
	x(U16, S8,  left);
	x(U16, S16, left);
	x(U16, S32, sex16to32);
	x(U16, S64, sex16to64);
	x(U16, F32, u16_to_f32);
	x(U16, F64, u16_to_f64);

	x(U32, U8,  left);
	x(U32, U16, left);
	x(U32, U32, left);
	x(U32, U64, sex32to64);
	x(U32, S8,  left);
	x(U32, S16, left);
	x(U32, S32, left);
	x(U32, S64, sex32to64);
	x(U32, F32, u32_to_f32);
	x(U32, F64, u32_to_f64);

	x(U64, U8,  left);
	x(U64, U16, left);
	x(U64, U32, left);
	x(U64, U64, left);
	x(U64, S8,  left);
	x(U64, S16, left);
	x(U64, S32, left);
	x(U64, S64, left);
	x(U64, F32, u64_to_f32);
	x(U64, F64, u64_to_f64);

	x(F32, U8,  f32_to_u8);
	x(F32, U16, f32_to_u16);
	x(F32, U32, f32_to_u32);
	x(F32, U64, f32_to_u64);
	x(F32, S8,  f32_to_s8);
	x(F32, S16, f32_to_s16);
	x(F32, S32, f32_to_s32);
	x(F32, S64, f32_to_s64);
	x(F32, F32, left);
	x(F32, F64, f32_to_f64);

	x(F64, U8,  f64_to_u8);
	x(F64, U16, f64_to_u16);
	x(F64, U32, f64_to_u32);
	x(F64, U64, f64_to_u64);
	x(F64, S8,  f64_to_s8);
	x(F64, S16, f64_to_s16);
	x(F64, S32, f64_to_s32);
	x(F64, S64, f64_to_s64);
	x(F64, F32, f64_to_f32);
	x(F64, F64, left);

	#undef x

	return result;
}();

BuiltinType as_builtin_type(BuiltinType t) { return t; }
BuiltinType as_builtin_type(BuiltinTypeName *t) { return t->type_kind; }
BuiltinType as_builtin_type(Expression *t) { return as<BuiltinTypeName>(t)->type_kind; }
BuiltinType as_builtin_type(Type t) { return as<BuiltinTypeName>(t)->type_kind; }

LowBinaryOperation builtin_type_conversion_low_op(auto source, auto target)
	requires requires { as_builtin_type(source); as_builtin_type(target); }
{
	return builtin_type_conversion_low_op_table[(int)as_builtin_type(source)][(int)as_builtin_type(target)];
}

Typechecker::ImplicitCastResult Typechecker::implicitly_cast(Expression **_expression, Expression *target_type) {
	auto &expression = *_expression;

	auto source_type = expression->type;
	auto direct_source_type = direct(source_type);
	auto direct_target_type = direct(target_type);

	// Equal types do not need implicit cast
	if (types_match(direct_source_type, direct_target_type)) {
		return { .did_cast = false, .success = true, .apply = []{},};
	}
	
	auto cast_applier = [=, &expression] {
		expression = make_cast(expression, target_type);
	};

	// Autocast
	if (auto unary = as<Unary>(expression)) {
		if (unary->operation == UnaryOperation::atcast) {
			Reporter temp_reporter;
			auto inner_cast_result = implicitly_cast(&unary->expression, target_type, &temp_reporter);
			if (inner_cast_result) {
				// Implicit cast available, just throw atcast out.
				return {
					.success = true,
					.apply = [=, &expression] () mutable {
						inner_cast_result.apply();
						expression = unary->expression;
					},
				};
			} else {
				// Try explicit cast
				auto cast = make_cast(unary->expression, target_type);
				
				with_unwind_strategy {
					typecheck(&cast);
				};

				if (cast->type) {
					return {
						.success = true,
						.apply = [=, &expression] { expression = cast; },
					};
				} else {
					return {.success = false};
				}
			}
		}
	}
	
	// Unsized integer to concrete
	if (types_match(direct_source_type, BuiltinType::UnsizedInteger)) {
		if (::is_concrete_integer(direct_target_type)) {
			return {
				.success = true,
				.apply = [=, &expression] {
					propagate_concrete_type(expression, target_type);
				},
			};
		}
	}

	// Unsized float to concrete
	if (types_match(direct_source_type, BuiltinType::UnsizedFloat)) {
		if (::is_concrete_float(direct_target_type)) {
			return {
				.success = true,
				.apply = [=, &expression] {
					propagate_concrete_type(expression, target_type);
				},
			};
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

							auto valid_due_to_masking = [&] {
								if (auto binary = as<Binary>(expression); binary && binary->operation == BinaryOperation::ban) {
									if (auto mask = coalesce(get_constant_integer(binary->left), get_constant_integer(binary->right))) {
										if ((u64)mask.value() <= ((u64)1 << (dst_size * 8)) - 1) {
											return true;
										}
									}
								}
								return false;
							};

							if (src_sign == dst_sign) {
								if (src_size <= dst_size) {
									return {
										.success = true,
										.apply = cast_applier,
									};
								} else {
									if (valid_due_to_masking()) {
										return {
											.success = true,
											.apply = cast_applier,
										};
									}

									reporter.error(expression->location, "Can't implicitly convert {} to {}, because source is bigger than destination, meaning that there could be information loss.", source_type, target_type);
									return {.success = false};
								}
							} else {
								if (src_size <= dst_size) {
									reporter.error(expression->location, "Can't implicitly convert {} to {}, because the signs don't match.", source_type, target_type);
									return {.success = false};
								} else {
									reporter.error(expression->location, "Can't implicitly convert {} to {}, because source is bigger than destination, meaning that there could be information loss, and the signs don't match.", source_type, target_type);
									return {.success = false};
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
	
	// Float to Float
	if (auto src_builtin_type = as<BuiltinTypeName>(direct_source_type)) {
		switch (src_builtin_type->type_kind) {
			case BuiltinType::F32:
			case BuiltinType::F64: {
				if (auto dst_builtin_type = as<BuiltinTypeName>(direct_target_type)) {
					switch (dst_builtin_type->type_kind) {
						case BuiltinType::F32:
						case BuiltinType::F64: {
							auto src_size = get_size(src_builtin_type->type_kind);
							auto dst_size = get_size(dst_builtin_type->type_kind);

							if (src_size <= dst_size) {
								return {
									.success = true,
									.apply = cast_applier,
								};
							} else {
								reporter.error(expression->location, "Can't implicitly convert {} to {}, because source is bigger than destination, meaning that there could be information loss.", source_type, target_type);
								return {.success = false};
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
			return {
				.success = true,
				.apply = [=, &expression] {
					auto dereference = Unary::create();
					dereference->operation = UnaryOperation::dereference;
					dereference->expression = expression;
					dereference->location = expression->location;
					dereference->type = target_type;
					expression = dereference;
				},
			};
		}
	}

	// None -> pointer
	if (auto none = as<NoneLiteral>(expression)) {
		if (auto target_pointer = as_pointer(direct_target_type)) {
			return {
				.success = true,
				.apply = cast_applier,
			};
		}
	}

	// Anything -> None
	if (types_match(direct_target_type, BuiltinType::None)) {
		return {
			.success = true,
			.apply = cast_applier,
		};
	}

	// Pointer -> Pointer
	if (auto source_pointer = as_pointer(direct_source_type)) {
		if (auto target_pointer = as_pointer(direct_target_type)) {

			bool valid_mutability = 
				(target_pointer->mutability == source_pointer->mutability) ||
				(target_pointer->mutability == Mutability::readonly && source_pointer->mutability == Mutability::variable);

			if (valid_mutability) {
				if (types_match(source_pointer->expression, target_pointer->expression)) {
					// *var T => *let T
					return {
						.success = true,
						.apply = cast_applier,
					};
				} else {
					if (types_match(target_pointer->expression, BuiltinType::None)) {
						// *T => *None
						return {
							.success = true,
							.apply = cast_applier,
						};
					}
				}
			}
		}
	}

	// Zero to pointer
	if (auto literal = as<IntegerLiteral>(expression)) {
		if (literal->value == 0) {
			if (auto pointer = as_pointer(target_type)) {
				return {
					.success = true,
					.apply = [=, &expression] {
						literal->type = get_builtin_type(BuiltinType::U64);
						expression = make_cast(expression, target_type);
					},
				};
			}
		}
	}
	
	// Int -> Enum
	if (auto Enum = as<::Enum>(direct_target_type)) {
		if (Enum->allow_from_int) {
			if (is_any_integer(direct_source_type)) {
				return {
					.success = true,
					.apply = [=, &expression] {
						auto cast = make_cast(expression, target_type);
						cast->low_operation = builtin_type_conversion_low_op(direct_source_type, Enum->underlying_type);
						expression = cast;
					},
				};
			}
		}
	}

	// Enum -> Int
	if (auto Enum = as<::Enum>(direct_source_type)) {
		if (Enum->allow_to_int) {
			if (is_any_integer(direct_target_type)) {
				return {
					.success = true,
					.apply = [=, &expression] {
						auto cast = make_cast(expression, target_type);
						cast->low_operation = builtin_type_conversion_low_op(Enum->underlying_type, direct_target_type);
						expression = cast;
					},
				};
			}
		}
	}

	// SomeEnum -> Enum
	if (auto Enum = as<::Enum>(direct_target_type)) {
		if (auto some_enum = as_some_enum(expression)) {
			auto name = as<Name>(some_enum->expression);
			assert(name);
			auto definition = try_find_enum_value(Enum, name);
			if (definition) {
				return {
					.success = true,
					.apply = [=, &expression] {
						auto literal = IntegerLiteral::create();
						literal->type = Enum;
						literal->location = expression->location;
						literal->value = definition->constant_value.value().U64;
						expression = literal;
					},
				};
			} else {
				reporter.error(expression->location, "Enum {} does not contain {}", Enum->definition ? Enum->definition->name : u8"unnamed"s, name->name);
				reporter.info(Enum->location, "Enum defined here");
				return {.success = false};
			}
		}
	}

	// Custom cast
	{
		auto old_reporter = reporter;
		reporter = {};

		Definition *selected_definition = 0;
		Lambda *selected_lambda = 0;

		bool result = with_unwind_strategy {
			GList<Definition *> possible_definitions;
			resolve_name(possible_definitions, expression->location, definition_name_for_implicit_as);

			for (auto definition : possible_definitions) {
				assert(definition->mutability == Mutability::constant);
				assert(definition->initial_value);

				auto lambda = as<Lambda>(definition->initial_value);
				assert(lambda);

				if (types_match(lambda->head.parameters_block.definition_list[0]->type, expression->type) &&
					types_match(lambda->head.return_type, direct_target_type)
				) {
					// TODO: should detect redefinition of cast operators earlier maybe?
					//       probably in typecheck(Lambda) ?
					if (selected_definition) {
						reporter.error(definition->location, "Redefinition of a cast operator.");
						reporter.error(selected_definition->location, "Previously declared here:");
						fail();
					}

					selected_definition = definition;
					selected_lambda = lambda;
				}
			}

			if (!selected_definition)
				return false;

			return true;
		};
		
		old_reporter.reports.add(reporter.reports);
		this->reporter = old_reporter;

		if (result) {
			return {
				.success = true,
				.apply = [=, &expression] {
					auto callable = Name::create();
					callable->name = definition_name_for_implicit_as;
					callable->location = expression->location;
					callable->possible_definitions.set(selected_definition);
					callable->type = selected_definition->type;

					auto call = Call::create();
					call->location = expression->location;
					call->callable = callable;
					call->arguments.add({.expression = expression, .parameter = selected_lambda->head.parameters_block.definition_list[0]});
					call->type = selected_lambda->head.return_type;
					call->call_kind = CallKind::lambda;
					expression = call;
				},
			};
		}
	
		atomic_increment(&context_base->stats.failed_custom_implicit_casts);
	}

	reporter.error(expression->location, "Expression of type `{}` is not implicitly convertible to `{}`.", source_type, target_type);
	if (auto match = as<Match>(expression)) {
		if (!match->default_case) {
			reporter.help(expression->location, "This match has type None because there is no default case.");
		}
	}

	return {.success = false};
}

void Typechecker::why_is_this_immutable(Expression *expr) {
	if (auto unary = as<Unary>(expr)) {
		if (unary->operation == UnaryOperation::dereference) {
			if (auto name = as<Name>(unary->expression)) {
				auto definition = name->definition();
				assert(definition);
				reporter.help(definition->location, "Because {} is a pointer to {}.", name->name, Meaning(definition->mutability));
				if (definition->initial_value) {
					why_is_this_immutable(definition->initial_value);
				}
			}
		} else if (unary->operation == UnaryOperation::addr) {
			if (auto name = as<Name>(unary->expression)) {
				auto definition = name->definition();
				assert(definition);
				reporter.help(definition->location, "Because {} is marked as {}. Mark it with `var` instead to make it mutable.", name->name, definition->mutability);
			}
		}
	} else if (auto name = as<Name>(expr)) {
		auto definition = name->definition();
		assert(definition);
		reporter.help(definition->location, "Because {} is {}.", name->name, Meaning(definition->mutability));
		if (definition->initial_value) {
			why_is_this_immutable(definition->initial_value);
		}
	} else if (auto dot = as<Binary>(expr); dot && dot->operation == BinaryOperation::dot) {
		if (auto pointer = as_pointer(dot->left->type)) {
			String location = dot->left->location;
			if (auto definition = direct_as<Definition>(dot->left)) {
				location = definition->location;
			}
			reporter.help(location, "Because {} is a pointer to {}.", dot->left, Meaning(pointer->mutability));
		}
	}
}

Expression *Typechecker::inline_body(Call *call, Lambda *lambda) {
	// Code after this assumes that null container means global block.
	// This checks that.
	// Because definition does not have a reference to parent block.
	if (!lambda->definition->container) {
		context->global_block.use([&](auto &global_block) {
			assert(find(global_block.definition_list, lambda->definition));
		});
	}

	if (lambda->definition && !lambda->definition->container) {
		entry->dependency = *typecheck_entries_by_node.find(lambda->definition).value;
	}

	if (!yield_while(call->location, [&] { return lambda->body->type == 0; })) {
		reporter.error(call->location, "Inlining a lambda requires its body to be fully typechecked. This is not the case here.");
		reporter.info(lambda->location, "Lambda defined here:");
		fail();
	}

	entry->dependency = 0;
		
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
					Break->location = ret->location;
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

Expression *Typechecker::make_broadcast(Expression *scalar, u64 count) {
	/*
	
	{
		let __scalar = `scalar`
		.[__scalar, __scalar, __scalar, __scalar]
	}

	*/
	auto block = Block::create();

	auto def = Definition::create();
	def->container = current_container;
	def->initial_value = scalar;
	def->location = scalar->location;
	def->mutability = Mutability::readonly;
	def->name = u8"__scalar"s;
	def->type = scalar->type;
	block->add(def);

	auto arr = ArrayConstructor::create();
	arr->location = scalar->location;
	arr->elements.resize(count);
	arr->type = make_array_type(scalar->type, count);
	for (umm i = 0; i < count; ++i) {
		auto name = Name::create();
		name->location = scalar->location;
		name->possible_definitions.set(def);
		name->name = def->name;
		name->type = def->type;
		arr->elements[i] = name;
	}
	block->add(arr);

	return block;
}

void calculate_parameter_offsets(LambdaHead *head) {
	u64 total_parameters_size = 0;
	for (auto parameter : head->parameters_block.definition_list) {
		parameter->offset = total_parameters_size;
		auto parameter_size = get_size(parameter->type);
		parameter_size = max((u64)1, parameter_size);
		total_parameters_size += parameter_size;
		total_parameters_size = ceil(total_parameters_size, (u64)8);
	}
	head->total_parameters_size = total_parameters_size;
}

Node *Typechecker::vectorize_node(Node *node) {
	scoped_replace(debug_current_location, node->location);
	switch (node->kind) {
		#define x(name) case NodeKind::name: return vectorize_node_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
		#undef x
		default: invalid_code_path();
	}
}
Expression *Typechecker::vectorize_node(Expression *expression) {
	expression = as<Expression>(vectorize_node((Node *)expression));
	assert(expression);
	return expression;
}
Statement *Typechecker::vectorize_node(Statement *statement) {
	statement = as<Statement>(vectorize_node((Node *)statement));
	assert(statement);
	return statement;
}
Block                *Typechecker::vectorize_node_impl(Block *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Call                 *Typechecker::vectorize_node_impl(Call *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Definition           *Typechecker::vectorize_node_impl(Definition *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Expression           *Typechecker::vectorize_node_impl(IntegerLiteral *original) {
	return make_broadcast(original, vc.vector_size);
}
FloatLiteral         *Typechecker::vectorize_node_impl(FloatLiteral *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
BooleanLiteral       *Typechecker::vectorize_node_impl(BooleanLiteral *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
NoneLiteral          *Typechecker::vectorize_node_impl(NoneLiteral *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
StringLiteral        *Typechecker::vectorize_node_impl(StringLiteral *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Lambda               *Typechecker::vectorize_node_impl(Lambda *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
LambdaHead           *Typechecker::vectorize_node_impl(LambdaHead *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Name                 *Typechecker::vectorize_node_impl(Name *original) {
	if (original->possible_definitions.count == 1) {
		auto &original_definition = original->possible_definitions[0];

		// TODO: something better than linear search
		auto found_index = find_index_of(vc.original_lambda->head.parameters_block.definition_list, original_definition);

		if (found_index < vc.original_lambda->head.parameters_block.definition_list.count) {
			auto result = Name::create();
			auto instantiated_definition = vc.instantiated_lambda->head.parameters_block.definition_list[found_index];
			result->possible_definitions.set(instantiated_definition);
			result->type = instantiated_definition->type;
			result->location = original->location;
			result->name = original->name;
			return result;
		}
	}
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Expression           *Typechecker::vectorize_node_impl(IfExpression *original) {
	auto result = IfExpression::create();
	result->location = original->location;
	result->condition = vectorize_node(original->condition);
	result->true_branch = vectorize_node(original->true_branch);
	result->false_branch = vectorize_node(original->false_branch);
	result->type = result->true_branch->type;
	result->is_array = true;
	return result;
}
BuiltinTypeName      *Typechecker::vectorize_node_impl(BuiltinTypeName *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Expression           *Typechecker::vectorize_node_impl(Binary *original) {
	switch (original->operation) {
		case BinaryOperation::add:
		case BinaryOperation::mul: {
			auto result = Binary::create();
			result->operation = original->operation;
			result->left = vectorize_node(original->left);
			result->right = vectorize_node(original->right);
			return result;
		}
	}
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Match                *Typechecker::vectorize_node_impl(Match *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Unary                *Typechecker::vectorize_node_impl(Unary *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Struct               *Typechecker::vectorize_node_impl(Struct *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
ArrayType            *Typechecker::vectorize_node_impl(ArrayType *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Enum                 *Typechecker::vectorize_node_impl(Enum *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Subscript            *Typechecker::vectorize_node_impl(Subscript *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
ArrayConstructor     *Typechecker::vectorize_node_impl(ArrayConstructor *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
ZeroInitialized      *Typechecker::vectorize_node_impl(ZeroInitialized *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
CallerLocation       *Typechecker::vectorize_node_impl(CallerLocation *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
CallerArgumentString *Typechecker::vectorize_node_impl(CallerArgumentString *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
IfStatement          *Typechecker::vectorize_node_impl(IfStatement *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Return               *Typechecker::vectorize_node_impl(Return *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
While                *Typechecker::vectorize_node_impl(While *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
For                  *Typechecker::vectorize_node_impl(For *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Continue             *Typechecker::vectorize_node_impl(Continue *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Break                *Typechecker::vectorize_node_impl(Break *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Import               *Typechecker::vectorize_node_impl(Import *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Defer                *Typechecker::vectorize_node_impl(Defer *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
Use                  *Typechecker::vectorize_node_impl(Use *original) {
	reporter.warning(original->location, "not implemented: {}", original->kind);
	fail();
}
VectorizedLambda Typechecker::get_or_instantiate_vectorized_lambda(Lambda *original_lambda, u64 vector_size, String instantiation_location) {
	return locked_use_ret(vectorized_lambdas) -> VectorizedLambda {
		if (auto found = vectorized_lambdas.find({ original_lambda, vector_size })) {
			return *found.value;
		}
		assert(original_lambda->definition, "Lambda requires a name to be vectorizable. Assign it to a definition.");

		String lambda_name = {};
		if (original_lambda->definition) {
			lambda_name = format(u8"__v_{}_{}"s, original_lambda->definition->name, vector_size);
		} else {
			auto location = get_source_location(original_lambda->location);
			lambda_name = format(u8"__v_{}_{}_{}_{}"s, Nameable(location.file), location.location_line_number, original_lambda->uid, vector_size);
		}

		Reporter reason_reporter;

		// The dumbest implementation that just performs operations one by one.
		// Use this when can't vectorize an operation.
		auto instantiate_fallback_vectorized_lambda = [&] () -> VectorizedLambda {
			StringBuilder source_builder;
			append(source_builder, Repeat{'\0', LEXER_PADDING_SIZE});
			append_format(source_builder, "const {} = fn ("s, lambda_name);
			foreach (it, original_lambda->head.parameters_block.definition_list) {
				auto [i, parameter] = it.key_value();
				if (i) {
					append(source_builder, ", ");
				}

				append_format(source_builder, "{}: [{}]{{{}}}", parameter->name, vector_size, parameter->type);
			}
			append_format(source_builder, "): [{}]{{{}}} => {{\n"
				"	var __i: S64\n"
				"	var __result: [{}]{{{}}}\n"
				"	while __i < {} {{\n"
				"		__result[__i] = {}("
				, vector_size, original_lambda->head.return_type, vector_size, original_lambda->head.return_type, vector_size, original_lambda->definition->name
			);
				
			foreach (it, original_lambda->head.parameters_block.definition_list) {
				auto [i, parameter] = it.key_value();
				if (i) {
					append(source_builder, ", ");
				}
				append_format(source_builder, "{}[__i]", parameter->name);
			}
				
			append_format(source_builder, ")\n"
				//"		println(x[i])\n"
				//"		println(c[i])\n"
				"		__i = __i + 1\n"
				"	}}\n"
				"	__result\n"
				"}}\n"
			);
			append(source_builder, Repeat{'\0', LEXER_PADDING_SIZE});

			auto source = (Span<utf8>)to_string(source_builder);
			source = source.skip(LEXER_PADDING_SIZE).skip(-LEXER_PADDING_SIZE);

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

			if (success) {
				can_generate_vectorized_lambdas = false;
				success = with_unwind_strategy {
					scoped_replace(current_block, &context->global_block.unprotected);
					scoped_replace(current_container, 0);
					scoped_replace(current_loop, 0);
					return typecheck(&definition_node);
				};
				can_generate_vectorized_lambdas = true;
			}

			{
				with(temporary_storage_checkpoint);
				write_entire_file(path, as_bytes(source));

				if (!success) {
					immediate_reporter.error(instantiation_location, "INTERNAL ERROR: Failed to instantiate vectorized lambda for this operation. Generated source code is saved at {}", path);
					fail();
				}
			}

			VectorizedLambda vectorized = {};

			vectorized.instantiated_definition = as<Definition>(definition_node);
			assert(vectorized.instantiated_definition);
			vectorized.instantiated_lambda = as<Lambda>(vectorized.instantiated_definition->initial_value);
			assert(vectorized.instantiated_lambda);

			return vectorized;
		};

		// returns null on failure
		auto instantiate_proper_vectorized_lambda = [&]() -> std::pair<Lambda *, bool> {
			scoped_exchange(reporter, reason_reporter);
			scoped_replace(current_block, &context->global_block.unprotected);
			scoped_replace(current_container, 0);
			scoped_replace(current_loop, 0);

			auto instantiated_lambda = Lambda::create();

			bool success = with_unwind_strategy {
				if (original_lambda->is_extern) {
					reporter.warning(original_lambda->location, "Lambda is extern");
					fail();
				}

				if (original_lambda->is_intrinsic) {
					reporter.warning(original_lambda->location, "Lambda is intrinsic");
					fail();
				}

				auto instantiated_definition = Definition::create();

				auto &original_parameters = original_lambda->head.parameters_block.definition_list;
				auto &instantiated_parameters = instantiated_lambda->head.parameters_block.definition_list;
				
				Copier{}.deep_copy(&original_lambda->head, &instantiated_lambda->head);
				instantiated_lambda->head.return_type = make_array_type(instantiated_lambda->head.return_type, vector_size);
				for (umm i = 0; i < instantiated_parameters.count; ++i) {
					auto &instantiated_parameter = instantiated_parameters[i];

					instantiated_parameter->type = make_array_type(instantiated_parameter->type, vector_size);
				}
				calculate_parameter_offsets(&instantiated_lambda->head);

				instantiated_lambda->inline_status = original_lambda->inline_status;
				instantiated_lambda->definition = instantiated_definition;

				instantiated_definition->initial_value = instantiated_lambda;
				instantiated_definition->container = 0;
				instantiated_definition->mutability = Mutability::constant;
				instantiated_definition->name = lambda_name;

				vc.instantiated_definition = instantiated_definition;
				vc.instantiated_lambda = instantiated_lambda;
				vc.original_lambda = original_lambda;
				vc.instantiation_location = instantiation_location;
				vc.vector_size = vector_size;
				
				current_container = instantiated_lambda;

				instantiated_lambda->body = vectorize_node(original_lambda->body);

				typecheck(&instantiated_lambda);
				
				locked_use_expr(global_block, context->global_block) {
					global_block.add(instantiated_definition);
				};
			
				return instantiated_lambda;
			};

			return {instantiated_lambda, success};
		};

		VectorizedLambda vectorized;
		auto [instantiated_lambda, success] = instantiate_proper_vectorized_lambda();

		if (success) {
			vectorized = {
				.original_lambda = original_lambda,
				.instantiated_lambda = instantiated_lambda,
				.instantiated_definition = instantiated_lambda->definition,
				.vector_size = vector_size,
			};
		} else {
			StringBuilder builder;
			{
				// TODO: print to builder directly?

				scoped_replace(current_printer, (Printer{
					[](Span<utf8> string, void *data) {
						append(*(StringBuilder *)data, string);
					},
					&builder,
				}));

				print_ast(instantiated_lambda->definition);
			}
			auto generated_source_path = tformat(u8"{}\\{}.failed.sp", context_base->generated_source_directory, lambda_name);
			write_entire_file(generated_source_path, builder);

			reporter.warning(instantiation_location, "Could not generate vectorized lambda. Falling back on array of scalar operations.");
			for (auto &report : reason_reporter.reports)
				report.indentation += 1;
			reporter.reports.add(reason_reporter.reports);
			reporter.info("Source of instantiated lambda can be seen in {}", generated_source_path);
			reporter.info(original_lambda->location, "Here is the original lambda:");

			vectorized = instantiate_fallback_vectorized_lambda();
		}
		
		vectorized_lambdas.insert({ original_lambda, vector_size }, vectorized);
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
				if (auto cl = as<CallerLocation>(parameter->initial_value)) {
					auto l = get_source_location(call_location);
					argument.expression = make_string(format(u8"{}:{}:{}", l.file, l.lines_start_number, l.location_column_number), call_location);
				} else if (auto cas = as<CallerArgumentString>(parameter->initial_value)) {
					auto target_argument = sorted_arguments[find_index_of(parameters, cas->parameter)];
					argument.expression = make_string(target_argument.expression ? target_argument.expression->location : String{}, call_location);
				} else {
					argument.expression = Copier{}.deep_copy(parameter->initial_value);
				}
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

	return typecheck_lambda_call(call, instantiated_lambda, &instantiated_lambda->head, true).expression;
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

Typechecker::TypecheckLambdaCallResult Typechecker::typecheck_lambda_call(Call *call, Lambda *lambda, LambdaHead *head, bool apply) {

	auto &arguments = call->arguments;
	auto &callable = call->callable;

	if (lambda && lambda->head.is_template) {
		immediate_reporter.warning(lambda->location, "TODO: implement template instantiation caching");
		return {.expression = instantiate_lambda_template(call, lambda)};
	}

	if (!yield_while(call->location, [&] { return head->return_type == 0; })) {
		reporter.error(call->location, "INTERNAL ERROR: Lambda `{}` was not properly typechecked. Its return type is not set.", call->callable->location);
		reporter.info(head->location, "That lambda is here:");
		fail();
	}

	auto &parameters = head->parameters_block.definition_list;
	
	sort_arguments(arguments, parameters, call->location, head, lambda ? lambda->definition : 0);

	if (can_generate_vectorized_lambdas && lambda && parameters.count) {
		bool vectorizable = true;
		u64 vector_size = 0;

		for (umm i = 0; i < arguments.count; ++i) {
			if (auto array = as<ArrayType>(arguments[i].expression->type)) {
				if (types_match(array->element_type, parameters[i]->type)) {
					if (vector_size == 0) {
						vector_size = array->count.value();
						continue;
					} else if (vector_size == array->count.value()) {
						continue;
					}
				}
			}
			vectorizable = false;
			break;
		}

		if (vectorizable) {
			auto vectorized_lambda = get_or_instantiate_vectorized_lambda(lambda, vector_size, call->location);

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

	TypecheckLambdaCallResult result = {};

	result.expression = call;

	List<ImplicitCastResult, TemporaryAllocator> argument_implicit_casts;

	for (umm i = 0; i < arguments.count; ++i) {
		auto &argument = arguments[i];
		auto &parameter = head->parameters_block.definition_list[i];

		argument.parameter = parameter;

		auto cast_result = implicitly_cast(&argument.expression, parameter->type);

		if (!cast_result) {
			reporter.info(parameter->location, "Parameter declared here:");
			fail();
		}

		result.number_of_implicit_casts += cast_result.did_cast;

		argument_implicit_casts.add(cast_result);
	}

	if (apply) {
		for (auto &cast : argument_implicit_casts) {
			cast.apply();
		}

		call->call_kind = CallKind::lambda;

		if (lambda && should_inline(call, lambda)) {
			result.expression = inline_body(call, lambda);
		} else {
			call->type = head->return_type;
		}
	}
	return result;
};
Expression *Typechecker::typecheck_constructor(Call *call, Struct *Struct) {
	call->call_kind = CallKind::constructor;

	auto &arguments = call->arguments;
	auto &members = Struct->member_list;
	
	sort_arguments(arguments, members, call->location, Struct, Struct->definition, {.allow_missing = true});

	for (umm i = 0; i < arguments.count; ++i) {
		auto &argument = arguments[i];
		auto &member = members[i];

		argument.parameter = member;

		if (argument.expression) {
			if (!implicitly_cast_apply(&argument.expression, member->type)) {
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

Definition *Typechecker::try_find_enum_value(Enum *Enum, Name *name) {
	auto found = Enum->block.definition_map.find(name->name);
	if (!found) {
		reporter.error(name->location, "Enum {} does not contain value named {}.", Enum, name->name);
		return 0;
	}

	auto &definitions = *found.value;
	assert(definitions.count == 1, "INTERNAL ERROR: parser was supposed to deal with multiple definitions with the same name in enum");

	auto &definition = definitions[0];
			
	if (!yield_while(name->location, [&] { return definition->type == 0; })) {
		reporter.error(name->location, "Could not wait for definition's type");
		reporter.info(definition->location, "Definition here");
		return 0;
	}

	if (!yield_while(name->location, [&] { return definition->type->type == 0; })) {
		reporter.error(name->location, "Could not wait for definition type's type");
		reporter.info(definition->location, "Definition here");
		return 0;
	}
	return definition;
}
Definition *Typechecker::find_enum_value(Enum *Enum, Name *name) {
	if (auto found = try_find_enum_value(Enum, name)) {
		return found;
	}
	fail();
}

Expression *Typechecker::typecheck_binary_dot(Binary *binary) {
	typecheck(&binary->left);
	auto direct_left_type = direct(binary->left->type);
	auto struct_ = as<Struct>(direct_left_type);
	if (!struct_) {
		if (auto pointer = as<Unary>(direct_left_type); pointer && pointer->operation == UnaryOperation::pointer) {
			struct_ = direct_as<Struct>(pointer->expression);
		}
	}

	if (!struct_) {
		if (types_match(direct_left_type, context->builtin_structs.String)) {
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
		if (auto Enum = direct_as<::Enum>(binary->left)) {
			auto name = as<Name>(binary->right);
			if (!name) {
				reporter.error(binary->right->location, "Expression after dot must be a name, because {} is an enum.", binary->left->location);
				fail();
			}

			auto definition = find_enum_value(Enum, name);
			auto literal = IntegerLiteral::create();
			literal->type = Enum;
			literal->location = name->location;
			literal->value = definition->constant_value.value().U64;
			return literal;
		}

		reporter.error(binary->left->location, "Left of the dot must be a struct / pointer to struct / enum.");
		fail();
	}

	auto member_name = as<Name>(binary->right);
	if (!member_name) {
		reporter.error(binary->right->location, "Expression after dot must be a name, because {} is a struct.", binary->left->location);
		fail();
	}

	if (auto found = struct_->member_map.find(member_name->name)) {
		auto definition = *found.value;
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
		for (auto Defer : block->defers) {
			defers.add(Defer);
		}
	}
}

void Typechecker::ensure_mutable(Expression *expression) {
	auto result = is_mutable(expression);
	if (!result) {
		reporter.error(expression->location, "This expression can not be modified.");
		// reporter.help(result.error()->location, "Because this is not mutable.");
		why_is_this_immutable(expression);
		fail();
	}
}

bool Typechecker::can_reference(Block *block, Definition *definition) {
	// Definition order in lambdas or heads matters - you can't refence stuff that is declared later.
	// Because lambda content typechecking happens in order, just check if the type is set - if it is,
	// the definition is declared before, otherwise after.

	if (block->container && block->container == definition->container && (as<Lambda>(block->container) || as<LambdaHead>(block->container))) {
		if (!definition->type) {
			return false;
		}
	}

	return true;
}

LambdaHead *container_as_lambda_head(Expression *container) {
	if (!container)
		return 0;
	switch (container->kind) {
		case NodeKind::Lambda: return &((Lambda *)container)->head;
		case NodeKind::LambdaHead: return (LambdaHead *)container;
		default: return 0;
	}
}

void Typechecker::resolve_name_in_block(GList<Definition *> &possible_definitions, Block *block, String location, String name) {
	// TODO: should take the lock if block is global, but
	// 
	// make_scoped(lock, LockIfBlockIsGlobal{block});
	scoped_lock_if_block_is_global(block);

	for (auto definition : to_optional(block->definition_map.find(name).value).value_or({})) {
		if (!can_reference(block, definition)) {
			continue;
		}
					
		possible_definitions.add(definition);


		if (block == &context->global_block.unprotected) {
			entry->dependency = *typecheck_entries_by_node.find(definition).value;
		}
			
		if (name == "test") {
			int x = 42;
		}

		if (!yield_while(location, [&] { return definition->type == 0; })) {
			// Sometimes this error is meaningless and noisy because is is caused by another error.
			// But other times compiler fails with only this error, which is not printed in case
			// print_wait_failures is false.

			//if (print_wait_failures) {
				reporter.error(location, "Definition referenced by this name was not properly typechecked.");
				reporter.info(definition->location, "Here is the bad definition:");
			//}
			fail();
		}

		entry->dependency = 0;
	}
}
void Typechecker::resolve_name(GList<Definition *> &possible_definitions, String location, String name) {
	if (name == "bar") {
		int x = 5;
	}

	possible_definitions.clear();

	// FIXME: Duplicated :REACHABLE_BLOCK_NAMES:
	auto previous_head = container_as_lambda_head(current_block->container);

	for (auto block = current_block; block; block = block->parent) {
		if (container_as_lambda_head(block->container) != previous_head) {
			// Skip outer container, so nested lambda can't access stuff in outer lambda.
			block = &context->global_block.unprotected;
			previous_head = 0;
		}

		resolve_name_in_block(possible_definitions, block, location, name);

		if (possible_definitions.count) {
			return;
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
				if (!implicitly_cast_apply(other, picked_value->type, &cast_reporter)) {
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
	if (!definition->is_parameter && !definition->is_template_parameter) {
		if (current_container) {
			if (auto lambda = as<Lambda>(current_container)) {
				lambda->locals.add(definition);
			}
		}
	}

	if (definition->uid == 3651) {
		int x = 41;
	}
	if (definition->name == "as_implicit") {
		int x = 41;
	}

	if (definition->parsed_type) {
		typecheck(&definition->parsed_type);
	} else {
		assert(definition->initial_value);
	}

	if (definition->initial_value) {
		typecheck(&definition->initial_value);

		if (definition->mutability == Mutability::constant) {

			if (definition->parsed_type) {
				if (!implicitly_cast_apply(&definition->initial_value, definition->parsed_type)) {
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
				if (!implicitly_cast_apply(&definition->initial_value, definition->parsed_type)) {
					fail();
				}
			} else {
				make_concrete(definition->initial_value);
			}
		}
		
		if (!current_container && current_block == &context->global_block.unprotected || definition->mutability == Mutability::constant) {
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
		scoped_lock_if_block_is_global(current_block);
		// This is false when definition is used as a expression inside something
		// assert(find(current_block->children, definition));
		assert(find(current_block->definition_list, definition));
		assert(current_block->definition_map.find(definition->name));
	}

	return definition;
}
IntegerLiteral   *Typechecker::typecheck_impl(IntegerLiteral *literal, bool can_substitute) {
	literal->type = get_builtin_type(BuiltinType::UnsizedInteger);
	return literal;
}
FloatLiteral     *Typechecker::typecheck_impl(FloatLiteral *literal, bool can_substitute) {
	literal->type = get_builtin_type(BuiltinType::UnsizedFloat);
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
		
		calculate_parameter_offsets(head);

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

	scoped_replace(currently_used_definitions, {});
	for (auto parameter : lambda->head.parameters_block.definition_list) {
		if (parameter->use) {
			currently_used_definitions.add(parameter);
		}
	}

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
				if (!implicitly_cast_apply(&ret->value, lambda->head.return_type)) {
					reporter.info(lambda->head.return_type->location, "Return type specified here:");
					fail();
				}
			}

			if (!all_paths_return) {
				if (lambda->body) {
					if (!implicitly_cast_apply(&lambda->body, lambda->head.return_type)) {
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
						if (!implicitly_cast_apply(other, picked_value->type, &cast_reporter)) {
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
							if (!implicitly_cast_apply(&ret->value, lambda->returns[0]->value->type)) {
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
	if (name->name == "all_true") {
		int x = 42;
	}

	name->possible_definitions.clear();
	
	// FIXME: Duplicated :REACHABLE_BLOCK_NAMES:
	auto previous_head = container_as_lambda_head(current_block->container);

	for (auto block = current_block; block; block = block->parent) {
		if (container_as_lambda_head(block->container) != previous_head) {
			// Skip outer container, so nested lambda can't access stuff in outer lambda.
			block = &context->global_block.unprotected;
			previous_head = 0;
		}

		resolve_name_in_block(name->possible_definitions, block, name->location, name->name);

		if (name->possible_definitions.count == 0) {
			Definition *previous_successful_definition = 0;
			Binary *previous_successful_dot = 0;

			for (auto definition : currently_used_definitions) {
				
				assert(can_reference(block, definition), "currently_used_definitions should not contain unreferencable stuff!");
				// if (!can_reference(definition)) {
				// 	continue;
				// }



				if (yield_while(name->location, [&] { return definition->type == 0; })) {
					auto direct_definition_type = direct(definition->type);
					auto Struct = as<::Struct>(direct_definition_type);
					if (!Struct) {
						if (auto pointer = as_pointer(direct_definition_type)) {
							Struct = direct_as<::Struct>(pointer->expression);
						}
					}

					if (Struct) {
						if (auto found = Struct->member_map.find(name->name)) {
							auto member = *found.value;
								
							auto definition_name = Name::create();
							definition_name->location = name->location;
			
							auto dot = make_binary(BinaryOperation::dot, definition_name, name, 0, name->location);

							definition_name->name = definition->name;
							definition_name->type = 0;

							name->type = 0;
							
							with_unwind_strategy {
								typecheck(&dot);
							};

							if (dot->type) {
								if (previous_successful_dot) {
									reporter.error(name->location, "Multiple `use`d definitions contain member {}.", name->name);
									reporter.indentation += 1;
									reporter.info(previous_successful_definition->location, "First:");
									reporter.info(definition->location, "Second:");
									reporter.help("Do {}.{} or {}.{} to disambiguate.", previous_successful_definition->name, name->name, definition->name, name->name);
									reporter.indentation -= 1;
									fail();
								}
								previous_successful_dot = dot;
								previous_successful_definition = definition;
							}
						}
					}
				}
			}

			if (previous_successful_dot) {
				return previous_successful_dot;
			}
		}

		if (name->possible_definitions.count == 0) {
			continue;
		} else if (name->possible_definitions.count == 1) {
			auto definition = name->possible_definitions.data[0];
			name->type = definition->type;

			if (context_base->constant_name_inlining) {
				if (definition->mutability == Mutability::constant) {
					// NOTE: Even though definition is constant, definition->initial_value can be null
					// if it is an unresolved template parameter.
					if (definition->initial_value) {
						switch (definition->initial_value->kind) {
							case NodeKind::Lambda:
							case NodeKind::Struct:
							case NodeKind::Enum:
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
	reporter.error(name->location, "`{}` was not declared.", name->name);
	fail();
	return 0;
}
Expression       *Typechecker::typecheck_impl(Call *call, bool can_substitute) {
	defer { assert(call->callable->type != 0); };
	if (call->location == u8"foo(.[1, 2, 3, 4], .[true, true, false, false])"s) {
		int x = 4;
	}
	if (call->uid == 356) {
		int x = 4;
	}
	if (auto binary = as<Binary>(call->callable)) {
		if (binary->operation == BinaryOperation::dot) {
			if (auto lambda_name = as<Name>(binary->right)) {
				Reporter dot_binop_reporter;

				{
					scoped_exchange(reporter, dot_binop_reporter);
					if (with_unwind_strategy { return call->callable = typecheck_binary_dot(binary); }) {
						dot_binop_reporter.reports.add(reporter.reports);
						goto typecheck_dot_succeeded;
					}
				}


				call->callable = binary->right;
						
				// Attempt passing `this` as follows (return on first successful attempt):
				//     1. As-is.
				//     2. By pointer.

				call->arguments.insert_at({.expression = binary->left}, 0);

				// :PERFORMANCE:
				// Deep copy on each dot call seems bad.
				// TODO: Could syntactically disambiguate dot member calls and dot lambda calls by parenthesizing.
				auto call_copy = Copier{}.deep_copy(call);

				Reporter as_is_reporter;
				{
					scoped_exchange(reporter, as_is_reporter);
					if (auto result = with_unwind_strategy { return typecheck_impl(call, can_substitute); }) {
						as_is_reporter.reports.add(reporter.reports);
						return result;
					}
				}
				
				call = call_copy;

				Reporter by_pointer_reporter;
				{
					scoped_exchange(reporter, by_pointer_reporter);

					auto first_argument_address = Unary::create();
					first_argument_address->location = binary->left->location;
					first_argument_address->expression = binary->left;
					first_argument_address->operation = UnaryOperation::addr;
					call->arguments[0].expression = first_argument_address;

					if (auto result = with_unwind_strategy { return typecheck_impl(call, can_substitute); }) {
						by_pointer_reporter.reports.add(reporter.reports);
						return result;
					}
				}

				// All three attempts failed. Report stuff.
					
				// Hacky ways of finding useless reports
				bool member_useful     = dot_binop_reporter.reports.count  && !find(dot_binop_reporter.reports[0].message, u8"does not contain member named"s);
				bool by_pointer_useful = by_pointer_reporter.reports.count && !find(by_pointer_reporter.reports[0].message, u8"is not addressable"s);
				bool by_value_useful = true;

				struct AttemptReport {
					String message;
					Span<Report> reports;
				};

				List<AttemptReport> useful_attempt_reports;
				if (member_useful)     useful_attempt_reports.add({tformat(u8"Attempt to find member {} in {}:", binary->right, binary->left),                       dot_binop_reporter.reports});
				if (by_value_useful)   useful_attempt_reports.add({tformat(u8"Attempt to pass {} as first argument to {} by value:", binary->left, binary->right),   as_is_reporter.reports});
				if (by_pointer_useful) useful_attempt_reports.add({tformat(u8"Attempt to pass {} as first argument to {} by pointer:", binary->left, binary->right), by_pointer_reporter.reports});
					
				// Don't include attempts that have identical reports.
				for (umm i = 1; i < useful_attempt_reports.count; ++i) {
					if (useful_attempt_reports[i].reports == useful_attempt_reports[0].reports) {
						useful_attempt_reports.erase_unordered_at(i);
						--i;
					}
				}

				if (useful_attempt_reports.count > 1) {
					reporter.error(call->location, "Unable to typecheck this dot call. Here are attempt results:");
					for (auto &attempt_reports : useful_attempt_reports) {
						for (auto &r : attempt_reports.reports) {
							++r.indentation;
						}
						reporter.info(String{}, attempt_reports.message);
						reporter.reports.add(attempt_reports.reports);
					}
				} else {
					if (useful_attempt_reports.count == 0) {
						immediate_reporter.error(call->location, "INTERNAL ERROR. Didn't produce reports for failed dot call!!!");
					}
					for (auto &attempt_reports : useful_attempt_reports) {
						reporter.reports.add(attempt_reports.reports);
					}
				}
				fail();
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
		return typecheck_lambda_call(call, lambda, &lambda->head).expression;
	} else if (auto definition = as<Definition>(directed_callable)) {
		if (auto lambda_head = direct_as<LambdaHead>(definition->type)) {
			return typecheck_lambda_call(call, 0, lambda_head).expression;
		} else {
			reporter.error(call->callable->location, "Expression of type {} is not callable.", call->callable->type);
			reporter.info(definition->location, "See definition");
			fail();
			return 0;
		}
	} else if (auto name = as<Name>(directed_callable)) {
		assert(name->possible_definitions.count > 1);


		struct Overload {
			Definition *definition = 0;
			Lambda *lambda = 0;
			LambdaHead *lambda_head = 0;
			umm number_of_implicit_casts = 0;
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
			with_unwind_strategy {
				auto result = typecheck_lambda_call(call, overload.lambda, overload.lambda_head, false);
				assert(result.expression);
				overload.number_of_implicit_casts = result.number_of_implicit_casts;
				matching_overloads.add(&overload);
			};
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

		quick_sort(matching_overloads, [&](Overload *o) { return o->number_of_implicit_casts; });

		if (matching_overloads.count == 1 || matching_overloads[0]->number_of_implicit_casts < matching_overloads[1]->number_of_implicit_casts) {
			auto matching_overload = matching_overloads[0];
			name->possible_definitions.set(matching_overload->definition);
			name->type = name->definition()->type;
			return typecheck_lambda_call(call, matching_overload->lambda, matching_overload->lambda_head).expression;
		}

		reporter.error(call->location, "Multiple matching overload were found:");
		foreach (it, matching_overloads) {
			auto [i, overload] = it.key_value();
			reporter.info(overload->definition->location, "Overload #{}:", i);
		}
		fail();
	} else if (auto head = as<LambdaHead>(directed_callable->type)) {
		return typecheck_lambda_call(call, 0, head, true).expression;
	}

	reporter.error(call->callable->location, "Expression of type {} can't be called", call->callable->type);
	fail();
	return 0;
}
Node             *Typechecker::typecheck_impl(IfStatement *If, bool can_substitute) {
	If->condition = make_cast(If->condition, get_builtin_type(BuiltinType::Bool));
	If->condition->type = 0;

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

	auto condition_array_type = direct_as<ArrayType>(If->condition->type);
	if (!condition_array_type) {
		// Not array. Must be scalar.
		If->condition = make_cast(If->condition, get_builtin_type(BuiltinType::Bool));
		If->condition->type = 0;

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
			auto t2f = implicitly_cast(&If->true_branch, If->false_branch->type, &cast_reporter);
			auto f2t = implicitly_cast(&If->false_branch, If->true_branch->type, &cast_reporter);

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
				t2f.apply();
				If->type = If->true_branch->type;
			} else {
				f2t.apply();
				If->type = If->false_branch->type;
			}
		}

		if (auto value_ = get_constant_value(If->condition)) {

			NOTE_LEAK(If);

			auto value = value_.value();
			assert(value.kind == ValueKind::Bool);
			return value.Bool ? If->true_branch : If->false_branch;
		}
	} else {
		If->is_array = true;
		typecheck(&If->true_branch);
		typecheck(&If->false_branch);
		if (!types_match(If->true_branch->type, If->false_branch->type)) {
			reporter.error(If->location, "Branch types of `array if` must match");
			reporter.info(If->true_branch->location, "Type of true branch is {}", If->true_branch->type);
			reporter.info(If->false_branch->location, "Type of false branch is {}", If->false_branch->type);
			fail();
		}
		If->type = If->true_branch->type;
	}

	return If;
}
BuiltinTypeName  *Typechecker::typecheck_impl(BuiltinTypeName *type, bool can_substitute) { 
	type->type = get_builtin_type(BuiltinType::Type);
	return type;
}
Expression       *Typechecker::typecheck_impl(Binary *binary, bool can_substitute) {
	if (binary->uid == 478) {
		int x = 4;
	}
	if (binary->location == "a += b") {
		int x = 4;
	}
	if (binary->operation == BinaryOperation::dot) {
		//
		// Struct member access
		//
		Reporter member_reporter;
		{
			scoped_exchange(reporter, member_reporter);
			if (auto result = with_unwind_strategy { return typecheck_binary_dot(binary); }) {
				member_reporter.reports.add(reporter.reports);
				return result;
			}
		}

		//
		// Get property
		// 
		// Replace
		// 
		//     x.length
		//
		// With
		// 
		//     get_length(x)

		Reporter property_reporter;
		{
			scoped_exchange(reporter, property_reporter);
			if (auto result = with_unwind_strategy {
				auto member_name = as<Name>(binary->right);
				if (!member_name) {
					reporter.error(binary->right->location, "Expected this to be a name for get property to work");
					fail();
				}

				auto callable = Name::create();
				callable->name = format(u8"get_{}", member_name->name);
				callable->location = member_name->location;

				auto call = Call::create();
				call->location = binary->location;
				call->callable = callable;
				call->arguments.add({.expression = make_address_if_addressable(binary->left)});

				typecheck(&call);

				NOTE_LEAK(binary);
				NOTE_LEAK(member_name);
				return call;
			}) {
				property_reporter.reports.add(reporter.reports);
				return result;
			}
		}

		#if 1
		reporter.reports.add(member_reporter.reports);
		// reporter.error(binary->location, "No member {} found, nor function get_{}.", member_name->name, member_name->name);
		#else
		bool member_useful = member_reporter.reports.count  && !find(member_reporter.reports[0].message, u8"does not contain member named"s);
		bool property_useful = property_reporter.reports.count  && !find(property_reporter.reports[0].message, tformat(u8"`get_{}` was not declared"s, member_name->name));
		if (member_useful && !property_useful) {
			reporter.reports.add(member_reporter.reports);
		} else if (!member_useful && property_useful) {
			reporter.reports.add(property_reporter.reports);
		} else {
			for (auto &r : member_reporter  .reports) r.indentation += 1; 
			for (auto &r : property_reporter.reports) r.indentation += 1; 

			reporter.error(binary->location, "Could not typecheck this dot operation.");
			reporter.info("Attempt to treat as member access:");
			reporter.reports.add(member_reporter.reports);
			reporter.info("Attempt to treat as get property:");
			reporter.reports.add(property_reporter.reports);
		}
		#endif

		fail();
	} else {
		if (binary->operation == BinaryOperation::ass) {
			auto old_binary = Copier{}.deep_copy(binary);

			//
			// Regular assignment
			//
			Reporter regular_reporter;
			{
				scoped_exchange(reporter, regular_reporter);
				if (auto result = with_unwind_strategy {
					typecheck(&binary->left);
					typecheck(&binary->right);

					if (!is_addressable(binary->left)) {
						reporter.error(binary->left->location, "This expression does not have an address, so it can't be modified");
						fail();
					}
					ensure_mutable(binary->left);
					if (!implicitly_cast_apply(&binary->right, binary->left->type)) {
						fail();
					}
					binary->type = get_builtin_type(BuiltinType::None);
					return binary;
				}) {
					regular_reporter.reports.add(reporter.reports);
					return result;
				}
			}

			binary = old_binary;
			if (auto dot_binop = as<Binary>(binary->left); dot_binop && dot_binop->operation == BinaryOperation::dot) {
				
				//
				// Set property
				//
				



				/*
				
				====================================
				@@@@@@@@ b is property
				====================================
						a.b = 42
					becomes
						set_b(&a, 42)
				------------------------------------
						a[1].b = 42
					becomes
						set_b(&a[1], 42)
				------------------------------------
						a.b[1] = 42
					becomes
						var __tmp = get_b(a)
						__tmp[1] = 42
						set_b(a, __tmp)
				====================================
				@@@@@@@@ b is property, c is member
				====================================
						a.b.c = 42
					becomes
						var __tmp = get_b(a)
						__tmp.c = 42
						set_b(&a, __tmp)
				====================================
				@@@@@@@@ b is property, c is property
				====================================
						a.b.c = 42
					becomes
						var __tmp = get_b(a)
						set_c(&__tmp, 42)
						set_b(&a, __tmp)
				------------------------------------
						a[1].b.c = 42
					becomes
						var __tmp = get_b(a[1])
						set_c(&__tmp, 42)
						set_b(&a[1], __tmp)
				------------------------------------
						a.b[1].c = 42
					becomes
						var __tmp = get_b(a)
						set_c(&__tmp[1], 42)
						set_b(&a, __tmp)

				
				
				
				
				
				*/



				auto is_base_case = [&]() -> bool {
					switch (dot_binop->left->kind) {
						case NodeKind::Name:
						case NodeKind::Subscript:
							return true;
					}
					return false;
				};

				if (is_base_case()) {
					/*
				
					a.b = 42

						Should become:

					set_b(&a, 42)

					*/
					auto name = as<Name>(dot_binop->right);
					assert(name);

					auto callable = Name::create();
					callable->location = name->location;
					callable->name = format(u8"set_{}", name->name);


					auto call = Call::create();
					call->location = binary->location;
					call->callable = callable;
					call->arguments.add({.expression = make_address_if_addressable(dot_binop->left)});
					call->arguments.add({.expression = binary->right});

					typecheck(&call);

					return call;
				} else {
					/*
					------------------------------------
							a.b.c.d = 42
						becomes
							var __tmp = a.b
							__tmp.c.d = 42
							a.b = __tmp
					------------------------------------
							a.b[1].c.d = 42
						becomes
							var __tmp = a.b
							__tmp[1].c.d = 42
							a.b = __tmp
					------------------------------------
					*/

					Binary *first_dot = dot_binop;
					Binary *second_dot = 0;
					Expression *current = dot_binop->left;
					while (1) {
						while (1) {
							if (auto subscript = as<Subscript>(current)) {
								current = subscript->subscriptable;
							} else {
								break;
							}
						}

						if (auto dot = as<Binary>(current); dot && dot->operation == BinaryOperation::dot) {
							second_dot = first_dot;
							first_dot = dot;
							current = dot->left;
							continue;
						} 

						break;
					}

					if (!second_dot) {
						reporter.error(dot_binop->location, "Could not split this expression into multiple property accesses");
						reporter.help(binary->location, "Try splitting this into multiple statements");
						fail();
					}


					auto block = Block::create();
					block->location = binary->location;
					block->parent = current_block;
					block->container = current_container;
				
					// var __tmp = a.b
					auto tmp = Definition::create();
					tmp->location = first_dot->location;
					tmp->container = current_container;
					tmp->initial_value = first_dot;
					tmp->mutability = Mutability::variable;
					tmp->name = format(u8"__tmp_{}", tmp->uid);
					block->add(tmp);

					// __tmp.c.d = 42
					if (auto subscript = as<Subscript>(second_dot->left)) {
						subscript->subscriptable = make_name(tmp, tmp->location);
					} else {
						second_dot->left = make_name(tmp, tmp->location);
					}
					block->add(binary);
				
					// a.b = __tmp
					auto ass = make_binary(BinaryOperation::ass, Copier{}.deep_copy(first_dot), make_name(tmp, tmp->location), 0, binary->location);
					block->add(ass);
				
					// Allow block to be substituted
					return as<Expression>(typecheck((Node *)block, true));
				}
			}

			reporter.reports.add(regular_reporter.reports);
			fail();
		}

		typecheck(&binary->left);
		typecheck(&binary->right);

		switch (binary->operation) {
			case BinaryOperation::as: {
				auto source_type = direct(binary->left->type);
				auto target_type = direct(binary->right);

				if (!is_type(target_type)) {
					reporter.error(binary->right->location, "Right hand side of `as` expression must be a type.");
					fail();
				}

				if (types_match(source_type, target_type)) {
					NOTE_LEAK(binary);
					return binary->left;
				}

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

					// To boolean
					if (types_match(target_type, get_builtin_type(BuiltinType::Bool))) {
						binary->low_operation = LowBinaryOperation::left_to_bool;
						binary->type = binary->right;
						return binary;
					}
				}
				
				// From concrete integer
				if (is_concrete_integer(source_type)) {
					// To integer
					if (is_concrete_integer(target_type)) {
						binary->low_operation = builtin_type_conversion_low_op(as<BuiltinTypeName>(source_type)->type_kind, as<BuiltinTypeName>(target_type)->type_kind);
						binary->type = binary->right;
						return binary;
					}
						
					// To float
					if (is_concrete_float(target_type)) {
						binary->low_operation = builtin_type_conversion_low_op(as<BuiltinTypeName>(source_type)->type_kind, as<BuiltinTypeName>(target_type)->type_kind);
						binary->type = binary->right;
						return binary;
					}
						
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						binary->low_operation = integer_extension_low_op(get_size(source_type), is_signed_integer(source_type), 8);
						binary->type = binary->right;
						return binary;
					}

					// To boolean
					if (types_match(target_type, get_builtin_type(BuiltinType::Bool))) {
						binary->low_operation = LowBinaryOperation::left_to_bool;
						binary->type = binary->right;
						return binary;
					}
				}

				// From integer literal
				if (auto literal = as<IntegerLiteral>(binary->left)) {
					// To integer
					if (is_concrete_integer(target_type)) {
						literal->type = binary->right;
						NOTE_LEAK(binary);
						return literal;
					}
						
					// To float
					if (is_concrete_float(target_type)) {
						literal->type = binary->right;
						NOTE_LEAK(binary);
						return literal;
					}
						
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						binary->type = binary->right;
						return binary;
					}
				}
				
				// From concrete float
				if (is_concrete_float(source_type)) {
					// To integer
					if (is_concrete_integer(target_type)) {
						binary->low_operation = builtin_type_conversion_low_op(as<BuiltinTypeName>(source_type)->type_kind, as<BuiltinTypeName>(target_type)->type_kind);
						binary->type = binary->right;
						return binary;
					}
						
					// To float
					if (is_concrete_float(target_type)) {
						binary->low_operation = builtin_type_conversion_low_op(as<BuiltinTypeName>(source_type)->type_kind, as<BuiltinTypeName>(target_type)->type_kind);
						binary->type = binary->right;
						return binary;
					}
						
					// To boolean
					if (types_match(target_type, get_builtin_type(BuiltinType::Bool))) {
						binary->low_operation = LowBinaryOperation::left_to_bool;
						binary->type = binary->right;
						return binary;
					}
				}

				// From float literal
				if (auto literal = as<FloatLiteral>(binary->left)) {
					// To integer
					if (is_concrete_integer(target_type)) {
						literal->type = binary->right;
						NOTE_LEAK(binary);
						return literal;
					}
						
					// To float
					if (is_concrete_float(target_type)) {
						literal->type = binary->right;
						NOTE_LEAK(binary);
						return literal;
					}
				}
				
				// From none to any type
				if (auto literal = as<NoneLiteral>(binary->left)) {
					binary->type = binary->right;
					binary->low_operation = LowBinaryOperation::zeroinit;
					return binary;
				}

				// Try implicit cast last
				// because of auto dereference
				Reporter cast_reporter;
				if (auto cast_result = implicitly_cast(&binary->left, binary->right, &cast_reporter)) {
					cast_result.apply();
					return binary->left;
				}

				reporter.error(binary->location, "No conversion from {} to {} is available.", binary->left->type, binary->right);
				fail();
				return 0;
			}
		}

		auto dleft  = direct(binary->left->type);
		auto dright = direct(binary->right->type);

		/* Pointer arithmetic */ {
			auto handle_ptr_plus_or_minus_int = [&](Unary *pointer_type, Expression *&pointer_value, Expression *&int_value, LowBinaryOperation op) -> Binary * {
				if (is_concrete_integer(int_value->type)) {
					if (get_size(int_value->type) != 8) {
						int_value = make_cast(int_value, get_builtin_type(BuiltinType::S64));
					}
					binary->type = pointer_value->type;
					binary->low_operation = op;
				} else if (auto literal = as<IntegerLiteral>(int_value)) {
					literal->type = get_builtin_type(BuiltinType::S64);
					binary->type = pointer_value->type;
					binary->low_operation = op;
				} else if (types_match(int_value->type, BuiltinType::UnsizedInteger)) {
					int_value = make_cast(int_value, get_builtin_type(BuiltinType::S64));
					binary->type = pointer_value->type;
					binary->low_operation = op;
				}

				if (binary->type) {
					// Multiply integer by size of pointer's type.
					auto mul = Binary::create();
					mul->left = int_value;
					mul->right = make_integer(get_size(pointer_type->expression), get_builtin_type(BuiltinType::S64));
					mul->operation = BinaryOperation::mul;
					mul->low_operation = LowBinaryOperation::mul64;
					mul->location = int_value->location;
					mul->type = get_builtin_type(BuiltinType::S64);
					int_value = mul;

					return binary;
				}

				return 0;
			};

			/* ptr + int */ {
				if (binary->operation == BinaryOperation::add) {
					if (auto pointer = as_pointer(dleft)) {
						if (auto result = handle_ptr_plus_or_minus_int(pointer, binary->left, binary->right, LowBinaryOperation::add64)) {
							return result;
						}
					}
				}
			}
			/* int + ptr */ {
				if (binary->operation == BinaryOperation::add) {
					if (auto pointer = as_pointer(dright)) {
						if (auto result = handle_ptr_plus_or_minus_int(pointer, binary->right, binary->left, LowBinaryOperation::add64)) {
							return result;
						}
					}
				}
			}
			/* ptr - int */ {
				if (binary->operation == BinaryOperation::sub) {
					if (auto pointer = as_pointer(dleft)) {
						if (auto result = handle_ptr_plus_or_minus_int(pointer, binary->left, binary->right, LowBinaryOperation::sub64)) {
							return result;
						}
					}
				}
			}
			/* ptr - ptr */ {
				if (binary->operation == BinaryOperation::sub) {
					if (auto left_pointer = as_pointer(dleft)) {
						if (auto right_pointer = as_pointer(dright)) {
							if (types_match(left_pointer->expression, right_pointer->expression)) {
								binary->low_operation = LowBinaryOperation::sub64;
								binary->type = get_builtin_type(BuiltinType::S64);

								auto div = Binary::create();
								div->left = binary;
								div->right = make_integer(get_size(left_pointer->expression), get_builtin_type(BuiltinType::S64));
								div->operation = BinaryOperation::div;
								div->low_operation = LowBinaryOperation::divs64;
								div->location = binary->location;
								div->type = get_builtin_type(BuiltinType::S64);

								return div;
							}
						}
					}
				}
			}
		}
		
		/* Range */ {
			if (binary->operation == BinaryOperation::ran) {
				binary->left = make_cast(binary->left, get_builtin_type(BuiltinType::S64));
				binary->right = make_cast(binary->right, get_builtin_type(BuiltinType::S64));
				binary->left->type = 0;
				binary->right->type = 0;
				typecheck(&binary->left);
				typecheck(&binary->right);

				auto call = Call::create();
				call->callable = make_name(context->builtin_structs.Range->definition);
				call->call_kind = CallKind::constructor;
				call->arguments.add({.expression = binary->left,  .parameter = context->builtin_structs.Range->member_list[0]});
				call->arguments.add({.expression = binary->right, .parameter = context->builtin_structs.Range->member_list[1]});
				call->location = binary->location;
				call->type = call->callable;

				NOTE_LEAK(binary);
				return call;
			}
		}

		// 
		// Pointer comparison
		//
		switch (binary->operation) {
			case BinaryOperation::equ:
			case BinaryOperation::neq:
			case BinaryOperation::les:
			case BinaryOperation::leq:
			case BinaryOperation::grt:
			case BinaryOperation::grq: {
				if (auto left_pointer = as_pointer(dleft)) {
					if (auto right_pointer = as_pointer(dright)) {
						if (types_match(left_pointer->expression, right_pointer->expression)) {
							binary->low_operation = [&]{
								switch (binary->operation) {
									case BinaryOperation::equ: return LowBinaryOperation::equ64;
									case BinaryOperation::neq: return LowBinaryOperation::neq64;
									case BinaryOperation::les: return LowBinaryOperation::ltu64;
									case BinaryOperation::leq: return LowBinaryOperation::leu64;
									case BinaryOperation::grt: return LowBinaryOperation::gtu64;
									case BinaryOperation::grq: return LowBinaryOperation::geu64;
									default: invalid_code_path();
								}
							}();
							binary->type = get_builtin_type(BuiltinType::Bool);
							return binary;
						}
					}
				}
				break;
			}
		}
		
		// 
		// Pointer coalescing
		//
		if (binary->operation == BinaryOperation::lor) {
			if (auto left_pointer = as_pointer(dleft)) {
				if (auto right_pointer = as_pointer(dright)) {
					if (types_match(left_pointer->expression, right_pointer->expression)) {
						/*
							a || b

						converts to
							
							{ if let _a = a then _a else b }
						*/
						
						auto block = Block::create();
						block->location = binary->location;
						block->parent = current_block;
						block->container = current_container;

						auto definition = Definition::create();
						definition->location = binary->location;
						definition->name = u8"__left"s;
						definition->initial_value = binary->left;
						definition->mutability = Mutability::readonly;
						definition->container = current_container;
						block->definition_list.add(definition);
						block->definition_map.get_or_insert(definition->name).add(definition);

						auto name = Name::create();
						name->location = binary->location;
						name->name = u8"__left"s;
						
						auto If = IfExpression::create();
						If->location = binary->location;
						If->condition = definition;
						If->true_branch = name;
						If->false_branch = binary->right;
						block->add(If);

						return as<Expression>(typecheck((Node *)block, true));
					}
				}
			}
		}

		if (auto found = locked_use(binary_typecheckers) { return binary_typecheckers.find({dleft, dright, binary->operation}); }) {
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
					if (auto found = locked_use(binary_typecheckers) { return binary_typecheckers.find({ dleft_element, dright_element, binary->operation }); }) {
						VectorizedBinaryValue vectorized = {};

						locked_use(vectorized_binarys) {
							VectorizedBinaryKey key = { dleft_element, dright_element, binary->operation, element_count };

							auto found = vectorized_binarys.find(key);
							if (found) {
								vectorized = *found.value;
							} else {
								auto lambda_name = format(u8"__v_{}_{}_{}_{}"s, Nameable(binary->operation), element_count, Nameable(dleft_element), Nameable(dright_element));
								auto source_list = format(u8R"({}
const {} = fn (a: {}, b: {}) => {{
var i: S64
var c: {}
while i < {} {{
	c[i] = a[i] {} b[i]
	i = i + 1
}}
c
}}
{})"s, Repeat{'\0', LEXER_PADDING_SIZE}, lambda_name, left_array, right_array, left_array, element_count, binary->operation, Repeat{'\0', LEXER_PADDING_SIZE});
							
								auto source = source_list.skip(LEXER_PADDING_SIZE).skip(-LEXER_PADDING_SIZE);
									
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

								success &= with_unwind_strategy {
									scoped_replace(current_block, &context->global_block.unprotected);
									scoped_replace(current_container, 0);
									scoped_replace(current_loop, 0);
									return typecheck(&definition_node);
								};
									
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
								
								vectorized_binarys.insert(key, vectorized);
							}
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
				if (auto pointer = is_pointer_to_none_comparison(binary->left, binary->right)) {
					binary->type = get_builtin_type(BuiltinType::Bool);
					binary->low_operation = 
						pointer == binary->left ? 
							binary->operation == BinaryOperation::neq ? LowBinaryOperation::left_to_bool  : LowBinaryOperation::left_to_bool_not  : 
							binary->operation == BinaryOperation::neq ? LowBinaryOperation::right_to_bool : LowBinaryOperation::right_to_bool_not ;
					return binary;
				}
				break;
			}
		}
		
		switch (binary->operation) {
			case BinaryOperation::equ:
			case BinaryOperation::neq: {
				if (types_match(binary->left->type, binary->right->type)) {
					binary->type = get_builtin_type(BuiltinType::Bool);
					binary->low_operation = binary->operation == BinaryOperation::equ ? LowBinaryOperation::memcmp_equ : LowBinaryOperation::memcmp_neq;
					return binary;
				}
				break;
			}
		}

		if (is_modass(binary->operation)) {
			// Turn   x += y;
			
			// Into   { let c = &x; *c = *c + y; } 
			
			ensure_mutable(binary->left);

			auto address_of_left = make_address(binary->left);

			auto definition = Definition::create();
			definition->location = binary->left->location;
			definition->mutability = Mutability::readonly;
			definition->initial_value = address_of_left;
			definition->type = address_of_left->type;
			definition->container = current_container;
			definition->name = u8"c"s;

			auto make_deref = [&] {
				auto deref = Unary::create();
				auto name = Name::create();

				name->location = binary->left->location;
				name->possible_definitions.set(definition);
				name->type = definition->type;
				name->location = binary->left->location;
				name->name = u8"c"s;

				deref->location = binary->left->location;
				deref->expression = name;
				deref->operation = UnaryOperation::dereference;
				deref->location = binary->left->location;
				deref->type = binary->left->type;
				return deref;
			};

			binary->left = make_deref();
			binary->operation = deass(binary->operation);
			typecheck(&binary);

			auto ass = Binary::create();
			ass->operation = BinaryOperation::ass;
			ass->location = binary->location;
			ass->left = make_deref();
			ass->right = binary;
			ass->type = get_builtin_type(BuiltinType::None);
			
			auto block = Block::create();
			block->location = binary->location;
			block->add(definition);
			block->add(ass);
			block->type = get_builtin_type(BuiltinType::None);
			block->parent = current_block;
			block->container = current_container;
			return block;
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
		for (auto &from : Case.froms) {
			typecheck(&from);

			if (!is_constant(from)) {
				reporter.error(from->location, "Match case expression must be constant.");
				fail();
			}

			if (!implicitly_cast_apply(&from, match->expression->type))
				fail();
		}

		typecheck(&Case.to);
	}

	bool is_expression = true;

	// All cases must be expressions for match to be an expression
	for (auto &Case : match->cases) {
		if (!Case.to_expression()) {
			is_expression = false;
			break;
		}
	}

	// Match expression should always return something, so default case is required
	if (!match->default_case) {
		is_expression = false;
	}

	if (is_expression) {
		for (auto &Case : match->cases) {
			assert(Case.to_expression());
		}

		// Select first concrete type to be the match's type
		for (auto &Case : match->cases) {
			if (is_concrete(Case.to_expression()->type)) {
				match->type = Case.to_expression()->type;
				break;
			}
		}

		// If theres no concrete types, select first inconcrete and make it concrete.
		if (!match->type) {
			for (auto &Case : match->cases) {
				make_concrete(Case.to_expression());
				match->type = Case.to_expression()->type;
				break;
			}
		}
		
		assert(match->type, "match->type should be set by now. something went wrong");

		for (auto &Case : match->cases) {
			if (!implicitly_cast_apply((Expression **)&Case.to, match->type)) {
				fail();
			}
		}
	} else {
		
		for (auto &Case : match->cases) {
			if (Case.to_expression()) {
				make_concrete(Case.to_expression());
			}
		}

		match->type = get_builtin_type(BuiltinType::None);
	}

	return match;
}
Expression       *Typechecker::typecheck_impl(Unary *unary, bool can_substitute) {
	switch (unary->operation) {
		case UnaryOperation::star:
		case UnaryOperation::dereference:
		case UnaryOperation::pointer: {
			typecheck(&unary->expression);
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
			typecheck(&unary->expression);

			if (!is_addressable(unary->expression)) {
				reporter.error(unary->location, "This expression is not addressable");
				reporter.help("You can only take address of definitions, names, dereferences, subscripts or blocks that end with an addressable expression.");
				fail();
			}

			unary->type = make_pointer(unary->expression->type, is_mutable(unary->expression) ? Mutability::variable : Mutability::readonly);

			break;
		}
		case UnaryOperation::plus: {
			typecheck(&unary->expression);
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
			typecheck(&unary->expression);
			if (auto literal = as<IntegerLiteral>(unary->expression)) {
				literal->value = -literal->value;
				return literal;
			}
			if (auto literal = as<FloatLiteral>(unary->expression)) {
				literal->value = -literal->value;
				return literal;
			}
			if (auto builtin = as<BuiltinTypeName>(unary->expression->type)) {
				switch (builtin->type_kind) {
					case BuiltinType::S8:
					case BuiltinType::S16:
					case BuiltinType::S32:
					case BuiltinType::S64:
					case BuiltinType::F32:
					case BuiltinType::F64:
						unary->type = unary->expression->type;
						break;
				}
			}

			if (!unary->type) {
				reporter.error(unary->location, "Unary {} can't be applied to expression of type {}", unary->operation, unary->expression->type);
				fail();
			}
			break;
		}
		case UnaryOperation::typeof: {
			typecheck(&unary->expression);
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
			typecheck(&unary->expression);
			if (!implicitly_cast_apply(&unary->expression, get_builtin_type(BuiltinType::Bool))) {
				fail();
			}
			unary->type = unary->expression->type;
			break;
		}
		case UnaryOperation::bnot: {
			typecheck(&unary->expression);
			if (auto literal = as<IntegerLiteral>(unary->expression)) {
				literal->value = ~literal->value;
				return literal;
			}
			if (auto builtin = as<BuiltinTypeName>(unary->expression->type)) {
				switch (builtin->type_kind) {
					case BuiltinType::S8:
					case BuiltinType::S16:
					case BuiltinType::S32:
					case BuiltinType::S64:
					case BuiltinType::U8:
					case BuiltinType::U16:
					case BuiltinType::U32:
					case BuiltinType::U64:
						unary->type = unary->expression->type;
						break;
				}
			}

			if (!unary->type) {
				reporter.error(unary->location, "Unary {} can't be applied to expression of type {}", unary->operation, unary->expression->type);
				fail();
			}
			break;
		}
		case UnaryOperation::atcast: {
			typecheck(&unary->expression);
			unary->type = unary->expression->type;
			break;
		}
		case UnaryOperation::dot: {
			auto name = as<Name>(unary->expression);
			if (!name) {
				reporter.error(unary->location, "Unary dot must be followed by a name");
				fail();
			}
			unary->type = get_builtin_type(BuiltinType::SomeEnum);
			break;
		}
		default: {
			immediate_reporter.error(unary->location, "UnaryOperation::{} not handled", unary->operation);
			fail();
			break;
		}
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
Block            *Typechecker::typecheck_impl(For *For, bool can_substitute) {
	typecheck(&For->range);
	if (!types_match(For->range->type, context->builtin_structs.Range)) {
		reporter.error(For->range->location, "This must be a range.");
		fail();
	}
	
	/*
	///////////////////////////
	for it: U64 in 0..10 {
		println(it)
	}
	///////////////////////////
	{
		let __r = 0..10
		var __i = __r.begin
		while __i != __r.end {
			let it: U64 = __i
			{
				println(it)
			}
			__i += 1
		}
	}
	///////////////////////////
	*/

	/* REVERSE EXAMPLE
	///////////////////////////
	for it in reverse 0..10 {
		println(it)
	}
	///////////////////////////
	{
		let __r = 0..10
		var __i = __r.end
		while __i != __r.begin {
			__i -= 1
			let it = __i
			{
				println(it)
			}
		}
	}
	///////////////////////////
	*/

	String begin_name = u8"begin"s;
	String end_name = u8"end"s;
	if (For->reverse)
		Swap(begin_name, end_name);

	auto outer_block = Block::create();
	outer_block->container = current_container;
	outer_block->parent = current_block;
	outer_block->location = For->location;
	
	auto __r = Definition::create();
	__r->name = u8"__r"s;
	__r->mutability = Mutability::readonly;
	__r->container = current_container;
	__r->initial_value = For->range;
	__r->location = For->range->location;
	outer_block->add(__r);

	auto __i = Definition::create();
	__i->name = u8"__i"s;
	__i->mutability = Mutability::variable;
	__i->container = current_container;
	__i->initial_value = make_binary(BinaryOperation::dot, make_name(__r, For->location), make_name(begin_name, For->location), 0, For->location);
	__i->location = For->location;
	outer_block->add(__i);

	auto While = While::create();
	While->condition = make_binary(BinaryOperation::neq, make_name(__i, For->location), make_binary(BinaryOperation::dot, make_name(__r, For->location), make_name(end_name, For->location), 0, For->location), 0, For->location);
	While->location = For->location;
	outer_block->add(While);

	auto body = Block::create();
	body->container = current_container;
	body->parent = outer_block;
	body->location = For->location;
	While->body = body;
	
	auto it = Definition::create();
	it->name = For->it_name;
	it->mutability = Mutability::readonly;
	it->container = current_container;
	it->initial_value = make_name(__i, For->it_name);
	if (For->it_parsed_type) {
		it->initial_value = make_binary(BinaryOperation::as, it->initial_value, For->it_parsed_type, 0, For->location);
	}
	it->location = For->it_name;

	if (auto block = as<Block>(For->body)) {
		block->parent = body;
	}

	if (For->reverse) {
		body->add(make_binary(BinaryOperation::subass, make_name(__i, For->location), make_integer(1, For->location), 0, For->location));
		body->add(it);
		body->add(For->body);
	} else {
		body->add(it);
		body->add(For->body);
		body->add(make_binary(BinaryOperation::addass, make_name(__i, For->location), make_integer(1, For->location), 0, For->location));
	}

	typecheck(&outer_block);

	NOTE_LEAK(For);
	return outer_block;
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
		for (auto &member : Struct->member_list) {
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
Enum             *Typechecker::typecheck_impl(Enum *Enum, bool can_substitute) {
	// NOTE:
	// values are typechecked as though their underlying type is an U64.
	// then 


	if (Enum->parsed_underlying_type) {
		typecheck(&Enum->parsed_underlying_type);
		if (!is_type(Enum->parsed_underlying_type)) {
			reporter.error(Enum->parsed_underlying_type->location, "This must be a type.");
			fail();
		}
		Enum->underlying_type = Enum->parsed_underlying_type;
	} else {
		Enum->underlying_type = get_builtin_type(BuiltinType::U64);
	}

	scoped_replace(current_container, Enum);
	scoped_replace(current_block, &Enum->block);

	u64 next_value = 0;
	for (auto element : Enum->block.definition_list) {
		if (element->initial_value) {
			element->parsed_type = Enum->underlying_type;

			typecheck(&element);

			assert(element->constant_value);
			switch (element->constant_value.value().kind) {
				case ValueKind::Bool:
				case ValueKind::U8:
				case ValueKind::U16:
				case ValueKind::U32:
				case ValueKind::U64:
				case ValueKind::S8:
				case ValueKind::S16:
				case ValueKind::S32:
				case ValueKind::S64:
				case ValueKind::UnsizedInteger:
					next_value = element->constant_value.value().S64 + 1;
					break;
				default:
					invalid_code_path(element->initial_value->location, "ValueKind should be an integer, but is {}", element->constant_value.value().kind);
					break;
			}
		} else {
			element->constant_value = Value(next_value);
			element->type = Enum->underlying_type;
			next_value += 1;
		}
	}

	Enum->type = get_builtin_type(BuiltinType::Type);
	for (auto element : Enum->block.definition_list) {
		element->parsed_type = 0;

		assert(Enum->definition, "Enums with no definition are not implemented");

		// HACK: :enum_value_type:
		// Enum->definition is currently being typechecked, but names already reference it.
		// This unfinished state will probably cause some bugs?
		// Though when accessing enum values, the enum name waits for definition type, so maybe its fine?
		element->type.expression = make_name(Enum->definition);
		element->type.expression->type = get_builtin_type(BuiltinType::Type);
	}

	locked_use(binary_typecheckers) {
		#define BINT(l, r, op) \
			binary_typecheckers.get_or_insert({.left_type = l, .right_type = r, .operation = BinaryOperation::op})

		auto some_enum = get_builtin_type(BuiltinType::SomeEnum);

		BINT(Enum, some_enum, add) = 
		BINT(Enum, some_enum, sub) = 
		BINT(Enum, some_enum, mul) = 
		BINT(Enum, some_enum, div) = 
		BINT(Enum, some_enum, mod) = 
		BINT(Enum, some_enum, bxo) = 
		BINT(Enum, some_enum, ban) = 
		BINT(Enum, some_enum, bor) = 
		BINT(Enum, some_enum, bsr) = 
		BINT(Enum, some_enum, bsl) = 
		BINT(Enum, some_enum, equ) = 
		BINT(Enum, some_enum, neq) = 
		BINT(Enum, some_enum, les) = 
		BINT(Enum, some_enum, leq) = 
		BINT(Enum, some_enum, grt) = 
		BINT(Enum, some_enum, grq) = 
			&Typechecker::bt_enum_and_some_enum;

		#undef BINT
	};
	return Enum;
}
ArrayType        *Typechecker::typecheck_impl(ArrayType *arr, bool can_substitute) {
	if (arr->uid == 585) {
		int x = 5;
	}

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

	auto direct_subscriptable_type = direct(Subscript->subscriptable->type);

	if (auto array_type = as<ArrayType>(direct_subscriptable_type)) {
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
	} else if (auto pointer = as_pointer(direct_subscriptable_type)) {
		typecheck(&Subscript->index);
		make_concrete(Subscript->index);
		if (!::is_concrete_integer(Subscript->index->type)) {
			reporter.error(Subscript->index->location, "This must be an integer.");
			fail();
		}
		Subscript->type = pointer->expression;
		return Subscript;
	} else if (auto Struct_ = as<Struct>(direct_subscriptable_type); Struct_ && Struct_->is_template) {
		return get_struct_template_instantiation(Struct_, Subscript->index);
	} else {
		reporter.error(Subscript->subscriptable->location, "Expression of type {} is not subscriptable.", Subscript->subscriptable->type);
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
		if (!implicitly_cast_apply(&element, arr->elements[0]->type)) {
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
CallerLocation   *Typechecker::typecheck_impl(CallerLocation *cl, bool can_substitute) {
	cl->type = make_name(context->builtin_structs.String->definition, cl->location);
	return cl;
}
CallerArgumentString *Typechecker::typecheck_impl(CallerArgumentString *cas, bool can_substitute) {
	cas->type = make_name(context->builtin_structs.String->definition, cas->location);
	return cas;
}
Use              *Typechecker::typecheck_impl(Use *Use, bool can_substitute) {
	typecheck(Use->name);
	currently_used_definitions.add(Use->name.possible_definitions);
	return Use;
}

Expression *Typechecker::bt_take_left(Binary *binary) {
	binary->type = binary->left->type;
	return binary;
};
Expression *Typechecker::bt_set_bool(Binary *binary) {
	binary->type = get_builtin_type(BuiltinType::Bool);
	return binary;
}
Expression *Typechecker::bt_equ(Binary *binary) {
	binary->type = get_builtin_type(BuiltinType::Bool);
	binary->low_operation = LowBinaryOperation::memcmp_equ;
	return binary;
}
Expression *Typechecker::bt_neq(Binary *binary) {
	binary->type = get_builtin_type(BuiltinType::Bool);
	binary->low_operation = LowBinaryOperation::memcmp_neq;
	return binary;
}
Expression *Typechecker::bt_unsized_int_and_sized_int_math(Binary *binary) {
	auto sized = binary->left;
	auto unsized = binary->right;

	if (is_concrete(unsized->type)) {
		Swap(sized, unsized);
	}

	propagate_concrete_type(unsized, sized->type);

	binary->type = sized->type;
	return binary;
}
Expression *Typechecker::bt_unsized_int_and_sized_int_comp(Binary *binary) {
	auto sized = binary->left;
	auto unsized = binary->right;

	if (is_concrete(unsized->type)) {
		Swap(sized, unsized);
	}

	propagate_concrete_type(unsized, sized->type);
	binary->type = get_builtin_type(BuiltinType::Bool);
	return binary;
}
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
}
Expression *Typechecker::bt_sized_int_modify_ass_unsized_int(Binary *binary) {
	ensure_mutable(binary->left);
	binary->right->type = binary->left->type;
	binary->type = get_builtin_type(BuiltinType::None);
	return binary;
}
Expression *Typechecker::bt_unsized_float_and_sized_float_math(Binary *binary) {
	auto sized = binary->left;
	auto unsized = binary->right;

	if (is_concrete(unsized->type)) {
		Swap(sized, unsized);
	}

	propagate_concrete_type(unsized, sized->type);

	binary->type = sized->type;
	return binary;
}
Expression *Typechecker::bt_unsized_float_and_sized_float_comp(Binary *binary) {
	auto sized = binary->left;
	auto unsized = binary->right;

	if (is_concrete(unsized->type)) {
		Swap(sized, unsized);
	}

	propagate_concrete_type(unsized, sized->type);
	binary->type = get_builtin_type(BuiltinType::Bool);
	return binary;
}
Expression *Typechecker::bt_unsized_float(Binary *binary) {
	auto l = get_constant_value(binary->left).value();
	auto r = get_constant_value(binary->right).value();
	assert(l.kind == ValueKind::UnsizedFloat);
	assert(r.kind == ValueKind::UnsizedFloat);
	switch (binary->operation) {
		case BinaryOperation::add: return make_float(l.UnsizedFloat + r.UnsizedFloat, binary->location);
		case BinaryOperation::sub: return make_float(l.UnsizedFloat - r.UnsizedFloat, binary->location);
		case BinaryOperation::mul: return make_float(l.UnsizedFloat * r.UnsizedFloat, binary->location);
		case BinaryOperation::div: return make_float(l.UnsizedFloat / r.UnsizedFloat, binary->location);
		case BinaryOperation::mod: return make_float(frac(l.UnsizedFloat / r.UnsizedFloat) * r.UnsizedFloat, binary->location);
		case BinaryOperation::equ: return make_boolean(l.UnsizedFloat == r.UnsizedFloat, binary->location);
		case BinaryOperation::neq: return make_boolean(l.UnsizedFloat != r.UnsizedFloat, binary->location);
		case BinaryOperation::les: return make_boolean(l.UnsizedFloat < r.UnsizedFloat, binary->location);
		case BinaryOperation::grt: return make_boolean(l.UnsizedFloat > r.UnsizedFloat, binary->location);
		case BinaryOperation::leq: return make_boolean(l.UnsizedFloat <= r.UnsizedFloat, binary->location);
		case BinaryOperation::grq: return make_boolean(l.UnsizedFloat >= r.UnsizedFloat, binary->location);
	}
	invalid_code_path("Attempt to evaluate binary {} on unsized floats. This is not supported/implemented", binary->operation);
}
Expression *Typechecker::bt_sized_float_modify_ass_unsized_float(Binary *binary) {
	ensure_mutable(binary->left);
	binary->right->type = binary->left->type;
	binary->type = get_builtin_type(BuiltinType::None);
	return binary;
}

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

Expression *Typechecker::bt_enum_and_some_enum(Binary *binary) {
	auto Enum = direct_as<::Enum>(binary->left->type);
	assert(Enum);
	auto some_enum = as_some_enum(binary->right);
	assert(some_enum);
	auto name = as<Name>(some_enum->expression);
	assert(name);

	implicitly_cast_apply(&binary->right, binary->left->type);
	
	switch (binary->operation) {
		case BinaryOperation::equ:
		case BinaryOperation::neq:
		case BinaryOperation::les:
		case BinaryOperation::leq:
		case BinaryOperation::grt:
		case BinaryOperation::grq:
			binary->type = get_builtin_type(BuiltinType::Bool);
			break;
		default:
			binary->type = binary->left->type;
	}

	auto enum_size = get_size(Enum);

	int size_op_offset = [&] {
		switch (enum_size) {
			case 1: return 0;
			case 2: return 1;
			case 4: return 2;
			case 8: return 3;
		}
		invalid_code_path("unhandled enum size: {}", enum_size);
	}();

	bool sign = is_signed_integer(Enum->underlying_type);

	switch (binary->operation) {
		case BinaryOperation::add: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::add8 + size_op_offset); break;
		case BinaryOperation::sub: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::sub8 + size_op_offset); break;
		case BinaryOperation::mul: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::mul8 + size_op_offset); break;
		case BinaryOperation::div: binary->low_operation = (LowBinaryOperation)((int)(sign ? LowBinaryOperation::divs8 : LowBinaryOperation::divu8) + size_op_offset); break;
		case BinaryOperation::mod: binary->low_operation = (LowBinaryOperation)((int)(sign ? LowBinaryOperation::mods8 : LowBinaryOperation::modu8) + size_op_offset); break;
		case BinaryOperation::bxo: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::bxor8 + size_op_offset); break;
		case BinaryOperation::ban: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::band8 + size_op_offset); break;
		case BinaryOperation::bor: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::bor8 + size_op_offset); break;
		case BinaryOperation::bsr: binary->low_operation = (LowBinaryOperation)((int)(sign ? LowBinaryOperation::bsrs8 : LowBinaryOperation::bsru8) + size_op_offset); break;
		case BinaryOperation::bsl: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::bsl8 + size_op_offset); break;
		case BinaryOperation::equ: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::equ8 + size_op_offset); break;
		case BinaryOperation::neq: binary->low_operation = (LowBinaryOperation)((int)LowBinaryOperation::neq8 + size_op_offset); break;
		case BinaryOperation::les: binary->low_operation = (LowBinaryOperation)((int)(sign ? LowBinaryOperation::lts8 : LowBinaryOperation::ltu8) + size_op_offset); break;
		case BinaryOperation::leq: binary->low_operation = (LowBinaryOperation)((int)(sign ? LowBinaryOperation::les8 : LowBinaryOperation::leu8) + size_op_offset); break;
		case BinaryOperation::grt: binary->low_operation = (LowBinaryOperation)((int)(sign ? LowBinaryOperation::gts8 : LowBinaryOperation::gtu8) + size_op_offset); break;
		case BinaryOperation::grq: binary->low_operation = (LowBinaryOperation)((int)(sign ? LowBinaryOperation::ges8 : LowBinaryOperation::geu8) + size_op_offset); break;
	}

	return binary;
}

void Typechecker::init_binary_typecheckers() {
	auto &binary_typecheckers = Typechecker::binary_typecheckers.unprotected;

	#define y(left, right, operation) binary_typecheckers.get_or_insert({ get_builtin_type(left), get_builtin_type(right), operation })
	#define x(left, right, operation) y(BuiltinType::left, BuiltinType::right, BinaryOperation::operation)

	//
	// Every type is equatable
	// 
	for (u32 i = 0; i < (u32)BuiltinType::count; ++i) {
		y((BuiltinType)i, (BuiltinType)i, BinaryOperation::equ) = &bt_equ;
		y((BuiltinType)i, (BuiltinType)i, BinaryOperation::neq) = &bt_neq;
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
	
	#define MATHABLE_FLOAT(type) \
		x(type, type, add) = &bt_math_opt<[](Value l, Value r){ return make_float(     l.type + r.type,           get_builtin_type(BuiltinType::type)); }>; \
		x(type, type, sub) = &bt_math_opt<[](Value l, Value r){ return make_float(     l.type - r.type,           get_builtin_type(BuiltinType::type)); }>; \
		x(type, type, mul) = &bt_math_opt<[](Value l, Value r){ return make_float(     l.type * r.type,           get_builtin_type(BuiltinType::type)); }>; \
		x(type, type, div) = &bt_math_opt<[](Value l, Value r){ return make_float(     l.type / r.type,           get_builtin_type(BuiltinType::type)); }>; \
		x(type, type, mod) = &bt_math_opt<[](Value l, Value r){ return make_float(frac(l.type / r.type) * r.type, get_builtin_type(BuiltinType::type)); }>;
	
	#define BITWISE(signed, unsigned) \
		x(unsigned, unsigned, bor) = &bt_take_left; \
		x(unsigned,   signed, bor) = &bt_take_left; \
		x(  signed, unsigned, bor) = &bt_take_left; \
		x(  signed,   signed, bor) = &bt_take_left; \
		x(unsigned, unsigned, ban) = &bt_take_left; \
		x(unsigned,   signed, ban) = &bt_take_left; \
		x(  signed, unsigned, ban) = &bt_take_left; \
		x(  signed,   signed, ban) = &bt_take_left; \
		x(unsigned, unsigned, bxo) = &bt_take_left; \
		x(unsigned,   signed, bxo) = &bt_take_left; \
		x(  signed, unsigned, bxo) = &bt_take_left; \
		x(  signed,   signed, bxo) = &bt_take_left; \

	#define SHIFTS(type) \
		x(type, U8, bsl) = &bt_take_left; \
		x(type, U8, bsr) = &bt_take_left; \
		x(type, U16, bsl) = &bt_take_left; \
		x(type, U16, bsr) = &bt_take_left; \
		x(type, U32, bsl) = &bt_take_left; \
		x(type, U32, bsr) = &bt_take_left; \
		x(type, U64, bsl) = &bt_take_left; \
		x(type, U64, bsr) = &bt_take_left; \
		x(type, S8, bsl) = &bt_take_left; \
		x(type, S8, bsr) = &bt_take_left; \
		x(type, S16, bsl) = &bt_take_left; \
		x(type, S16, bsr) = &bt_take_left; \
		x(type, S32, bsl) = &bt_take_left; \
		x(type, S32, bsr) = &bt_take_left; \
		x(type, S64, bsl) = &bt_take_left; \
		x(type, S64, bsr) = &bt_take_left; \

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
		SYMMETRIC(t, UnsizedInteger, grq) = &bt_unsized_int_and_sized_int_comp; \
		/* x(t, UnsizedInteger, addass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, subass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, mulass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, divass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, modass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, bxoass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, banass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, borass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, bslass) = &bt_sized_int_modify_ass_unsized_int; */ \
		/* x(t, UnsizedInteger, bsrass) = &bt_sized_int_modify_ass_unsized_int; */ \

	#define UNSIZED_FLOAT_AND_SIZED_FLOAT(t) \
		SYMMETRIC(t, UnsizedFloat, add) = &bt_unsized_float_and_sized_float_math; \
		SYMMETRIC(t, UnsizedFloat, sub) = &bt_unsized_float_and_sized_float_math; \
		SYMMETRIC(t, UnsizedFloat, mul) = &bt_unsized_float_and_sized_float_math; \
		SYMMETRIC(t, UnsizedFloat, div) = &bt_unsized_float_and_sized_float_math; \
		SYMMETRIC(t, UnsizedFloat, mod) = &bt_unsized_float_and_sized_float_math; \
		SYMMETRIC(t, UnsizedFloat, equ) = &bt_unsized_float_and_sized_float_comp; \
		SYMMETRIC(t, UnsizedFloat, neq) = &bt_unsized_float_and_sized_float_comp; \
		SYMMETRIC(t, UnsizedFloat, les) = &bt_unsized_float_and_sized_float_comp; \
		SYMMETRIC(t, UnsizedFloat, leq) = &bt_unsized_float_and_sized_float_comp; \
		SYMMETRIC(t, UnsizedFloat, grt) = &bt_unsized_float_and_sized_float_comp; \
		SYMMETRIC(t, UnsizedFloat, grq) = &bt_unsized_float_and_sized_float_comp; \

	ORDERABLE(Bool);
	ORDERABLE(U8);
	ORDERABLE(U16);
	ORDERABLE(U32);
	ORDERABLE(U64);
	ORDERABLE(S8);
	ORDERABLE(S16);
	ORDERABLE(S32);
	ORDERABLE(S64);

	ORDERABLE(F32);
	ORDERABLE(F64);

	MATHABLE_INTEGER(U8);
	MATHABLE_INTEGER(U16);
	MATHABLE_INTEGER(U32);
	MATHABLE_INTEGER(U64);
	MATHABLE_INTEGER(S8);
	MATHABLE_INTEGER(S16);
	MATHABLE_INTEGER(S32);
	MATHABLE_INTEGER(S64);

	MATHABLE_FLOAT(F32);
	MATHABLE_FLOAT(F64);

	BITWISE(S8,  U8 );
	BITWISE(S16, U16);
	BITWISE(S32, U32);
	BITWISE(S64, U64);
	
	SHIFTS(U8);
	SHIFTS(U16);
	SHIFTS(U32);
	SHIFTS(U64);
	SHIFTS(S8);
	SHIFTS(S16);
	SHIFTS(S32);
	SHIFTS(S64);

	UNSIZED_INT_AND_SIZED_INT(U8);
	UNSIZED_INT_AND_SIZED_INT(U16);
	UNSIZED_INT_AND_SIZED_INT(U32);
	UNSIZED_INT_AND_SIZED_INT(U64);
	UNSIZED_INT_AND_SIZED_INT(S8);
	UNSIZED_INT_AND_SIZED_INT(S16);
	UNSIZED_INT_AND_SIZED_INT(S32);
	UNSIZED_INT_AND_SIZED_INT(S64);

	UNSIZED_FLOAT_AND_SIZED_FLOAT(F32);
	UNSIZED_FLOAT_AND_SIZED_FLOAT(F64);
	
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

	x(UnsizedFloat, UnsizedFloat, add) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, sub) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, mul) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, div) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, mod) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, equ) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, neq) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, les) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, leq) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, grt) = &bt_unsized_float;
	x(UnsizedFloat, UnsizedFloat, grq) = &bt_unsized_float;

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
	
u64 get_typechecking_progress() {
	u64 result = 0;
	for (auto &entry : typecheck_entries) {
		if (entry.typechecker)
			result += entry.typechecker->progress;
	}
	return result;
}
