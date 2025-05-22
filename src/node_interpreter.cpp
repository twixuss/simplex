#include "node_interpreter.h"
#include "nodes.h"
#include "debug.h"

NodeInterpreter *NodeInterpreter::create(Node *node) {
	auto context = DefaultAllocator{}.allocate<NodeInterpreter>();
	context->parent_fiber = init_or_get_current_fiber();
	context->fiber = get_new_fiber();
	set_start(context->fiber, [](void *param) { ((NodeInterpreter *)param)->fiber_main(); }, context);
	context->node_to_execute = node;
	return context;
}

Result<Value, NodeInterpreter::YieldResult> NodeInterpreter::run() {
	tl::yield(fiber);
	add_fiber_to_reuse(fiber);
	fiber = {};
	if (yield_result == YieldResult::success) {
		return result_value;
	}
	return yield_result;
}

void NodeInterpreter::fiber_main() {
	yield_result = YieldResult::fail;

	auto &scope = scope_stack.add(); // global scope

	result_value = execute(node_to_execute);
	yield(YieldResult::success);
}

void NodeInterpreter::yield(YieldResult result) {
	this->yield_result = result;
	tl::yield_reuse(parent_fiber, fiber);
}

#define PERFORM_WITH_BREAKS(name, execute, node)               \
	name = execute(node);                                      \
	switch (name.kind) {                                       \
		case ValueKind::return_:                               \
		case ValueKind::break_:                                \
		case ValueKind::continue_: return name;                \
		default:                                               \
			if (currently_breaking_from) {                     \
				if (currently_breaking_from == current_node) { \
					currently_breaking_from = 0;               \
				}                                              \
				return name;                                   \
			}                                                  \
			break;                                             \
	}

#define EXECUTE_INTO(name, node) PERFORM_WITH_BREAKS(name, execute, node)
#define EXECUTE_DEFN(name, node) auto EXECUTE_INTO(name, node)

#define LOAD_ADDRESS_INTO(name, node) PERFORM_WITH_BREAKS(name, load_address, node)
#define LOAD_ADDRESS_DEFN(name, node) auto LOAD_ADDRESS_INTO(name, node)

// NOTE: don't call _impl directly, because current_node needs to be updated.

Value NodeInterpreter::load_address(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	scoped_replace(current_node, expression);

	Value result = {};

	switch (expression->kind) {
#define x(name) case NodeKind::name: result = load_address_impl((name *)expression); break;
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
		default:
			invalid_code_path();
	}

	switch (result.kind) {
		case ValueKind::pointer:
		case ValueKind::break_:
		case ValueKind::continue_:
		case ValueKind::return_:
			break;
		default:
			invalid_code_path("invalid value returned from load_address_impl");
	}

	return result;
}
Value NodeInterpreter::load_address_impl(IntegerLiteral *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(BooleanLiteral *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(NoneLiteral *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(StringLiteral *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(Definition *definition) {
	for (auto &scope : reversed(scope_stack)) {
		if (auto found = scope.variables.find(definition)) {
			return Value(found.value);
		}
	}
	invalid_code_path();
}
Value NodeInterpreter::load_address_impl(Block *block) {
	assert(block->children.count > 0);
	scoped_replace(current_block, block);
	for (auto child : block->children.skip(-1)) {
		EXECUTE_DEFN(ignored, child);
	}
	auto last_expression = as<Expression>(block->children.back());
	assert(last_expression);
	return load_address(last_expression);
}
Value NodeInterpreter::load_address_impl(Lambda *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(LambdaHead *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(Name *name) {
	auto definition = name->definition();
	assert(definition);
	return load_address(definition);
}
Value NodeInterpreter::load_address_impl(Call *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(IfExpression *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(BuiltinTypeName *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(Binary *binary) {
	assert(binary->operation == BinaryOperation::dot);
	auto name = as<Name>(binary->right);
	assert(name);

	Struct *Struct = 0;
	Value struct_address = {};

	if (Struct = direct_as<::Struct>(binary->left->type)) {
		LOAD_ADDRESS_INTO(struct_address, binary->left);
	} else if (auto pointer = as_pointer(direct(binary->left->type))) {
		Struct = direct_as<::Struct>(pointer->expression);
		assert(Struct);
		EXECUTE_INTO(struct_address, binary->left);
	} else {
		invalid_code_path();
	}

	return Value(&struct_address.pointer->elements[find_index_of(Struct->members, name->definition())]);
}
Value NodeInterpreter::load_address_impl(Match *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(Unary *unary) {
	if (unary->operation == UnaryOperation::dereference) {
		auto pointer = execute(unary->expression);
		assert(pointer.kind == ValueKind::pointer);
		return pointer;
	}
	invalid_code_path();
}
Value NodeInterpreter::load_address_impl(Struct *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(Enum *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(ArrayType *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(Subscript *subscript) {
	LOAD_ADDRESS_DEFN(arr, subscript->subscriptable);
	EXECUTE_DEFN(index, subscript->index);
	return Value(&element_at(arr.pointer->elements, index));
}
Value NodeInterpreter::load_address_impl(ArrayConstructor *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(ZeroInitialized *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(CallerLocation *) { invalid_code_path(); }
Value NodeInterpreter::load_address_impl(CallerArgumentString *) { invalid_code_path(); }

Value NodeInterpreter::execute(Node *node) {
	scoped_replace(debug_current_location, node->location);
	scoped_replace(current_node, node);

	switch (node->kind) {
#define x(name) case NodeKind::name: return execute_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
	invalid_code_path();
}
Value NodeInterpreter::execute_impl(IntegerLiteral *literal) {
	if (auto builtin_type = direct_as<BuiltinTypeName>(literal->type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::U8: return Value((u8)literal->value);
			case BuiltinType::U16: return Value((u16)literal->value);
			case BuiltinType::U32: return Value((u32)literal->value);
			case BuiltinType::U64: return Value((u64)literal->value);
			case BuiltinType::S8: return Value((s8)literal->value);
			case BuiltinType::S16: return Value((s16)literal->value);
			case BuiltinType::S32: return Value((s32)literal->value);
			case BuiltinType::S64: return Value((s64)literal->value);
			case BuiltinType::UnsizedInteger: return Value(unsized_integer_tag, literal->value);
		}
	}

	immediate_reporter.error(literal->location, "Could not execute this literal because it does not have a concrete type, its type is {}. This is probably a bug in the compiler.", literal->type);
	yield(YieldResult::fail);
	return {};
}
Value NodeInterpreter::execute_impl(BooleanLiteral *literal) {
	return Value(literal->value);
}
Value NodeInterpreter::execute_impl(NoneLiteral *literal) {
	return Value(ValueKind::none);
}
Value NodeInterpreter::execute_impl(StringLiteral *literal) {
	return Value(literal->value);
}
Value NodeInterpreter::execute_impl(Definition *definition) {
	if (definition->mutability == Mutability::constant) {
		assert(definition->constant_value);
		return definition->constant_value.value();
	}

	Value value = {};
	if (definition->initial_value) {
		EXECUTE_INTO(value, definition->initial_value);
	} else {
		default_initialize(&value, definition->type);
	}
	scope_stack.back().variables.insert(definition, value);
	return value;
}
Value NodeInterpreter::execute_impl(Return *return_) {
	if (return_->value) {
		EXECUTE_INTO(return_value, return_->value);
	}
	return Value(ValueKind::return_);
}
Value NodeInterpreter::execute_impl(Block *block) {
	Value result = {};
	scoped_replace(current_block, block);
	for (auto child : block->children) {
		EXECUTE_INTO(result, child);
		switch (result.kind) {
			case ValueKind::return_:
			case ValueKind::break_:
			case ValueKind::continue_:
				return result;
		}
	}

	if (block->children.count != 0 && as<Expression>(block->children.back()))
		return result;

	return {};
}
Value NodeInterpreter::execute_impl(Lambda *lambda) {
	return Value(lambda);
}
Value NodeInterpreter::execute_impl(LambdaHead *head) {
	return Value(Type(head));
}
Value NodeInterpreter::execute_impl(Name *name) {
	auto definition = name->definition();
	assert(definition);

	if (!definition->container) {
		// Globals may need to be waited for.
		auto &scope = scope_stack[0];
		if (auto found = scope.variables.find(definition)) {
			return *found.value;
		} else {
			if (!yield_while_null(definition->location, &definition->type)) {
				yield(YieldResult::fail);
			}

			auto value = execute(definition->initial_value);
			scope.variables.get_or_insert(definition) = value;
			return value;
		}
	}


	for (auto &scope : reversed(scope_stack)) {
		if (auto found = scope.variables.find(definition)) {
			return *found.value;
		}
	}
	invalid_code_path();
}
Value NodeInterpreter::execute_impl(Call *call) {
	auto &arguments = call->arguments;

	auto callable = execute(call->callable);
	auto struct_ = callable.kind == ValueKind::Type ? direct_as<Struct>(callable.Type) : 0;

	if ((callable.kind != ValueKind::lambda || !callable.lambda) && (!struct_)) {
		immediate_reporter.error(call->location, "You can only call constant lambdas or structs right now.");
		yield(YieldResult::fail);
	}

	if (callable.kind == ValueKind::lambda) {
		auto lambda = callable.lambda;
		auto &parameters = lambda->head.parameters_block.definition_list;

		List<Value, TemporaryAllocator> argument_values;
		argument_values.reserve(arguments.count);
		for (auto argument : arguments) {
			EXECUTE_DEFN(argument_value, argument.expression);
			argument_values.add(argument_value);
		}

		if (lambda->is_intrinsic) {
			assert(lambda->definition);

			auto name = lambda->definition->name;

			if (name == u8"println"s) {
				assert(arguments.count == 1);

				scoped(temporary_allocator_and_checkpoint);

				List<utf8> result;

				switch (argument_values[0].kind) {
					case ValueKind::U8: result = to_string(argument_values[0].U8); break;
					case ValueKind::U16: result = to_string(argument_values[0].U16); break;
					case ValueKind::U32: result = to_string(argument_values[0].U32); break;
					case ValueKind::U64: result = to_string(argument_values[0].U64); break;
					case ValueKind::S8: result = to_string(argument_values[0].S8); break;
					case ValueKind::S16: result = to_string(argument_values[0].S16); break;
					case ValueKind::S32: result = to_string(argument_values[0].S32); break;
					case ValueKind::S64: result = to_string(argument_values[0].S64); break;
					case ValueKind::Type: result = to_string(argument_values[0].Type); break;
					case ValueKind::Bool: result = to_string(argument_values[0].Bool); break;
					case ValueKind::String: result = to_list(argument_values[0].String); break;
					case ValueKind::lambda: result = format(u8"(lambda {})", argument_values[0].lambda->uid); break;
					case ValueKind::pointer: result = format(u8"0x{}", FormatInt{.value = (umm)argument_values[0].pointer, .radix = 16}); break;
					case ValueKind::none: result = to_list(u8"none"s); break;
					default: {
						immediate_reporter.error(arguments[0].expression->location, "Unknown type {} in println", argument_values[0].kind);
						yield(YieldResult::fail);
					}
				}

				println(result);

				return Value((s64)result.count);
			} else if (name == u8"exit"s) {
				yield(YieldResult::success);
			}

			immediate_reporter.error(lambda->location, "Attempt to execute invalid intrinsic '{}'.", name);
			yield(YieldResult::fail);

		} else if (lambda->is_extern) {

			Dll dll = {};
			if (auto found = loaded_extern_libraries.find(lambda->extern_library)) {
				dll = *found.value;
			} else {
				dll = load_dll(lambda->extern_library);
				if (!dll) {
					immediate_reporter.error(call->location, "Failed to load extern library {}", lambda->extern_library);
					immediate_reporter.info(lambda->location, "Here is the definition:");
					yield(YieldResult::fail);
				}

				loaded_extern_libraries.insert(lambda->extern_library, dll);
			}

			assert(lambda->definition);

			auto func = (u64(*)(u64,u64,u64,u64,u64,u64,u64,u64))get_symbol(dll, lambda->definition->name);
			if (!func) {
				immediate_reporter.error(call->location, "Failed to load function {} from extern library {}", lambda->definition->name, lambda->extern_library);
				immediate_reporter.info(lambda->location, "Here is the definition:");
				yield(YieldResult::fail);
			}

			if (arguments.count > 8) {
				immediate_reporter.error(call->location, "Can't pass more than 8 arguments to extern function.");
				immediate_reporter.info(lambda->location, "Here is the definition:");
				yield(YieldResult::fail);
			}

			u64 args[8]{};
			for (umm i = 0; i < arguments.count; ++i) {
				auto argument = arguments[i];
				auto value = argument_values[i];
				switch (value.kind) {
					case ValueKind::Bool: args[i] = value.Bool; break;
					case ValueKind::U8:	  args[i] = value.U8;   break;
					case ValueKind::U16:  args[i] = value.U16;  break;
					case ValueKind::U32:  args[i] = value.U32;  break;
					case ValueKind::U64:  args[i] = value.U64;  break;
					case ValueKind::S8:	  args[i] = value.S8;   break;
					case ValueKind::S16:  args[i] = value.S16;  break;
					case ValueKind::S32:  args[i] = value.S32;  break;
					case ValueKind::S64:  args[i] = value.S64;  break;
					default:
						immediate_reporter.error(argument.expression->location, "Can't pass value of type {} to extern function.", argument.expression->type);
						immediate_reporter.info(lambda->location, "Here is the definition:");
						yield(YieldResult::fail);
				}
					
			}

			auto result = func(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);

			if (types_match(call->type, BuiltinType::None)) {
				return {};
			} else if (is_unsigned_integer(call->type)) {
				return Value((u64)result);
			} else if (is_signed_integer(call->type)) {
				return Value((s64)result);
			} else if (types_match(call->type, BuiltinType::Bool)) {
				return Value((bool)result);
			} else {
				immediate_reporter.error(call->location, "Can't return value of type {} from extern function.", call->type);
				immediate_reporter.info(lambda->location, "Here is the definition:");
				yield(YieldResult::fail);
				return {};
			}
		}

		++recursion_level;
		defer { --recursion_level; };

		if (recursion_level >= recursion_limit) {
			immediate_reporter.error(call->location, "Recursion limit of {} was exceeded.", recursion_limit);
			exit(1);
		}

		auto prev_scope_stack = scope_stack;
		scope_stack = {};
		scope_stack.add(prev_scope_stack[0]);
		defer {
			prev_scope_stack[0] = scope_stack[0];
			free(scope_stack);
			scope_stack = prev_scope_stack;
		};

		auto &param_scope = scope_stack.add();

		for (umm i = 0; i < arguments.count; ++i) {
			param_scope.variables.insert(parameters[i], argument_values[i]);
		}

		auto result = execute(lambda->body);
		if (result.kind == ValueKind::return_) {
			return return_value;
		} else {
			return result;
		}
	} else {
		assert(struct_);

		auto &members = struct_->members;

		assert(members.count == arguments.count);

		List<Value, TemporaryAllocator> argument_values;
		argument_values.reserve(arguments.count);
		for (auto argument : arguments) {
			EXECUTE_DEFN(argument_value, argument.expression);
			argument_values.add(argument_value);
		}

		return Value(struct_tag, argument_values);
	}
}
Value NodeInterpreter::execute_impl(IfStatement *If) {
	EXECUTE_DEFN(condition, If->condition);
	assert(condition.kind == ValueKind::Bool);
	if (condition.Bool) {
		EXECUTE_DEFN(value, If->true_branch);
		return value;
	} else {
		if (If->false_branch) {
			EXECUTE_DEFN(value, If->false_branch);
			return value;
		}
	}
	return {};
}
Value NodeInterpreter::execute_impl(IfExpression *If) {
	EXECUTE_DEFN(condition, If->condition);
	assert(condition.kind == ValueKind::Bool);
	EXECUTE_DEFN(value, condition.Bool ? If->true_branch : If->false_branch);
	return value;
}
Value NodeInterpreter::execute_impl(While *While) {
	while (true) {
		EXECUTE_DEFN(condition, While->condition);
		assert(condition.kind == ValueKind::Bool);
		if (!condition.Bool) {
			break;
		}

		auto body_result = execute(While->body);
		switch (body_result.kind) {
			case ValueKind::return_: return body_result;
			case ValueKind::break_: return {};
			case ValueKind::continue_: continue;
		}
	}
	return {};
}
Value NodeInterpreter::execute_impl(Continue *Continue) {
	return Value(ValueKind::continue_);
}
Value NodeInterpreter::execute_impl(Break *Break) {
	if (!Break->value) {
		return Value(ValueKind::break_);
	}

	assert(Break->tag_block);

	EXECUTE_DEFN(value, Break->value);
	currently_breaking_from = Break->tag_block;
	return value;
}
Value NodeInterpreter::execute_impl(BuiltinTypeName *type) { 
	return Value(Type(type));
}
Value NodeInterpreter::execute_impl(Binary *binary) {
	switch (binary->operation) {
		case BinaryOperation::ass: {
			LOAD_ADDRESS_DEFN(address, binary->left);
			EXECUTE_DEFN(value, binary->right);
			*address.pointer = value;
			return {};
		}
		case BinaryOperation::dot: {
			EXECUTE_DEFN(struct_value, binary->left);
			assert(struct_value.kind == ValueKind::struct_);

			auto struct_ = direct_as<Struct>(binary->left->type);
			assert(struct_);
			auto name = as<Name>(binary->right);
			assert(name);

			return struct_value.elements[find_index_of(struct_->members, name->definition())];
		}
		case BinaryOperation::as: {
			EXECUTE_DEFN(val, binary->left);
			auto type = binary->right;
			if (is_concrete_integer(binary->left->type) || types_match(binary->left->type, BuiltinType::UnsizedInteger)) {
				if (as_pointer(binary->right)) {
					return Value((Value *)val.U64);
				}
			}

			break;
		}
	}

	EXECUTE_DEFN(left, binary->left);
	EXECUTE_DEFN(right, binary->right);

#define OPS(t, v)                                                                                                          \
case ValueKind::v: {                                                                                                   \
	if (right.kind == left.kind) {                                                                                     \
		switch (binary->operation) {                                                                                   \
			case BinaryOperation::add: return Value((t)(left.v + right.v));                                            \
			case BinaryOperation::sub: return Value((t)(left.v - right.v));                                            \
			case BinaryOperation::mul: return Value((t)(left.v * right.v));                                            \
			case BinaryOperation::div: return Value((t)(left.v / right.v));                                            \
			case BinaryOperation::mod: return Value((t)(left.v % right.v)); /* TODO: ensure this is right operation */ \
			case BinaryOperation::bor: return Value((t)(left.v | right.v));                                            \
			case BinaryOperation::ban: return Value((t)(left.v & right.v));                                            \
			case BinaryOperation::bxo: return Value((t)(left.v ^ right.v));                                            \
			case BinaryOperation::bsl: return Value((t)(left.v << right.v));                                           \
			case BinaryOperation::bsr: return Value((t)(left.v >> right.v));                                           \
			case BinaryOperation::equ: return Value(left.v == right.v);                                                \
			case BinaryOperation::neq: return Value(left.v != right.v);                                                \
			case BinaryOperation::les: return Value(left.v <  right.v);                                                \
			case BinaryOperation::leq: return Value(left.v <= right.v);                                                \
			case BinaryOperation::grt: return Value(left.v >  right.v);                                                \
			case BinaryOperation::grq: return Value(left.v >= right.v);                                                \
			default: invalid_code_path();                                                                              \
		}                                                                                                              \
	}                                                                                                                  \
	break;                                                                                                             \
}

	switch (left.kind) {
		OPS(u8, U8);
		OPS(u16, U16);
		OPS(u32, U32);
		OPS(u64, U64);
		OPS(s8, S8);
		OPS(s16, S16);
		OPS(s32, S32);
		OPS(s64, S64);
		case ValueKind::Bool: {
			if (right.kind == left.kind) {
				switch (binary->operation) {                                
					case BinaryOperation::bor: return Value((bool)(left.Bool | right.Bool));
					case BinaryOperation::ban: return Value((bool)(left.Bool & right.Bool));
					case BinaryOperation::bxo: return Value((bool)(left.Bool ^ right.Bool));
					case BinaryOperation::lor: return Value(left.Bool || right.Bool);
					case BinaryOperation::lan: return Value(left.Bool && right.Bool);
					case BinaryOperation::equ: return Value(left.Bool == right.Bool); 
					case BinaryOperation::neq: return Value(left.Bool != right.Bool); 
					case BinaryOperation::les: return Value(left.Bool <  right.Bool); 
					case BinaryOperation::leq: return Value(left.Bool <= right.Bool); 
					case BinaryOperation::grt: return Value(left.Bool >  right.Bool); 
					case BinaryOperation::grq: return Value(left.Bool >= right.Bool); 
					default: invalid_code_path();                            
				}   
			}
			break;
		}
	}

#undef OPS

	immediate_reporter.error("NODE INTERPTRETER ERROR:");
	immediate_reporter.error(binary->location, "Invalid binary operation: {} {} {}", left.kind, binary->operation, right.kind);
	yield(YieldResult::fail);
	return {};
}
Value NodeInterpreter::execute_impl(Match *match) {
	EXECUTE_DEFN(value, match->expression);
	assert(value.kind == ValueKind::S64, "Only this is implemented");

	for (auto Case : match->cases) {
		for (auto from : Case.froms) {
			EXECUTE_DEFN(from_value, from);
			assert(from_value.kind == ValueKind::S64, "Only this is implemented");
			if (value.S64 == from_value.S64) {
				return execute(Case.to);
			}
		}
	}

	if (match->default_case) {
		return execute(match->default_case);
	}

	invalid_code_path(match->location, "match did not match value {}", value);
}
Value NodeInterpreter::execute_impl(Unary *unary) {
	switch (unary->operation) {
		case UnaryOperation::plus: {
			return execute(unary->expression);
		}
		case UnaryOperation::minus: {
			EXECUTE_DEFN(value, unary->expression);
			switch (value.kind) {
				case ValueKind::S64: value.S64 = -value.S64; break;
				default: invalid_code_path();
			}
			return value;
		}
		case UnaryOperation::addr: {
			return load_address(unary->expression);
		}
		case UnaryOperation::dereference: {
			EXECUTE_DEFN(pointer, unary->expression);
			assert(pointer.kind == ValueKind::pointer);
			return *pointer.pointer;
		}
		case UnaryOperation::typeof: {
			return Value(Type(unary->expression->type));
		}
		case UnaryOperation::pointer: {
			return Value(Type(unary));
		}
		default:
			invalid_code_path();
	}
}
Value NodeInterpreter::execute_impl(Struct *Struct) { 
	if (Struct->is_template) {
		immediate_reporter.error(Struct->location, "Interpreting a template struct is not valid.");
		yield(YieldResult::fail);
	}
	return Value(Type(Struct));
}
Value NodeInterpreter::execute_impl(Enum *Enum) { 
	return Value(Type(Enum));
}
Value NodeInterpreter::execute_impl(ArrayType *array) {
	return Value((Type)array);
}
Value NodeInterpreter::execute_impl(Subscript *node) {
	EXECUTE_DEFN(array, node->subscriptable);
	EXECUTE_DEFN(index, node->index);
	return element_at(array.elements, index);
}
Value NodeInterpreter::execute_impl(ArrayConstructor *node) {
	Value result;
	result.kind = ValueKind::array;
	result.elements = {};
	result.elements.resize(node->elements.count);
	for (umm i = 0; i < node->elements.count; ++i) {
		EXECUTE_INTO(result.elements[i], node->elements[i]);
	}
	return result;
}
Value NodeInterpreter::execute_impl(Import *import) { invalid_code_path(); }
Value NodeInterpreter::execute_impl(Defer *defer_) { not_implemented(); }
Value NodeInterpreter::execute_impl(ZeroInitialized *zi) {
	return zero_of_type(zi->type);
}
Value NodeInterpreter::execute_impl(CallerLocation *) {
	invalid_code_path("Should have been replaced by a string literal when typechecking caller arguments");
}
Value NodeInterpreter::execute_impl(CallerArgumentString *) {
	invalid_code_path("Should have been replaced by a string literal when typechecking caller arguments");
}

#undef PERFORM_WITH_BREAKS
#undef EXECUTE_INTO
#undef EXECUTE_DEFN
#undef LOAD_ADDRESS_INTO
#undef LOAD_ADDRESS_DEFN
