#include "builder.h"
#include "../nodes.h"
#include "../reporter.h"
#include "../builtin_structs.h"
#include "../make_node.h"
#include "../get_constant_value.h"
#include "../compiler_context.h"

namespace Bytecode {

#define MI(name, ...)                  \
	Instruction {                      \
		.kind = InstructionKind::name, \
		.v_##name = { __VA_ARGS__ },   \
		.file = __FILE_NAME__, \
		.line = __LINE__, \
		.source_location = current_location, \
	}
#define I(name, ...) (output_bytecode.instructions.add(MI(name, __VA_ARGS__)), 0)

#define tmpreg(name) \
	auto name = allocate_register(); \
	defer { deallocate(name); }

#define tmpaddr(name, size) \
	auto CONCAT(_size_, __LINE__) = size; \
	auto name = allocate_temporary(CONCAT(_size_, __LINE__)); \
	defer { temporary_offset -= CONCAT(_size_, __LINE__); }

#define tmpval(name, size) \
	auto name = create_destination(size); \
	defer { deallocate(name); }

Builder::Builder() {
	for (umm i = 0; i < (umm)Register::base; ++i) {
		available_registers.set(i, true);
	}
	I(intrinsic, Intrinsic::panic, u8"Called NULL!!!"s);
}

Bytecode Builder::build(Expression *expression) {
	output_bytecode.entry_point_instruction_index = output_bytecode.instructions.count;
	I(sub8, Register::stack, Register::stack, 8);
	tmpval(destination, get_size(expression->type));
	output(destination, expression);
	for (auto [index, lambda] : calls_to_patch) {
		auto &i = output_bytecode.instructions[index];

		if (lambda->is_extern) {
			immediate_reporter.error(lambda->location, "External calls not implemented yet");
			// TODO: better failing
			invalid_code_path();
		}

		switch (i.kind) {
			case InstructionKind::call: {
				i.call().d = lambda->first_instruction_index;
				break;
			}
			case InstructionKind::copy: {
				i.copy().s = lambda->first_instruction_index;
				break;
			}
			default: {
				invalid_code_path("can't patch '{}' instruction", i.kind);
			}
		}
	}
		
	for (auto relocation : lambda_relocations) {
		*(u64 *)&(*relocation.section)[relocation.offset] = relocation.lambda->first_instruction_index;
	}

	for (auto patch : pointers_to_patch) {
		patch.to.visit(Combine{
			[&](Lambda *lambda) {
				*(u64 *)&(*patch.in_section)[patch.in_section_offset] = lambda->first_instruction_index;
			},
			[&](PointerInSection::ToSection to) {
				*(u64 *)&(*patch.in_section)[patch.in_section_offset] = (u64)(to.section->data + to.offset);
			},
		});
	}

	return output_bytecode;
}


void Builder::append_global_definition(Definition *definition) {
	if (definition->mutability == Mutability::constant)
		return;

	auto &section = [&] () -> List<u8> & {
		switch (definition->mutability) {
			case Mutability::variable:
				return output_bytecode.global_mutable_data;
			case Mutability::readonly:
				return output_bytecode.global_readonly_data;
		}
		invalid_code_path();
	} ();
		
	definition->offset = section.count;

	if (definition->initial_value) {
		if (is_type(definition->initial_value))
			return;

		auto value = get_constant_value(definition->initial_value).value();

		append_to_section(section, definition->constant_value.value(), definition->type);
	} else {
		section.resize(section.count + get_size(definition->type));
	}
}

void Builder::append_lambda(Lambda *lambda) {
	assert(!lambda->head.is_template);
	scoped_replace(current_location, lambda->location);
	scoped_replace(locals_size, 0);
	scoped_replace(max_temporary_size, 0);
	scoped_replace(max_size_reserved_for_arguments, 0);
	scoped_replace(current_lambda, lambda);
	assert(available_registers.count() == (umm)Register::base);
	jumps_to_ret.clear();

	auto first_instruction_index = output_bytecode.instructions.count;

	auto head = &lambda->head;

	I(push, Register::base);
	I(copy, Register::base, Register::stack, register_size);
	auto reserver_index = output_bytecode.instructions.count;
	I(sub8, Register::stack, Register::stack, 0);

	u64 return_value_size = get_size(head->return_type);

	auto return_value_destination = Address { .base = Register::returns };

	I(set, .d = return_value_destination, .value = 0, .size = return_value_size);

	output(return_value_destination, lambda->body);

	scoped_replace(current_location, lambda->body->location.take(-1));

	lambda->first_instruction_index = first_instruction_index;
	output_bytecode.first_instruction_to_lambda.get_or_insert(first_instruction_index) = lambda;

	u64 reserved_stack_size = locals_size + max_temporary_size;
	reserved_stack_size += max_size_reserved_for_arguments;

	output_bytecode.instructions[reserver_index].sub8().b = reserved_stack_size;
	auto ret_destination = output_bytecode.instructions.count;
	I(add8, Register::stack, Register::stack, (s64)reserved_stack_size);
	I(pop, Register::base);
	I(ret);

	auto lambda_instructions = output_bytecode.instructions.skip(first_instruction_index);

	for (auto i : jumps_to_ret) {
		auto &jmp = output_bytecode.instructions[i].jmp();
		jmp.d = ret_destination;
	}

	if (lambda->definition) {
		dbgln(lambda->definition->name);
	} else {
		dbgln(get_source_location(lambda->location));
	}
	if (context_base->is_debugging) {
		println("    locals_size: {}, temporary_size: {}, total_parameters_size: {}, return_value_size: {}", locals_size, max_temporary_size, head->total_parameters_size, return_value_size);
	}
	if (context_base->is_debugging) {
		print_instructions(lambda_instructions);
	}

	lambda->locals_size = locals_size;
	lambda->temporary_size = max_temporary_size;
	lambda->space_for_call_arguments = max_size_reserved_for_arguments;
	lambda->stack_frame_size = max_size_reserved_for_arguments + max_temporary_size + locals_size + 16 + lambda->head.total_parameters_size + return_value_size;

	for (auto &i : lambda_instructions) {
		i.visit_addresses([&] (Address &a) {
			if (a.base) {
				switch(a.base.value()) {
					case Register::locals: {
						a.base = Register::base;
						a.offset -= locals_size;
						break;
					}
					case Register::temporary: {
						a.base = Register::base;
						a.offset -= max_temporary_size + locals_size;
						break;
					}
					case Register::arguments: {
						a.base = Register::base;
						a.offset += 16; // saved base + return address
						break;
					}
					case Register::returns: {
						a.base = Register::base;
						a.offset += head->total_parameters_size + 16; // parameters + saved base + return address
						break;
					}
				}
			}
		});

		if (i.kind == InstructionKind::copy) {
			if (i.copy().s.is_constant()) {
				assert(i.copy().size <= 8);
			}
		}
	}
}

void Builder::write(List<u8> const &section, u8 *dst, Value value, Type type, u64 size) {
	u64 dst_offset = dst - section.data;
	// TODO: use variant & visit
	switch (value.kind) {
		case ValueKind::Bool:
		case ValueKind::U8:
		case ValueKind::U16:
		case ValueKind::U32:
		case ValueKind::U64:
		case ValueKind::S8:
		case ValueKind::S16:
		case ValueKind::S32:
		case ValueKind::S64: {
			memcpy(dst, &value.S64, size);
			break;
		}
		case ValueKind::lambda: {
			lambda_relocations.add({autocast &section, dst_offset, value.lambda});
			break;
		}
		case ValueKind::struct_: {
			auto struct_ = direct_as<Struct>(type);
			assert(struct_);
			assert(value.elements.count == struct_->members.count);
			for (umm i = 0; i < value.elements.count; ++i) {
				auto member = struct_->members[i];
				write(section, dst + member->offset, value.elements[i], member->type, get_size(member->type));
			}
			break;
		}
		case ValueKind::pointer: {
			if (value.pointer) {
				PointerInSection pis = {
					.in_section = autocast &section,
					.in_section_offset = dst_offset,
				};

				switch (value.pointer->kind) {
					case ValueKind::lambda: {
						pis.to = value.pointer->lambda;
						break;
					}
					default: {
						not_implemented("pointer to ValueKind::{}", value.pointer->kind);
					}
				}

				pointers_to_patch.add(pis);
			}
			break;
		}
		case ValueKind::String: {
			u8 *data = dst;
			u8 *count = dst + 8;

			pointers_to_patch.add({
				.in_section = autocast &section,
				.in_section_offset = (u64)(data - section.data),
				.to = PointerInSection::ToSection{
					.section = &output_bytecode.global_readonly_data,
					.offset = string_literal_offset(value.String),
				}
			});

			*(u64 *)count = value.String.count;
			break;
		}
		default:
			invalid_code_path("Writing {} to section is not handled.", value.kind);
	}
}

u64 Builder::align_size(u64 x) { return ceil<u64>(max<u64>(1, x), 8); }
s64 Builder::align_size(s64 x) { return ceil<s64>(max<s64>(1, x), 8); }

Register Builder::allocate_register() {
	return (Register)available_registers.pop().value();
}
void Builder::deallocate(Register r) {
	assert(!available_registers.get((umm)r));
	available_registers.set((umm)r, true);
}

Address Builder::allocate_temporary(u64 size) {
	Address result;
	result.base = Register::temporary;
	result.offset = temporary_offset;
	temporary_offset += size;
	max_temporary_size = max(max_temporary_size, temporary_offset);
	return result;
}

void Builder::reserve_space_for_arguments(u64 size) {
	max_size_reserved_for_arguments = max(max_size_reserved_for_arguments, size);
}

Site Builder::create_destination(u64 size) {
	if (size <= 8) {
		return allocate_register();
	} else {
		return allocate_temporary(size);
	}
}
void Builder::deallocate(Site d) {
	if (d.is_register()) {
		deallocate(d.get_register());
	}
}
	
void Builder::output_integer_conversion(Site destination, Expression *expression, u64 target_size, u64 source_size, bool source_signed) {
	if (target_size == source_size) {
		output(destination, expression);
		// Same size, noop
	} else if (target_size >= source_size) {
		output(destination, expression);
		// Sign or zero extend
		if (source_signed) {
			switch (target_size) {
				case 2: 
					I(sex21, destination, destination);
					break;
				case 4:
					switch (source_size) {
						case 1: I(sex41, destination, destination); break;
						case 2: I(sex42, destination, destination); break;
					}
					break;
				case 8:
					switch (source_size) {
						case 1: I(sex81, destination, destination); break;
						case 2: I(sex82, destination, destination); break;
						case 4: I(sex84, destination, destination); break;
					}
					break;
			}
		} else {
			switch (target_size) {
				case 2: 
					I(and2, destination, destination, 0x00ff);
					break;
				case 4:
					switch (source_size) {
						case 1: I(and4, destination, destination, 0x000000ff); break;
						case 2: I(and4, destination, destination, 0x0000ffff); break;
					}
					break;
				case 8:
					switch (source_size) {
						case 1: I(and8, destination, destination, 0x00000000000000ff); break;
						case 2: I(and8, destination, destination, 0x000000000000ffff); break;
						case 4: I(and8, destination, destination, 0x00000000ffffffff); break;
					}
					break;
			}
		}
	} else {
		tmpreg(tmp);
		output(tmp, expression);
		// Truncate
		I(copy, destination, tmp, target_size);
	}
}

void Builder::output(Site destination, Expression *expression) {
	scoped_replace(current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return output_impl(destination, (name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path("invalid expression kind {}", expression->kind);
}

void Builder::output(Statement *statement) {
	scoped_replace(current_location, statement->location);
	switch (statement->kind) {
#define x(name) case NodeKind::name: return output_impl((name *)statement);
		ENUMERATE_STATEMENT_KIND(x)
#undef x
	}
	invalid_code_path("invalid statement kind {}", statement->kind);
}

void Builder::output_discard(Node *node) {
	scoped_replace(current_location, node->location);
	if (auto definition = as<Definition>(node)) {
		if (definition->mutability != Mutability::constant) {
			output_local_definition({}, definition);
		}
	} else if (auto expression = as<Expression>(node)) {
		tmpval(destination, get_size(expression->type));
		output(destination, expression);
	} else {
		auto statement = as<Statement>(node);
		assert(statement);
		output(statement);
	}
}

void Builder::output_local_definition(Optional<Site> destination, Definition *definition) {
	assert(definition->mutability != Mutability::constant);
	auto offset = locals_size;
	definition->offset = locals_size;
	auto definition_size = get_size(definition->type);
	locals_size = ceil<umm>(locals_size + definition_size, 8);
	auto address = Address{.base = Register::locals, .offset = (s64)offset};
	if (definition->initial_value) {
		output(address, definition->initial_value);
	} else {
		if (auto struct_ = direct_as<Struct>(definition->type)) {
			for (auto member : struct_->members) {
				if (member->initial_value) {
					immediate_reporter.warning(definition->location, "default struct values with custom initializers are not implemented. initializing with zero");
					break;
				}
			}
		}
		I(set, .d = address, .value = 0, .size = definition_size);
	}
	if (destination) {
		I(copy, .d = destination.value(), .s = address, .size = definition_size);
	}
}

Address Builder::get_definition_address(Definition *definition) {
	assert(definition->offset != Definition::invalid_offset);
	if (definition->is_parameter) {
		return Address{.base = Register::arguments, .offset = (s64)definition->offset};
	} else if (!definition->container) {
		if (definition->mutability == Mutability::variable) {
			return Address{.base = Register::global_mutable, .offset = (s64)definition->offset};
		} else {
			return Address{.base = Register::global_readonly, .offset = (s64)definition->offset};
		}
	} else {
		return Address{.base = Register::locals, .offset = (s64)definition->offset};
	}
}

bool Builder::is_addressable(Expression *expression) {
	if (expression->kind == NodeKind::Name) {
		return true;
	}
	return false;
}

u64 Builder::string_literal_offset(String string) {
	auto found = string_literal_offsets.find(string);
	if (found) {
		return *found.value;
	} else {
		auto offset = output_bytecode.global_readonly_data.count;
		output_bytecode.global_readonly_data.add((Span<u8>)string);
		output_bytecode.global_readonly_data.add('\0');
		string_literal_offsets.insert(string, offset);
		return offset;
	}
}
void Builder::output_impl(Site destination, Block *block) {
	scoped_replace(current_block, block);

	auto &info = block_infos.get_or_insert(block);
	if (block->breaks.count) {
		info.destination = destination;
	}
	info.break_jump_indices.clear();

	defer {
		for (auto i : info.break_jump_indices) {
			output_bytecode.instructions[i].jmp().d = output_bytecode.instructions.count;
		}
	};

	auto output_children = [&] {
		if (block->children.count) {
			if (auto expression = as<Expression>(block->children.back())) {
				for (auto child : block->children.skip(-1)) {
					output_discard(child);
				}
				output(destination, expression);
				return;
			}
		}

		for (auto child : block->children) {
			output_discard(child);
		}
	};

	output_children();
	for (auto defer_ : block->defers) {
		output_discard(defer_->body);
	}
} 
void Builder::output_impl(Site destination, Call *call) {
	auto [lambda, head, Struct] = get_lambda_and_head_or_struct(call->callable);
	switch (call->call_kind) {
		case CallKind::lambda: {
			assert(head);

			s64 return_value_size = get_size(head->return_type);

			auto registers_to_save = ~available_registers;
			if (destination.is_register())
				registers_to_save.set((umm)destination.get_register(), false);

			tmpaddr(saved_registers, registers_to_save.count() * 8);

			umm debug = 0;
			for_each(registers_to_save, [&] (umm r) {
				++debug;
				I(copy, saved_registers, (Register)r, 8);
				saved_registers.offset += 8;
			});
			assert(debug == registers_to_save.count());

			for (umm i = 0; i < call->arguments.count; ++i) {
				auto argument = call->arguments[i];
				assert(argument.parameter);
				auto offset = (s64)argument.parameter->offset;
				assert(offset != Definition::invalid_offset);

				Site destination = Address{.base = Register::stack, .offset = offset};

				output(destination, argument.expression);
			}

			if (lambda) {
				if (lambda->is_intrinsic) {
					auto definition = lambda->definition;
					assert(definition, lambda->location, "Intrinsic function is expected to have a definition, but for some reason this doesn't");
					if (definition->name == "print") {
						auto &parameters = lambda->head.parameters_block.definition_list;
						assert(parameters.count == 1, lambda->location, "Intrinsic function 'print' is expected to have exactly one parameter");
						auto parameter_type = parameters[0]->type;
						if (types_match(parameter_type, BuiltinType::S64)) {
							I(intrinsic, Intrinsic::print_S64, {});
						} else if (types_match(parameter_type, context->builtin_structs.String)) {
							I(intrinsic, Intrinsic::print_String, {});
						} else {
							invalid_code_path(definition->location, "Unsupported parameter type '{}' in 'print' intrinsic", parameter_type);
						}
					} else if (definition->name == "panic") {
						I(intrinsic, Intrinsic::panic, {});
					} else if (definition->name == "debug_break") {
						I(intrinsic, Intrinsic::debug_break, {});
					} else if (definition->name == "assert") {
						I(intrinsic, Intrinsic::assert, call->arguments[0].expression->location);
					} else {
						invalid_code_path(definition->location, "Unknown intrinsic name '{}'", definition->name);
					}
				} else if (lambda->is_extern) {
					assert(lambda->definition);
					assert(lambda->definition->name.count);
					I(callext, .lambda = lambda, .lib = lambda->extern_library, .name = lambda->definition->name);
				} else {
					calls_to_patch.add({output_bytecode.instructions.count, lambda});
					I(call, .d = 0);
				}
			} else {
				// TODO: Can't distinguish externs and intrinsics here.
				//       Assume local lambda.

				tmpreg(callable);

				output(callable, call->callable);

				I(call, callable);
			}
				
			for_each<ForEach_reverse>(registers_to_save, [&] (umm r) {
				saved_registers.offset -= 8;
				I(copy, (Register)r, saved_registers, 8);
			});

			reserve_space_for_arguments(head->total_parameters_size + align_size(return_value_size));
			I(copy, .d = destination, .s = Address{.base = Register::stack, .offset = (s64)head->total_parameters_size}, .size = (u64)return_value_size);

			break;
		}
		case CallKind::constructor: {
			assert(Struct);
			for (umm i = 0; i < Struct->members.count; ++i) {
				auto argument = call->arguments[i];
				auto member = Struct->members[i];

				assert(destination.is_address());

				auto member_address = destination.get_address();
				member_address.offset += member->offset;

				output(member_address, argument.expression);
			}
			break;
		}
		default: 
			invalid_code_path("CallKind::{}", call->call_kind);
	}
} 
void Builder::output_impl(Site destination, Definition *definition) {
	output_local_definition(destination, definition);
}
void Builder::output_impl(Site destination, IntegerLiteral *literal) {
	I(copy, .d = destination, .s = (s64)literal->value, .size = get_size(literal->type));
} 
void Builder::output_impl(Site destination, BooleanLiteral *literal) {
	I(copy, .d = destination, .s = (s64)literal->value, .size = 1);
} 
void Builder::output_impl(Site destination, NoneLiteral *literal) {
	I(copy, .d = destination, .s = 0, .size = get_size(literal->type));
}
void Builder::output_impl(Site destination, StringLiteral *literal) {
	umm offset = string_literal_offset(literal->value);

	assert(destination.is_address(), "not implemented");

	I(lea, .d = destination, .s = Address{ .base = Register::global_readonly, .offset = (s64)offset });
	destination.get_address().offset += 8;
	I(copy, .d = destination, .s = (s64)literal->value.count, .size = 8);
} 
void Builder::output_impl(Site destination, Lambda *lambda) {
	calls_to_patch.add({output_bytecode.instructions.count, lambda});
	I(copy, .d = destination, .s = 0, .size = 8);
} 
void Builder::output_impl(Site destination, LambdaHead *node) { not_implemented(); } 
void Builder::output_impl(Site destination, Name *name) {
	auto definition = name->definition();
	I(copy, .d = destination, .s = get_definition_address(definition), .size = get_size(name->type));
} 
void Builder::output_impl(Site destination, IfExpression *If) {
	umm jf_index;
	{
		tmpreg(cr);
		output(cr, If->condition);
		jf_index = output_bytecode.instructions.count;
		I(jf, cr, 0);
	}
	output(destination, If->true_branch);
	auto jmp_index = output_bytecode.instructions.count;
	I(jmp, 0);
	output_bytecode.instructions[jf_index].jf().d = output_bytecode.instructions.count;
	output(destination, If->false_branch);
	output_bytecode.instructions[jmp_index].jmp().d = output_bytecode.instructions.count;
} 
void Builder::output_impl(Site destination, BuiltinTypeName *node) {
	I(copy, destination, (s64)node->type_kind, 8);
} 
void Builder::output_impl(Site destination, Binary *binary) {
	if (binary->uid == 332) {
		int x = 4;
	}
	switch (binary->low_operation) {
		case LowBinaryOperation::add8:
		case LowBinaryOperation::add16:
		case LowBinaryOperation::add32:
		case LowBinaryOperation::add64:
		case LowBinaryOperation::sub8:
		case LowBinaryOperation::sub16:
		case LowBinaryOperation::sub32:
		case LowBinaryOperation::sub64:
		case LowBinaryOperation::mul8:
		case LowBinaryOperation::mul16:
		case LowBinaryOperation::mul32:
		case LowBinaryOperation::mul64:
		case LowBinaryOperation::divu8:
		case LowBinaryOperation::divu16:
		case LowBinaryOperation::divu32:
		case LowBinaryOperation::divu64:
		case LowBinaryOperation::divs8:
		case LowBinaryOperation::divs16:
		case LowBinaryOperation::divs32:
		case LowBinaryOperation::divs64:
		case LowBinaryOperation::modu8:
		case LowBinaryOperation::modu16:
		case LowBinaryOperation::modu32:
		case LowBinaryOperation::modu64:
		case LowBinaryOperation::mods8:
		case LowBinaryOperation::mods16:
		case LowBinaryOperation::mods32:
		case LowBinaryOperation::mods64:
		case LowBinaryOperation::equ8:
		case LowBinaryOperation::equ16:
		case LowBinaryOperation::equ32:
		case LowBinaryOperation::equ64:
		case LowBinaryOperation::neq8:
		case LowBinaryOperation::neq16:
		case LowBinaryOperation::neq32:
		case LowBinaryOperation::neq64:
		case LowBinaryOperation::lts8:
		case LowBinaryOperation::lts16:
		case LowBinaryOperation::lts32:
		case LowBinaryOperation::lts64:
		case LowBinaryOperation::les8:
		case LowBinaryOperation::les16:
		case LowBinaryOperation::les32:
		case LowBinaryOperation::les64:
		case LowBinaryOperation::gts8:
		case LowBinaryOperation::gts16:
		case LowBinaryOperation::gts32:
		case LowBinaryOperation::gts64:
		case LowBinaryOperation::ges8:
		case LowBinaryOperation::ges16:
		case LowBinaryOperation::ges32:
		case LowBinaryOperation::ges64:
		case LowBinaryOperation::ltu8:
		case LowBinaryOperation::ltu16:
		case LowBinaryOperation::ltu32:
		case LowBinaryOperation::ltu64:
		case LowBinaryOperation::leu8:
		case LowBinaryOperation::leu16:
		case LowBinaryOperation::leu32:
		case LowBinaryOperation::leu64:
		case LowBinaryOperation::gtu8:
		case LowBinaryOperation::gtu16:
		case LowBinaryOperation::gtu32:
		case LowBinaryOperation::gtu64:
		case LowBinaryOperation::geu8:
		case LowBinaryOperation::geu16:
		case LowBinaryOperation::geu32:
		case LowBinaryOperation::geu64: {
			tmpreg(left);
			output(left, binary->left);
			tmpreg(right);
			output(right, binary->right);
			switch (binary->low_operation) {
				case LowBinaryOperation::add8:  I(add1, destination, left, right); break;
				case LowBinaryOperation::add16: I(add2, destination, left, right); break;
				case LowBinaryOperation::add32: I(add4, destination, left, right); break;
				case LowBinaryOperation::add64: I(add8, destination, left, right); break;
				case LowBinaryOperation::sub8:  I(sub1, destination, left, right); break;
				case LowBinaryOperation::sub16: I(sub2, destination, left, right); break;
				case LowBinaryOperation::sub32: I(sub4, destination, left, right); break;
				case LowBinaryOperation::sub64: I(sub8, destination, left, right); break;
				case LowBinaryOperation::mul8:  I(mul1, destination, left, right); break;
				case LowBinaryOperation::mul16: I(mul2, destination, left, right); break;
				case LowBinaryOperation::mul32: I(mul4, destination, left, right); break;
				case LowBinaryOperation::mul64: I(mul8, destination, left, right); break;
				case LowBinaryOperation::divu8:  I(divu1, destination, left, right); break;
				case LowBinaryOperation::divu16: I(divu2, destination, left, right); break;
				case LowBinaryOperation::divu32: I(divu4, destination, left, right); break;
				case LowBinaryOperation::divu64: I(divu8, destination, left, right); break;
				case LowBinaryOperation::divs8:  I(divs1, destination, left, right); break;
				case LowBinaryOperation::divs16: I(divs2, destination, left, right); break;
				case LowBinaryOperation::divs32: I(divs4, destination, left, right); break;
				case LowBinaryOperation::divs64: I(divs8, destination, left, right); break;
				case LowBinaryOperation::modu8:  I(modu1, destination, left, right); break;
				case LowBinaryOperation::modu16: I(modu2, destination, left, right); break;
				case LowBinaryOperation::modu32: I(modu4, destination, left, right); break;
				case LowBinaryOperation::modu64: I(modu8, destination, left, right); break;
				case LowBinaryOperation::mods8:  I(mods1, destination, left, right); break;
				case LowBinaryOperation::mods16: I(mods2, destination, left, right); break;
				case LowBinaryOperation::mods32: I(mods4, destination, left, right); break;
				case LowBinaryOperation::mods64: I(mods8, destination, left, right); break;
				case LowBinaryOperation::equ8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::equals); break;
				case LowBinaryOperation::equ16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::equals); break;
				case LowBinaryOperation::equ32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::equals); break;
				case LowBinaryOperation::equ64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::equals); break;
				case LowBinaryOperation::neq8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::not_equals); break;
				case LowBinaryOperation::neq16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::not_equals); break;
				case LowBinaryOperation::neq32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::not_equals); break;
				case LowBinaryOperation::neq64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::not_equals); break;
				case LowBinaryOperation::lts8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less); break;
				case LowBinaryOperation::lts16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less); break;
				case LowBinaryOperation::lts32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less); break;
				case LowBinaryOperation::lts64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less); break;
				case LowBinaryOperation::les8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less_equals); break;
				case LowBinaryOperation::les16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less_equals); break;
				case LowBinaryOperation::les32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less_equals); break;
				case LowBinaryOperation::les64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less_equals); break;
				case LowBinaryOperation::gts8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater); break;
				case LowBinaryOperation::gts16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater); break;
				case LowBinaryOperation::gts32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater); break;
				case LowBinaryOperation::gts64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater); break;
				case LowBinaryOperation::ges8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater_equals); break;
				case LowBinaryOperation::ges16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater_equals); break;
				case LowBinaryOperation::ges32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater_equals); break;
				case LowBinaryOperation::ges64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater_equals); break;
				case LowBinaryOperation::ltu8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less); break;
				case LowBinaryOperation::ltu16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less); break;
				case LowBinaryOperation::ltu32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less); break;
				case LowBinaryOperation::ltu64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less); break;
				case LowBinaryOperation::leu8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less_equals); break;
				case LowBinaryOperation::leu16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less_equals); break;
				case LowBinaryOperation::leu32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less_equals); break;
				case LowBinaryOperation::leu64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less_equals); break;
				case LowBinaryOperation::gtu8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater); break;
				case LowBinaryOperation::gtu16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater); break;
				case LowBinaryOperation::gtu32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater); break;
				case LowBinaryOperation::gtu64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater); break;
				case LowBinaryOperation::geu8:  I(cmp1, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater_equals); break;
				case LowBinaryOperation::geu16: I(cmp2, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater_equals); break;
				case LowBinaryOperation::geu32: I(cmp4, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater_equals); break;
				case LowBinaryOperation::geu64: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater_equals); break;
			}
			return;
		}
		case LowBinaryOperation::memcmp_equ:
		case LowBinaryOperation::memcmp_neq: {
			auto size = get_size(binary->left->type);
			switch (size) {
				case 1:
				case 2:
				case 4:
				case 8: {
					tmpreg(left);
					output(left, binary->left);
					tmpreg(right);
					output(right, binary->right);
					auto cmp = [&] {
						switch (binary->low_operation) {
							case LowBinaryOperation::memcmp_equ: return Comparison::equals;
							case LowBinaryOperation::memcmp_neq: return Comparison::not_equals;
						}
						invalid_code_path();
					}();

					switch (size) {
						case 1: I(cmp1, .d = destination, .a = left, .b = right, .cmp = cmp); break;
						case 2: I(cmp2, .d = destination, .a = left, .b = right, .cmp = cmp); break;
						case 4: I(cmp4, .d = destination, .a = left, .b = right, .cmp = cmp); break;
						case 8: I(cmp8, .d = destination, .a = left, .b = right, .cmp = cmp); break;
					}
					return;
				}
			}
		}
		case LowBinaryOperation::zeroinit: {
			if (destination.is_address()) {
				I(set, destination.get_address(), 0, get_size(binary->right));
			} else {
				I(copy, destination.get_register(), 0, get_size(binary->right));
			}
			return;
		}
		case LowBinaryOperation::left_to_bool:
		case LowBinaryOperation::right_to_bool: {
			auto selected = binary->low_operation == LowBinaryOperation::left_to_bool ? binary->left : binary->right;
			tmpreg(value);
			output(value, selected);
			auto size = get_size(selected->type);
			switch (size) {
				case 1: I(cmp1, .d = destination, .a = value, .b = 0, .cmp = Comparison::not_equals); break;
				case 2: I(cmp2, .d = destination, .a = value, .b = 0, .cmp = Comparison::not_equals); break;
				case 4: I(cmp4, .d = destination, .a = value, .b = 0, .cmp = Comparison::not_equals); break;
				case 8: I(cmp8, .d = destination, .a = value, .b = 0, .cmp = Comparison::not_equals); break;
			}
			return;
		}
		case LowBinaryOperation::zex8to16:  { output(destination, binary->left); I(and2, destination, destination, 0xff      ); return; }
		case LowBinaryOperation::zex8to32:  { output(destination, binary->left); I(and4, destination, destination, 0xff      ); return; }
		case LowBinaryOperation::zex16to32: { output(destination, binary->left); I(and4, destination, destination, 0xffff    ); return; }
		case LowBinaryOperation::zex8to64:  { output(destination, binary->left); I(and8, destination, destination, 0xff      ); return; }
		case LowBinaryOperation::zex16to64: { output(destination, binary->left); I(and8, destination, destination, 0xffff    ); return; }
		case LowBinaryOperation::zex32to64: { output(destination, binary->left); I(and8, destination, destination, 0xffffffff); return; }
		case LowBinaryOperation::sex8to16:  { output(destination, binary->left); I(sex21, destination, destination); return; }
		case LowBinaryOperation::sex8to32:  { output(destination, binary->left); I(sex41, destination, destination); return; }
		case LowBinaryOperation::sex16to32: { output(destination, binary->left); I(sex42, destination, destination); return; }
		case LowBinaryOperation::sex8to64:  { output(destination, binary->left); I(sex81, destination, destination); return; }
		case LowBinaryOperation::sex16to64: { output(destination, binary->left); I(sex82, destination, destination); return; }
		case LowBinaryOperation::sex32to64: { output(destination, binary->left); I(sex84, destination, destination); return; }
		case LowBinaryOperation::left: {
			output(destination, binary->left);
			return;
		}
	}
		
	auto dleft  = binary->left->type ? direct(binary->left->type) : 0;
	auto dright = binary->right->type ? direct(binary->right->type) : 0;

	if (binary->operation == BinaryOperation::dot) {
		auto Struct = as<::Struct>(dleft);
		auto member_size = get_size(binary->type);
		auto member = as<Name>(binary->right)->definition();
		if (Struct) {
			tmpaddr(struct_addr, Struct->size);
			output(struct_addr, binary->left);
			
			auto member_address = struct_addr;
			member_address.offset += member->offset;
			I(copy, destination, member_address, member_size);
		} else {
			assert(as_pointer(dleft));
			tmpreg(struct_addr_reg);
			output(struct_addr_reg, binary->left);
			I(copy, destination, Address { .base = struct_addr_reg, .offset = (s64)member->offset }, member_size);
		}
		return;
	}
	if (binary->operation == BinaryOperation::ass) {
		tmpreg(addr);
		load_address(addr, binary->left);
		output(Address { addr }, binary->right);
		return;
	}
	switch (binary->operation) {
		case BinaryOperation::add:
		case BinaryOperation::sub:
		case BinaryOperation::mul:
		case BinaryOperation::div:
		case BinaryOperation::mod:
		case BinaryOperation::bxo:
		case BinaryOperation::ban:
		case BinaryOperation::bor:
		case BinaryOperation::bsl:
		case BinaryOperation::bsr: {
			auto result_size = get_size(binary->type);
			auto left_size = get_size(dleft);
			auto right_size = get_size(dright);
			assert(result_size == left_size);
			assert(result_size == right_size);
			assert(result_size <= 8);

			tmpreg(left);
			output(left, binary->left);
			tmpreg(right);
			output(right, binary->right);


			switch (result_size) {
				#define x(n)                                                                                       \
					case n: {                                                                                      \
						switch (binary->operation) {                                                               \
							case BinaryOperation::add: I(add##n, .d = destination, .a = left, .b = right);  break; \
							case BinaryOperation::sub: I(sub##n, .d = destination, .a = left, .b = right);  break; \
							case BinaryOperation::mul: I(mul##n, .d = destination, .a = left, .b = right);  break; \
							case BinaryOperation::div:                                                             \
								if (is_signed_integer(dleft)) {                                                    \
									I(divs##n, .d = destination, .a = left, .b = right);                           \
								} else {                                                                           \
									I(divu##n, .d = destination, .a = left, .b = right);                           \
								}                                                                                  \
								break;                                                                             \
							case BinaryOperation::mod:                                                             \
								if (is_signed_integer(dleft)) {                                                    \
									I(mods##n, .d = destination, .a = left, .b = right);                           \
								} else {                                                                           \
									I(modu##n, .d = destination, .a = left, .b = right);                           \
								}                                                                                  \
								break;                                                                             \
							case BinaryOperation::bxo: I(xor##n, .d = destination, .a = left, .b = right);  break; \
							case BinaryOperation::ban: I(and##n, .d = destination, .a = left, .b = right);  break; \
							case BinaryOperation::bor: I(or##n,  .d = destination, .a = left, .b = right);  break; \
							case BinaryOperation::bsl: I(sll##n, .d = destination, .a = left, .b = right);  break; \
							case BinaryOperation::bsr: {                                                           \
								if (is_signed_integer(dleft))                                                      \
									I(sra##n, .d = destination, .a = left, .b = right);                            \
								else                                                                               \
									I(srl##n, .d = destination, .a = left, .b = right);                            \
								break;                                                                             \
							}                                                                                      \
							default: not_implemented();                                                            \
						}                                                                                          \
						break;                                                                                     \
					}
				ENUMERATE_1248
				#undef x
			}

			return;
		}
		case BinaryOperation::equ:
		case BinaryOperation::neq:
		case BinaryOperation::les:
		case BinaryOperation::grt:
		case BinaryOperation::leq:
		case BinaryOperation::grq: {
			if (is_concrete_integer(dleft) && is_concrete_integer(dright)) {
				auto left_size = get_size(dleft);
				auto right_size = get_size(dright);
				assert(right_size == left_size);
				assert(right_size <= 8);

				tmpreg(left);
				output(left, binary->left);
				tmpreg(right);
				output(right, binary->right);

				Comparison c;
				switch (binary->operation) {
					case BinaryOperation::equ: c = Comparison::equals;     break;
					case BinaryOperation::neq: c = Comparison::not_equals; break;
					case BinaryOperation::les: c = ::is_signed_integer(dleft) ? Comparison::signed_less           : Comparison::unsigned_less;           break;
					case BinaryOperation::grt: c = ::is_signed_integer(dleft) ? Comparison::signed_greater        : Comparison::unsigned_greater;        break;
					case BinaryOperation::leq: c = ::is_signed_integer(dleft) ? Comparison::signed_less_equals    : Comparison::unsigned_less_equals;    break;
					case BinaryOperation::grq: c = ::is_signed_integer(dleft) ? Comparison::signed_greater_equals : Comparison::unsigned_greater_equals; break;
					default: not_implemented();
				}

				switch (left_size) {
					case 1: I(cmp1, .d = destination, .a = left, .b = right, .cmp = c); break;
					case 2: I(cmp2, .d = destination, .a = left, .b = right, .cmp = c); break;
					case 4: I(cmp4, .d = destination, .a = left, .b = right, .cmp = c); break;
					case 8: I(cmp8, .d = destination, .a = left, .b = right, .cmp = c); break;
					default: not_implemented();
				}
				return;
			}
			switch (binary->operation) {
				case BinaryOperation::equ:
				case BinaryOperation::neq: {
					if (auto pointer_expr = is_pointer_to_none_comparison(binary->left, binary->right)) {
						tmpreg(ptr);
						output(ptr, pointer_expr);
						I(cmp8, .d = destination, .a = ptr, .b = 0, .cmp = binary->operation == BinaryOperation::equ ? Comparison::equals : Comparison::not_equals);
					}
					break;
				}
			}
			break;
		}
		case BinaryOperation::lan: {
			output(destination, binary->left);
			auto jf_index = output_bytecode.instructions.count;
			I(jf, destination, 0);
			output(destination, binary->right);
			output_bytecode.instructions[jf_index].jf().d = output_bytecode.instructions.count;
			return;
		}
		case BinaryOperation::lor: {
			output(destination, binary->left);
			auto jt_index = output_bytecode.instructions.count;
			I(jt, destination, 0);
			output(destination, binary->right);
			output_bytecode.instructions[jt_index].jt().d = output_bytecode.instructions.count;
			return;
		}
		case BinaryOperation::as: {
			auto source_type = direct(dleft);
			auto target_type = direct(binary->right);

			// From none
			if (types_match(source_type, BuiltinType::None)) {
				// To pointer
				if (auto right_pointer = as_pointer(target_type)) {
					I(copy, destination, 0, pointer_size);
					return;
				}
			}

			// From lambda
			if (auto lambda = direct_as<Lambda>(binary->left)) {
				// To pointer
				if (auto right_pointer = as_pointer(target_type)) {
					if (lambda->is_extern) {
						I(copyext, destination, lambda->extern_library, lambda->definition->name);
						return;
					} else {
						// TODO: only works with direct lambdas.
						auto bridge = target_platform->create_bridge(lambda);
						I(copy, destination, (s64)bridge, pointer_size);
						return;
					}
				}
			}
				
			// From pointer
			if (auto left_pointer = as_pointer(source_type)) {
				// To pointer
				if (auto right_pointer = as_pointer(target_type)) {
					output(destination, binary->left);
					return;
				}
				// To integer
				if (is_concrete_integer(target_type)) {
					output(destination, binary->left);
					return;
				}
			}
				

			// From integer
			if (is_concrete_integer(source_type)) {
				// To integer
				if (is_concrete_integer(target_type)) {
					auto target_size = get_size(target_type);
					auto source_size = get_size(source_type);
					bool source_is_signed = is_signed_integer(source_type);

					output_integer_conversion(destination, binary->left, target_size, source_size, source_is_signed);
					return;
				}

				// To pointer
				if (auto right_pointer = as_pointer(target_type)) {
					auto source_size = get_size(source_type);
					bool source_is_signed = is_signed_integer(source_type);
					output_integer_conversion(destination, binary->left, 8, source_size, source_is_signed);
					return;
				}
			}

			if (types_match(target_type, BuiltinType::None)) {
				tmpval(discard, get_size(source_type));
				output(discard, binary->left);
				return;
			}

			break;
		}
	} 
	invalid_code_path("Unhandled binary operation {} {} {}", binary->left->type, binary->operation, binary->right->type);
}
void Builder::output_impl(Site destination, Match *match) {
	// FIXME: match without a default case can't always yield a value, so I think 
	// there should be separation between match expression and match statement.
	// Or just insert a default case that will panic?

	assert(is_concrete_integer(match->expression->type) || direct_as<Enum>(match->expression->type), "Match currently only works with integers and enums");

	auto matchee_size = get_size(match->expression->type);
	assert(matchee_size <= 8);
	tmpreg(matchee);
	output(matchee, match->expression);

	auto output_case = [&](Match::Case &Case) {
		if (Case.to_expression()) {
			output(destination, Case.to_expression());
		} else {
			output((Statement *)Case.to);
		}
	};

	List<umm> jumps_out_of_match;

	for (auto &Case : match->cases) {
		if (!Case.froms)
			continue;

		List<umm> jumps_to_case;
		for (auto &from : Case.froms) {
			tmpreg(f);
			output(f, from);
			scoped_replace(current_location, from->location);
			auto size = get_size(match->expression->type);
			switch (size) {
				case 1: I(cmp1, f, matchee, f, Comparison::equals); break;
				case 2: I(cmp2, f, matchee, f, Comparison::equals); break;
				case 4: I(cmp4, f, matchee, f, Comparison::equals); break;
				case 8: I(cmp8, f, matchee, f, Comparison::equals); break;
				default: invalid_code_path("`match` only works on sizes 1, 2, 4 or 8, but not {}", size);
			}
			jumps_to_case.add(output_bytecode.instructions.count);
			I(jt, f, 0);
		}
		auto jump_over_case = output_bytecode.instructions.count;
		I(jmp, 0);

		for (auto i : jumps_to_case) {
			output_bytecode.instructions[i].jt().d = output_bytecode.instructions.count;
		}
		output_case(Case);
		
		jumps_out_of_match.add(output_bytecode.instructions.count);
		I(jmp, 0);

		output_bytecode.instructions[jump_over_case].jmp().d = output_bytecode.instructions.count;
	}
		
	if (match->default_case) {
		output_case(*match->default_case);
	} else {
		if (!types_match(match->type, BuiltinType::None)) {
			reserve_space_for_arguments(16);
			StringLiteral message;
			message.value = tformat(u8"{}: failed to execute `match` expression with no default case", get_source_location(match->location));
			message.location = match->location;
			message.type = make_name(context->builtin_structs.String->definition);
			output(Address{.base = Register::stack}, &message);
			I(intrinsic, Intrinsic::print_String, {});
			I(add8, Register::stack, Register::stack, 16);
			I(intrinsic, Intrinsic::panic, {});
		}
	}

	for (auto i : jumps_out_of_match) {
		output_bytecode.instructions[i].jmp().d = output_bytecode.instructions.count;
	}
}
void Builder::output_impl(Site destination, Unary *unary) {
	switch (unary->operation) {
		case UnaryOperation::addr: {
			load_address(destination, unary->expression);
			break;
		}
		case UnaryOperation::dereference: {
			tmpreg(addr);
			output(addr, unary->expression);
			I(copy, .d = destination, .s = Address { .base = addr }, .size = get_size(unary->expression->type));
			break;
		}
		case UnaryOperation::lnot: {
			output(destination, unary->expression);
			I(cmp1, destination, 1, destination, Comparison::not_equals);
			break;
		}
		default: not_implemented();
	}
} 
void Builder::output_impl(Site destination, Struct *node) { not_implemented(); } 
void Builder::output_impl(Site destination, Enum *node) { not_implemented(); } 
void Builder::output_impl(Site destination, ArrayType *node) { not_implemented(); } 
void Builder::output_impl(Site destination, Subscript *subscript) {
	if (auto array_type = direct_as<ArrayType>(subscript->subscriptable->type)) {
		auto element_size = get_size(array_type->element_type);

		if (is_addressable(subscript->subscriptable)) {
			tmpreg(array_address);
			load_address(array_address, subscript->subscriptable);

			tmpreg(index_reg);
			output(index_reg, subscript->index);

			Address element_address = {};
			element_address.base = array_address;
			element_address.element_index = index_reg;
			assert(element_size < 256);
			element_address.element_size = element_size;

			I(copy, destination, element_address, element_size);
		} else {
			not_implemented();
		}
	} else if (auto pointer = as_pointer(direct(subscript->subscriptable->type))) {
		tmpreg(p);
		output(p, subscript->subscriptable);
		tmpreg(i);
		output(i, subscript->index);
		auto element_size = get_size(subscript->type);
		assert(element_size < 256);
		I(lea, p, Address{.base = p, .element_index = i, .element_size = (u8)element_size});
		I(copy, destination, Address{.base = p}, element_size);
	} else {
		invalid_code_path();
	}
} 
void Builder::output_impl(Site destination, ArrayConstructor *arr) {
	auto element_size = get_size(as<ArrayType>(arr->type)->element_type);
	assert(destination.is_address());
	auto element_destination = destination.get_address();
	for (auto element : arr->elements) {
		output(element_destination, element);
		element_destination.offset += element_size;
	}
}
void Builder::output_impl(Site destination, ZeroInitialized *zi) {
	if (destination.is_register()) {
		I(copy, destination, 0, get_size(zi->type));
	} else {
		I(set, destination.get_address(), 0, get_size(zi->type));
	}
}
void Builder::output_impl(Site destination, CallerLocation *) {
	invalid_code_path("Should have been replaced by a string literal when typechecking caller arguments");
}
void Builder::output_impl(Site destination, CallerArgumentString *) {
	invalid_code_path("Should have been replaced by a string literal when typechecking caller arguments");
}
void Builder::output_impl(Return *ret) {
	if (ret->value) {
		Site return_value_destination = Address { .base = Register::returns };
		output(return_value_destination, ret->value);
	}

	for (auto Defer : ret->defers) {
		output_discard(Defer->body);
	}

	jumps_to_ret.add(output_bytecode.instructions.count);
	I(jmp, 0);
} 
void Builder::output_impl(While *While) {
	umm condition_index;
	umm jf_index;
	{
		tmpreg(cr);
		condition_index = output_bytecode.instructions.count;
		output(cr, While->condition);
		jf_index = output_bytecode.instructions.count;
		I(jf, cr, 0);
	}
	output_discard(While->body);
	scoped_replace(current_location, While->body->location.take(-1));
	I(jmp, (s64)condition_index);
	output_bytecode.instructions[jf_index].jf().d = output_bytecode.instructions.count;
	for (auto i : continue_jump_indices.get_or_insert(While)) {
		output_bytecode.instructions[i].jmp().d = condition_index;
	}
	for (auto i : loop_break_indices.get_or_insert(While)) {
		output_bytecode.instructions[i].jmp().d = output_bytecode.instructions.count;
	}
} 
void Builder::output_impl(Continue *node) {
	for (auto Defer : node->defers) {
		output_discard(Defer->body);
	}

	continue_jump_indices.get_or_insert(node->loop).add(output_bytecode.instructions.count);
	I(jmp, 0);
} 
void Builder::output_impl(Break *node) {
	if (node->tag_block) {
		auto &info = *block_infos.find(node->tag_block).value;
		output(info.destination, node->value);
			
		for (auto Defer : node->defers) {
			output_discard(Defer->body);
		}

		info.break_jump_indices.add(output_bytecode.instructions.count);
		I(jmp, 0);
	} else {
		assert(node->loop);
		
		for (auto Defer : node->defers) {
			output_discard(Defer->body);
		}
			
		loop_break_indices.get_or_insert(node->loop).add(output_bytecode.instructions.count);
		I(jmp, 0);
	}

}
void Builder::output_impl(IfStatement *If) {
	umm jf_index;
	{
		tmpreg(cr);
		output(cr, If->condition);
		jf_index = output_bytecode.instructions.count;
		I(jf, cr, 0);
	}
	output_discard(If->true_branch);
	if (If->false_branch) {
		auto jmp_index = output_bytecode.instructions.count;
		I(jmp, 0);
		output_bytecode.instructions[jf_index].jf().d = output_bytecode.instructions.count;
		output_discard(If->false_branch);
		output_bytecode.instructions[jmp_index].jmp().d = output_bytecode.instructions.count;
	} else {
		output_bytecode.instructions[jf_index].jf().d = output_bytecode.instructions.count;
	}
} 
void Builder::output_impl(Import *import) { invalid_code_path(); } 
void Builder::output_impl(Defer *defer_) {
	/* Defers are outputted at end of blocks and at return statements */
} 

void Builder::load_address(Site destination, Expression *expression) {
	scoped_replace(current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: load_address_impl(destination, (name *)expression); break;
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
}

void Builder::load_address_impl(Site destination, Block *block) {
	for (auto child : block->children.skip(-1)) {
		output_discard(child);
	}
	return load_address(destination, as<Expression>(block->children.back()));
}
void Builder::load_address_impl(Site destination, Call *node) { invalid_code_path(); }
void Builder::load_address_impl(Site destination, Definition *definition) {
	output_local_definition({}, definition);
	I(lea, .d = destination, .s = get_definition_address(definition));
}
void Builder::load_address_impl(Site destination, IntegerLiteral *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, BooleanLiteral *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, NoneLiteral *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, StringLiteral *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, Lambda *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, LambdaHead *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, Name *name) {
	auto definition = name->definition();
	I(lea, destination, get_definition_address(definition));
}
void Builder::load_address_impl(Site destination, IfExpression *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, BuiltinTypeName *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, Binary *binary) {
	assert(binary->operation == BinaryOperation::dot);
	auto member = as<Name>(binary->right)->definition();
	auto Struct = direct_as<::Struct>(binary->left->type);
	if (Struct) {
		load_address(destination, binary->left);
	} else {
		assert(as_pointer(binary->left->type));
		output(destination, binary->left);
	}
	I(add8, destination, destination, (s64)member->offset);
}
void Builder::load_address_impl(Site destination, Match *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, Unary *unary) {
	switch (unary->operation) {
		case UnaryOperation::dereference: {
			output(destination, unary->expression);
			break;
		}
		default: not_implemented();
	}
}
void Builder::load_address_impl(Site destination, Struct *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, Enum *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, ArrayType *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, Subscript *node) {
	load_address(destination, node->subscriptable);
	tmpreg(offset);
	output(offset, node->index);
	s64 element_size = get_size(node->type);
	I(mul8, offset, offset, element_size);
	I(add8, destination, destination, offset);
}
void Builder::load_address_impl(Site destination, ArrayConstructor *node) { not_implemented(); }
void Builder::load_address_impl(Site destination, ZeroInitialized *zi) { not_implemented(); }
void Builder::load_address_impl(Site destination, CallerLocation *) { invalid_code_path(); }
void Builder::load_address_impl(Site destination, CallerArgumentString *) { invalid_code_path(); }

#undef MI
#undef I
#undef tmpreg
#undef tmpaddr
#undef tmpval

} // namespace Bytecode