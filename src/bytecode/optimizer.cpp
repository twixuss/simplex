#include "optimizer.h"
#include "../reporter.h"
#include <tl/macros.h>
#include <tl/static_set.h>

namespace Bytecode {

extern bool debug_print;

// Return Empty if instruction does not need to be optimized
// EDIT:
//     MSVC does some bullshit...
//     So linker can't find Instruction::`scalar deleting destructor' in ~OptionalBaseNonTrivial()
//     I could not solve that, so using bool.
bool optimize_one_instruction(Instruction &i) {
	#define I(name, ...)                  \
		Instruction {                      \
			.kind = InstructionKind::name, \
			.v_##name = { __VA_ARGS__ },   \
			.file = __FILE_NAME__, \
			.line = __LINE__, \
			.source_location = i.source_location, \
		}

	switch (i.kind) {
		using enum InstructionKind;

		case set: if (i.set().size == 0 || i.set().count == 0) { i = I(nop); return true; } break;
		case copy: if (i.copy().size == 0) { i = I(nop); return true; } break;
		case add: 
			if (i.add().a.as_constant() == (s64)0) { i = I(copy, i.add().d, i.add().b, (u64)i.add().layout.total_size()); return true; }
			if (i.add().b.as_constant() == (s64)0) { i = I(copy, i.add().d, i.add().a, (u64)i.add().layout.total_size()); return true; }
			break;
		case sub: if (i.sub().b.as_constant() == (s64)0) { i = I(copy, i.sub().d, i.sub().a, (u64)i.sub().layout.total_size()); return true; } break;
		case mul: 
			if (i.mul().a.as_constant() == (s64)1) { i = I(copy, i.mul().d, i.mul().b, (u64)i.mul().layout.total_size()); return true; }
			if (i.mul().b.as_constant() == (s64)1) { i = I(copy, i.mul().d, i.mul().a, (u64)i.mul().layout.total_size()); return true; }
			break;
		case divu: if (i.divu().b.as_constant() == (s64)1) { i = I(copy, i.divu().d, i.divu().a, (u64)i.divu().layout.total_size()); return true; } break;
		case divs: if (i.divs().b.as_constant() == (s64)1) { i = I(copy, i.divs().d, i.divs().a, (u64)i.divs().layout.total_size()); return true; } break;
		case bxor:
			if (i.bxor().b.as_constant() == (s64)0) { i = I(copy, i.bxor().d, i.bxor().a, (u64)i.bxor().layout.total_size()); return true; }
			if (i.bxor().a.as_constant() == (s64)0) { i = I(copy, i.bxor().d, i.bxor().b, (u64)i.bxor().layout.total_size()); return true; }
			if (i.bxor().a == i.bxor().b)           { i = I(copy, i.bxor().d,          0, (u64)i.bxor().layout.total_size()); return true; }
			break;
		case band:
			if (i.band().b.as_constant() == (s64)0)                  { i = I(copy, i.band().d,          0, (u64)i.band().layout.total_size()); return true; }
			if (i.band().a.as_constant() == (s64)0)                  { i = I(copy, i.band().d,          0, (u64)i.band().layout.total_size()); return true; }
			if (i.band().b.as_constant() == (s64)0xffffffffffffffff) { i = I(copy, i.band().d, i.band().a, (u64)i.band().layout.total_size()); return true; }
			if (i.band().a.as_constant() == (s64)0xffffffffffffffff) { i = I(copy, i.band().d, i.band().b, (u64)i.band().layout.total_size()); return true; }
			if (i.band().a == i.band().b)                            { i = I(copy, i.band().d, i.band().a, (u64)i.band().layout.total_size()); return true; }
			break;
		case bor:
			if (i.bor().b.as_constant() == (s64)0)                  { i = I(copy, i.bor().d,               i.bor().a, (u64)i.bor().layout.total_size()); return true; }
			if (i.bor().a.as_constant() == (s64)0)                  { i = I(copy, i.bor().d,               i.bor().b, (u64)i.bor().layout.total_size()); return true; }
			if (i.bor().b.as_constant() == (s64)0xffffffffffffffff) { i = I(copy, i.bor().d, (s64)0xffffffffffffffff, (u64)i.bor().layout.total_size()); return true; }
			if (i.bor().a.as_constant() == (s64)0xffffffffffffffff) { i = I(copy, i.bor().d, (s64)0xffffffffffffffff, (u64)i.bor().layout.total_size()); return true; }
			if (i.bor().a == i.bor().b)                             { i = I(copy, i.bor().d,               i.bor().a, (u64)i.bor().layout.total_size()); return true; }
			break;

		case jmp: if (i.jmp().d == 1) { i = I(nop); return true; } break;
		case jt:  if (i.jt().d == 1)  { i = I(nop); return true; } break;
		case jf:  if (i.jf().d == 1)  { i = I(nop); return true; } break;
	}

	#undef I

	return false;
}

// NOTE:
// This assumes `is` are instructions of a SINGLE lambda.
// Probably will not work with whole bytecode.
PackedInstructions optimize(Span<Instruction> is) {
	PackedInstructions result;
	auto presult = &result; // stupid debugger does not show result
	
	result.instructions.set(is);
	
	constexpr umm max_repetitions = 8;
	umm repetition = 0;
	bool something_was_optimized = true;
	for (; repetition < max_repetitions && something_was_optimized; ++repetition) {
		something_was_optimized = false;

		//
		// Look an one instruction at a time and see what does.
		// Replace it with noop if nothing, or with something cheaper.
		//
		for (umm i = 0; i < result.instructions.count; ++i) {
			while (1) {
				if (optimize_one_instruction(result.instructions[i])) {
					something_was_optimized = true;
				} else {
					break;
				}
			}
		}


		//
		// Remove copies.
		// 
		//   Substituting constants:
		//
		//     | Original |          | Optimized |
		// 
		//     copy r1, 77           nop
		//     mul r3, r2, r1        mul r3, r2, 77
		//
		//   Substituting registers:
		// 
		//      +- copy r1, r4           nop
		//      |  mul r3, r2, r1        mul r3, r2, r4
		//      |  ...
		//      |  ...
		//      |  ...
		// RUB -+  copy r4, 42      <- write to original register. if a read from repl happens after
		//      |                      that, abort all substitutions for this block.
		//      +- mul r3, r2, r1
		//         copy r1, x       <- substituting r1 with r4 ends here.
		// 
		// RUB - Register Usage Block
		// repl - r1 - what we are trying to replace with something else.
		// orig - r4 - what we are trying to place instead of r1
		// 
		//
		// If a read of replica occurs after writing to original, don't substitute anything.
		//     copy r1, r4     <- init repl
		//     mul r3, r2, r1  <- read repl
		//     copy r4, 42     <- write orig
		//     mul r3, r2, r1  <- read repl
		//     copy r1, x      <- write repl. end

		for (umm i = 0; i < result.instructions.count; ++i) {
			if (result.instructions[i].kind == InstructionKind::copy) {
				if (result.instructions[i].copy().d.is_register() && !result.instructions[i].copy().s.is_address()) {
					auto &orig = result.instructions[i].copy().s;
					auto &repl = result.instructions[i].copy().d.get_register();

					List<InputValue *> reads_from_repl;
					List<Address *> reads_from_repl_via_address;
					List<Site *> reads_from_repl_via_writes_to_address;

					bool stop = false;
					bool written_to_orig = false;
					bool discard = false;

					for (umm j = i + 1; j < result.instructions.count && !stop; ++j) {
					
						auto check_read_repl = [&](Register reg, auto *value, auto &reads) {
							if (reg == repl) {
								if (written_to_orig) {
									stop = true;
									discard = true;
								} else {
									reads.add(value);
								}
							}
						};

						auto read_from_address = [&](Address a, auto *value, auto &reads) {
							if (a.base) {
								check_read_repl(a.base.value(), value, reads);
							}
							if (a.element_size) {
								check_read_repl(a.element_index, value, reads);
							}
						};

						auto handle_operand = Combine {
							[](auto){},
							[&](Site &site) {
								// Write
								if (site.is_register()) {
									if (site.get_register() == orig) {
										written_to_orig = true;
									}
									if (site.get_register() == repl) {
										stop = true;
									}
								} else {
									read_from_address(site.get_address(), &site, reads_from_repl_via_writes_to_address);
								}
							},
							[&](InputValue &value) {
								// Read
								if (value.is_register()) {
									check_read_repl(value.get_register(), &value, reads_from_repl);
								} else if (value.is_address()) {
									read_from_address(value.get_address(), &value, reads_from_repl);
								}
							},
							[&](Address &address) {
								// Read
								read_from_address(address, &address, reads_from_repl_via_address);
							},
						};

						switch (result.instructions[j].kind) {
							// NOTE: Idk why the f msvc proprocessor does that, but
							// comma for some reason turns out before the call,
							// e.g. `,handle_operand(I.name)`
							// That's why 0 is first, not last.
							#define y(type, name) handle_operand(I.name),
							#define x(name, fields)           \
								case InstructionKind::name: { \
									auto &I = result.instructions[j].v_##name; \
									(0 TL_REVERSE fields);       \
									break;                    \
								}
							ENUMERATE_BYTECODE_INSTRUCTION_KIND
							#undef x
							#undef y
						}
					}

					if (discard) {
						continue;
					}
				
					// All checks passed, can substitute.
				
					auto patch_address = [&](Address &a) {
						if (orig.is_register()) {
							if (a.base == repl) {
								a.base = orig.get_register();
							}

							if (a.element_index == repl) {
								a.element_index = orig.get_register();
							}
						} else {
							if (a.base == repl) {
								a.base = {};
								a.offset += orig.get_constant();
							}

							if (a.element_index == repl) {
								a.offset += a.element_size * orig.get_constant();
								a.element_size = {};
							}
						}
					};

					for (auto read : reads_from_repl) {
						if (read->is_register()) {
							*read = orig;
						} else if (read->is_address()) {
							patch_address(read->get_address());
						}
					}
					for (auto read : reads_from_repl_via_address) {
						patch_address(*read);
					}
					for (auto read : reads_from_repl_via_writes_to_address) {
						assert(read->is_address());

						patch_address(read->get_address());
					}

					result.instructions[i] = {
						.kind = InstructionKind::nop, 
						.file = __FILE_NAME__, 
						.line = __LINE__, 
						.source_location = result.instructions[i].source_location, 
					};
					something_was_optimized = true;
				}
			}
		}
	

		//
		// Remove `lea`s
		//
		//      +- lea r0, [r2 + 16]     nop
		//      |  copy r1, [r0 + 8]     copy r1, [r2 + 24]
		// RUB -+  ...
		//      |  copy r2, 0     <- overwrite orig
		//      +- mul r1, r1, r0 <- read repl
		//         copy r0, 0     <- overwrite repl
		// 
		//	repl - r0
		//	orig - r2
		// 
		//  Here orig is a list of 0 to 2 register, as that's how many can be used in an address operand.
		// 
		//  Again like in copy elimination step, if a read from repl happens after modifying any of the original
		//	registers, cancel the substitution.
		//         
		for (umm i = 0; i < result.instructions.count; ++i) {
			if (result.instructions[i].kind == InstructionKind::lea) {
				if (result.instructions[i].lea().d.is_register()) {
					auto repl = result.instructions[i].lea().d.get_register();
					auto orig_address = result.instructions[i].lea().s;

					List<InputValue *> reads_from_repl;
					List<Address *>    reads_from_repl_via_address;
					List<Site *>       reads_from_repl_via_writes_to_address;

					StaticSet<Register, 2> origs;
				
					if (orig_address.base) {
						origs.add(orig_address.base.value());
					}
					if (orig_address.element_size) {
						origs.add(orig_address.element_index);
					}

					bool stop = false;
					bool written_to_orig = false;
					bool discard = false;

					for (umm j = i + 1; j < result.instructions.count && !stop; ++j) {
						auto check_read_repl = [&](Register reg, auto *value, auto &reads) {
							if (reg == repl) {
								if (written_to_orig) {
									stop = true;
									discard = true;
								} else {
									reads.add(value);
								}
							}
						};

						auto read_from_address = [&](Address a, auto *value, auto &reads) {
							if (a.base) {
								check_read_repl(a.base.value(), value, reads);
							}
							if (a.element_size) {
								check_read_repl(a.element_index, value, reads);
							}
						};

						auto handle_operand = Combine {
							[](auto){},
							[&](Site &site) {
								// Write
								if (site.is_register()) {
									for (auto orig : origs) {
										if (site.get_register() == orig) {
											written_to_orig = true;
										}
									}
									if (site.get_register() == repl) {
										stop = true;
									}
								} else {
									read_from_address(site.get_address(), &site, reads_from_repl_via_writes_to_address);
								}
							},
							[&](InputValue &value) {
								// Read
								if (value.is_register()) {
									check_read_repl(value.get_register(), &value, reads_from_repl);
								} else if (value.is_address()) {
									read_from_address(value.get_address(), &value, reads_from_repl);
								}
							},
							[&](Address &address) {
								// Read
								read_from_address(address, &address, reads_from_repl_via_address);
							},
						};

						switch (result.instructions[j].kind) {
							#define y(type, name) handle_operand(I.name),
							#define x(name, fields)           \
								case InstructionKind::name: { \
									auto &I = result.instructions[j].v_##name; \
									(0 TL_REVERSE fields);       \
									break;                    \
								}
							ENUMERATE_BYTECODE_INSTRUCTION_KIND
							#undef x
							#undef y
						}
					}
				
					if (discard) {
						continue;
					}

					// Contents of this list will replace stuff in the lists above.
					// Next, check the reads for validity of substitution, if ok - add to list, otherwise cancel.
					List<Address> substitutions;

					auto substitute_address = [&](Address a) {
						// 
						// Example that can't work:
						// 
						// lea r0, [r0 + r1]       nop
						// set [r0 + r2], 0, 8     set [r0 + r1 + r2], 0, 8
						// 
						// Substitution will result in more than two registers in an address operand, which can't be.
						// 

						//
						// lea r0, [r0 + r1]       nop
						// set [r0 + r0], 0, 8     set [r0*2 + r1*2], 0, 8
						//
					
						//
						// Build a set of register needed for resulting address
						// 
						struct RegisterAndMultiplier {
							Register reg;
							umm multiplier;
						};

						StaticList<RegisterAndMultiplier, 4> needed_registers;
						
						auto add_register = [&](Register new_reg, umm new_multiplier) {
							for (auto &[reg, multiplier] : needed_registers) {
								if (reg == new_reg) {
									multiplier *= new_multiplier;
									return;
								}
							}

							needed_registers.add({new_reg, new_multiplier});
						};
						auto add_original_address = [&] (umm multiplier) {
							if (orig_address.base) {
								add_register(orig_address.base.value(), multiplier);
							}
							if (orig_address.element_size) {
								add_register(orig_address.element_index, multiplier * orig_address.element_size);
							}
						};

						if (a.base) {
							if (a.base.value() == repl) {
								add_original_address(1);
							} else {
								add_register(a.base.value(), 1);
							}
						}
						if (a.element_size) {
							if (a.element_index == repl) {
								add_original_address(a.element_size);
							} else {
								add_register(a.element_index, a.element_size);
							}
						}

						// Remove zero multipliers
						for (umm k = 0; k < needed_registers.count;) {
							if (needed_registers[k].multiplier == 0) {
								needed_registers.erase_at_unordered(k);
							} else if (needed_registers[k].multiplier >= 256) {
								return false;
							} else {
								++k;
							}
						}

						if (needed_registers.count == 0) {
							// no registers needed, just the offset of original and from the read
							substitutions.add(Address {
								.offset = orig_address.offset + a.offset,	
							});
						} else if (needed_registers.count == 1) {
							if (needed_registers[0].multiplier == 1) {
								substitutions.add(Address {
									.base = needed_registers[0].reg,
									.offset = orig_address.offset + a.offset,	
								});
							} else {
								substitutions.add(Address {
									.element_index = needed_registers[0].reg,
									.element_size = (u8)needed_registers[0].multiplier,
									.offset = orig_address.offset + a.offset,	
								});
							}
						} else if (needed_registers.count == 2) {
							// Sort so [0] has lowest multiplier
							if (needed_registers[0].multiplier > needed_registers[1].multiplier)
								Swap(needed_registers[0], needed_registers[1]);

							// can't have both register have 1+ multiplier
							if (needed_registers[0].multiplier != 1) {
								return false;
							}
							
							substitutions.add(Address {
								.base = needed_registers[0].reg,
								.element_index = needed_registers[1].reg,
								.element_size = (u8)needed_registers[1].multiplier,
								.offset = orig_address.offset + a.offset,	
							});
						} else {
							// can't have more that two register in an address
							return false;
						}

						return true;
					};

					// Check that reads can be substituted.
					// NOTE: These three loops are in specific order, which should be matched in substitution step!
					for (auto read : reads_from_repl) {
						if (read->is_register()) {
							// Can't substitute a read from register with an address.
							goto cancel;
						} else {
							if (!substitute_address(read->get_address()))
								goto cancel;
						}
					}
					for (auto read : reads_from_repl_via_address) {
						if (!substitute_address(*read))
							goto cancel;
					}
					for (auto read : reads_from_repl_via_writes_to_address) {
						if (read->is_register()) {
							// Can't substitute a read from register with an address.
							goto cancel;
						} else {
							if (!substitute_address(read->get_address()))
								goto cancel;
						}
					}

					assert(substitutions.count == reads_from_repl.count + reads_from_repl_via_address.count + reads_from_repl_via_writes_to_address.count);


					// 
					// All reads may be substituted, do the thing
					//
				
					// NOTE: These three loops are in specific order, which should be matched in step above!
					umm substitution_index = 0;
					for (auto read : reads_from_repl) {
						*read = substitutions[substitution_index++];
					}
					for (auto read : reads_from_repl_via_address) {
						*read = substitutions[substitution_index++];
					}
					for (auto read : reads_from_repl_via_writes_to_address) {
						*read = substitutions[substitution_index++];
					}

					// Replace the initial `lea` with noop.
					result.instructions[i] = {
						.kind = InstructionKind::nop, 
						.file = __FILE_NAME__, 
						.line = __LINE__, 
						.source_location = result.instructions[i].source_location, 
					};
					something_was_optimized = true;
				}
			}

		cancel:;
		}
	}

	if (something_was_optimized) {
		immediate_reporter.warning("{} repetitions is not enough to fully optimize", max_repetitions);
	}

	//
	// Remove noops by packing, patch indices
	//
	result.old_to_new.resize(result.instructions.count);
	result.new_to_old.resize(result.instructions.count);
	
	//             01234567
	// current:    abNNcdNN
	// want:       abcd
	// old_to_new: 01222344
	// new_to_old: 0145
	
	// Pack
	umm dest_index = 0;
	for (umm i = 0; i < result.instructions.count; ++i) {
		result.old_to_new[i] = dest_index;
		if (result.instructions[i].kind != InstructionKind::nop) {
			result.instructions[dest_index] = result.instructions[i];
			result.new_to_old[dest_index] = i;
			dest_index += 1;
		}
	}
	result.instructions.count = dest_index;
	result.new_to_old.count = dest_index;

	// Patch
	for (umm i = 0; i < result.instructions.count; ++i) {
		auto patch_delta = [&](umm delta) {
			auto old_i = result.new_to_old[i];
			auto old_target = old_i + delta;
			auto new_target = result.old_to_new[old_target];
			auto new_delta = new_target - i;
			return new_delta;
		};

		switch (result.instructions[i].kind) {
			using enum InstructionKind;
			case jmp: result.instructions[i].jmp().d = patch_delta(result.instructions[i].jmp().d); break;
			case jt:  result.instructions[i].jt().d  = patch_delta(result.instructions[i].jt().d ); break;
			case jf:  result.instructions[i].jf().d  = patch_delta(result.instructions[i].jf().d ); break;
		}
	}

	return result;
}

}
