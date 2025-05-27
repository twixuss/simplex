#define TL_IMPL
#include "common.h"
#include "targets\x64\x64.h"
#include <tl/main.h>
#include <tl/process.h>
#include <tl/variant.h>
#include <tl/contiguous_hash_map.h>
#include <tl/bucket_hash_map.h>
#include <tl/dynamic_lib.h>

#include "x.h"
#include "reporter.h"
#include "token.h"
#include "escape.h"
#include "lexer.h"
#include "parser.h"
#include "nodes.h"
#include "binary_operation.h"
#include "capitalized.h"
#include "builtin_structs.h"
#include "make_node.h"
#include "node_interpreter.h"
#include "bytecode/builder.h"
#include "bytecode/interpreter.h"
#include "typechecker.h"
#include "visit.h"
#include "backend.h"
#include "compiler_context.h"
#include "c2simplex.h"
#include "print_ast.h"
#include "cmd_args.h"

CompilerContext _context, *context = &_context;

#define ENABLE_STRING_HASH_COUNT 0

forceinline constexpr u64 read_u64(utf8 *data) {
	if (std::is_constant_evaluated()) {
		return 
			((u64)data[0] << (0*8)) | 
			((u64)data[1] << (1*8)) |
			((u64)data[2] << (2*8)) |
			((u64)data[3] << (3*8)) |
			((u64)data[4] << (4*8)) |
			((u64)data[5] << (5*8)) |
			((u64)data[6] << (6*8)) |
			((u64)data[7] << (7*8));
	} else {
		return *(u64 *)data;
	}
}

#if ENABLE_STRING_HASH_COUNT
u32 string_hash_count;
#endif

template <>
constexpr u64 get_hash(String const &string) {
#if ENABLE_STRING_HASH_COUNT
	if (!std::is_constant_evaluated())
		++string_hash_count;
#endif

	if (string.count >= 8) {
		u64 first = read_u64(string.data);
		u64 last = read_u64(string.end() - 8);
		return string.count * 462591913 + first * 315861 + last * 5737893;
	}

	tl::u64 result = 0xdeadc0debabeface;
	for (u64 i = 0; i < string.count; ++i) {
		result += string.data[i] * (i * 0xdeadc0debabeface + 1);
	}
	return result;
}

/*
struct GlobalAllocator : AllocatorBase<GlobalAllocator> {

	static void init() {
		base = (u8 *)VirtualAlloc(0, buffer_size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
		assert(base);
		cursor = base;
	}

	static AllocationResult allocate_impl(umm size, umm alignment TL_LP) {
		scoped(lock);

		assert(size <= buffer_size);

		auto target = ceil(cursor, alignment);
		cursor = target + size;
		if (cursor > base + buffer_size) {
			// No memory left. Allocate new block.
			base = (u8 *)VirtualAlloc(0, buffer_size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
			assert(base);
			cursor = base;
			target = base;
		}
		return AllocationResult { .data = target, .count = size, .is_zeroed = true };
	}
	static AllocationResult reallocate_impl(void *old_data, umm old_size, umm new_size, umm alignment TL_LP) {
		tl::lock(lock);

		if (cursor == (u8 *)old_data + old_size && (u8 *)old_data + new_size < base + buffer_size) {
			// This allocation is at the end of the buffer and it can be extended.
			cursor = (u8 *)old_data + new_size;
			
			tl::unlock(lock);

			return { .data = old_data, .count = new_size, .is_zeroed = true };
		} else {
			// This allocation is in the middle of the buffer or it can't be extended. We have to allocate new memory.

			tl::unlock(lock);

			auto new_data = allocate_impl(new_size, alignment);
			memcpy(new_data.data, old_data, old_size);
			
			return new_data;
		}
	}
	static void deallocate_impl(void *data, umm size, umm alignment TL_LP) {}

	static GlobalAllocator current() { return {}; }

private:

	inline static constexpr umm buffer_size = 1 * MiB;
	inline static u8 *base = 0;
	inline static u8 *cursor = 0;
	inline static SpinLock lock;
};
*/

void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, Span<char> message) {
	scoped(context_base->stdout_mutex);

	println("Call stack:");
	println(resolve_names(get_call_stack().skip(1).skip(-7)));

	if (!location.data)
		location = debug_current_location;

	immediate_reporter.error(debug_current_location, "{} {} at {}:{} in function {}\n{}", cause_string, expression, file, line, function, message);
}

template <int byte_count>
auto chars_as_int(utf8 const *chars) {
	using Int = UintWithBits<byte_count * 8>;
	Int result = *(Int *)chars;
	result &= ((1ull<<(8*byte_count))-1);
	return result;
}

void Block::add(Node *child) {
	children.add(child);
	switch (child->kind) {
		case NodeKind::Block: {
			auto block = (Block *)child;
			block->parent = this;
			break;
		}
		case NodeKind::Definition: {
			auto definition = (Definition *)child;
			definition_list.add(definition);
			definition_map.get_or_insert(definition->name).add(definition);
			break;
		}
	}
}

bool is_expression(Node *node) {
	if (auto block = as<Block>(node)) {
		if (block->children.count == 0)
			return false;
		return is_expression(block->children.back());
	}

	return as<Expression>(node);
}

std::tuple<Lambda *, Definition *> find_main_lambda() {
	for (auto node : context->global_block.unprotected.children) {
		if (auto definition = as<Definition>(node)) {
			if (definition->name == u8"main"s) {
				if (!definition->initial_value) {
					immediate_reporter.error(definition->location, "main must be a lambda");
					return {};
				}

				if (definition->mutability != Mutability::constant) {
					immediate_reporter.error(definition->location, "main must be constant");
					return {};
				}

				auto lambda = as<Lambda>(definition->initial_value);
				if (!lambda) {
					immediate_reporter.error(definition->location, "main must be a lambda");
					return {};
				}

				if (!types_match(lambda->head.return_type, get_builtin_type(BuiltinType::None)) &&
					!::is_concrete_integer(lambda->head.return_type)) 
				{
					immediate_reporter.error(definition->location, "main must return integer or None, not {}.", lambda->head.return_type);
					return {};
				}

				return {lambda, definition};
			}
		}
	}
	return {};
}

String target_string = u8"bytecode"s;

GList<String> arguments_for_backend;

bool find_main_and_run() {
	auto [lambda, definition] = find_main_lambda();
	
	if (!lambda) {
		immediate_reporter.error("Main lambda was not found.");
		immediate_reporter.help(R"(Create main lambda:
fn main() {
	return 0;
}
)");
		return false;
	}

	auto call = Call::create();
	call->callable = lambda;
	call->type = lambda->head.return_type;
	call->call_kind = CallKind::lambda;
	if (target_string == "ast") {
		if (context_base->run_compiled_code) {
			auto context = NodeInterpreter::create(call);
			auto result = context->run();
			if (result.is_value()) {
				println("main returned {}", result.value());
			} else {
				with(ConsoleColor::red, println("main failed to execute"));
			}
		}
	} else {
		auto generate_bytecode = [&] {
			dbgln("\nBytecode:\n");
			Bytecode::Builder builder;

			auto target_platform = Bytecode::Interpreter::target_platform();

			builder.target_platform = &target_platform;

			for (auto definition : context->global_block.unprotected.definition_list) {
				builder.append_global_definition(definition);
			}
			visit(&context->global_block.unprotected, Combine {
				[&] (auto) {},
				[&] (Lambda *lambda) {
					if (lambda->body && !lambda->head.is_template) {
						builder.append_lambda(lambda);
					}
				},
			});
			auto bytecode = builder.build(call);
			if (context_base->is_debugging) {
				println("\nFinal instructions:\n");
				print_instructions(bytecode.instructions);
			}

			return bytecode;
		};

		if (target_string == "bytecode") {
			auto bytecode = generate_bytecode();
			if (context_base->run_compiled_code) {
				timed_block("executing main");
				auto result = Bytecode::Interpreter{}.run(&bytecode, bytecode.entry_point_instruction_index, context_base->run_interactive);
				if (!result)
					return false;
				println("main returned {}", result.value());
			}
		} else {
			auto dll_path = tformat(u8"{}\\targets\\{}.dll", context_base->compiler_bin_directory, target_string);
			auto dll = load_dll(dll_path);
			debug_add_module(dll.handle, (Span<char>)dll_path);
			
			#define x(ret, name, decls, defns, args) ret (*name) defns = autocast get_symbol(dll, u8###name##s); 
			ENUMERATE_BACKEND_API(x)
			#undef x
			
			#define ENSURE(name)                                                                                                    \
				if (!name) {                                                                                                        \
					immediate_reporter.error("Backend '{}' does not contain required function '{}'.", target_string, u8###name##s); \
				}


			ENSURE(init);
			init(context, arguments_for_backend);

			if (convert_bytecode) {
				auto bytecode = generate_bytecode();
				if (!convert_bytecode(bytecode)) {
					return false;
				}
			} else if (convert_ast) {
				if (!convert_ast(&context->global_block.unprotected, lambda, definition)) {
					return false;
				}
			} else {
				immediate_reporter.error("Backend '{}' does not contain required function 'convert_bytecode' or 'convert_ast'.", target_string);
			}

			if (context_base->run_compiled_code) {
				ENSURE(run);
				run();
			}

			#undef ENSURE
		}
	}
	return true;
}

CmdArg args_handlers[] = {
	{"-threads",                   +[](u64 number){ context_base->requested_thread_count = (u32)number; }},
	{"-nested-reports-verbosity",  +[](u64 number) { context_base->nested_reports_verbosity = number; }},
	{"-print-tokens",              +[] { context_base->print_tokens = true; }},
	{"-print-ast",                 +[] { context_base->should_print_ast = true; }},
	{"-print-uids",                +[] { context_base->print_uids = true; }},
	{"-no-constant-name-inlining", +[] { context_base->constant_name_inlining = false; }},
	{"-report-yields",             +[] { context_base->report_yields = true; }},
	{"-log-time",                  +[] { context_base->enable_time_log = true; }},
	{"-debug",                     +[] { context_base->is_debugging = true; }},
	{"-print-wait-failures",       +[] { context_base->print_wait_failures = true; }},
	{"-log-error-path",            +[] { context_base->enable_log_error_path = true; }},
	{"-run",                       +[] { context_base->run_compiled_code = true; }},
	{"-stats",                     +[] { context_base->print_stats = true; }},
	{"-interactive",               +[] { context_base->run_interactive = true; }},
	{"-auto-inline",               +[] { context_base->should_inline_unspecified_lambdas = true; }},
	{"-keep-build-artifacts",      +[] { context_base->keep_build_artifacts = true; }},
	{"-limit-time", +[] {
		create_thread([] {
			int seconds_limit = 10;
			sleep_milliseconds(seconds_limit * 1000);
			immediate_reporter.error("Time limit of {} seconds exceeded.", seconds_limit);
			exit(-1);
		});
	}},
	{"-target", +[](String target) { target_string = target; }},
};

bool parse_arguments(Span<Span<utf8>> args) {


	for (umm i = 1; i < args.count; ++i) {

		for (auto handler : args_handlers) {
			auto cmd = args[i];
			if (args[i] == handler.key) {
				handler.run.visit(Combine {
					[&](void (*run)()) {
						run();
					},
					[&](void (*run)(u64 x)) {
						if (++i < args.count) {
							if (auto number = parse_u64(args[i])) {
								run(number.value());
							} else {
								immediate_reporter.error("Could not parse number after {}. Ignoring.", cmd);
							}
						} else {
							immediate_reporter.error("Expected a number after {}.", cmd);
						}
					},
					[&](void (*run)(String x)) {
						if (++i < args.count) {
							run(args[i]);
						} else {
							immediate_reporter.error("Expected a string after {}.", cmd);
						}
					},
				});
				goto next_arg;
			}
		}
		if (args[i][0] == '-') {
			if (starts_with(args[i], u8"-target-"s)) {
				arguments_for_backend.add(args[i].skip(7));
			} else {
				immediate_reporter.warning("Unknown command line parameter: {}", args[i]);
			}
		} else {
			if (context_base->input_source_path.count) {
				with(ConsoleColor::red, println("No multiple input files allowed"));
				return {};
			} else {
				context_base->input_source_path = normalize_path(make_absolute_path(args[i]));
			}
		}
	next_arg:;
	}

	if (!context_base->input_source_path.count) {
		with(ConsoleColor::red, println("No input file was specified"));
		return false;
	} 

	return true;
}

void init_builtin_types() {
	// Disable checks, as the types are not ready yet.
	scoped_replace(context_base->check_that_types_are_types, false);

	context->global_block.unprotected.type = get_builtin_type(BuiltinType::None);

	#define x(name) \
		{ \
			auto type = get_builtin_type(BuiltinType::name); \
			type->type_kind = BuiltinType::name; \
			type->type = get_builtin_type(BuiltinType::Type); \
		}
	ENUMERATE_BUILTIN_TYPES(x)
	#undef x
		
	/* String */ {
		auto s = Struct::create();
		auto d = Definition::create();

		auto data = Definition::create();
		data->name = u8"data"s;
		data->mutability = Mutability::variable;
		data->offset = 0;
		data->type = make_pointer(get_builtin_type(BuiltinType::U8), Mutability::variable);
		data->container = s;
		s->members.add(data);

		auto count = Definition::create();
		count->name = u8"count"s;
		count->mutability = Mutability::variable;
		count->offset = 8;
		count->type = get_builtin_type(BuiltinType::U64);
		count->container = s;
		s->members.add(count);

		s->definition = d;
		s->is_template = false;
		s->size = 16;
		s->type = get_builtin_type(BuiltinType::Type);

		d->initial_value = s;
		d->constant_value = Value((Type)s);
		d->mutability = Mutability::constant;
		d->name = u8"String"s;
		d->type = s->type;

		context->builtin_structs.String = s;

		context->global_block.unprotected.add(d);
	}

	/* Range */ {
		auto s = Struct::create();
		auto d = Definition::create();

		auto begin = Definition::create();
		begin->name = u8"begin"s;
		begin->mutability = Mutability::variable;
		begin->offset = 0;
		begin->type = get_builtin_type(BuiltinType::S64);
		begin->container = s;
		s->members.add(begin);

		auto end = Definition::create();
		end->name = u8"end"s;
		end->mutability = Mutability::variable;
		end->offset = 8;
		end->type = get_builtin_type(BuiltinType::S64);
		end->container = s;
		s->members.add(end);

		s->definition = d;
		s->is_template = false;
		s->size = 16;
		s->type = get_builtin_type(BuiltinType::Type);

		d->initial_value = s;
		d->constant_value = Value((Type)s);
		d->mutability = Mutability::constant;
		d->name = u8"Range"s;
		d->type = s->type;

		context->builtin_structs.Range = s;

		context->global_block.unprotected.add(d);
	}
}

#if 0
#include "c_parser.h"

struct C2Simplex {
	List<Span<utf8>> include_directories = to_list({
		u8"C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.22621.0\\shared"s,
		u8"C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.22621.0\\um"s,
		u8"D:\\Programs\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.40.33807\\include"s,
	});


	Span<utf8> find_in_include_directories(Span<utf8> path) {
		Span<utf8> full_path;
		for (auto directory : include_directories) {
			auto checkpoint = current_temporary_allocator.checkpoint();
			full_path = tformat(u8"{}\\{}"s, directory, path);
			if (file_exists(full_path)) {
				break;
			}
			current_temporary_allocator.reset(checkpoint);
			full_path = {};
		}
		return full_path;
	}
	void process_c_header(Span<utf8> full_path) {
		auto source_buffer = read_entire_file(full_path);
		defer { free(source_buffer); };
		auto source = as_utf8(source_buffer);
		c_parser::Preprocessor preprocessor;
		auto result = preprocessor.preprocess_source(source, c_parser::PreprocessSourceOptions{
			.on_parsed_define = [&](c_parser::Macro macro) {
				println("#define \"{}\" \"{}\"", macro.name, macro.value);
			},
			.on_parsed_include = [&](Span<utf8> path, c_parser::IncludeForm form) {
				Span<utf8> full_path_to_include;
				switch (form) {
					case c_parser::IncludeForm::angle_bracket: {
						full_path_to_include = find_in_include_directories(path);
						break;
					}
					case c_parser::IncludeForm::quoted: {
						auto checkpoint = current_temporary_allocator.checkpoint();
						full_path_to_include = tformat(u8"{}\\{}", parse_path(full_path).directory, path);
						if (file_exists(full_path_to_include)) {
							break;
						}
						current_temporary_allocator.reset(checkpoint);
						full_path_to_include = {};
						break;
					}
				}
				if (full_path_to_include.count == 0) {
					println("Could not find file to include: {}", path);
					exit(1);
				}

				println("==== Including {} ====", full_path_to_include);
				process_c_header(full_path_to_include);
				println("==== End {} ====", full_path_to_include);
			},
		});

		println(result);
	}
};

s32 tl_main(Span<Span<utf8>> args) {
	C2Simplex converter;
	converter.process_c_header(converter.find_in_include_directories(u8"Windows.h"s));

	return 0;
}
#endif

s32 tl_main(Span<Span<utf8>> args) {
	debug_init();

	set_console_encoding(Encoding::utf8);
	
	/*
	c2simplex(u8R"(
#define X1 1
#ifdef X0
	#define A 2
#elifdef X0
	#define B 3
#elifdef X1
	#define C 4
#elifdef X1
	#define D 5
#else
	#define E 6
#endif
)"s);
	return 1;
	*/

	defer {
		if (context_base->enable_time_log) {
			for (auto time : context_base->timed_results) {
				println("{} took {} ms", time.name, time.seconds * 1000);
			}
		}

		if (context_base->print_stats) {
			println("Fiber allocations: {}", get_allocated_fiber_count());
		}

#if ENABLE_STRING_HASH_COUNT
		println("Total string hashes: {}", string_hash_count);
#endif

#if ENABLE_NOTE_LEAK
		println("\nLEAKS:");
		for (auto leak : leaks) {
			println(leak);
		}
#endif
	};

	node_arena = AtomicArenaAllocator::create(1*MiB);

	init_builtin_types();
	Typechecker::init_binary_typecheckers();
	
	timed_function();

	context_base->compiler_path = args[0];
	context_base->compiler_bin_directory = make_absolute_path(parse_path(context_base->compiler_path).directory);
	context_base->compiler_root_directory = format(u8"{}\\..", context_base->compiler_bin_directory);
	context_base->generated_source_directory = format(u8"{}\\generated", context_base->compiler_root_directory);

	for_each_file(context_base->generated_source_directory, {}, [&](String path) {
		return ForEach_erase;
	});

	if (!parse_arguments(args)) {
		immediate_reporter.error("Failed to parse arguments.");
		return 1;
	}
	
	auto cpu_info = get_cpu_info();

	u32 thread_count;
	if (context_base->requested_thread_count == 0) {
		thread_count = cpu_info.logical_processor_count;
	} else {
		thread_count = min(context_base->requested_thread_count, cpu_info.logical_processor_count);
	}

	
	TaskQueueThreadPool thread_pool;
	thread_pool.init(thread_count - 1);
	defer { thread_pool.deinit(); };

	imports.unprotected.add_file({.path = context_base->input_source_path, .location = {}});
	imports.unprotected.add_file({.path = normalize_path(make_absolute_path(format(u8"{}\\import\\base.sp", context_base->compiler_root_directory))), .location = {}});

	static bool failed = false;

	while (1) {
		while (1) {
			auto popped = locked_use(imports) { return imports.files_to_import.pop(); };
			if (!popped) {
				break;
			}

			thread_pool += [to_parse = popped.value()] {
				bool success = read_file_and_parse_into_global_block(to_parse.location, to_parse.path);
				atomic_or(&failed, !success);
			};
		}
	
		thread_pool.wait_for_completion(WaitForCompletionOption::do_my_task);

		if (imports.unprotected.files_to_import.count == 0) {
			break;
		}
	}
	
	defer {
		if (context_base->should_print_ast) {
			print_ast(&context->global_block.unprotected);
			if (debug_node_to_print) {
				println();
				println("Debug Node:");
				println();
				print_ast(debug_node_to_print);
			}
		}
	};

	if (failed) {
		LOG_ERROR_PATH("Parsing failed");
		return 1;
	}

	{
		timed_block("typecheck");


		failed = false;

		for (auto node : context->global_block.unprotected.children) {
			typecheck_entries.add({.node = node});
		}


		u32 round_index = 0;
		while (true) {
			auto initial_progress = get_typechecking_progress();

			static auto perform_typechecking = [] (TypecheckEntry &entry) {
				entry.status = TypecheckEntryStatus::unfinished;

				if (!entry.typechecker) {
					entry.typechecker = Typechecker::create(entry.node);
				}

				auto result = entry.typechecker->continue_typechecking(&entry);

				entry.typechecker->stop();

				if (result != Typechecker::YieldResult::wait) {
					entry.typechecker->retire();

					if (result == Typechecker::YieldResult::fail) {
						entry.status = TypecheckEntryStatus::failed;
						failed = true;
					} else {
						entry.status = TypecheckEntryStatus::succeeded;
					}
				}
			};

			for (auto &entry : typecheck_entries) {
				if (entry.status == TypecheckEntryStatus::unfinished || entry.status == TypecheckEntryStatus::unstarted) {
					thread_pool += [&entry] {
						perform_typechecking(entry);
					};
				}
			}

			thread_pool.wait_for_completion(WaitForCompletionOption::do_my_task);

			auto current_progress = get_typechecking_progress();
			assert(current_progress >= initial_progress);

			if (current_progress == initial_progress) {

				// Inform all typecheckers that there's no more progress
				// and that they should report whatever they couldn't wait for.

				no_more_progress = true;

				for (auto &entry : typecheck_entries) {
					if (entry.status == TypecheckEntryStatus::unfinished) {
						thread_pool += [&entry] {
							perform_typechecking(entry);
							assert(entry.status != TypecheckEntryStatus::unfinished);
						};
					}
				}

				thread_pool.wait_for_completion(WaitForCompletionOption::do_my_task);

				break;
			}
		}


		for (auto &entry : typecheck_entries) {
			assert(entry.status != TypecheckEntryStatus::unfinished);
		}

		auto find_cyclic_dependencies = [&] {
			enum class VertexState : u8 {
				none,
				visited,
				finished,
			};
			struct Vertex {
				VertexState state = {};
				TypecheckEntry *parent = 0;
				List<TypecheckEntry *> pointees;
			};

			HashMap<TypecheckEntry *, Vertex> vertices;

			auto add_edge = [&] (TypecheckEntry *from, TypecheckEntry *to) {
				vertices.get_or_insert(from).pointees.add(to);
			};

			for (auto &dependency : typecheck_entries) {
				List<TypecheckEntry *> dependants;
				for (auto &entry : typecheck_entries) {
					if (entry.dependency == &dependency)
						dependants.add(&entry);
				}
				for (auto &dependant : dependants) {
					add_edge(dependant, &dependency);
				}
			}

			List<TypecheckEntry *> cycle;
			List<List<TypecheckEntry *>> cycles;

			auto dfs = [&](this auto &&self, TypecheckEntry *u) -> void {
				vertices.get_or_insert(u).state = VertexState::visited;
				for (auto v : vertices.get_or_insert(u).pointees) {
					switch (vertices.get_or_insert(v).state) {
						case VertexState::none: {
							vertices.get_or_insert(v).parent = u;
							self(v);
							break;
						}
						case VertexState::visited: {
							// cycle found, backtrack to find vertices in cycle
							auto p = u;
							cycle.add(v);
							while (p != v) {
								cycle.add(p);
								p = vertices.get_or_insert(p).parent;
							}
							reverse_in_place(cycle); // reverse to get correct order
							cycles.add(cycle);
							cycle.clear();
							break;
						}
					}
				}
				vertices.get_or_insert(u).state = VertexState::finished;
			};

			for (auto &entry : typecheck_entries) {
				if (vertices.get_or_insert(&entry).state == VertexState::none) {
					dfs(&entry);
				}
			}

			return cycles;
		};

		auto cyclic_dependencies = find_cyclic_dependencies();
		if (cyclic_dependencies.count) {
			immediate_reporter.error("Cyclic dependencies detected.");
			for (umm i = 0; i < cyclic_dependencies.count; ++i) {
				immediate_reporter.warning("Cycle #{}:", i);
				auto &cycle = cyclic_dependencies[i];

				bool all_are_lambdas = true;
				Lambda *lambda_with_no_return_type = 0;
				for (umm j = 0; j < cycle.count; ++j) {
					auto &entry = *cycle[j];

					if (auto definition = as<Definition>(entry.node); definition && definition->initial_value) {
						if (auto lambda = as<Lambda>(definition->initial_value)) {
							if (!lambda->head.return_type) {
								lambda_with_no_return_type = lambda;
							}
						} else {
							all_are_lambdas = false;
						}
					} else {
						all_are_lambdas = false;
					}
				}

				if (all_are_lambdas && lambda_with_no_return_type) {
					immediate_reporter.help(lambda_with_no_return_type->location, "All nodes in this cycle are lambdas. Recursive functions must have explicit return types. This lambda does not have one.");
				}


				for (umm j = 0; j < cycle.count; ++j) {
					auto &entry = *cycle[j];
					auto &next_entry = *cycle[(j + 1) % cycle.count];

					immediate_reporter.info(entry.node->location, "{} depends on {}.", entry.node->location, next_entry.node->location);
				}
			}
		}

		for (auto &report : deferred_reports.unprotected) {
			report.print();
		}

		if (failed) {
			LOG_ERROR_PATH("Typechecking failed.");
		}
	}

	if (failed) {
		return 1;
	}

	if (!find_main_and_run()) {
		with(ConsoleColor::red, println("Build failed"));
		return 1;
	}

	with(ConsoleColor::green, println("Build success"));

	return 0;
}