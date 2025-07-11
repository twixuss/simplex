#define TL_IMPL
#include <tl/main.h>
#include <tl/file.h>
#include <tl/process.h>
#include <tl/thread.h>
#include <tl/cpu.h>
#include <tl/precise_time.h>
#include <conio.h>
#include <algorithm>

using namespace tl;

using String = Span<utf8>;

List<utf8> unescape_string(Span<utf8> string) {
	List<utf8> result;
	for (umm i = 0; i < string.count; ++i) {
		if (string[i] == '\\') {
			++i;
			switch (string[i]) {
				case 'n': result.add('\n'); break;
				case 't': result.add('\t'); break;
				case 'r': result.add('\r'); break;
				default: result.add('\\'); result.add(string[i]); break;
			}
		} else {
			result.add(string[i]);
		}
	}
	return result;
}

struct RanProcess {
	u32 exit_code = {};
	List<utf8> output = {};
	bool timed_out = {};
};

void free(RanProcess &p) {
	free(p.output);
}

SpinLock stdout_lock;

bool show_box = true;

RanProcess run_process(String command, String working_directory = {}) {
	auto last_error_mode = GetErrorMode();
	if (!show_box)
		SetErrorMode(SEM_NOGPFAULTERRORBOX);
	defer { SetErrorMode(last_error_mode); };

	Process process;
	if (working_directory) {
		scoped(stdout_lock);
		auto old_current = get_current_directory();
		set_current_directory(working_directory);
		process = start_process(to_utf16(command));
		set_current_directory(old_current);
	} else {
		process = start_process(to_utf16(command));
	}

	assert(is_valid(process));
	
	u8 buf[256];

	StringBuilder output_builder;
	defer { free(output_builder); };

	while (1) {
		auto bytes_read = process.standard_out->read(array_as_span(buf));
		if (!bytes_read)
			break;
		append(output_builder, Span((utf8 *)buf, bytes_read));
	}
	
	bool timed_out = !wait(process, 5000);

	if (timed_out) {
		terminate(process);
	}

	RanProcess result {
		.exit_code = get_exit_code(process),
		.output = (List<utf8>)to_string(output_builder),
		.timed_out = timed_out,
	};

	return result;
}

String current_directory;

String make_relative(String path) {
	if (starts_with(path, current_directory)) {
		return path.skip(current_directory.count + 1);
	} else {
		return path;
	}
}

s32 tl_main(Span<String> arguments) {
	auto executable_path = get_executable_path();
	auto executable_directory = parse_path(executable_path).directory;
	auto compiler_path = format(u8"{}\\simplex.exe", executable_directory);

	auto root_directory = normalize_path(format(u8"{}\\..", executable_directory), '\\');

	auto test_directory = format(u8"{}\\tests", root_directory);
	auto examples_directory = format(u8"{}\\examples", root_directory);

	current_directory = get_current_directory();

	struct TestToRun {
		String path;
		bool run;
	};

	StringBuilder extra_options_builder;
	List<TestToRun> tests_to_run;
	bool all = true;
	bool do_coverage = false;
	bool loop_until_failure = false;
	enum class State {
		adding_tests,
		adding_args,
	};

	State state = State::adding_tests;

	for (int i = 1; i < arguments.count; ++i) {
		switch (state) {
			case State::adding_tests: {
				if (arguments[i] == u8"nobox"s) {
					show_box = false;
				} else if (arguments[i] == u8"coverage"s) {
					do_coverage = true;
				} else if (arguments[i] == u8"testloop"s) {
					loop_until_failure = true;
				} else if (arguments[i] == u8"--"s) {
					state = State::adding_args;
				} else if (arguments[i] != u8"all"s) {
					all = false;
					tests_to_run.add({.path = arguments[i]});
				}
				break;
			}
			case State::adding_args: {
				append_format(extra_options_builder, "{} ", arguments[i]);
				break;
			}
		}
	}
	auto extra_options = to_string(extra_options_builder);

	if (all) {
		bool run = false;
		auto add_tests_from_directory = [&] (this auto &&self, String directory) -> void {
			for (auto item : get_items_in_directory(directory)) {
				if (item.kind == FileItem_file) {
					if (ends_with(item.name, u8".sp"s)) {
						tests_to_run.add({.path = format(u8"{}\\{}"s, directory, item.name), .run = run});
					}
				} else if (item.kind == FileItem_directory) {
					self(format(u8"{}\\{}"s, directory, item.name));
				}
			}
		};

		run = true;
		add_tests_from_directory(test_directory);
		run = false;
		add_tests_from_directory(examples_directory);
	}

	List<String> extra_options_sets;
	extra_options_sets.add(u8"-target c"s);
	extra_options_sets.add(u8"-target bytecode"s);

	TaskQueueThreadPool thread_pool;
	thread_pool.init(get_cpu_info().logical_processor_count - 1);
	defer { thread_pool.deinit(); };

	u32 testloop_index = 0;
	
	delete_directory(tformat(u8"{}\\tmp\\tests", root_directory));

reloop:
	
	u32 n_failed = 0;
	u32 n_succeeded = 0;
	u32 test_index = 0;

	for (auto option_set : extra_options_sets) {
		println("==========================");
		println("= Option set: \"{}\"", option_set);
		println("==========================");

		n_failed = 0;
		n_succeeded = 0;

		for (auto test : tests_to_run) {
			thread_pool += [=, &n_failed, &n_succeeded, test_index = test_index++] {
				bool fail = false;
				defer {
					atomic_add(&n_failed, fail);
					atomic_add(&n_succeeded, !fail);
				};

				auto test_filename = parse_path(test.path).name_and_extension();

				auto do_fail = [&](auto fn) {
					fail = true;
					withs(stdout_lock) {
						with(ConsoleColor::yellow, print("    Test {} failed:\n", test.path));
						fn();
					};
				};

				auto test_source_buffer = read_entire_file(test.path);
				defer { free(test_source_buffer); };

				auto test_source = (String)test_source_buffer;


				bool compiler_should_error = find((String)test_source, u8"// COMPILER ERROR"s);

				auto find_param = [&](String param_prefix) -> String {
					if (auto found = find(test_source, param_prefix)) {
						auto param_start = found + param_prefix.count;
						return unescape_string({param_start, find_any(String{param_start, test_source.end()}, as_span({u8'\r', u8'\n'}))});
					}
					return {};
				};

				auto find_all_params = [&](String param_prefix) -> List<String> {
					List<String> result;
					find_all(test_source, param_prefix, [&] (String prefix) {
						result.add(unescape_string({prefix.end(), find_any(String{prefix.end(), test_source.end()}, as_span({u8'\r', u8'\n'}))}));
					});
					return result;
				};

				List<String> expected_compiler_output = find_all_params(u8"// COMPILER OUTPUT "s);
				List<String> not_expected_compiler_output = find_all_params(u8"// NO COMPILER OUTPUT "s);

				not_expected_compiler_output.add(u8"Time limit of "s); // time limit exceeded
				String expected_program_output = find_param(u8"// PROGRAM OUTPUT "s);
				auto expected_program_exit_code = parse_u64(find_param(u8"// PROGRAM CODE "s));

				auto compile_command = format(u8"{} \"{}\" -limit-time {} {}"s, compiler_path, test.path, extra_options, option_set);
				if (test.run) {
					compile_command.add(u8" -run"s);
				}
			
				with(stdout_lock, println(test.path));
				//with(stdout_lock, println(compile_command));

				auto working_dir = tformat(u8"{}\\tmp\\tests\\{}", root_directory, test_index);
				create_directories(working_dir);

				if (do_coverage) {
					auto coverage_command = format(u8"opencppcoverage --sources {}\\src\\ --export_type=binary:coverage.cov -- {}"s, root_directory, compile_command);
					auto result = run_process(coverage_command, working_dir);
					defer { free(result); };
					withs(stdout_lock) {
						println(result.output);
					};
					return;
				}

				auto actual_compiler = run_process(compile_command, working_dir);
				defer { free(actual_compiler); };

				delete_file(tformat(u8"{}.exe", parse_path(test.path).path_without_extension()));
				delete_file(tformat(u8"{}.pdb", parse_path(test.path).path_without_extension()));

				if (actual_compiler.timed_out) {
					do_fail([&] {
						with(ConsoleColor::red, print("Compiler timed out\n"));
					});
					return;
				}

				List<String> expected_but_not_present_strings;
				if (expected_compiler_output.count) {
					for (auto expected_string : expected_compiler_output) {
						if (!find(actual_compiler.output, expected_string)) {
							expected_but_not_present_strings.add(expected_string);
						}
					}
				}
			
				List<String> unexpected_but_present_strings;
				if (not_expected_compiler_output.count) {
					for (auto not_expected_string : not_expected_compiler_output) {
						if (find(actual_compiler.output, not_expected_string)) {
							unexpected_but_present_strings.add(not_expected_string);
						}
					}
				}

				String return_code_message = {};
				if (compiler_should_error) {
					if (actual_compiler.exit_code == 0) {
						return_code_message = u8"Compiler should have failed and returned non-zero exit code"s;
					}
				} else {
					if (actual_compiler.exit_code != 0) {
						return_code_message = tformat(u8"Compiler should have succeeded and returned zero exit code, but got {}. Output:"s, actual_compiler.exit_code);
					}
				}

				if (expected_but_not_present_strings.count || unexpected_but_present_strings.count || return_code_message.count) {
					do_fail([&] {
						if (return_code_message.count) {
							with(ConsoleColor::red, println(return_code_message));
						}
					
						if (expected_but_not_present_strings.count || unexpected_but_present_strings.count) {
							with(ConsoleColor::red, println("Compiler output mismatch:"));
							if (expected_but_not_present_strings.count) {
								with(ConsoleColor::cyan, println("Expected but not present:"));
								for (auto expected_string : expected_but_not_present_strings) {
									println(expected_string);
								}
							}
							if (unexpected_but_present_strings.count) {
								with(ConsoleColor::cyan, println("Not expected but present:"));
								for (auto not_expected_string : unexpected_but_present_strings) {
									println(not_expected_string);
								}
							}
						}
						with(ConsoleColor::cyan, println("Actual output:"));
						umm output_byte_limit = 16*1024;
						if (actual_compiler.output.count > output_byte_limit) {
							println("*** MORE THAN {} BYTES PRINTED, OMITTING ***", output_byte_limit);
						} else {
							println(actual_compiler.output);
						}
					});
					return;
				}
			};
		}

		thread_pool.wait_for_completion(WaitForCompletionOption::do_any_task);

		if (n_failed) {
			with(ConsoleColor::red,   print("{}/{} tests failed.\n", n_failed, tests_to_run.count));
		} else {
			with(ConsoleColor::green, print("All {} tests succeeded.\n", tests_to_run.count));
		}
	}

	if (loop_until_failure) {
		if (!n_failed) {
			system("cls");
			println("# {}", ++testloop_index);
			goto reloop;
		}
	}

	if (do_coverage) {
		println("Merging coverage results...");
		StringBuilder builder;
		append_format(builder, "sources={}\\src\\\n", root_directory);
		append_format(builder, "sources={}\\backends\\\n", root_directory);
		for (u32 i = 0; i < test_index; ++i) {
			append_format(builder, "input_coverage={}\\tmp\\tests\\{}\\coverage.cov\n", root_directory, i);
		}
		write_entire_file(u8"coverage.config"s, builder);

		auto result = run_process(u8"opencppcoverage --config_file coverage.config"s);
		println(result.output);
	}

	return 0;
}

