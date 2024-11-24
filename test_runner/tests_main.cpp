#define TL_IMPL
#include <tl/main.h>
#include <tl/file.h>
#include <tl/process.h>
#include <tl/thread.h>
#include <tl/cpu.h>
#include <conio.h>
#include <algorithm>

using namespace tl;

using String = Span<utf8>;

List<utf8> normalize_path(String path) {
	auto dotdot = find(path, u8".."s);
	if (!dotdot || dotdot == path.begin())
		return to_list(path);

	auto X = find_last(Span(path.begin(), dotdot-1), u8'\\');

	List<utf8> result;
	result.add(String{(utf8 *)path.begin(), X});
	result.add(String{dotdot + 2, (utf8 *)path.end()});
	return result;
}

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
	String output = {};
	bool timed_out = {};
};

SpinLock stdout_lock;

bool show_box = true;

RanProcess run_process(String command) {
	auto last_error_mode = GetErrorMode();
	if (!show_box)
		SetErrorMode(SEM_NOGPFAULTERRORBOX);
	defer { SetErrorMode(last_error_mode); };

	auto process = start_process(to_utf16(command));
	assert(is_valid(process));
	
	u8 buf[256];

	StringBuilder output_builder;
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
		.output = as_utf8(to_string(output_builder)),
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

	auto project_directory = normalize_path(format(u8"{}\\..", executable_directory));

	auto test_directory = format(u8"{}\\..\\tests", executable_directory);

	current_directory = get_current_directory();

	StringBuilder extra_options_builder;
	List<String> test_filenames;
	bool all = true;
	bool do_coverage = false;
	bool loop_until_failure = false;
	for (int i = 1; i < arguments.count; ++i) {
		if (arguments[i] == u8"nobox"s) {
			show_box = false;
		} else if (arguments[i] == u8"coverage"s) {
			do_coverage = true;
		} else if (arguments[i] == u8"testloop"s) {
			loop_until_failure = true;
		} else if (arguments[i][0] == '-') {
			append_format(extra_options_builder, "{} ", arguments[i]);
		} else if (arguments[i] != u8"all"s) {
			all = false;
			test_filenames.add(arguments[i]);
		}
	}
	auto extra_options = to_string(extra_options_builder);

	if (all) {
		auto add_tests_from_directory = [&] (this auto &&self, String directory, String dirname = {}) -> void {
			for (auto item : get_items_in_directory(directory)) {
				if (item.kind == FileItem_file) {
					if (ends_with(item.name, u8".sp"s)) {
						if (dirname.count) {
							test_filenames.add(format(u8"{}\\{}"s, dirname, item.name));
						} else {
							test_filenames.add(item.name);
						}
					}
				} else if (item.kind == FileItem_directory) {
					if (dirname.count) {
						self(format(u8"{}\\{}"s, directory, item.name), format(u8"{}\\{}"s, dirname, item.name));
					} else {
						self(format(u8"{}\\{}"s, directory, item.name), item.name);
					}
				}
			}
		};

		add_tests_from_directory(test_directory);
	}

reloop:
	u32 n_failed = 0;
	u32 n_succeeded = 0;

	TaskQueueThreadPool thread_pool;
	thread_pool.init(get_cpu_info().logical_processor_count - 1);
	defer { thread_pool.deinit(); };

	u32 volatile test_counter = 0;

	for (auto test_filename : test_filenames) {
		thread_pool += [=, &n_failed, &n_succeeded, &test_counter] {
			auto test_index = atomic_add(&test_counter, 1);

			bool fail = false;
			defer {
				atomic_add(&n_failed, fail);
				atomic_add(&n_succeeded, !fail);
			};

			auto test_path = format(u8"{}\\{}", test_directory, test_filename);

			auto do_fail = [&](auto fn) {
				fail = true;
				withs(stdout_lock) {
					with(ConsoleColor::yellow, print("    Test {} failed:\n", test_filename));
					fn();
				};
			};

			auto test_source_buffer = read_entire_file(test_path);
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

			auto compile_command = format(u8"{} \"{}\" -limit-time {}"s, make_relative(compiler_path), make_relative(test_path), extra_options);
			
			with(stdout_lock, print("{}\n", test_filename));

			if (do_coverage) {
				auto coverage_command = format(u8"opencppcoverage --sources {}\\src\\ --export_type=binary:codecov\\{}.cov -- {}"s, project_directory, test_index, compile_command);
				auto result = run_process(coverage_command);
				withs(stdout_lock) {
					println(result.output);
				};
				return;
			}

			auto actual_compiler = run_process(compile_command);

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
						with(ConsoleColor::cyan, println("Actual:"));
						umm output_byte_limit = 16*1024;
						if (actual_compiler.output.count > output_byte_limit) {
							println("*** MORE THAN {} BYTES PRINTED, OMITTING ***", output_byte_limit);
						} else {
							println(actual_compiler.output);
						}
					}
				});
				return;
			}


			// Generating executables is no implemented yet.
#if false

			auto program_path = format(u8"{}.exe"s, parse_path(test_filename).name);
			if (!file_exists(program_path)) {
				do_fail([&]{
					with(ConsoleColor::red, println("Compiler was expected to generate an executable, but it didn't"));
				});

				return;
			}
			defer { delete_file(program_path); };

			auto actual_program = run_process(program_path);

			if (actual_program.timed_out) {
				do_fail([&] {
					with(ConsoleColor::red, print("Program timed out\n"));
				});
				return;
			}

			if (expected_program_output.count && !find(actual_program.output, expected_program_output)) {
				do_fail([&] {
					with(ConsoleColor::red, print("Program output mismatch:\n"));
					with(ConsoleColor::cyan, print("Expected:\n"));
					print("{}\n", expected_program_output);
					with(ConsoleColor::cyan, print("Actual:\n"));
					print("{}\n", actual_program.output);
				});

				return;
			}

			if (expected_program_exit_code.has_value()) {
				if (actual_program.exit_code != expected_program_exit_code.value()) {
					do_fail([&]{
						with(ConsoleColor::red, println("Program should have returned {} exit code. But actual is {}", expected_program_exit_code.value(), actual_program.exit_code));
					});
					return;
				}
			} else {
				if (actual_program.exit_code == 0xC0000005) {
					do_fail([&]{
						with(ConsoleColor::red, println("Program crashed"));
					});

					return;
				}
			}
#endif
		};
	}

	thread_pool.wait_for_completion(WaitForCompletionOption::do_any_task);

	if (loop_until_failure) {
		if (!n_failed) {
			system("cls");
			goto reloop;
		}
	}

	if (do_coverage) {
		println("Merging coverage results...");
		StringBuilder builder;
		append_format(builder, "opencppcoverage --sources {}\\src\\", project_directory);
		auto test_count = test_counter;
		for (u32 i = 0; i < test_count; ++i) {
			append_format(builder, " --input_coverage=codecov\\{}.cov", i);
		}
		auto result = run_process(as_utf8(to_string(builder)));
		println(result.output);
	}

	if (n_failed) {
		with(ConsoleColor::red,   print("{}/{} tests failed.\n", n_failed, test_filenames.count));
		return 1;
	} else {
		with(ConsoleColor::green, print("All {} tests succeeded.\n", test_filenames.count));
		return 0;
	}
}

