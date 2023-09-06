#define TL_IMPL
#include <tl/main.h>
#include <tl/file.h>
#include <tl/process.h>
#include <tl/thread.h>
#include <tl/cpu.h>
#include <tl/random.h>
#include <tl/time.h>
#include <conio.h>
#include <algorithm>

#include "../src/x.h"

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
	List<utf8> output = {};
	bool timed_out = {};
};

SpinLock stdout_lock;

RanProcess run_process(String command) {
	auto prev = GetErrorMode();
	SetErrorMode(SEM_NOGPFAULTERRORBOX);
	defer { SetErrorMode(prev); };

	auto process = start_process(to_utf16(command));
	defer { free(process); };
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
		.output = autocast to_string(output_builder),
		.timed_out = timed_out,
	};

	return result;
}

s32 tl_main(Span<String> arguments) {
	auto executable_path = get_executable_path();
	auto executable_directory = parse_path(executable_path).directory;

	auto fuzz_directory = format(u8"{}\\..\\fuzzer", executable_directory);
	auto fuzz_path = format(u8"{}\\fuzz.sp", fuzz_directory);
	auto run_command = format(u8"simplex -t1 {}"s, fuzz_path);

	xorshift32 r{get_performance_counter()};

	while (1) {
		StringBuilder builder;
		defer { free(builder); };

		auto random_tokens = [&] {
			char const *tokens[] = {
#define x(name) #name,
				ENUMERATE_KEYWORDS(x)
#undef x
#define x(name) name,
				ENUMERATE_DOUBLE_CHAR_TOKENS(x)
				ENUMERATE_TRIPLE_CHAR_TOKENS(x)
#undef x
				"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
				"main",
				"0","1","2","3","4","5","6","7","8","0",
				"{","}","[","]","(",")",
				"`","~","!","@","#","$",
				"%","^","&","*","-","=",
				"+",";",":",",",".","/",
				"?",
			};

			u32 count = 1 << (next(r) % 16);
			for (u32 i = 0; i < count; ++i) {
				append(builder, tokens[next(r) % count_of(tokens)]);
				if (((next(r) >> 16) & 3) == 0) {
					append(builder, '\n');
				} else {
					append(builder, ' ');
				}
			}
		};

		auto garbage_bytes = [&] {
			u32 count = 1 << (next(r) % 16);
			for (u32 i = 0; i < count; ++i) {
				append(builder, value_as_bytes(next(r)));
			}
		};

		auto garbage_ascii = [&] {
			static auto list = []{
				List<char> list;
				// Give more chance to new lines
				for (u32 i = 0; i < 4; ++i)
					list.add('\n');
				list.add('\r');
				list.add('\t');
				for (u32 i = 0x20; i < 0x7f; ++i) {
					list.add(i);
				}
				return list;
			}();
			u32 count = 1 << (next(r) % 16);
			for (u32 i = 0; i < count; ++i) {
				append(builder, list[next(r) % list.count]);
			}
		};

#if 0
		garbage_ascii();
#else
		switch ((next(r) >> 16) % 3) {
			case 0: random_tokens(); break;
			case 1: garbage_bytes(); break;
			case 2: garbage_ascii(); break;
		}
#endif

		auto source = to_string(builder);
		defer { free(source); };

		write_entire_file(fuzz_path, source);

		auto result = run_process(run_command);
		defer { free(result.output); };

		switch (result.exit_code) {
			case 0:
			case 1:
				print(".");
				continue;
			default: {
				println();
				println("EXIT CODE:");
				println(result.exit_code);
				println("OUTPUT:");
				println(result.output);
				return 0;
			}
		}

		temporary_allocator.clear();
	}


	return 0;
}

