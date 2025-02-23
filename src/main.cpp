#define TL_IMPL
#include "common.h"
#include "targets\x64\x64.h"
#include <tl/main.h>
#include <tl/process.h>
#include <tl/variant.h>

#define ENABLE_STRING_HASH_COUNT 0
#define ENABLE_NOTE_LEAK 0
#define __FILE_NAME__ ([]{auto e = __FILE__;while (*e) ++e;while (*e != '\\') --e;return e + 1;}())

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

inline bool operator==(String a, char const *b) {
	return a == as_utf8(as_span(b));
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

struct TimedResult {
	char const *name = 0;
	f64 seconds = 0;
};

List<TimedResult> timed_results;

String compiler_path;
String compiler_bin_directory;
String compiler_root_directory;
String generated_source_directory;

bool constant_name_inlining = true;
bool print_uids = false;
bool report_yields = false;
bool enable_time_log = false;
bool is_debugging = false;
bool print_tokens = false;
bool print_wait_failures = false;
bool enable_log_error_path = false;
bool break_on_error = false;
bool run_compiled_code = false;
bool print_stats = false;
bool should_print_ast = false;
bool run_interactive = false;
u32 nested_reports_verbosity = 1;

enum class InterpretMode {
	bytecode,
	ast,
};

String input_source_path;
u32 requested_thread_count = 0;
InterpretMode interpret_mode = {};

void log_error_path(char const *file, int line, auto &&...args) {
	with(ConsoleColor::dark_yellow, print("{}:{}: ", file, line));
	println(args...);
}

#define LOG_ERROR_PATH(...) \
	if (enable_log_error_path) { \
		log_error_path(__FILE__, __LINE__ __VA_OPT__(,) __VA_ARGS__); \
	}

#define dbgln(...) (is_debugging ? println(__VA_ARGS__) : 0)

#define timed_block(name) \
	if (enable_time_log) println("{} ...", name); \
	auto timer = create_precise_timer(); \
	defer { if (enable_time_log) timed_results.add({name, elapsed_time(timer)}); }

#define timed_function() \
	static constexpr auto funcname = __FUNCTION__; \
	timed_block(funcname)

#define timed_expression_named(name, expression) \
	[&] { \
		timed_block(name); \
		return expression; \
	}()

#define timed_expression(expression) timed_expression_named(#expression, expression)

template <class T>
struct Capitalized {
	T value;
};

template <class T>
umm append(StringBuilder &builder, Capitalized<T> capitalized) {
	u8 *c = builder.last->end();
	umm result = append(builder, capitalized.value);
	*c = to_upper((char )*c);
	return result;
}

inline StaticList<char, 4> escape_character(char ch) {
	StaticList<char, 4> result;
	switch (ch) {
		case '\x0': result.add("\\x0"s); break;
		case '\x1': result.add("\\x1"s); break;
		case '\x2': result.add("\\x2"s); break;
		case '\x3': result.add("\\x3"s); break;
		case '\x4': result.add("\\x4"s); break;
		case '\x5': result.add("\\x5"s); break;
		case '\x6': result.add("\\x6"s); break;
		case '\a': result.add("\\a"s); break;
		case '\b': result.add("\\b"s); break;
		case '\t': result.add("\\t"s); break;
		case '\n': result.add("\\n"s); break;
		case '\v': result.add("\\v"s); break;
		case '\f': result.add("\\f"s); break;
		case '\r': result.add("\\r"s); break;
		case '\x0e': result.add("\\x0e"s); break;
		case '\x0f': result.add("\\x0f"s); break;
		case '\x10': result.add("\\x10"s); break;
		case '\x11': result.add("\\x11"s); break;
		case '\x12': result.add("\\x12"s); break;
		case '\x13': result.add("\\x13"s); break;
		case '\x14': result.add("\\x14"s); break;
		case '\x15': result.add("\\x15"s); break;
		case '\x16': result.add("\\x16"s); break;
		case '\x17': result.add("\\x17"s); break;
		case '\x18': result.add("\\x18"s); break;
		case '\x19': result.add("\\x19"s); break;
		case '\x1a': result.add("\\x1a"s); break;
		case '\x1b': result.add("\\x1b"s); break;
		case '\x1c': result.add("\\x1c"s); break;
		case '\x1d': result.add("\\x1d"s); break;
		case '\x1e': result.add("\\x1e"s); break;
		case '\x1f': result.add("\\x1f"s); break;
		case '\\': result.add("\\\\"s); break;
		default:
			result.add(ch);
			break;
	}
	return result;
}

inline umm escape_string(Span<utf8> string, auto write) {
	StaticList<utf8, 4096> buffer;
	umm count = 0;
	for (auto ch : string) {
		auto escaped = escape_character((char)ch);
		for (auto ch : escaped) {
			buffer.add(ch); 
			if (buffer.count == buffer.capacity) {
				write(buffer.span());
				count += buffer.count;
				buffer.clear();
			}
		}
	}
	write(buffer.span());
	count += buffer.count;
	return count;
}

struct EscapedString {
	String unescaped_string;
};

inline umm append(StringBuilder &builder, EscapedString string) {
	return escape_string(string.unescaped_string, [&](auto s) { append(builder, s); });
}

struct UnescapeResult {
	List<utf8> string;
	String failed_at;
	String fail_reason;

	operator bool() { return failed_at.count == 0; }
};
UnescapeResult unescape_string(Span<utf8> string) {
	if (string.count == 0) {
		return { .string = to_list(string) };
	}

	List<utf8> result;

	UnescapeResult unfinished_result = {
		.failed_at = {&string.back(), 1},
		.fail_reason = u8"Unfinished escape sequence"s,
	}; 

#define NEXT() if (++cursor == string.end()) return unfinished_result

	for (auto cursor = string.begin(); cursor != string.end(); ++cursor) {
		if (*cursor == '\\') {
			NEXT();
			switch (*cursor) {
				case 'a': result.add('\a'); break;
				case 'b': result.add('\b'); break;
				case 'f': result.add('\f'); break;
				case 'v': result.add('\v'); break;
				case 'n': result.add('\n'); break;
				case 't': result.add('\t'); break;
				case 'r': result.add('\r'); break;
				case 'x': {
					u32 digits[2];
					
					for (auto &digit : digits) {
						NEXT();
						digit = *cursor;
						/**/ if ('0' <= digit && digit <= '9') digit -= '0';
						else if ('a' <= digit && digit <= 'f') digit = digit - 'a' + 10;
						else if ('A' <= digit && digit <= 'F') digit = digit - 'A' + 10;
						else return {
							.failed_at = {cursor, 1},
							.fail_reason = u8"Invalid hex digit"s,
						};
					}

					result.add(digits[0] * 16 + digits[1]);

					break;
				}
				default: 
					return {
						.failed_at = {cursor, 1},
						.fail_reason = u8"Unknown escape sequence"s,
					};
			}
		} else {
			result.add(*cursor);
		}
	}

#undef NEXT

	return {result};
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

template <class T>
using GList = tl::List<T, DefaultAllocator>;

template <class Key, class Value, class Traits = DefaultHashTraits<Key>>
using GHashMap = tl::ContiguousHashMap<Key, Value, Traits, DefaultAllocator>;

template <class Value, class Traits = DefaultHashTraits<Value>>
using GHashSet = tl::ContiguousHashMap<Value, Empty, Traits, DefaultAllocator>;

#include "x.h"

consteval u64 const_string_to_token_kind(Span<char> string) {
	assert(string.count <= 8);
	u64 result = 0;
	for (umm i = string.count - 1; i != -1; --i) {
		result <<= 8;
		result |= string.data[i];
	}
	return result;
}
consteval u32 const_string_to_token_kind(char a, char b) {
	char buffer[] { a, b };
	return const_string_to_token_kind(array_as_span(buffer));
}
consteval u32 const_string_to_token_kind(char a, char b, char c) {
	char buffer[] { a, b, c };
	return const_string_to_token_kind(array_as_span(buffer));
}

enum TokenKind : u64 {
	#define x(name) Token_##name = const_string_to_token_kind(#name##s),
	#define y(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KIND(x, y)
	#undef y
	#undef x
};

String enum_name(TokenKind k) {
	switch (k) {
		#define x(name) case Token_##name: return u8 ## #name ## s;
		#define y(name, value) x(name)
			ENUMERATE_TOKEN_KIND(x, y)
		#undef y
		#undef x
	}
	List<utf8, TemporaryAllocator> s;
	if (k >= 256) {
		s.add((utf8)(k >> 8));
	}
	s.add((utf8)k);
	return s;
}

inline umm append(StringBuilder &builder, TokenKind kind) {
	switch (kind) {
		case Token_eof:    return append(builder, "end of file");
		case Token_eol:    return append(builder, "end of line");
		case Token_name:   return append(builder, "name");
		case Token_number: return append(builder, "number");
		case Token_string: return append(builder, "string");
	}

	if ((u32)kind <= 0xff)
		return append(builder, (char)kind);

	switch (kind) {
#define x(name) case Token_##name: return append(builder, #name);
#define y(name, value) x(name)
		ENUMERATE_TOKEN_KIND(x, y)
#undef y
#undef x
	}

	return append_format(builder, "(unknown TokenKind 0x{} \"{}\")", FormatInt{.value=(u64)kind, .radix=16}, as_chars(value_as_bytes(kind)));
}

struct Token {
	TokenKind kind = {};
	String string;
};

inline umm append(StringBuilder &builder, Token token) {
	switch (token.kind) {
		case Token_eof: return append(builder, "end of file");
		case Token_eol: return append(builder, "end of line");
	}

	return append(builder, token.string);
}

#define PASTE_CASE(x) case x:
#define PASTE_CASE_0(x) case x[0]:

struct SourceLine {
	String string;
	u32 number = 0;
};

struct SourceLocation {
	String location;
	String file;
	u32 location_line_number = 0;
	u32 location_column_number = 0;

	u32 lines_start_number = 0;
	List<String> lines;
};

LockProtected<GHashMap<utf8 *, String>, SpinLock> content_start_to_file_name;

struct GetSourceLocationOptions {
	int lines_before = 0;
	int lines_after = 0;
};

SourceLocation get_source_location(String location, GetSourceLocationOptions options = {}) {
	SourceLocation result;
	result.location = location;
	result.location_column_number = 0;

	utf8 *chunk_start = location.begin();
	utf8 *chunk_end = location.end();

	if (location == u8"\n"s) {
		// Show preceding line in this case
		chunk_start--;
		chunk_end--;
	}

	while (*chunk_start && *chunk_start != '\n') {
		--chunk_start;
		++result.location_column_number;
	}
	++chunk_start;
	// Now chunk_start it is at the beggining of first line of `location`

	while (*chunk_end && *chunk_end != '\n') {
		++chunk_end;
	}
	// Now chunk_end it is at the end of last line of `location`

	// chunk_* now points at line(s) with `location`. Extend to desired line count.
	
	int missing_lines_before = 0;
	for (int i = 0; i < options.lines_before; ++i) {
		--chunk_start;
		if (*chunk_start == 0) {
			missing_lines_before = options.lines_before - i;
			break;
		}
		assert(*chunk_start == '\n');
		--chunk_start;
		
		while (*chunk_start && *chunk_start != '\n') {
			--chunk_start;
		}
		++chunk_start;
	}
	
	int missing_lines_after = 0;
	for (int i = 0; i < options.lines_after; ++i) {
		if (*chunk_end == 0) {
			missing_lines_after = options.lines_after - i;
			break;
		}

		++chunk_end;
		while (*chunk_end && *chunk_end != '\n') {
			++chunk_end;
		}
	}

	for (int i = 0; i < missing_lines_before; ++i) {
		result.lines.add(Span(chunk_start, (umm)0));
	}
	split_by_one(Span(chunk_start, chunk_end), u8'\n', [&](String line) {
		result.lines.add(line);
	});
	for (int i = 0; i < missing_lines_after; ++i) {
		result.lines.add(Span(chunk_end, (umm)0));
	}

	assert(result.lines.count);
	assert(result.lines.front().begin() <= location.begin());
	if (location != u8"\n"s) {
		assert(result.lines.back().end() >= location.end());
	}

	result.lines_start_number = 1;
	utf8 *cursor = chunk_start;
	while (*cursor != '\0') {
		if (*cursor == '\n') 
			++result.lines_start_number;
		--cursor;
	}

	result.location_line_number = result.lines_start_number + options.lines_before;

	auto found_file_name = locked_use(content_start_to_file_name) { return content_start_to_file_name.find(cursor + 1); };
	assert(found_file_name);
	result.file = found_file_name->value;

	return result;
}

inline umm append(StringBuilder &builder, SourceLocation location) {
	return append_format(builder, "{}:{}:{}", location.file, location.location_line_number, location.location_column_number);
}

void print_replacing_tabs_with_spaces(String string) {
	const umm n_spaces = 4;
	for (auto c : string) {
		if (c == '\t')
			for (umm i = 0; i < n_spaces; ++i)
				print(' ');
		else 
			print(c);
	}
}
void print_with_length_of(char r, umm count) {
	for (umm i = 0; i < count; ++i) {
		print(r);
	}
}
void print_with_length_of(char r, String string) {
	for (auto c : string) {
		if (c == '\t')
			for (umm i = 0; i < 4;++i) 
				tl::print(r);
		else 
			print(r);
	}
}

enum class ReportKind : u8 {
	info,
	warning,
	error,
	help,
};

void print_report_indentation(int indentation) {
	for (u32 i = indentation; i--;) {
		tl::print(u8"|   "s);
	}
}

void print_source_chunk(SourceLocation source_location, int indentation, ConsoleColor highlight_color) {
	auto log10_ceil = [](int x) {
		if (x < 1) return 0;
		if (x < 10) return 1;
		if (x < 100) return 2;
		if (x < 1000) return 3;
		if (x < 10000) return 4;
		if (x < 100000) return 5;
		if (x < 1000000) return 6;
		return 7;
	};

	auto max_line_number = source_location.lines_start_number + source_location.lines.count - 1;
	auto line_number_width = log10_ceil(max_line_number);
	auto line_number_alignment = align_right(line_number_width, ' ');

	auto output_line = [&](u32 line_number, String line, String highlight) {
		print_report_indentation(indentation);
		umm chars_pre_source = print(u8" {} | ", Format(line_number, line_number_alignment));

		highlight.set_begin(clamp(highlight.begin(), line.begin(), line.end()));
		highlight.set_end(clamp(highlight.end(), line.begin(), line.end()));
		if (highlight.count) {
			auto prefix = String(line.begin(), highlight.begin());
			auto postfix = String(highlight.end(), line.end());

			print_replacing_tabs_with_spaces(prefix);
			with(highlight_color, print_replacing_tabs_with_spaces(highlight));
			print_replacing_tabs_with_spaces(postfix);
			println();
			if (!is_stdout_console()) {
				withs(highlight_color) {
					print_with_length_of(' ', chars_pre_source);
					print_with_length_of(' ', prefix);
					print_with_length_of('~', highlight);
					print_with_length_of(' ', postfix);
					println();
				};
			}
		} else {
			print_replacing_tabs_with_spaces(line);
			println();
			if (source_location.location == u8"\n"s) {
				withs(highlight_color) {
					print_with_length_of(' ', chars_pre_source);
					print_with_length_of(' ', line);
					println('~');
				};
			}
		}

	};

	for (int i = 0; i < source_location.lines.count; ++i) {
		output_line(source_location.lines_start_number + i, source_location.lines[i], source_location.location);
	}
}

struct Report {
	ReportKind kind = {};
	String location = {};
	String message = {};
	u32 indentation = 0;

	static Report create(ReportKind kind, u32 indentation, String location, auto const &message) {
		return {
			.kind = kind,
			.location = location,
			.message = (String)to_string(message),
			.indentation = indentation,
		};
	}
	static Report create(ReportKind kind, u32 indentation, String location, char const *format, auto const &arg, auto const &...args) {
		return {
			.kind = kind,
			.location = location,
			.message = (String)tl::format(format, arg, args...),
			.indentation = indentation,
		};
	}

	ConsoleColor get_color(ReportKind kind) {
		switch (kind) {
			case ReportKind::info:    return ConsoleColor::cyan;
			case ReportKind::warning: return ConsoleColor::yellow;
			case ReportKind::error:   return ConsoleColor::red;
			case ReportKind::help:    return ConsoleColor::green;
		}
		return {};
	}

	umm print_report_kind(ReportKind kind) {
		switch (kind) {
			case ReportKind::info:    return with(get_color(kind), ::print("Info"));
			case ReportKind::warning: return with(get_color(kind), ::print("Warning"));
			case ReportKind::error:   return with(get_color(kind), ::print("Error"));
			case ReportKind::help:    return with(get_color(kind), ::print("Help"));
		}
		return 0;
	}

	void print() {
		scoped(temporary_allocator_and_checkpoint);

		bool verbose = indentation < nested_reports_verbosity;
		
		if (verbose) {
			print_report_indentation(indentation);
			println();
		}

		if (location.data) {
			auto source_location = get_source_location(location);

			print_report_indentation(indentation);
			tl::print("{}: ", source_location);

			if (verbose) {
				println();
				print_report_indentation(indentation);
				print_report_kind(kind);
				println(": {}",  message);

				print_source_chunk(source_location, indentation, get_color(kind));
			} else {
				print_report_kind(kind);
				println(": {}",  message);
			}
		} else {
			print_report_indentation(indentation);
			print_report_kind(kind);
			println(": {}", message);
		}
	}
};

struct ReporterBase {
	u32 indentation = 0;
	void info   (this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::info,    self.indentation, location, args...)); }
	void warning(this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::warning, self.indentation, location, args...)); }
	void error  (this auto &&self, String location, auto const &...args) {
		if (break_on_error) {
			debug_break();
		}
		self.on_report(Report::create(ReportKind::error, self.indentation, location, args...));
	}
	void help   (this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::help, self.indentation, location, args...)); }
	void info   (this auto &&self, auto const &...args) { return self.info   (String{}, args...); }
	void warning(this auto &&self, auto const &...args) { return self.warning(String{}, args...); }
	void error  (this auto &&self, auto const &...args) { return self.error  (String{}, args...); }
	void help   (this auto &&self, auto const &...args) { return self.help   (String{}, args...); }
};

struct Reporter : ReporterBase {
	List<Report> reports;

	void add(Report report) {
		report.indentation = indentation;
		reports.add(report);
	}
	void on_report(Report report) {
		reports.add(report);
	}
	void print_all() {
		scoped(stdout_mutex);
		for (auto &report : reports) {
			report.print();
		}
	}
};

struct ImmediateReporter : ReporterBase {
	void on_report(Report report) {
		scoped(stdout_mutex);
		report.print();
	}
} immediate_reporter;

thread_local String debug_current_location;

void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, Span<char> message) {
	if (!location.data)
		location = debug_current_location;
	immediate_reporter.error(debug_current_location, "COMPILER ERROR: {} {} at {}:{} in function {}", cause_string, expression, file, line, function);
	if (message.count)
		println("Message: {}", message);
}

void print_crash_info() {
	println("Call stack:");
	println(resolve_names(get_call_stack().skip(1).skip(-7)));
}

template <int byte_count>
auto chars_as_int(utf8 const *chars) {
	using Int = UintWithBits<byte_count * 8>;
	Int result = *(Int *)chars;
	result &= ((1ull<<(8*byte_count))-1);
	return result;
}


void print_token(umm i, Token token) {
	print("{}) {} ", i, enum_name(token.kind));

	switch (token.kind) {
		case Token_number:
		case Token_name: 
		case Token_directive: 
			print("\"{}\"", EscapedString{token.string}); 
			break;
		case Token_string: 
			print("{}", EscapedString{token.string}); 
			break;
	}

	println();
}

constexpr u32 custom_precedence = 4;

enum class BinaryOperation : u8 {
#define x(name, token, precedence) name,
	ENUMERATE_BINARY_OPERATIONS(x)
#undef x
};

enum class UnaryOperation : u8 {
#define x(name, token) name,
#define y(name) name,
	ENUMERATE_UNARY_OPERATIONS(x, y)
#undef y
#undef x
};

umm append(StringBuilder &builder, BinaryOperation operation) {
	switch (operation) {
#define x(name, token, precedence) case BinaryOperation::name: return append(builder, token);
		ENUMERATE_BINARY_OPERATIONS(x)
#undef x
	}
	return append_format(builder, "(unknown binary {})", (u32)operation);
}

umm append(StringBuilder &builder, UnaryOperation operation) {
	switch (operation) {
		case UnaryOperation::pointer: return append(builder, '*');
		case UnaryOperation::dereference: return append(builder, '*');
	}
	switch (operation) {
#define x(name, token) case UnaryOperation::name: return append(builder, token);
#define y(name) case UnaryOperation::name: return append(builder, #name);
		ENUMERATE_UNARY_OPERATIONS(x, y)
#undef y
#undef x
	}
	return append_format(builder, "(unknown unary {})", (u32)operation);
}

u32 get_precedence(BinaryOperation operation) {
	switch (operation) {
#define x(name, token, precedence) case BinaryOperation::name: return precedence;
		ENUMERATE_BINARY_OPERATIONS(x)
#undef x
	}
	invalid_code_path();
}

Optional<BinaryOperation> as_binary_operation(TokenKind kind) {
	switch (kind) {
#define x(name, token, precedence) case (TokenKind)const_string_to_token_kind(token##s): return BinaryOperation::name;
		ENUMERATE_BINARY_OPERATIONS(x)
#undef x
	}
	return {};
}

Optional<UnaryOperation> as_unary_operation(TokenKind kind) {
	switch (kind) {
#define x(name, token) case (TokenKind)const_string_to_token_kind(token##s): return UnaryOperation::name;
#define y(name)
		ENUMERATE_UNARY_OPERATIONS(x, y)
#undef y
#undef x
	}
	return {};
}

inline bool is_right_associative(BinaryOperation operation) {
	return false;
}

enum class NodeKind : u8 {
	unknown,
#define x(name) name,
	ENUMERATE_NODE_KIND(x)
#undef x
	count,
};

inline umm append(StringBuilder &builder, NodeKind kind) {
	switch (kind) {
#define x(name) case NodeKind::name: return append(builder, #name);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
	return 0;
}

volatile u32 uid_counter;

struct Node {
	u32 uid = atomic_add(&uid_counter, 1);
	NodeKind kind = {};
	String location;

	Node() {
		int x = 0;
	}
};

#include "mutability.h"

struct Expression : Node {
	Expression *type = 0;
};

struct Statement : Node {

};

template <class T>
struct NodeTypeToKind;

#define x(name) \
	template <> \
	struct NodeTypeToKind<struct name> { \
		inline static constexpr NodeKind kind = NodeKind::name; \
	};
ENUMERATE_NODE_KIND(x)
#undef x

template <class T>
T *as(Node *node) {
	if (node->kind == NodeTypeToKind<T>::kind)
		return (T *)node;
	return 0;
}

template <>
Expression *as(Node *node) {
	switch (node->kind) {
#define x(name) case NodeKind::name:
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
			return (Expression *)node;
	}
	return 0;
}

template <>
Statement *as(Node *node) {
	switch (node->kind) {
#define x(name) case NodeKind::name:
		ENUMERATE_STATEMENT_KIND(x)
#undef x
			return (Statement *)node;
	}
	return 0;
}

struct AtomicArenaAllocator : AllocatorBase<AtomicArenaAllocator> {
	Allocator parent_allocator;
	umm buffer_size = 0;
	u8 *base = 0;
	u8 * volatile cursor = 0;

	forceinline static AtomicArenaAllocator create(umm size TL_LP) {
		AtomicArenaAllocator result = {};
		result.parent_allocator = TL_GET_CURRENT(allocator);
		result.buffer_size = size;
		result.cursor = result.base = result.parent_allocator.allocate_uninitialized<u8>(size TL_LA);
		return result;
	}

	forceinline static AtomicArenaAllocator current() { return {}; }

	forceinline AllocationResult allocate_impl(umm size, umm alignment TL_LP) {
		assert(cursor, "arena allocator was not initialized");

		u8 *target = 0;

		atomic_update(&cursor, [=, this, &target](u8 *cursor) {
			target = ceil(cursor, alignment);
			cursor = target + size;
			assert(cursor <= base + buffer_size, "Out of arena memory");
			return cursor;
		});

		return AllocationResult { .data = target, .count = size, .is_zeroed = true };
	}
	forceinline AllocationResult reallocate_impl(void *old_data, umm old_size, umm new_size, umm alignment TL_LP) {
		auto new_data = allocate_impl(new_size, alignment);
		memcpy(new_data.data, old_data, old_size);
		return new_data;
	}
	forceinline void deallocate_impl(void *data, umm size, umm alignment TL_LP) {
		(void)data;
		(void)size;
		(void)alignment;
	}

	forceinline operator Allocator() {
		return {
			.func = [](AllocatorAction action, void *data, umm old_size, umm new_size, umm align, void *state TL_LPD) -> AllocationResult {
				return ((AtomicArenaAllocator *)state)->execute(action, data, old_size, new_size, align TL_LA);
			},
			.state = this
		};
	}

	forceinline void clear() {
		atomic_set(&cursor, base);
	}
	forceinline void free() {
		if (!base)
			return;

		parent_allocator.free(base);
		base = 0;
		cursor = 0;
		buffer_size = 0;
	}
};

AtomicArenaAllocator node_arena;

template <class T>
struct NodeBase {
	NodeBase() {
		((T *)this)->kind = NodeTypeToKind<T>::kind;
	}
	static T *create() {
		return node_arena.allocate<T>();
	}
	void free() {
		((T *)this)->free_impl();
	}
};

template <class T>
struct Nameable {
	T value = {};
};

inline umm append(StringBuilder &builder, Nameable<String> str) {
	char c = str.value[0];
	if (c == '_' || isalpha(c)) {
		append(builder, c);
	} else {
		append(builder, '_');
	}
	for (auto c : str.value.skip(1)) {
		if (c == '_' || isalnum(c)) {
			append(builder, c);
		} else {
			append(builder, '_');
		}
	}
	return str.value.count;
}

inline umm append(StringBuilder &builder, Nameable<BinaryOperation> op) {
	switch (op.value) {
		#define x(name, token, precedence) case BinaryOperation::name: return append(builder, #name##s);
		ENUMERATE_BINARY_OPERATIONS(x)
		#undef x
	}
	return append_format(builder, "unknown_BinaryOperation_{}", (u32)op.value);
}

#define DEFINE_EXPRESSION(name) struct name : Expression, NodeBase<name>
#define DEFINE_STATEMENT(name) struct name : Statement, NodeBase<name>

enum class BuiltinType : u8 {
	#define x(name) name,
	ENUMERATE_BUILTIN_TYPES(x)
	#undef x
	count,
};

umm append(StringBuilder &builder, BuiltinType type_kind) {
	switch (type_kind) {
		#define x(name) case BuiltinType::name: return append(builder, #name);
		#define y(name, value) x(name)
		ENUMERATE_BUILTIN_TYPES(x)
		#undef y
		#undef x
	}
	return append_format(builder, "(unknown BuiltinType {})", (u32)type_kind);
}

inline BuiltinType to_builtin_type_kind(TokenKind kind) {
	switch (kind) {
#define x(name) case Token_##name: return BuiltinType::name;
		ENUMERATE_CONCRETE_BUILTIN_TYPES(x);
#undef x
	}

	invalid_code_path();
	return {};
}

enum class Inline : u8 {
#define x(name) name,
	ENUMERATE_INLINE_STATUS
#undef x
};

inline umm append(StringBuilder &builder, Inline status) {
	switch (status) {
#define x(name) case Inline::name: return append(builder, #name ## s);
		ENUMERATE_INLINE_STATUS
#undef x
	}
	return 0;
}


enum class ValueKind : u8 {
#define x(name) name,
	ENUMERATE_EXECUTION_VALUE_KIND
#undef x
};

inline umm append(StringBuilder &builder, ValueKind kind) {
	switch (kind) {
#define x(name) case ValueKind::name: return append(builder, #name);
		ENUMERATE_EXECUTION_VALUE_KIND
#undef x
	}
	return append_format(builder, "(unknown ValueKind {})", (u32)kind);
}

bool is_type(Expression *expression);


#define CHECK_THAT_TYPES_ARE_TYPES 1//BUILD_DEBUG

#if CHECK_THAT_TYPES_ARE_TYPES
struct Type {
	Expression *expression = 0;
	Type() = default;
	Type(Expression *expression) : expression(expression) {
		if (expression) {
			assert(is_type(expression));
		}
	}
	operator Expression *() { return expression; }
	Expression *operator->() { return expression; }
	Expression &operator*() { return *expression; }
	template <class Expr>
	operator Expr*();
};
#else
using Type = Expression *;
#endif

inline umm append(StringBuilder &builder, Node *node);

inline umm append(StringBuilder &builder, Type type) {
	return append(builder, (Node *)type);
}

struct StructTag {} struct_tag;
struct ArrayTag {} array_tag;
struct UnsizedIntegerTag {} unsized_integer_tag;

struct Value {
	ValueKind kind = {};
	union {
		u8 U8;
		u16 U16;
		u32 U32;
		u64 U64;
		s8 S8;
		s16 S16;
		s32 S32;
		s64 S64;
		s64 UnsizedInteger;
		bool Bool;
		String String;
		Lambda *lambda;
		Type Type;
		Value *pointer;
		List<Value> elements;
	};
	Value() {
		memset(this, 0, sizeof(*this));
	}
	Value(const Value &that) {
		memcpy(this, &that, sizeof(Value));
	}
	Value(Value &&that) { 
		memcpy(this, &that, sizeof(Value));
		memset(&that, 0, sizeof(Value));
	}
	~Value() {
		memset(this, 0, sizeof(Value));
	}
	Value &operator=(const Value &that) { return this->~Value(), *new(this) Value(that); }
	Value &operator=(Value &&that) { return this->~Value(), *new(this) Value(std::move(that)); }
	explicit Value(ValueKind kind) : kind(kind) {
		switch (kind) {
			case ValueKind::return_:
			case ValueKind::break_:
			case ValueKind::continue_:
			case ValueKind::none:
				break;
			default:
				invalid_code_path();
		}
	}
	explicit Value(u8          value) : kind(ValueKind::U8     ), U8     (value) {}
	explicit Value(u16         value) : kind(ValueKind::U16    ), U16    (value) {}
	explicit Value(u32         value) : kind(ValueKind::U32    ), U32    (value) {}
	explicit Value(u64         value) : kind(ValueKind::U64    ), U64    (value) {}
	explicit Value(s8          value) : kind(ValueKind::S8     ), S8     (value) {}
	explicit Value(s16         value) : kind(ValueKind::S16    ), S16    (value) {}
	explicit Value(s32         value) : kind(ValueKind::S32    ), S32    (value) {}
	explicit Value(s64         value) : kind(ValueKind::S64    ), S64    (value) {}
	explicit Value(bool        value) : kind(ValueKind::Bool   ), Bool   (value) {}
	explicit Value(::String    value) : kind(ValueKind::String ), String (value) {}
	explicit Value(Lambda     *value) : kind(ValueKind::lambda ), lambda (value) {}
	explicit Value(::Type      value) : kind(ValueKind::Type   ), Type   (value) {}
	explicit Value(Value      *value) : kind(ValueKind::pointer), pointer(value) {}
	explicit Value(StructTag, Span<Value> value) : kind(ValueKind::struct_), elements(to_list(value)) {}
	explicit Value(ArrayTag,  Span<Value> value) : kind(ValueKind::array),   elements(to_list(value)) {}
	explicit Value(UnsizedIntegerTag, s64 value) : kind(ValueKind::UnsizedInteger), UnsizedInteger(value) {}
	
	Value copy() {
		Value result = *this;
		switch (kind) {
			case ValueKind::array:
			case ValueKind::struct_:
				result.elements = tl::copy(result.elements);
				break;
		}
		return result;
	}
	void free() {
		switch (kind) {
			case ValueKind::array:
			case ValueKind::struct_:
				tl::free(elements);
				break;
		}
	}
};

enum class CallKind {
	unknown,
	lambda,
	constructor,
};

constexpr u64 invalid_definition_offset = 1ull << 63;

DEFINE_EXPRESSION(Block) {
	Block *parent = 0;
	Expression *container = 0;

	GList<Node *> children;
	GList<Definition *> definition_list;
	GHashMap<String, GList<Definition *>> definition_map;

	String tag;
	List<Break *> breaks;

	GList<Defer *> defers;

	void add(Node *child);
	void free_impl() {
		tl::free(children);
		tl::free(definition_list);
		tl::free(definition_map);
	}
};
DEFINE_EXPRESSION(Call) {
	struct Argument {
		String name = {};
		Expression *expression = 0;
		Definition *parameter = 0;
	};

	Expression *callable = 0;
	GList<Argument> arguments;

	Inline inline_status = {};
	CallKind call_kind = {};
};
DEFINE_EXPRESSION(Definition) {
	Expression *container = 0;
	String name;
	Expression *parsed_type = 0;
	Expression *initial_value = 0;
	Optional<Value> constant_value = {};
	Mutability mutability = {};
	u64 offset = invalid_definition_offset;
	bool is_parameter : 1 = false;
	bool is_template_parameter : 1 = false;
};
DEFINE_EXPRESSION(IntegerLiteral) {
	u64 value = 0;
};
DEFINE_EXPRESSION(BooleanLiteral) {
	bool value = false;
};
DEFINE_EXPRESSION(NoneLiteral) {};
DEFINE_EXPRESSION(StringLiteral) {
	String value;
};
DEFINE_EXPRESSION(LambdaHead) {
	LambdaHead() {
		template_parameters_block.container = this;
		parameters_block.container = this;

		parameters_block.parent = &template_parameters_block;
	}
	// Definitions in these block must have unique names.
	// Assume .definition_map[...].count == 1
	Block template_parameters_block;
	Block parameters_block;
	Expression *parsed_return_type = 0;
	Expression *return_type = 0;
	u64 total_parameters_size = 0;

	// When true, template parameters are unresolved
	// If false, all template parameters must have their initial values set to resolved types.
	bool is_template : 1 = false;
};
DEFINE_EXPRESSION(Lambda) {
	Definition *definition = 0;
	Expression *body = 0;
	LambdaHead head;

	GList<Return *> returns;
	
	u64 first_instruction_index = -1;

	Inline inline_status = {};

	String extern_library = {};

	GList<Definition *> locals = {};
	u64 space_for_call_arguments = 0;
	u64 temporary_size = 0;
	u64 locals_size = 0;
	u64 stack_frame_size = 0;

	bool is_intrinsic : 1 = false;
	bool is_extern    : 1 = false;
};
DEFINE_EXPRESSION(Name) {
	String name;
	List<Definition *> possible_definitions;

	bool allow_overload : 1 = false;

	Definition *definition() {
		if (possible_definitions.count == 1)
			return possible_definitions[0];
		return 0;
	}
};
DEFINE_EXPRESSION(IfExpression) {
	Expression *condition = 0;
	Expression *true_branch = 0;
	Expression *false_branch = 0;
};
DEFINE_EXPRESSION(BuiltinTypeName) {
	BuiltinType type_kind = {};
};
DEFINE_EXPRESSION(Binary) {
	Expression *left = 0;
	Expression *right = 0;
	BinaryOperation operation = {};
};
DEFINE_EXPRESSION(Match) {
	struct Case {
		Expression *from = 0; // null in default case.
		Expression *to = 0;
	};

	Expression *expression = 0;
	GList<Case> cases;
	Case *default_case = 0;
};
DEFINE_EXPRESSION(Unary) {
	Expression *expression = 0;
	UnaryOperation operation = {};
	Mutability mutability = {};
};
DEFINE_EXPRESSION(Struct) {
	Definition *definition = 0;
	GList<Definition *> members;
	s64 size = -1;
	bool must_be_fully_initialized : 1 = false;
};
DEFINE_EXPRESSION(ArrayType) {
	Expression *element_type = 0;
	Expression *count_expression = 0;
	Optional<u64> count;
};
DEFINE_EXPRESSION(Subscript) {
	Expression *subscriptable = 0;
	Expression *index = 0;
};
DEFINE_EXPRESSION(ArrayConstructor) {
	List<Expression *> elements;
};
DEFINE_EXPRESSION(ZeroInitialized) {}; // Might get rid of this if `none as type` becomes real
DEFINE_STATEMENT(Return) {
	Expression *value = 0;
	Lambda *lambda = 0;
};
DEFINE_STATEMENT(While) {
	Expression *condition = 0;
	Node *body = 0;
};
DEFINE_STATEMENT(Continue) {
	While *loop = 0;
};
DEFINE_STATEMENT(Break) {
	While *loop = 0;
	Block *tag_block = 0;
	Expression *value = 0;
};
DEFINE_STATEMENT(IfStatement) {
	Expression *condition = 0;
	Node *true_branch = 0;
	Node *false_branch = 0; // May be null
};
DEFINE_STATEMENT(Import) {
	String path;
};
DEFINE_STATEMENT(Defer) {
	Node *body = 0;
};

template <class T>
concept CNode = OneOf<T, Expression, Statement
#define x(name) , name
	ENUMERATE_NODE_KIND(x)
#undef x
>;

template <class T>
concept CExpression = OneOf<T, Expression
#define x(name) , name
	ENUMERATE_EXPRESSION_KIND(x)
#undef x
>;

template <class T>
concept CStatement = OneOf<T, Statement
#define x(name) , name
	ENUMERATE_STATEMENT_KIND(x)
#undef x
>;

#if CHECK_THAT_TYPES_ARE_TYPES
template <class Expr>
Type::operator Expr*() {
	static_assert(std::is_same_v<Expr, Node> || CExpression<Expr>);
	return (Expr *)expression;
}
#endif

auto switch_(Node *node, auto &&visitor) {
	switch (node->kind) {
		#define x(name) case NodeKind::name: return visitor((name *)node);
		ENUMERATE_NODE_KIND(x)
		#undef x
	}
	invalid_code_path();
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

inline umm append(StringBuilder &builder, Node *node);

inline umm append(StringBuilder &builder, Value value) {
	switch (value.kind) {
		case ValueKind::none: return 0;
		case ValueKind::U8: return append(builder, value.U8);
		case ValueKind::U16: return append(builder, value.U16);
		case ValueKind::U32: return append(builder, value.U32);
		case ValueKind::U64: return append(builder, value.U64);
		case ValueKind::S8: return append(builder, value.S8);
		case ValueKind::S16: return append(builder, value.S16);
		case ValueKind::S32: return append(builder, value.S32);
		case ValueKind::S64: return append(builder, value.S64);
		case ValueKind::UnsizedInteger: return append(builder, value.UnsizedInteger);
		case ValueKind::Bool: return append(builder, value.Bool);
		case ValueKind::Type:    return append(builder, value.Type);
		case ValueKind::lambda:  return append(builder, value.lambda);
		case ValueKind::array: {
			umm result = 0;
			result += append(builder, ".[");
			result += append(builder, value.elements[0]);
			for (auto element : value.elements.skip(1)) {
				result += append(builder, ", ");
				result += append(builder, element);
			}
			result += append(builder, "]");
			return result;
		}
	}
	return append_format(builder, "(unknown Value {})", value.kind);
}

inline umm append(StringBuilder &builder, Nameable<Node *> node) {
	switch (node.value->kind) {
		case NodeKind::Name: {
			auto name = (Name *)node.value;
			return append(builder, name->name);
		}
		case NodeKind::BuiltinTypeName: {
			auto name = (BuiltinTypeName *)node.value;
			return append(builder, name->type_kind);
		}
	}
	invalid_code_path();
	return 0;
}
inline umm append(StringBuilder &builder, Nameable<Expression *> node) {
	return append(builder, Nameable((Node *)node.value));
}

#include "visit.h"

BuiltinTypeName builtin_types[(u32)BuiltinType::count];

// NOTE: Do not use this for types in the source code. These do not have a location.
BuiltinTypeName *get_builtin_type(BuiltinType kind) {
	return &builtin_types[(u32)kind];
}

Expression *direct(Expression *node) {
	while (true) {
		if (auto name = as<Name>(node)) {
			if (name->possible_definitions.count == 1) {
				auto definition = name->definition();
				assert(definition);
				if (definition->mutability == Mutability::constant) {
					node = definition->initial_value;
					continue;
				} else {
					return definition;
				}
			} else {
				return name;
			}
		}
		break;
	}
	if (auto bt = as<BuiltinTypeName>(node)) {
		return get_builtin_type(bt->type_kind);
	}
	return node;
}

template <class T>
T *direct_as(Expression *node) {
	node = direct(node);
	if (node->kind == NodeTypeToKind<T>::kind) {
		return (T *)node;
	}

	return 0;
}

struct CheckResult2 {
	bool result = {};
	Node *failed_node1 = {};
	Node *failed_node2 = {};

	CheckResult2(bool result) : result(result) {}
	CheckResult2(bool result, Node *failed_node1, Node *failed_node2) : result(result), failed_node1(failed_node1), failed_node2(failed_node2) {}

	operator bool() { return result; }
};

#define TYPES_MUST_MATCH(a, b)          \
	if (auto _ = types_match(a, b); !_) \
		return _

CheckResult2 types_match(Type a, Type b) {
	assert(a);
	assert(b);
	a = direct(a);
	b = direct(b);
	assert(a);
	assert(b);

	if (a->kind == b->kind) {
		switch (a->kind) {
			case NodeKind::BuiltinTypeName: {
				REDECLARE_VAL(a, (BuiltinTypeName *)a);
				REDECLARE_VAL(b, (BuiltinTypeName *)b);

				if (a->type_kind == b->type_kind) {
					return true;
				}
				break;
			}
			case NodeKind::LambdaHead: {
				REDECLARE_VAL(a, (LambdaHead *)a);
				REDECLARE_VAL(b, (LambdaHead *)b);

				if (a->parameters_block.definition_list.count == b->parameters_block.definition_list.count) {
					TYPES_MUST_MATCH(a->return_type, b->return_type);

					for (umm i = 0; i < a->parameters_block.definition_list.count; ++i) {
						TYPES_MUST_MATCH(a->parameters_block.definition_list[i]->type, b->parameters_block.definition_list[i]->type);
					}
					return true;
				}
				break;
			}
			case NodeKind::Unary: {
				REDECLARE_VAL(a, (Unary *)a);
				REDECLARE_VAL(b, (Unary *)b);

				if (a->operation == b->operation) {
					assert(a->operation == UnaryOperation::pointer);

					TYPES_MUST_MATCH(a->expression, b->expression);

					if (a->mutability == b->mutability) {
						return true;
					}
				}
				break;
			}
			case NodeKind::Struct: {
				REDECLARE_VAL(a, (Struct *)a);
				REDECLARE_VAL(b, (Struct *)b);

				if (a == b) {
					return true;
				}
				break;
			}
			case NodeKind::ArrayType: {
				REDECLARE_VAL(a, (ArrayType *)a);
				REDECLARE_VAL(b, (ArrayType *)b);

				TYPES_MUST_MATCH(a->element_type, b->element_type);
				assert(a->count);
				assert(b->count);

				if (a->count.value() == b->count.value()) {
					return true;
				}

				break;
			}
			default:
				invalid_code_path("invalid node kind {} in types_match", a->kind);
		}
	}
	return {false, a, b};
}

CheckResult2 types_match(Expression *a, BuiltinType b) {
	assert(a);
	a = direct(a);
	assert(a);

	if (auto ab = as<BuiltinTypeName>(a)) {
		if (ab->type_kind == b) {
			return true;
		}
	}

	return {false, a, get_builtin_type(b)};
}
CheckResult2 types_match(BuiltinType a, Expression *b) {
	return types_match(b, a);
}

bool is_type(Expression *expression) {
	return types_match(expression->type, BuiltinType::Type);
}

bool is_concrete_integer(Type type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::U8:
			case BuiltinType::U16:
			case BuiltinType::U32:
			case BuiltinType::U64:
			case BuiltinType::S8:
			case BuiltinType::S16:
			case BuiltinType::S32:
			case BuiltinType::S64:
				return true;
		}
	}

	return false;
}
bool is_signed_integer(Type type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::S8:
			case BuiltinType::S16:
			case BuiltinType::S32:
			case BuiltinType::S64:
				return true;
		}
	}

	return false;
}
bool is_unsigned_integer(Type type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::U8:
			case BuiltinType::U16:
			case BuiltinType::U32:
			case BuiltinType::U64:
				return true;
		}
	}

	return false;
}

bool is_concrete(Type type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::UnsizedInteger:
				return false;
		}
	}

	return true;
}

void propagate_concrete_type(Expression *expression, Type type) {
	switch (expression->kind) {
		case NodeKind::IntegerLiteral: 
		case NodeKind::Name:
			expression->type = type;
			break;
		case NodeKind::Unary: {
			auto unary = (Unary *)expression;
			unary->type = type;
			propagate_concrete_type(unary->expression, type);
			break;
		}
		case NodeKind::IfExpression: {
			auto If = (::IfExpression *)expression;
			If->type = type;
			propagate_concrete_type(If->true_branch, type);
			propagate_concrete_type(If->false_branch, type);
			break;
		}
		case NodeKind::Block: {
			auto block = (Block *)expression;
			if (block->children.count) {
				if (auto last_expr = as<Expression>(block->children.back())) {
					block->type = type;
					propagate_concrete_type(last_expr, type);
				}
			}
			break;
		}
		default: invalid_code_path(); break;
	}
}

void make_concrete(Expression *expression) {
	auto type = direct(expression->type);

	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::UnsizedInteger: {
				propagate_concrete_type(expression, get_builtin_type(BuiltinType::S64));
				return;
			}
		}
	}
}

u64 get_size(BuiltinType type_kind) {
	switch (type_kind) {
		case BuiltinType::None:   return 0;
		case BuiltinType::U8:     return 1;
		case BuiltinType::U16:    return 2;
		case BuiltinType::U32:    return 4;
		case BuiltinType::U64:    return 8;
		case BuiltinType::S8:     return 1;
		case BuiltinType::S16:    return 2;
		case BuiltinType::S32:    return 4;
		case BuiltinType::S64:    return 8;
		case BuiltinType::Bool:   return 1;
		case BuiltinType::Type:   return 8;
		case BuiltinType::String: return 16;
		default: invalid_code_path("Invalid BuiltinType {}", type_kind);
	}
}

u64 get_size(Type type);
u64 get_size_impl(LambdaHead *node) {
	return 8;
}
u64 get_size_impl(BuiltinTypeName *node) {
	return get_size(node->type_kind);
}
u64 get_size_impl(Unary *node) {
	if (node->operation == UnaryOperation::pointer)
		return 8;
	invalid_code_path("get_size(Unary {}) is invalid", node->operation);
}
u64 get_size_impl(Struct *Struct) {
	assert(Struct->size != -1);
	return Struct->size;
}
u64 get_size_impl(ArrayType *arr) {
	return get_size(arr->element_type) * arr->count.value();
}

u64 get_size(Type type) {
	type = direct(type);
	switch (type->kind) {
#define x(name) case NodeKind::name: return get_size_impl((name *)type);
		ENUMERATE_TYPE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path("get_size({}) is invalid", type->kind);
}

enum class Sign : u8 {
	Unsigned,
	Signed
};
 
Sign get_sign(BuiltinType type_kind) {
	switch (type_kind) {
		case BuiltinType::U8: 
		case BuiltinType::U16:
		case BuiltinType::U32:
		case BuiltinType::U64: 
			return Sign::Unsigned;
		case BuiltinType::S8:  
		case BuiltinType::S16: 
		case BuiltinType::S32: 
		case BuiltinType::S64: 
			return Sign::Signed;
		default: invalid_code_path("Invalid BuiltinType {}", type_kind);
	}
}

int tabs = 0;
void print_tabs() {
	for (int i = 0; i < tabs; ++i)
		print("    ");
}

struct {
	void operator+=(auto fn) {
		++tabs;
		fn();
		--tabs;
	}
} _tabber;

#define tabbed _tabber += [&]

void print_ast(Node *node);
void print_ast(Value value) {
	switch (value.kind) {
		case ValueKind::lambda: print_ast(value.lambda); break;
		default: print(value); break;
	}
}
void print_ast_impl(Block *block) {
	print("{");
	if (block->breaks.count) {
		print(" :{}", block->tag);
	}
	print('\n');
	tabbed {
		for (auto child : block->children) {
			print_tabs();
			print_ast(child);
			println();
		}
	};
	print_tabs();
	println("}");
}
void print_ast_impl(Call *call) {
	print_ast(call->callable);
	print('(');
	tabbed {
		for (auto &argument : call->arguments) {
			if (&argument != call->arguments.data) {
				print(", ");
			}
			print_ast(argument.expression);
		}
	};
	print(')');
}
void print_ast_impl(Definition *definition) {
	switch (definition->mutability) {
		case Mutability::constant: print("const"); break;
		case Mutability::readonly: print("let"); break;
		case Mutability::variable: print("var"); break;
		default: invalid_code_path();
	}
	print(' ');
	print(definition->name);
	if (print_uids) {
		print('_');
		print(definition->uid);
	}
	print(": ");
	print_ast(definition->type);
	if (definition->constant_value) {
		print(" = ");
		print_ast(definition->constant_value.value());
	} else if (definition->initial_value) {
		print(" = ");
		print_ast(definition->initial_value);
	}
}
void print_ast_impl(IntegerLiteral *literal) {
	print('{');
	tabbed {
		print(literal->value);
		print(" as ");
		print_ast(literal->type);
	};
	print('}');
}
void print_ast_impl(BooleanLiteral *literal) {
	print(literal->value);
}
void print_ast_impl(NoneLiteral *) {
	print("none");
}
void print_ast_impl(StringLiteral *literal) {
	print("\"{}\"", EscapedString{literal->value});
}
void print_ast_impl(LambdaHead *head, bool print_braces = true) {
	if (print_braces) {
		print("{");
		++tabs;
	}
	print("(");
	tabbed {
		List<List<Definition *>> grouped_parameters;

		for (auto parameter : head->parameters_block.definition_list) {
			if (grouped_parameters.count && types_match(grouped_parameters.back().front()->type, parameter->parsed_type)) {
				grouped_parameters.back().add(parameter);
			} else {
				List<Definition *> group;
				group.add(parameter);
				grouped_parameters.add(group);
			}
		}

		for (auto &group : grouped_parameters) {
			if (&group != grouped_parameters.begin())
				print(", ");

			for (auto &parameter : group) {
				if (&parameter != group.begin())
					print(", ");

				print(parameter->name);
			}
			print(": ");
			print_ast(group[0]->type);
		}
	};
	print("): ");
	print_ast(head->return_type);
	if (print_braces) {
		--tabs;
		print("}");
	}
}
void print_ast_impl(Lambda *lambda) {
	print("{");
	tabbed {
		switch (lambda->inline_status) {
			case Inline::always: print("inline "); break;
			case Inline::never: print("noinline "); break;
		}
		print_ast_impl(&lambda->head, false);
		print(" => ");
		if (lambda->is_intrinsic) 
			print("#intrinsic");
		if (lambda->is_extern) 
			print("#extern \"{}\"", EscapedString(lambda->extern_library));
		if (lambda->body) {
			print_ast(lambda->body);
		}
	};
	print_tabs();
	print("}");
}
void print_ast_impl(Name *name) {
	print(name->name);
	if (print_uids) {
		print('_');
		assert(name->definition());
		print(name->definition()->uid);
	}
}
void print_ast_impl(Return *return_) {
	print("return");
	if (return_->value) {
		print(' ');
		print_ast(return_->value);
	}
}
void print_ast_impl(IfStatement *If) {
	print("if ");
	print_ast(If->condition);
	print(" then ");
	print_ast(If->true_branch);
	if (If->false_branch) {
		print(" else ");
		print_ast(If->false_branch);
	}
}
void print_ast_impl(IfExpression *If) {
	print("if ");
	print_ast(If->condition);
	print(" then ");
	print_ast(If->true_branch);
	print(" else ");
	print_ast(If->false_branch);
}
void print_ast_impl(While *While) {
	print("while ");
	print_ast(While->condition);
	print(" then ");
	print_ast(While->body);
}
void print_ast_impl(BuiltinTypeName *type) {
	switch (type->type_kind) {
		#define x(name) case BuiltinType::name: print(#name); return;
		#define y(name, value) x(name)
		ENUMERATE_BUILTIN_TYPES(x)
		#undef y
		#undef x
	}
	invalid_code_path();
}
void print_ast_impl(Continue *) { print("continue"); }
void print_ast_impl(Break *Break) {
	print("break");
	if (Break->value) {
		print(" :{} ", Break->tag_block->tag);
		print_ast(Break->value);
	}
}
void print_ast_impl(Binary *binary) {
	print('{');
	tabbed {
		print_ast(binary->left);
		print(' ');
		print(binary->operation);
		print(' ');
		print_ast(binary->right);
	};
	print('}');
}
void print_ast_impl(Match *match) {
	print("match ");
	print_ast(match->expression);
	print(" {\n");
	tabbed {
		for (auto Case : match->cases) {
			print_tabs();
			if (Case.from) {
				print_ast(Case.from);
			} else {
				print("else");
			}
			print(" => ");
			print_ast(Case.to);
			print("\n");
		}
	};
	print_tabs();
	print("}");
}
void print_ast_impl(Unary *unary) {
	print('{');
	tabbed {
		print(unary->operation);
		print_ast(unary->expression);
	};
	print('}');
}
void print_ast_impl(Struct *Struct) {
	print("struct {\n");
	tabbed {
		for (auto member : Struct->members) {
			print_ast(member);
		}
	};
	print("}");
}
void print_ast_impl(ArrayType *Array) {
	print("{[");
	print(Array->count.value());
	print("]");
	print_ast(Array->element_type);
	print("}");
}
void print_ast_impl(Subscript *Subscript) {
	print("{");
	print_ast(Subscript->subscriptable);
	print("[");
	print_ast(Subscript->index);
	print("]}");
}
void print_ast_impl(ArrayConstructor *arr) {
	print(".[");
	if (arr->elements.count)
		print_ast(arr->elements[0]);
	for (auto element : arr->elements.skip(1)) {
		print(", ");
		print_ast(element);
	}
	print("]");
}
void print_ast_impl(Import *import) {
	print("import \"{}\"", EscapedString{import->path});
}
void print_ast_impl(Defer *defer_) {
	print("defer {\n");
	tabbed {
		print_tabs();
		print_ast(defer_->body);
		println();
	};
	print_tabs();
	print("}");
}
void print_ast_impl(ZeroInitialized *zi) {
	print("{none as ");
	print_ast(zi->type);
	print("}");
}
void print_ast(Node *node) {
	if (!node) {
		print("<NULL>");
		return;
	}
	switch (node->kind) {
#define x(name) case NodeKind::name: return print_ast_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
}

inline umm append(StringBuilder &builder, Node *node) {
	switch (node->kind) {
		case NodeKind::Name: {
			return append(builder, ((Name *)node)->name);
		}
		case NodeKind::BuiltinTypeName: {
			switch (((BuiltinTypeName *)node)->type_kind) {
#define x(name) case BuiltinType::name: return append(builder, #name);
				ENUMERATE_BUILTIN_TYPES(x)
#undef x
			}
			return append(builder, "(unknown BuiltinTypeName)");
		}
		case NodeKind::LambdaHead: {
			auto head = (LambdaHead *)node;

			umm result = 0;
			auto write = [&] (auto &&...args) {
				result += append(builder, args...);
			};

			write('(');
			for (auto &parameter : head->parameters_block.definition_list) {
				if (&parameter != head->parameters_block.definition_list.data) {
					write(", ");
				}

				write(parameter->name);
				write(": ");
				write(parameter->type);
			}
			write(") ");
			write(head->return_type);

			return result;
		}
		case NodeKind::Unary: {
			auto unary = (Unary *)node;
			if (unary->operation == UnaryOperation::pointer) {
				return append_format(builder, "*{} {}", unary->mutability, unary->expression);
			}
			break;
		}
		case NodeKind::Struct: {
			auto Struct = (::Struct *)node;
			if (Struct->definition)
				return append(builder, Struct->definition->name);
			else
				return append(builder, "struct");
			break;
		}
		case NodeKind::ArrayType: {
			auto arr = (ArrayType *)node;
			
			return 
				append(builder, '[') +
				append(builder, arr->count.value()) +
				append(builder, ']') +
				append(builder, arr->element_type);

			break;
		}
	}
	return append(builder, "(unknown)");
}

#if ENABLE_NOTE_LEAK

GList<String> leaks;

void note_leak(String expression, Node *node, String message = {}, std::source_location location = std::source_location::current()) {
	if (message)
		leaks.add(format(u8"{} ({}) at {}:{} - {}. {}", expression, node->kind, location.file_name(), location.line(), location.function_name(), message));
	else 
		leaks.add(format(u8"{} ({}) at {}:{} - {}", expression, node->kind, location.file_name(), location.line(), location.function_name()));
}

#define NOTE_LEAK(node, ...) note_leak(u8#node##s, node, __VA_ARGS__)

#else

#define NOTE_LEAK(node, ...)

#endif

Block global_block;
SpinLock global_block_lock;

struct Imports {
	struct FileToImport {
		String path;
		String location;
	};
	GList<FileToImport> files_to_import;
	GHashSet<String> imported_files;

	void add_file(Imports::FileToImport file_to_import) {
		if (!imported_files.find(file_to_import.path)) {
			imported_files.insert(file_to_import.path, {});
			files_to_import.add(file_to_import);
		}
	}
};
LockProtected<Imports, SpinLock> imports;

Optional<Mutability> to_mutability(TokenKind token_kind) {
	switch (token_kind) {
		case Token_const: return Mutability::constant;
		case Token_let:   return Mutability::readonly;
		case Token_var:   return Mutability::variable;
	}
	return {};
}

bool is_expression(Node *node) {
	if (auto block = as<Block>(node)) {
		if (block->children.count == 0)
			return false;
		return is_expression(block->children.back());
	}

	return as<Expression>(node);
}

bool is_substitutable(Block *block) {
	return block->children.count == 1 && block->breaks.count == 0;
}

LockProtected<GList<ReusableFiber>, SpinLock> fibers_to_reuse;
u32 allocated_fiber_count;

ReusableFiber get_new_fiber() {
	return locked_use_ret(fibers_to_reuse) {
		if (auto popped = fibers_to_reuse.pop()) {
			return popped.value();
		} else {
			atomic_increment(&allocated_fiber_count);
			return create_reusable_fiber();
		}
	};
}

void add_fiber_to_reuse(ReusableFiber fiber) {
	locked_use(fibers_to_reuse) {
		fibers_to_reuse.add(fiber);
	};
}

#include "lexer.h"
#include "parser.h"

bool parse_source(String source, auto on_parse_global_node) {
	#if ENABLE_ASSERTIONS
	locked_use(content_start_to_file_name) {
		assert(content_start_to_file_name.find(source.data));
	};
	#endif

	Parser parser = {};
	parser.init(source);
	defer { 
		parser.reporter.print_all(); 
		parser.free();
	};

	Node *node = 0;
	while (node = parser.parse_next_node()) {
		on_parse_global_node(node);
	}

	return parser.last_yield_result == Parser::YieldResult::success;
}
bool read_file_and_parse_into_global_block(String import_location, String path) {
	timed_function();

	if (!file_exists(path)) {
		immediate_reporter.error(import_location, "File {} does not exist", path);
		return false;
	}

	// Will be used after function exits, don't free.
	auto source_buffer = read_entire_file(path, {.extra_space_before = 1, .extra_space_after = 1});

	// Null-terminate from both sides.
	// At the end to
	auto source = (String)source_buffer.subspan(1, source_buffer.count - 2);
	
	locked_use(content_start_to_file_name) {
		content_start_to_file_name.get_or_insert(source.data) = path;
	};

	bool success = parse_source(source, [&](Node *node) {
		scoped(global_block_lock);
		global_block.add(node);
	});
	
	if (!success) {
		LOG_ERROR_PATH("Failed to parse this file: {}", path);
		return false;
	}

	return true;
}

enum class YieldResult : u8 {
	fail,
	success,
	wait,
};
bool no_more_progress = false;

ValueKind to_value_kind(Type type) {
	type = direct(type);
	switch (type->kind) {
		case NodeKind::BuiltinTypeName: {
			REDECLARE_VAL(type, (BuiltinTypeName *)type);
			switch (type->type_kind) {
				case BuiltinType::Type:   return ValueKind::Type;
				case BuiltinType::U8:     return ValueKind::U8;
				case BuiltinType::U16:    return ValueKind::U16;
				case BuiltinType::U32:    return ValueKind::U32;
				case BuiltinType::U64:    return ValueKind::U64;
				case BuiltinType::S8:     return ValueKind::S8;
				case BuiltinType::S16:    return ValueKind::S16;
				case BuiltinType::S32:    return ValueKind::S32;
				case BuiltinType::S64:    return ValueKind::S64;
				case BuiltinType::Bool:   return ValueKind::Bool;
				case BuiltinType::String: return ValueKind::String;
			}
			break;
		}
		case NodeKind::ArrayType: { return ValueKind::array; }
	}
	invalid_code_path("to_value_kind: can't convert from {}", type->kind);
}

void default_initialize(Value *value, Type type) {
	value->kind = to_value_kind(type);
	switch (value->kind) {
		case ValueKind::none: { return; }
		case ValueKind::U8: { value->U8 = 0; return; }
		case ValueKind::U16: { value->U16 = 0; return; }
		case ValueKind::U32: { value->U32 = 0; return; }
		case ValueKind::U64: { value->U64 = 0; return; }
		case ValueKind::S8: { value->S8 = 0; return; }
		case ValueKind::S16: { value->S16 = 0; return; }
		case ValueKind::S32: { value->S32 = 0; return; }
		case ValueKind::S64: { value->S64 = 0; return; }
		case ValueKind::Bool: { value->Bool = false; return; }
		case ValueKind::String: { value->String = {}; return; }
		case ValueKind::lambda: { value->lambda = {}; return; }
		case ValueKind::Type: { value->Type = {}; return; }
		case ValueKind::pointer: { value->pointer = {}; return; }
		case ValueKind::struct_: { 
			auto struct_ = direct_as<Struct>(type);
			assert(struct_);
			value->elements = {};
			value->elements.resize(struct_->members.count);
			for (umm i = 0; i < struct_->members.count; ++i) {
				default_initialize(&value->elements[i], struct_->members[i]->type);
			}
			return; 
		}
		case ValueKind::array: {
			auto array = direct_as<ArrayType>(type);
			assert(array);
			value->elements = {};
			value->elements.resize(array->count.value());
			for (umm i = 0; i < array->count.value(); ++i) {
				default_initialize(&value->elements[i], array->element_type);
			}
			return;
		}
	}
	invalid_code_path("default_initialize: invalid value kind {}", value->kind);
}

decltype(auto) element_at(auto &&collection, Value index) {
	switch (index.kind) {
		case ValueKind::U8: return collection[index.U8];
		case ValueKind::U16: return collection[index.U16];
		case ValueKind::U32: return collection[index.U32];
		case ValueKind::U64: return collection[index.U64];
		case ValueKind::S8: return collection[index.S8];
		case ValueKind::S16: return collection[index.S16];
		case ValueKind::S32: return collection[index.S32];
		case ValueKind::S64: return collection[index.S64];
		default: invalid_code_path("invalid index kind: {}", index.kind);
	}
}

Unary *as_pointer(Type type) {
	if (auto unary = as<Unary>(type); unary && unary->operation == UnaryOperation::pointer) {
		return unary;
	}
	return 0;
}


Value zero_of_type(Type type) {
	Value result = {};
	auto direct_type = direct(type);
	if (auto struct_ = as<Struct>(direct_type)) {
		for (int i = 0; i < struct_->members.count; ++i) {
			result.elements.add(zero_of_type(struct_->members[i]->type));
		}
	} 
	else if (types_match(direct_type, BuiltinType::Bool)) { result = Value(false); }
	else if (types_match(direct_type, BuiltinType::U8)) { result = Value((u8)0); }
	else if (types_match(direct_type, BuiltinType::U16)) { result = Value((u16)0); }
	else if (types_match(direct_type, BuiltinType::U32)) { result = Value((u32)0); }
	else if (types_match(direct_type, BuiltinType::U64)) { result = Value((u64)0); }
	else if (types_match(direct_type, BuiltinType::S8)) { result = Value((s8)0); }
	else if (types_match(direct_type, BuiltinType::S16)) { result = Value((s16)0); }
	else if (types_match(direct_type, BuiltinType::S32)) { result = Value((s32)0); }
	else if (types_match(direct_type, BuiltinType::S64)) { result = Value((s64)0); }
	else if (types_match(direct_type, BuiltinType::String)) { result = Value(String{}); }
	else if (types_match(direct_type, BuiltinType::UnsizedInteger)) { result = Value(unsized_integer_tag, 0); }
	else if (auto pointer = as_pointer(direct_type)) { result = Value((Value *)0); }
	else {
		invalid_code_path("zero_of_type({}) is invalid", direct_type);
	}
	return result;
}

#include "node_interpreter.h"

std::pair<Lambda *, LambdaHead *> get_lambda_and_head(Expression *expression) {
	auto directed = direct(expression);
	auto lambda = as<Lambda>(directed);
	LambdaHead *head = 0;
	if (lambda) {
		head = &lambda->head;
	} else {
		if (auto definition = as<Definition>(directed)) {
			head = direct_as<LambdaHead>(definition->type);
		}
	}
	return {lambda, head};
}
std::tuple<Lambda *, LambdaHead *, Struct *> get_lambda_and_head_or_struct(Expression *expression) {
	auto directed = direct(expression);
	auto lambda = as<Lambda>(directed);
	auto struct_ = as<Struct>(directed);
	LambdaHead *head = 0;
	if (lambda) {
		head = &lambda->head;
	} else {
		head = direct_as<LambdaHead>(expression->type);
	}
	return {lambda, head, struct_};
}

#include "bytecode/builder.h"
#include "bytecode/interpreter.h"

namespace Bytecode {

Callback generate_callback(Lambda *lambda) {
	// arg0 - rcx
	// arg1 - rdx
	// arg2 - r8
	// arg3 - r9
	assert(lambda->head.parameters_block.definition_list.count == 4, "Other count of arguments not implemented");
	List<u8> bytes;
	
	constexpr u8 stack_size = 40;
	// sub rsp, 40 // 8 bytes for lambda, 32 bytes for shadow space -----, 8 dummy bytes to keep stack aligned
	bytes.add({0x48, 0x83, 0xec, stack_size});

	// mov r10, lambda
	bytes.add({0x49, 0xba});
	bytes.add(value_as_bytes(lambda));

	// mov r11, ffi_callback
	bytes.add({0x49, 0xbb});
	bytes.add(value_as_bytes(&ffi_callback));
	
	// mov qword ptr[rsp+32], r10
	bytes.add({0x4c, 0x89, 0x54, 0x24, 32});

	// call r11
	bytes.add({0x41, 0xff, 0xd3});
	
	// add rsp, 40
	bytes.add({0x48, 0x83, 0xc4, stack_size});

	// ret
	bytes.add({0xc3});


	#if OS_WINDOWS
	void *page = VirtualAlloc(0, bytes.count, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	#elif OS_LINUX
	void *page = mmap(NULL, bytes.count, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	#endif

	memcpy(page, bytes.data, bytes.count);
	memset((char *)page + bytes.count, 0xcc, ceil(bytes.count, (umm)4096) - bytes.count);

	#if OS_WINDOWS
	DWORD old_protect;
	if (!VirtualProtect(page, bytes.count, PAGE_EXECUTE_READ, &old_protect)) {
		immediate_reporter.error(lambda->location, "FATAL: VirtualProtect failed: {}", win32_error());
		exit(-1);
	}
	#elif OS_LINUX
    if (mprotect(page, bytes.count, PROT_READ | PROT_EXEC) == -1) {
		immediate_reporter.error(lambda->location, "FATAL: mprotect failed: {}", strerror(errno));
    }
	#endif

	return {page};
}

}


Type make_pointer(Type type, Mutability mutability) {
	auto pointer = Unary::create();
	pointer->expression = type;
	pointer->operation = UnaryOperation::pointer;
	pointer->mutability = mutability;
	pointer->type = get_builtin_type(BuiltinType::Type);
	return pointer;
}

struct CheckResult {
	bool result = {};
	Node *failed_node = {};

	CheckResult(bool result) : result(result) {}
	CheckResult(bool result, Node *failed_node) : result(result), failed_node(failed_node) {}

	operator bool() { return result; }
};

#define MUST_BE_CONSTANT(node) \
	if (auto _ = is_constant(node); !_) \
		return _

CheckResult is_constant(Expression *expression);
CheckResult is_constant_impl(Block *block) {
	if (block->children.count != 1) {
		immediate_reporter.error(block->location, "is_constant_impl: not implemented");
		invalid_code_path();
	}

	auto last_expression = as<Expression>(block->children.back());
	assert(last_expression, "not implemented");

	return is_constant(last_expression);
}
CheckResult is_constant_impl(Definition *definition) {
	if (definition->mutability == Mutability::constant)
		return true;

	return {false, definition};
}
CheckResult is_constant_impl(IntegerLiteral *literal) { return true; }
CheckResult is_constant_impl(BooleanLiteral *literal) { return true; }
CheckResult is_constant_impl(NoneLiteral *literal) { return true; }
CheckResult is_constant_impl(StringLiteral *literal) { return true; }
CheckResult is_constant_impl(Lambda *lambda) { return true; }
CheckResult is_constant_impl(LambdaHead *head) { return true; }
CheckResult is_constant_impl(Name *name) { 
	auto definition = name->definition();
	assert(definition);
	return is_constant_impl(definition);
}
CheckResult is_constant_impl(Call *call) { 
	MUST_BE_CONSTANT(call->callable);

	for (auto argument : call->arguments) {
		MUST_BE_CONSTANT(argument.expression);
	}

	return true;
}
CheckResult is_constant_impl(IfExpression *If) { 
	MUST_BE_CONSTANT(If->condition);
	MUST_BE_CONSTANT(If->true_branch);
	MUST_BE_CONSTANT(If->false_branch);
	return true;
}
CheckResult is_constant_impl(BuiltinTypeName *type) { return true; }
CheckResult is_constant_impl(Binary *binary) {
	MUST_BE_CONSTANT(binary->left);
	MUST_BE_CONSTANT(binary->right);
	return true;
}
CheckResult is_constant_impl(Match *match) {
	MUST_BE_CONSTANT(match->expression);
	for (auto &Case : match->cases) {
		MUST_BE_CONSTANT(Case.to);
	}

	return true;
}
CheckResult is_constant_impl(Unary *unary) { 
	return is_constant(unary->expression);
}
CheckResult is_constant_impl(Struct *) { return true; }
CheckResult is_constant_impl(ArrayType *) { return true; }
CheckResult is_constant_impl(Subscript *node) {
	MUST_BE_CONSTANT(node->subscriptable);
	MUST_BE_CONSTANT(node->index);
	return true;
}
CheckResult is_constant_impl(ArrayConstructor *node) {
	for (auto element : node->elements) {
		MUST_BE_CONSTANT(element);
	}
	return true;
}
CheckResult is_constant_impl(ZeroInitialized *) { return true; }
CheckResult is_constant(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return is_constant_impl((name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path();
}

#undef MUST_BE_CONSTANT

#define MUST_BE_MUTABLE(node) \
	if (auto _ = is_mutable(node); !_) \
		return _

CheckResult is_mutable(Expression *expression);
CheckResult is_mutable_impl(Block *block) { 
	if (block->children.count != 0) {
		if (auto expression = as<Expression>(block->children.back())) {
			MUST_BE_MUTABLE(expression);
			return true;
		}
	}
	return {false, block};
}
CheckResult is_mutable_impl(Definition *definition) {
	if (definition->mutability == Mutability::variable)
		return true;
	return {false, definition};
}
CheckResult is_mutable_impl(IntegerLiteral *literal) { return {false, literal}; }
CheckResult is_mutable_impl(BooleanLiteral *literal) { return {false, literal}; }
CheckResult is_mutable_impl(NoneLiteral *literal) { return {false, literal}; }
CheckResult is_mutable_impl(StringLiteral *literal) { return {false, literal}; }
CheckResult is_mutable_impl(Lambda *lambda) { return {false, lambda}; }
CheckResult is_mutable_impl(LambdaHead *head) { return {false, head}; }
CheckResult is_mutable_impl(Name *name) { 
	auto definition = name->definition();
	assert(definition);
	return is_mutable_impl(definition);
}
CheckResult is_mutable_impl(Call *call) { return {false, call}; }
CheckResult is_mutable_impl(IfExpression *If) { return {false, If}; }
CheckResult is_mutable_impl(BuiltinTypeName *type) { return {false, type}; }
CheckResult is_mutable_impl(Binary *binary) {
	if (binary->operation == BinaryOperation::dot) {
		if (auto pointer = direct_as<Unary>(binary->left->type); pointer && pointer->operation == UnaryOperation::pointer) {
			if (pointer->mutability == Mutability::variable) {
				return true;
			}
		} else {
			return is_mutable(binary->left);
		}
	}
	return {false, binary};
}
CheckResult is_mutable_impl(Match *match) { return {false, match}; }
CheckResult is_mutable_impl(Unary *unary) {
	if (unary->operation == UnaryOperation::dereference) {
		auto pointer = as_pointer(unary->expression->type);
		assert(pointer);
		if (pointer->mutability == Mutability::variable) {
			return true;
		}
	}

	return {false, unary};
}
CheckResult is_mutable_impl(Struct *Struct) { return {false, Struct}; }
CheckResult is_mutable_impl(ArrayType *Array) { return {false, Array}; }
CheckResult is_mutable_impl(Subscript *Subscript) { return is_mutable(Subscript->subscriptable); }
CheckResult is_mutable_impl(ArrayConstructor *Array) { return {false, Array}; }
CheckResult is_mutable_impl(ZeroInitialized *zi) { return {false, zi}; }
CheckResult is_mutable(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return is_mutable_impl((name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path();
}

#undef MUST_BE_MUTABLE

#define x(name) Result<Value, Node *> get_constant_value(name *node);
ENUMERATE_NODE_KIND(x)
#undef x

Result<Value, Node *> get_constant_value(Node *node);
Result<Value, Node *> get_constant_value_impl(Block *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Call *call) {
	switch (call->call_kind) {
		case CallKind::constructor: {
			Value result;
			result.kind = ValueKind::struct_;
			result.elements = {};
			for (auto argument : call->arguments) {
				auto argument_value = get_constant_value(argument.expression);
				if (argument_value.is_error()) {
					return argument_value.error();
				}
				result.elements.add(argument_value.value());
			}
			return result;
		}
	}
	return call;
}
Result<Value, Node *> get_constant_value_impl(Definition *node) {
	if (node->mutability != Mutability::constant) {
		return node;
	}
	return node->constant_value.value();
}
Result<Value, Node *> get_constant_value_impl(IntegerLiteral *node) { return Value(((IntegerLiteral *)node)->value); }
Result<Value, Node *> get_constant_value_impl(BooleanLiteral *node) { return Value(((BooleanLiteral *)node)->value); }
Result<Value, Node *> get_constant_value_impl(NoneLiteral *node) { return Value(ValueKind::none); }
Result<Value, Node *> get_constant_value_impl(StringLiteral *node) { return Value(((StringLiteral *)node)->value); }
Result<Value, Node *> get_constant_value_impl(Lambda *lambda) { return Value(lambda); }
Result<Value, Node *> get_constant_value_impl(LambdaHead *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Name *node) { 
	auto definition = node->definition();
	if (!definition) {
		return node;
	}
	return get_constant_value_impl(definition);
}
Result<Value, Node *> get_constant_value_impl(IfStatement *node) { return node; }
Result<Value, Node *> get_constant_value_impl(IfExpression *node) { return node; }
Result<Value, Node *> get_constant_value_impl(BuiltinTypeName *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Binary *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Match *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Unary *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Struct *node) { return node; }
Result<Value, Node *> get_constant_value_impl(ArrayType *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Subscript *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Return *node) { return node; }
Result<Value, Node *> get_constant_value_impl(While *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Continue *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Break *node) { return node; }
Result<Value, Node *> get_constant_value_impl(ArrayConstructor *node) {
	Value result;
	result.kind = ValueKind::array;
	result.elements = {};
	result.elements.reserve(node->elements.count);
	for (auto element : node->elements) {
		if (auto element_value = get_constant_value(element)) {
			result.elements.add(element_value.value());
		} else {
			return element_value;
		}
	}
	return result;
}
Result<Value, Node *> get_constant_value_impl(Import *node) { return node; }
Result<Value, Node *> get_constant_value_impl(Defer *node) { return node; }
Result<Value, Node *> get_constant_value_impl(ZeroInitialized *zi) { return zero_of_type(zi->type); }
Result<Value, Node *> get_constant_value(Node *node) {
	scoped_replace(debug_current_location, node->location);
	switch (node->kind) {
#define x(name) case NodeKind::name: return get_constant_value_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
	invalid_code_path("get_constant_value: Invalid node kind {}", node->kind);
}

Result<s64, Node *> get_constant_integer(Node *node) {
	auto result = get_constant_value(node);
	if (result.is_error())
		return result.error();

	auto value = result.value();
	switch (value.kind) {
		case ValueKind::UnsizedInteger: return value.UnsizedInteger;
		case ValueKind::U8:             return value.U8;
		case ValueKind::U16:            return value.U16;
		case ValueKind::U32:            return value.U32;
		case ValueKind::U64:            return value.U64;
		case ValueKind::S8:             return value.S8;
		case ValueKind::S16:            return value.S16;
		case ValueKind::S32:            return value.S32;
		case ValueKind::S64:            return value.S64;
	}

	return node;
}

LockProtected<List<struct Typechecker *>, SpinLock> retired_typecheckers;

#define locked_use_it(protected, expr) protected.use([&](auto &it) { return expr; })

volatile u32 typechecker_uid_counter;

// TODO: this is redundant
enum class TypecheckEntryStatus : u8 {
	unstarted,
	unfinished,
	succeeded,
	failed,
};

struct TypecheckEntry {
	Node** node = 0;
	Typechecker* typechecker = 0;
	TypecheckEntryStatus status = TypecheckEntryStatus::unstarted;

	TypecheckEntry *dependency = 0;
};

StaticBlockList<TypecheckEntry, 256> typecheck_entries;

LockProtected<List<Report>, SpinLock> deferred_reports;

struct BinaryTypecheckerKey {
	Expression *left_type = 0;
	Expression *right_type = 0;
	BinaryOperation operation = {};

	constexpr auto operator<=>(BinaryTypecheckerKey const &) const = default;
};

template <>
u64 get_hash(BinaryTypecheckerKey const &key) {
	return (u64)key.left_type ^ rotate_left((u64)key.left_type, 21) ^ rotate_left((u64)key.operation, 42);
}

struct Copier {
	HashMap<Node *, Node *> copied_nodes;

#define COPY(x) to->x = from->x
#define DEEP_COPY(x) to->x = deep_copy(from->x)
#define DEEP_COPY_INPLACE(x) deep_copy(&from->x, &to->x)
#define LOOKUP_COPY(x)                               \
	if (auto found = copied_nodes.find(from->x)) {   \
		assert(found->value->kind == from->x->kind); \
		to->x = autocast found->value;               \
	} else {                                         \
		to->x = from->x;                             \
	}
#define COPY_LIST(x, COPY_MODE)               \
	to->x.resize(from->x.count);              \
	for (umm i = 0; i < from->x.count; ++i) { \
		COPY_MODE(x[i]);                      \
	}
	
	template <class T>
	[[nodiscard]] void copy_base(T *from, T *to) {
		copied_nodes.get_or_insert(from) = to;
		to->location = from->location;
	}
	template <class T>
	[[nodiscard]] T *copy_base(T *from) {
		auto to = T::create();
		copy_base(from, to);
		return to;
	}
	
	template <class T>
	[[nodiscard]] void deep_copy(T *from, T *to) {
		copy_base(from, to);
		deep_copy_impl(from, to);
		if (auto to_expression = as<Expression>(to)) {
			auto from_expression = as<Expression>(from);
			assert(from_expression);
			assert((bool)from_expression->type == (bool)to_expression->type);
		}
	}
	template <class T>
	[[nodiscard]] T *deep_copy(T *from) {
		auto to = T::create();
		deep_copy(from, to);
		return to;
	}

	[[nodiscard]] Node *deep_copy(Node *from) {
		switch (from->kind) {
#define x(name) case NodeKind::name: return deep_copy((name *)from);
			ENUMERATE_NODE_KIND(x)
#undef x
		}
		invalid_code_path();
	}
	[[nodiscard]] Expression *deep_copy(Expression *from) {
		switch (from->kind) {
#define x(name) case NodeKind::name: return deep_copy((name *)from);
			ENUMERATE_EXPRESSION_KIND(x)
#undef x
		}
		invalid_code_path();
	}
	[[nodiscard]] Statement *deep_copy(Statement *from) {
		switch (from->kind) {
#define x(name) case NodeKind::name: return deep_copy((name *)from);
			ENUMERATE_STATEMENT_KIND(x)
#undef x
		}
		invalid_code_path();
	}
	[[nodiscard]] Call::Argument deep_copy(Call::Argument from) {
		Call::Argument to = from;
		deep_copy_impl(&from, &to);
		return to;
	}

	void deep_copy_impl(Block *from, Block *to) {
		LOOKUP_COPY(parent);

		for (auto from_child : from->children) {
			auto to_child = deep_copy(from_child);
			to->add(to_child);
		}

		COPY_LIST(defers, LOOKUP_COPY);
		LOOKUP_COPY(type);
	} 
	void deep_copy_impl(Call *from, Call *to) {
		DEEP_COPY(callable);
		COPY_LIST(arguments, DEEP_COPY);
		COPY(inline_status);
		COPY(call_kind);
		LOOKUP_COPY(type);
	}
	void deep_copy_impl(Call::Argument *from, Call::Argument *to) {
		DEEP_COPY(expression);
		LOOKUP_COPY(parameter);
		to->name = from->name;
	}
	void deep_copy_impl(Definition *from, Definition *to) {
		COPY(name);
		if (from->parsed_type)
			DEEP_COPY(parsed_type);
		if (from->initial_value)
			DEEP_COPY(initial_value);

		COPY(is_parameter);
		COPY(is_template_parameter);
		COPY(mutability);

		LOOKUP_COPY(container);
		LOOKUP_COPY(type);
	} 
	void deep_copy_impl(IntegerLiteral *from, IntegerLiteral *to) {
		COPY(value);
		COPY(type); // builtin, no need to look up
	} 
	void deep_copy_impl(BooleanLiteral *from, BooleanLiteral *to) {
		COPY(value);
		COPY(type); // builtin, no need to look up
	} 
	void deep_copy_impl(NoneLiteral *from, NoneLiteral *to) {
		COPY(type); // builtin, no need to look up
	} 
	void deep_copy_impl(StringLiteral *from, StringLiteral *to) {
		COPY(value);
		COPY(type); // builtin, no need to look up
	} 
	void deep_copy_impl(Lambda *from, Lambda *to) {
		COPY(inline_status);
		COPY(is_intrinsic);
		DEEP_COPY_INPLACE(head);
		DEEP_COPY(body);
		LOOKUP_COPY(definition);

		// returns will be updated by deep_copy_impl(Return)
		
		LOOKUP_COPY(type);
	} 
	void deep_copy_impl(LambdaHead *from, LambdaHead *to) {
		DEEP_COPY_INPLACE(template_parameters_block);
		DEEP_COPY_INPLACE(parameters_block);
		if (from->parsed_return_type) {
			DEEP_COPY(parsed_return_type);
		}
		LOOKUP_COPY(return_type);
		COPY(is_template);
		COPY(type); // builtin, no need to look up
	} 
	void deep_copy_impl(Name *from, Name *to) {
		COPY(name);
		COPY_LIST(possible_definitions, LOOKUP_COPY);
		LOOKUP_COPY(type);
	} 
	void deep_copy_impl(IfStatement *from, IfStatement *to) {
		DEEP_COPY(condition);
		DEEP_COPY(true_branch);
		if (from->false_branch)
			DEEP_COPY(false_branch);
	} 
	void deep_copy_impl(IfExpression *from, IfExpression *to) {
		DEEP_COPY(condition);
		DEEP_COPY(true_branch);
		DEEP_COPY(false_branch);
		LOOKUP_COPY(type);
	} 
	void deep_copy_impl(BuiltinTypeName *from, BuiltinTypeName *to) {
		COPY(type_kind);
		COPY(type); // builtin, no need to look up
	} 
	void deep_copy_impl(Binary *from, Binary *to) {
		DEEP_COPY(left);
		DEEP_COPY(right);
		COPY(operation);
		LOOKUP_COPY(type);
	} 
	void deep_copy_impl(Match *from, Match *to) {
		DEEP_COPY(expression);
		to->cases.resize(from->cases.count);
		for (umm i = 0; i < to->cases.count; ++i) {
			if (from->cases[i].from)
				DEEP_COPY(cases[i].from);
			DEEP_COPY(cases[i].to);
		}
		LOOKUP_COPY(type);
	} 
	void deep_copy_impl(Unary *from, Unary *to) {
		DEEP_COPY(expression);
		COPY(operation);
		COPY(mutability);
		LOOKUP_COPY(type);
	} 
	void deep_copy_impl(Return *from, Return *to) {
		DEEP_COPY(value);
		LOOKUP_COPY(lambda);
		to->lambda->returns.add(to);
	}
	void deep_copy_impl(While *from, While *to) {
		DEEP_COPY(condition);
		DEEP_COPY(body);
	} 
	void deep_copy_impl(Continue *from, Continue *to) {
		LOOKUP_COPY(loop);
	} 
	void deep_copy_impl(Break *from, Break *to) {
		LOOKUP_COPY(tag_block);
		LOOKUP_COPY(loop);
	}
	void deep_copy_impl(Struct *from, Struct *to) { not_implemented(); }
	void deep_copy_impl(ArrayType *from, ArrayType *to) { not_implemented(); }
	void deep_copy_impl(Subscript *from, Subscript *to) { not_implemented(); }
	void deep_copy_impl(ArrayConstructor *from, ArrayConstructor *to) {
		COPY_LIST(elements, DEEP_COPY);
		LOOKUP_COPY(type);
	}
	void deep_copy_impl(Import *from, Import *to) {
		COPY(path);
	}
	void deep_copy_impl(Defer *from, Defer *to) {
		DEEP_COPY(body);
	}
	void deep_copy_impl(ZeroInitialized *from, ZeroInitialized *to) {
		LOOKUP_COPY(type);
	}

#undef LOOKUP_COPY
#undef DEEP_COPY
#undef COPY

};

inline String get_non_block_location(Node *node) {
	while (auto block = as<Block>(node)) {
		if (!block->children.count) {
			break;
		}
		node = block->children.back();
	}
	return node->location;
}

inline bool do_all_paths_return(Node *node);
inline bool do_all_paths_return_impl(Block *block) {
	for (auto child : block->children) {
		if (do_all_paths_return(child)) {
			return true;
		}
	}
	return false;
}
inline bool do_all_paths_return_impl(Call *call) {
	if (do_all_paths_return(call->callable)) {
		return true;
	}
	for (auto argument : call->arguments) {
		if (do_all_paths_return(argument.expression)) {
			return true;
		}
	}
	return false;
}
inline bool do_all_paths_return_impl(Definition *definition) {
	if (definition->initial_value) {
		if (do_all_paths_return(definition->initial_value)) {
			return true;
		}
	}
	return false;
}
inline bool do_all_paths_return_impl(IntegerLiteral *literal) { return false; }
inline bool do_all_paths_return_impl(BooleanLiteral *literal) { return false; }
inline bool do_all_paths_return_impl(NoneLiteral *literal) { return false; }
inline bool do_all_paths_return_impl(StringLiteral *literal) { return false; }
inline bool do_all_paths_return_impl(Lambda *lambda) { return false; }
inline bool do_all_paths_return_impl(LambdaHead *head) { return false; }
inline bool do_all_paths_return_impl(Name *name) { return false; }
inline bool do_all_paths_return_impl(IfExpression *If) {
	if (do_all_paths_return(If->condition)) {
		return true;
	}
	if (do_all_paths_return(If->true_branch) && do_all_paths_return(If->false_branch)) {
		return true;
	}
	return false;
}
inline bool do_all_paths_return_impl(BuiltinTypeName *name) { return false; }
inline bool do_all_paths_return_impl(Binary *bin) {
	if (do_all_paths_return(bin->left)) {
		return true;
	}
	if (do_all_paths_return(bin->right)) {
		return true;
	}
	return false;
}
inline bool do_all_paths_return_impl(Match *match) {
	if (do_all_paths_return(match->expression)) {
		return true;
	}
	for (auto Case : match->cases) {
		if (do_all_paths_return(Case.to)) {
			return true;
		}
	}
	return false;
}
inline bool do_all_paths_return_impl(Unary *un) {
	if (do_all_paths_return(un->expression)) {
		return true;
	}
	return false;
}
inline bool do_all_paths_return_impl(Struct *Struct) { return false; }
inline bool do_all_paths_return_impl(ArrayType *arr) {
	if (do_all_paths_return(arr->count_expression)) {
		return true;
	}
	if (do_all_paths_return(arr->element_type)) {
		return true;
	}
	return false;
}
inline bool do_all_paths_return_impl(Subscript *sub) {
	if (do_all_paths_return(sub->subscriptable)) {
		return true;
	}
	if (do_all_paths_return(sub->index)) {
		return true;
	}
	return false;
}
inline bool do_all_paths_return_impl(ArrayConstructor *arr) {
	for (auto element : arr->elements) {
		if (do_all_paths_return(element)) {
			return true;
		}
	}
	return false;
}
inline bool do_all_paths_return_impl(IfStatement *If) {
	if (do_all_paths_return(If->condition)) {
		return true;
	}
	if (!If->false_branch) {
		return false;
	}
	if (do_all_paths_return(If->true_branch) && do_all_paths_return(If->false_branch)) {
		return true;
	}
	return false;
}
inline bool do_all_paths_return_impl(Return *node) {
	return true;
}
inline bool do_all_paths_return_impl(While *node) {
	// TODO: check constant condition
	return false;
}
inline bool do_all_paths_return_impl(Continue *node) { return false; }
inline bool do_all_paths_return_impl(Break *node) { return false; }
inline bool do_all_paths_return_impl(Import *node) { return false; }
inline bool do_all_paths_return_impl(Defer *node) { return false; }
inline bool do_all_paths_return_impl(ZeroInitialized *zi) { return false; }
inline bool do_all_paths_return(Node *node) {
	switch (node->kind) {
		#define x(name) case NodeKind::name: return do_all_paths_return_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
		#undef x
	}
	invalid_code_path("invalid node kind {}", node->kind);
	return false;
}

IntegerLiteral *make_integer(u64 value, String location, Type type = get_builtin_type(BuiltinType::UnsizedInteger)) {
	auto result = IntegerLiteral::create();
	result->value = value;
	result->type = type;
	result->location = location;
	return result;
}
IntegerLiteral *make_integer(u64 value, Type type = get_builtin_type(BuiltinType::UnsizedInteger)) {
	return make_integer(value, {}, type);
}
BooleanLiteral *make_boolean(bool value, String location, Type type = get_builtin_type(BuiltinType::Bool)) {
	auto result = BooleanLiteral::create();
	result->value = value;
	result->type = type;
	result->location = location;
	return result;
}
BooleanLiteral *make_boolean(bool value, Type type = get_builtin_type(BuiltinType::Bool)) {
	return make_boolean(value, {}, type);
}
StringLiteral *make_string(String value, String location) {
	auto result = StringLiteral::create();
	result->value = value;
	result->type = get_builtin_type(BuiltinType::String);
	result->location = location;
	return result;
}
StringLiteral *make_string(String value) {
	return make_string(value, {});
}

ArrayType *make_array_type(Type element_type, u64 count) {
	auto result = ArrayType::create();
	result->element_type = element_type;
	result->count = count;
	result->type = get_builtin_type(BuiltinType::Type);
	return result;
}

Expression *to_node(Value value) {
	switch (value.kind) {
		case ValueKind::U8:  return make_integer(value.U8,  get_builtin_type(BuiltinType::U8));
		case ValueKind::U16: return make_integer(value.U16, get_builtin_type(BuiltinType::U16));
		case ValueKind::U32: return make_integer(value.U32, get_builtin_type(BuiltinType::U32));
		case ValueKind::U64: return make_integer(value.U64, get_builtin_type(BuiltinType::U64));
		case ValueKind::S8:  return make_integer(value.S8,  get_builtin_type(BuiltinType::S8));
		case ValueKind::S16: return make_integer(value.S16, get_builtin_type(BuiltinType::S16));
		case ValueKind::S32: return make_integer(value.S32, get_builtin_type(BuiltinType::S32));
		case ValueKind::S64: return make_integer(value.S64, get_builtin_type(BuiltinType::S64));
		case ValueKind::UnsizedInteger: return make_integer(value.UnsizedInteger, get_builtin_type(BuiltinType::UnsizedInteger));
		case ValueKind::Bool: return make_boolean(value.Bool);
		case ValueKind::String: return make_string(value.String);
		case ValueKind::Type: return value.Type;
		default: invalid_code_path();
	}
}

Binary *make_cast(Expression *expression, Type type) {
	auto as = Binary::create();
	as->operation = BinaryOperation::as;
	as->left = expression;
	as->right = type;
	as->type = type;
	as->location = expression->location;
	return as;
}

struct VectorizedLambdaKey {
	Lambda *lambda;
	u64 vector_size;
	constexpr auto operator<=>(VectorizedLambdaKey const &) const = default;
};

struct VectorizedLambdaKeyHashTraits : DefaultHashTraits<VectorizedLambdaKey> {
	inline static constexpr u64 get_hash(VectorizedLambdaKey const &k) {
		return (u64)k.lambda ^ k.vector_size;
	}
};

struct VectorizedLambda {
	Lambda *original_lambda = 0;
	Lambda *instantiated_lambda = 0;
	Definition *instantiated_definition = 0;
	u64 vector_size;
};

LockProtected<HashMap<VectorizedLambdaKey, VectorizedLambda, VectorizedLambdaKeyHashTraits>, SpinLock> vectorized_lambdas;

struct VectorizedBinaryKey {
	Expression *left_element_type = 0;
	Expression *right_element_type = 0;
	BinaryOperation operation = {};
	u64 element_count = 0;
	constexpr auto operator<=>(VectorizedBinaryKey const &) const noexcept = default;
};

template <>
inline umm get_hash(VectorizedBinaryKey const &key) {
	Array a = {
		(u64)key.left_element_type,
		(u64)key.right_element_type,
		(u64)key.operation,
		(u64)key.element_count,
	};
	constexpr Array b = {
		(u64)0xf7b5137fe0781f5f,
		(u64)0x0011744a2c3e8509,
		(u64)0x91f27bcf27efadf7,
		(u64)0xcdd6a76381fb3c83,
	};
	return dot(a, b);
}

struct VectorizedBinaryValue {
	Definition *definition = 0;
	Lambda *lambda = 0;
};

LockProtected<HashMap<VectorizedBinaryKey, VectorizedBinaryValue>, SpinLock> vectorized_binarys;

enum class FailStrategy {
	yield,
	unwind,
};

// NOTE: jmp_buf is an array alias, which forces to use memcpy. Put it in a struct to avoid that.
struct CopyableJmpBuf {
	jmp_buf buf;
};

struct Typechecker {
	const u32 uid = atomic_add(&typechecker_uid_counter, 1);
	u32 progress = 0;

	static Typechecker *create(Node **node) {
		assert(node);

		Typechecker *typechecker = 0;
		
		if (auto popped = locked_use_it(retired_typecheckers, it.pop())) {
			typechecker = popped.value();
			// immediate_reporter.info("created cached typechecker {} for node {}", typechecker->uid, node->location);

			assert(typechecker->debug_stopped);
			assert(typechecker->yield_result == YieldResult{});
			assert(typechecker->reporter.reports.count == 0);
			assert(typechecker->initial_node == 0);
			assert(typechecker->current_block == 0);
		} else {
			typechecker = default_allocator.allocate<Typechecker>();
			++allocated_fiber_count;
			typechecker->fiber = create_fiber([](void *param) {
				((Typechecker *)param)->fiber_main(); 
			}, typechecker);

			// immediate_reporter.info("created new typechecker {} for node {}", typechecker->uid, node->location);
		}

		typechecker->reporter.reports.clear();
		typechecker->current_block = &global_block;
		typechecker->initial_node = node;
		typechecker->debug_stopped = true;
		return typechecker;
	}

	YieldResult continue_typechecking(TypecheckEntry *entry) {
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

	void stop() {
		debug_stop();
	}
	void retire() {
		parent_fiber = {};
		yield_result = {};
		reporter.reports.clear();
		initial_node = 0;
		current_block = 0;

		locked_use_it(retired_typecheckers, it.add(this));
	}

private:
	Fiber parent_fiber = {};
	Fiber fiber = {};
	YieldResult yield_result = {};
	Reporter reporter;
	Node **initial_node = 0;
	Block *current_block = 0;
	Expression *current_container = 0;
	While *current_loop = 0;
	List<Node *> node_stack;
	TypecheckEntry *entry = 0;
	CopyableJmpBuf main_loop_unwind_point = {};
	CopyableJmpBuf current_unwind_point = {};
	bool can_generate_vectorized_lambdas = true;

	static constexpr int fail_unwind_tag = 42;

	FailStrategy fail_strategy = FailStrategy::yield;

	void fail_impl() {
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

	auto with_unwind_strategy(auto &&fn) -> decltype(fn()) {
		scoped_replace(fail_strategy, FailStrategy::unwind);
		auto saved_unwind_point = current_unwind_point;
		defer { 
			current_unwind_point = saved_unwind_point;
		};
		if (setjmp(current_unwind_point.buf) == fail_unwind_tag) {
			return decltype(fn()){};
		}
		return fn();
	}

	[[nodiscard]]
	bool yield_while(String location, auto predicate) {
		while (true) {
			if (predicate()) {
				if (report_yields)
					immediate_reporter.info(location, "Yield");

				yield_smt();
				switch_thread();

				yield(YieldResult::wait);

				if (no_more_progress)
					return false;
			} else {
				return true;
			}
		}
	}

	[[nodiscard]] 
	bool yield_while(auto predicate) {
		return yield_while({}, predicate);
	}

	template <class T>
	[[nodiscard]] 
	bool yield_while_null(String location, T **pointer) {
		return yield_while(location, [&] {
			return !*pointer;
		});
	}

	struct Unwind {};

	void yield(YieldResult result) {
		yield_result = result;
		scoped_replace(debug_current_location, {});
		tl::yield(parent_fiber);
		if (result == YieldResult::fail) {
			longjmp(main_loop_unwind_point.buf, 1);
		}
	}

	void fiber_main() {
		while (1) {
			setjmp(main_loop_unwind_point.buf);
			assert(initial_node);
			can_generate_vectorized_lambdas = true;
			*initial_node = typecheck(*initial_node, true);
			yield(YieldResult::success);
		}
	}

	bool implicitly_cast(Expression **_expression, Expression *target_type, Reporter *reporter, bool apply) {
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
											expression->type = target_type;
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

		// String -> *U8
		if (types_match(direct_source_type, BuiltinType::String)) {
			if (auto target_pointer = as_pointer(direct_target_type)) {
				if (types_match(target_pointer->expression, BuiltinType::U8)) {
					if (apply) {
						expression = make_cast(expression, target_type);
					}
					return true;
				} else {
					reporter->error(expression->location, "`String` can only be converted to `*U8`, but you provided `{}`", target_type);
					return false;
				}
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
	bool implicitly_cast(Expression **expression, Expression *target_type, bool apply) {
		return implicitly_cast(expression, target_type, &reporter, apply);
	}

	void why_is_this_immutable(Expression *expr) {
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

	Expression *inline_body(Call *call, Lambda *lambda) {
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

	Name *get_bottom_name(Node *node) {
		while (true) {
			switch (node->kind) {
				case NodeKind::Name:
					return (Name *)node;
				case NodeKind::Block: {
					auto block = (Block *)node;
					if (block->children.count == 0) {
						return 0;
					}

					node = block->children.back();
					continue;
				}
				default:
					return 0;
			}
		}
	}

	Value execute(Node *node) {
		auto context = NodeInterpreter::create(node);
		while (true) {
			auto result = context->run();
			if (result.is_value()) {
				return result.value();
			}

			switch (result.error()) {
				case YieldResult::fail: {
					reporter.error(node->location, "Failed to evaluate value.");
					yield(YieldResult::fail);
					break;
				}
				case YieldResult::wait: {
					yield(YieldResult::wait);
					break;
				}
			}
		}
	}

	VectorizedLambda get_or_instantiate_vectorized_lambda(Lambda *original_lambda, u64 vector_size, String instantiation_location) {
		return locked_use(vectorized_lambdas) {
			VectorizedLambda vectorized = {};
			auto found = vectorized_lambdas.find({ original_lambda, vector_size });
			if (found) {
				vectorized = found->value;
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
				for (auto [i, parameter] : enumerate(original_lambda->head.parameters_block.definition_list)) {
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
				
				for (auto [i, parameter] : enumerate(original_lambda->head.parameters_block.definition_list)) {
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

				locked_use(content_start_to_file_name) {
					content_start_to_file_name.get_or_insert(source.data) = lambda_name;
				};

				Node *definition_node = 0;

				bool success = parse_source(source, [&](Node *node) {
					assert(!definition_node, "Only one node expected");
					definition_node = node;
					scoped(global_block_lock);
					global_block.add(node);
				});

				can_generate_vectorized_lambdas = false;
				success &= with_unwind_strategy([&] {
					return typecheck(&definition_node);
				});
				can_generate_vectorized_lambdas = true;

				{
					with(temporary_storage_checkpoint);
					auto path = tformat(u8"{}\\{}.sp", generated_source_directory, lambda_name);
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
				return found->value;
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

			withs(global_block_lock) {
				global_block.add(new_lambda_definition);
			};

			vectorized_lambdas.insert({ original_lambda, vector_size }, new_lambda_definition);
			return new_lambda_definition;
			#endif
		};
	}

	struct SortArgumentOptions {
		bool allow_missing = false;
	};

	void sort_arguments(GList<Call::Argument> &arguments, GList<Definition *> &parameters, String call_location, Node *lambda_head_or_struct, Definition *lambda_or_struct_definition, SortArgumentOptions options = {}) {
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

		if (!options.allow_missing) {
			for (umm i = 0; i < sorted_arguments.count; ++i) {
				auto &argument = sorted_arguments[i];
				auto &parameter = parameters[i];
				if (!argument.expression) {
					reporter.error(call_location, "Too few arguments. Value for {} was not provided.", parameter->name);
					reporter.info(lambda_head_or_struct->location, "Here's the {} list:", definition_element_name);
					fail();
				}
			}
		}

		arguments.set(sorted_arguments);
	}
	
	struct TemplateInstantiationForReport {
		Lambda *original_lambda;
		Lambda *instantiated_lambda;
		List<Definition *> template_parameters;
	};

	List<TemplateInstantiationForReport> template_instantiation_stack_for_reports;

	Expression *instantiate_template(Call *old_call, Lambda *lambda) {
		// Keep passed in `call` unmodified
		auto call = Copier{}.deep_copy(old_call);
		auto new_callable_name = Name::create();
		auto instantiated_lambda_definition = Definition::create();
		auto instantiated_lambda = Copier{}.deep_copy(lambda);

		auto &arguments = call->arguments;
		auto &parameters = lambda->head.parameters_block.definition_list;

		assert(arguments.count == 1, "not_implemented");
		assert(parameters.count == 1, "not_implemented");
		
		make_concrete(arguments[0].expression);

		instantiated_lambda->definition = instantiated_lambda_definition;
		instantiated_lambda->head.is_template = false;
		instantiated_lambda->head.template_parameters_block.definition_list[0]->initial_value = arguments[0].expression->type;
		instantiated_lambda->head.template_parameters_block.definition_list[0]->type = arguments[0].expression->type->type;
		instantiated_lambda->head.template_parameters_block.definition_list[0]->constant_value = Value(Type(arguments[0].expression->type));

		// Make `typecheck` not early out.
		instantiated_lambda->head.type = 0;
		instantiated_lambda->type = 0;
		call->type = 0;

		template_instantiation_stack_for_reports.add({
			.original_lambda = lambda,
			.instantiated_lambda = instantiated_lambda,
			.template_parameters = instantiated_lambda->head.template_parameters_block.definition_list,
		});

		typecheck(&instantiated_lambda);
		
		template_instantiation_stack_for_reports.pop();

		instantiated_lambda_definition->location = lambda->location;
		instantiated_lambda_definition->mutability = Mutability::constant;
		instantiated_lambda_definition->initial_value = instantiated_lambda;
		instantiated_lambda_definition->name = format(u8"{}"s, instantiated_lambda->uid);
		instantiated_lambda_definition->type = &instantiated_lambda->head;
		withs(global_block_lock) {
			global_block.add(instantiated_lambda_definition);
		};

		new_callable_name->name = instantiated_lambda_definition->name;
		new_callable_name->possible_definitions.set(instantiated_lambda_definition);
		new_callable_name->type = instantiated_lambda_definition->type;
		call->callable = new_callable_name;

		return typecheck_lambda_call(call, instantiated_lambda, &instantiated_lambda->head, true);
	}

	// Lambda can be null if it's a function pointer call
	Expression *typecheck_lambda_call(Call *call, Lambda *lambda, LambdaHead *head, bool apply = true) {

		auto &arguments = call->arguments;
		auto &callable = call->callable;

		if (lambda && lambda->head.is_template) {
			return instantiate_template(call, lambda);
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

		if (lambda) {
			Inline inline_status = {};
			if (call->inline_status == Inline::unspecified) {
				inline_status = lambda->inline_status;
			} else {
				inline_status = call->inline_status;
				if ((call->inline_status == Inline::always && lambda->inline_status == Inline::never) ||
					(call->inline_status == Inline::never && lambda->inline_status == Inline::always))
				{
					// What if that was intended? TODO: There should be an option to turn this on/off.
					reporter.warning(call->location, "Conflicting inlining specifiers: {} at call size, {} at lambda definition. The one at call site ({}) will take place.", call->inline_status, lambda->inline_status, call->inline_status);
					reporter.info(lambda->location, "Here is the lambda:");
				}
			}

			if (inline_status == Inline::always) {
				if (apply) {
					return inline_body(call, lambda);
				} else {
					return call;
				}
			}
		}

		if (apply) {
			call->call_kind = CallKind::lambda;
			call->type = head->return_type;
		}
		return call;
	};
	Expression *typecheck_constructor(Call *call, Struct *Struct) {
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

	bool typecheck_binary_dot(Binary *binary, Reporter &reporter) {
		typecheck(&binary->left);
		auto struct_ = direct_as<Struct>(binary->left->type);
		if (!struct_) {
			if (auto pointer = direct_as<Unary>(binary->left->type); pointer && pointer->operation == UnaryOperation::pointer) {
				struct_ = direct_as<Struct>(pointer->expression);
			}
		}

		if (!struct_) {
			reporter.error(binary->left->location, "Left of the dot must have struct or pointer to struct type.");
			fail();
		}

		auto member_name = as<Name>(binary->right);
		assert(member_name);

		if (auto found = find_if(struct_->members, [&](auto member) { return member->name == member_name->name; })) {
			auto definition = *found;
			member_name->possible_definitions.set(definition);
			binary->type = definition->type;
		} else {
			reporter.error(binary->right->location, "Struct {} does not contain member named {}.", struct_, member_name->name);
			fail();
		}

		return true;
	}

	bool ensure_not_overloaded(Name *name) {
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
	
	bool ensure_not_overloaded(Expression *expression) {
		if (auto name = as<Name>(expression)) {
			return ensure_not_overloaded(name);
		}
		return true;
	}

	//
	// These `typecheck` overloads automatically substitute old node with new one.
	//
	bool typecheck(Node **node) {
		*node = typecheck(*node, true);
		return *node != 0;
	}
	template <CNode T>
	bool typecheck(T **node) { 
		auto new_node = typecheck(*node, true);
		if (new_node) {
			*node = as<T>(new_node);
			assert(*node);
		}
		return new_node != 0;
	}

	//
	// This `typecheck` overload doesn't substitute the node
	//
	template <CNode T>
	bool typecheck(T &node) {
		return typecheck(&node, false) != 0;
	}

	[[nodiscard]] Node *typecheck(Node *node, bool can_substitute) {
		++progress;
		defer { ++progress; };

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
			}
		}

		return new_node;
	}
	[[nodiscard]] Expression *typecheck_impl(Block *block, bool can_substitute) {
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
						reporter.info(get_non_block_location(picked_value), "Block's type {} was deduced from this expression:", picked_value->type);
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
	[[nodiscard]] Definition *typecheck_impl(Definition *definition, bool can_substitute) {
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
			
			if (current_block == &global_block || definition->mutability == Mutability::constant) {
				auto constant_check = is_constant(definition->initial_value);
				if (!constant_check) {
					if (definition->mutability == Mutability::constant) {
						reporter.error(definition->location, "Initial value is not constant.");
					} else {
						reporter.error(definition->location, "Initial value of global variables must be constant.");
					}
					assert(constant_check.failed_node);
					reporter.info(constant_check.failed_node->location, "Because this is not constant.");
					fail();
				}

				// NOTE:
				// Maybe this should not be an error, I just don't wanna deal with it rigt now.
				if (types_match(definition->initial_value->type, BuiltinType::None)) {
					reporter.error(definition->location, "Definitions with type None can't exist.");
					fail();
				}

				definition->constant_value = execute(definition->initial_value);
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

		return definition;
	}
	[[nodiscard]] IntegerLiteral *typecheck_impl(IntegerLiteral *literal, bool can_substitute) {
		literal->type = get_builtin_type(BuiltinType::UnsizedInteger);
		return literal;
	}
	[[nodiscard]] BooleanLiteral *typecheck_impl(BooleanLiteral *literal, bool can_substitute) {
		literal->type = get_builtin_type(BuiltinType::Bool);
		return literal;
	}
	[[nodiscard]] NoneLiteral *typecheck_impl(NoneLiteral *literal, bool can_substitute) {
		literal->type = get_builtin_type(BuiltinType::None);
		return literal;
	}
	[[nodiscard]] StringLiteral *typecheck_impl(StringLiteral *literal, bool can_substitute) {
		literal->type = get_builtin_type(BuiltinType::String);
		return literal;
	}
	[[nodiscard]] LambdaHead *typecheck_impl(LambdaHead *head, bool can_substitute) {
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
	[[nodiscard]] Lambda *typecheck_impl(Lambda *lambda, bool can_substitute) {
		if (lambda->is_intrinsic) {
			if (lambda->inline_status != Inline::unspecified) {
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
								reporter.info(get_non_block_location(picked_value), "Return type {} was deduced from this expression:", picked_value->type);
								reporter.info("Here's the conversion attempt report:");
								reporter.reports.add(cast_reporter.reports);
								fail();
							}
						}

						if (lambda->returns.count && lambda->returns.count == empty_returns.count) {
							if (!types_match(lambda->body->type, BuiltinType::None)) {
								reporter.error(lambda->location, "All return statements in this lambda do not provide a value, but lambda's body has a type {}", lambda->body->type);
								reporter.info(get_non_block_location(lambda->body), "Here's the expression that is implicitly returned");
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
	[[nodiscard]] Expression *typecheck_impl(Name *name, bool can_substitute) {
		for (auto block = current_block; block; block = block->parent) {
			if (auto found_definitions = block->definition_map.find(name->name)) {
				auto definitions = found_definitions->value;
				
				if (definitions.count == 0) {
					continue;
				}

				for (auto definition : definitions) {
					auto definition_index = find_index_of(block->children, definition);
					assert(definition_index < block->children.count);

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


					if (block == &global_block) {
						for (auto &typecheck_entry : typecheck_entries) {
							if (*typecheck_entry.node == definition) {
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

					if (constant_name_inlining) {
						if (definition->mutability == Mutability::constant) {
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
	[[nodiscard]] Expression *typecheck_impl(Call *call, bool can_substitute) {
		defer { assert(call->callable->type != 0); };
		if (auto binary = as<Binary>(call->callable)) {
			if (binary->operation == BinaryOperation::dot) {
				if (auto lambda_name = as<Name>(binary->right)) {
					Reporter reporter2;
					if (with_unwind_strategy([&] { return typecheck_binary_dot(binary, reporter2); })) {
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
				return typecheck_lambda_call(call, matching_overload->lambda, matching_overload->lambda_head);
			}
			if (matching_overloads.count == 0) {
				reporter.error(call->location, "No matching overload was found.");
				for (auto [i, overload] : enumerate(overloads)) {
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
			for (auto [i, overload] : enumerate(matching_overloads)) {
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
	[[nodiscard]] Node *typecheck_impl(IfStatement *If, bool can_substitute) {
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
	[[nodiscard]] Expression *typecheck_impl(IfExpression *If, bool can_substitute) {
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
	[[nodiscard]] BuiltinTypeName *typecheck_impl(BuiltinTypeName *type, bool can_substitute) { 
		type->type = get_builtin_type(BuiltinType::Type);
		return type;
	}
	[[nodiscard]] Expression *typecheck_impl(Binary *binary, bool can_substitute) {
		if (binary->operation == BinaryOperation::dot) {
			if (!with_unwind_strategy([&] { return typecheck_binary_dot(binary, reporter); })) {
				fail();
			}
			return binary;
		} else {
			typecheck(&binary->left);
			typecheck(&binary->right);

			switch (binary->operation) {
				case BinaryOperation::ass: {
					auto result = is_mutable(binary->left);
					if (!result) {
						reporter.error(binary->left->location, "This expression can not be modified.");
						assert(result.failed_node);
						//reporter.info(result.failed_node->location, "Because this is not mutable.");
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
				return (this->*(found->value))(binary);
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
									vectorized = found->value;
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

									locked_use(content_start_to_file_name) {
										content_start_to_file_name.get_or_insert(source.data) = lambda_name;
									};

									Node *definition_node = 0;

									bool success = parse_source(source, [&](Node *node) {
										assert(!definition_node, "Only one node expected");
										definition_node = node;
										scoped(global_block_lock);
										global_block.add(node);
									});

									success &= with_unwind_strategy([&] {
										return typecheck(&definition_node);
									});
									
									{
										with(temporary_storage_checkpoint);
										auto path = tformat(u8"{}\\{}.sp", generated_source_directory, lambda_name);
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

							return call;
						}
					}
				}
			}

		no_binop:
			reporter.error(binary->location, "No binary operation {} defined for types {} and {}.", binary->operation, binary->left->type, binary->right->type);
			fail();
			return 0;
		}
	}
	[[nodiscard]] Match *typecheck_impl(Match *match, bool can_substitute) {
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
	[[nodiscard]] Expression *typecheck_impl(Unary *unary, bool can_substitute) {
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
				if (auto name = get_bottom_name(unary->expression)) {
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
	[[nodiscard]] Return *typecheck_impl(Return *return_, bool can_substitute) {
		if (return_->value)
			typecheck(&return_->value);

		return return_;
	}
	[[nodiscard]] While *typecheck_impl(While *While, bool can_substitute) {
		typecheck(&While->condition);

		scoped_replace(current_loop, While);

		if (auto builtin_type = direct_as<BuiltinTypeName>(While->condition->type); !builtin_type || builtin_type->type_kind != BuiltinType::Bool) {
			reporter.error(While->condition->location, "Condition type must be Bool.");
			fail();
		}

		typecheck(&While->body);

		return While;
	}
	[[nodiscard]] Continue *typecheck_impl(Continue *Continue, bool can_substitute) {
		assert(current_loop);
		Continue->loop = current_loop;
		return Continue;
	}
	[[nodiscard]] Break *typecheck_impl(Break *Break, bool can_substitute) {
		if (Break->value) {
			typecheck(&Break->value);
		} else {
			assert(current_loop);
			Break->loop = current_loop;
		}
		return Break;
	}
	[[nodiscard]] Struct *typecheck_impl(Struct *Struct, bool can_substitute) {
		defer {
			assert(Struct->size != -1);
		};

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
	[[nodiscard]] ArrayType *typecheck_impl(ArrayType *arr, bool can_substitute) {
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
		
		typecheck(&arr->element_type);
		if (!is_type(arr->element_type)) {
			reporter.error(arr->element_type->location, "This must be a type.");
			reporter.info(arr->location, "Because this is an array.");
			fail();
		}

		arr->type = get_builtin_type(BuiltinType::Type);
		return arr;
	}
	[[nodiscard]] Expression *typecheck_impl(Subscript *Subscript, bool can_substitute) {
		typecheck(&Subscript->subscriptable);
		auto array_type = direct_as<ArrayType>(Subscript->subscriptable->type);
		if (!array_type) {
			reporter.error(Subscript->subscriptable->location, "This must be an array.");
			fail();
		}

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
	}
	[[nodiscard]] ArrayConstructor *typecheck_impl(ArrayConstructor *arr, bool can_substitute) {
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
	[[nodiscard]] Import *typecheck_impl(Import *import, bool can_substitute) {
		return import;
	}
	[[nodiscard]] Defer *typecheck_impl(Defer *defer_, bool can_substitute) {
		typecheck(&defer_->body);
		current_block->defers.add(defer_);
		return defer_;
	}
	[[nodiscard]] ZeroInitialized *typecheck_impl(ZeroInitialized *zi, bool can_substitute) {
		invalid_code_path("ZeroInitialized cannot be typechecked.");
	}

	u32 debug_thread_id = 0;
	bool debug_stopped = false;

	void debug_start() {
		assert(debug_stopped, "attempt to start already started typechecker {}", uid);
		debug_stopped = false;
		// println("started typechecker {}", uid);
	}
	void debug_stop() {
		assert(!debug_stopped, "attempt to stop already stopped typechecker {}", uid);
		debug_stopped = true;
		// println("stopped typechecker {}", uid);
	}

public:
	/////////////////////////
	// Binary Typecheckers //
	/////////////////////////
	
	inline static GHashMap<BinaryTypecheckerKey, Expression *(Typechecker::*)(Binary *)> binary_typecheckers;

	Expression *bt_take_left(Binary *binary) {
		binary->type = binary->left->type;
		return binary;
	};
	Expression *bt_set_bool(Binary *binary) {
		binary->type = get_builtin_type(BuiltinType::Bool);
		return binary;
	};
	Expression *bt_unsized_int_and_sized_int_math(Binary *binary) {
		auto sized = binary->left;
		auto unsized = binary->right;

		if (is_concrete(unsized->type)) {
			Swap(sized, unsized);
		}

		propagate_concrete_type(unsized, sized->type);

		binary->type = sized->type;
		return binary;
	};
	Expression *bt_unsized_int_and_sized_int_comp(Binary *binary) {
		auto sized = binary->left;
		auto unsized = binary->right;

		if (is_concrete(unsized->type)) {
			Swap(sized, unsized);
		}

		propagate_concrete_type(unsized, sized->type);
		binary->type = get_builtin_type(BuiltinType::Bool);
		return binary;
	};
	Expression *bt_unsized_int(Binary *binary) {
		binary->left->type =
		binary->right->type = get_builtin_type(BuiltinType::S64);

		auto l = get_constant_value(binary->left).value();
		auto r = get_constant_value(binary->right).value();

		switch (binary->operation) {
			case BinaryOperation::add: return make_integer(l.S64 + r.S64, binary->location);
			case BinaryOperation::sub: return make_integer(l.S64 - r.S64, binary->location);
			case BinaryOperation::mul: return make_integer(l.S64 * r.S64, binary->location);
			case BinaryOperation::div: return make_integer(l.S64 / r.S64, binary->location);
			case BinaryOperation::mod: return make_integer(l.S64 % r.S64, binary->location);
			case BinaryOperation::bxo: return make_integer(l.S64 ^ r.S64, binary->location);
			case BinaryOperation::ban: return make_integer(l.S64 & r.S64, binary->location);
			case BinaryOperation::bor: return make_integer(l.S64 | r.S64, binary->location);
			case BinaryOperation::bsl: return make_integer(l.S64 << r.S64, binary->location);
			case BinaryOperation::bsr: return make_integer(l.S64 >> r.S64, binary->location);
			case BinaryOperation::equ: return make_boolean(l.S64 == r.S64, binary->location);
			case BinaryOperation::neq: return make_boolean(l.S64 != r.S64, binary->location);
			case BinaryOperation::les: return make_boolean(l.S64 < r.S64, binary->location);
			case BinaryOperation::grt: return make_boolean(l.S64 > r.S64, binary->location);
			case BinaryOperation::leq: return make_boolean(l.S64 <= r.S64, binary->location);
			case BinaryOperation::grq: return make_boolean(l.S64 >= r.S64, binary->location);
		}
		invalid_code_path("Attempt to evaluate binary {} on unsized integers. This is not supported/implemented", binary->operation);
	};

	template <bool invert>
	Expression *bt_comp_Type(Binary *binary) {
		return make_boolean(invert ^ types_match(binary->left, binary->right));
	}

	template <auto operation>
	Expression *bt_math_opt(Binary *binary) {
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

	static void init_binary_typecheckers() {
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
};

u64 get_typechecking_progress() {
	u64 result = 0;
	for (auto &entry : typecheck_entries) {
		if (entry.typechecker)
			result += entry.typechecker->progress;
	}
	return result;
}

void init_globals() {
	construct(timed_results);
	timed_results.reserve(16);
	construct(content_start_to_file_name);
	construct(retired_typecheckers);
	construct(global_block);
	construct(typecheck_entries);
	construct(deferred_reports);
	construct(vectorized_lambdas);

	//GlobalAllocator::init();
}

struct CmdArg {
	char const *key;
	Variant<
		void (*)(),
		void (*)(u64)
	> run;
};

CmdArg args_handlers[] = {
	{"-threads", +[](u64 number){ requested_thread_count = (u32)number; }},
	{"-nested-reports-verbosity", +[](u64 number) { nested_reports_verbosity = number; }},
	{"-print-tokens", +[] { print_tokens = true; }},
	{"-print-ast", +[] { should_print_ast = true; }},
	{"-print-uids", +[] { print_uids = true; }},
	{"-no-constant-name-inlining", +[] { constant_name_inlining = false; }},
	{"-report-yields", +[] { report_yields = true; }},
	{"-log-time", +[] { enable_time_log = true; }},
	{"-debug", +[] { is_debugging = true; }},
	{"-run-bytecode", +[] { interpret_mode = InterpretMode::bytecode; }},
	{"-run-ast", +[] { interpret_mode = InterpretMode::ast; }},
	{"-limit-time", +[] {
		create_thread([] {
			int seconds_limit = 10;
			sleep_milliseconds(seconds_limit * 1000);
			immediate_reporter.error("Time limit of {} seconds exceeded.", seconds_limit);
			exit(-1);
		});
	}},
	{"-print-wait-failures", +[] { print_wait_failures = true; }},
	{"-log-error-path", +[] { enable_log_error_path = true; }},
	{"-run", +[] { run_compiled_code = true; }},
	{"-stats", +[] { print_stats = true; }},
	{"-interactive", +[] { run_interactive = true; }},
};

bool parse_arguments(Span<Span<utf8>> args) {
	for (umm i = 1; i < args.count; ++i) {

		for (auto handler : args_handlers) {
			if (args[i] == handler.key) {
				handler.run.visit(Combine {
					[&](void (*run)()) {
						run();
					},
					[&](void (*run)(u64 x)) {
						if (++i < args.count) {
							if (auto number = parse_u64(args[i])) {
								run(number.value());
								return;
							}
						}
						immediate_reporter.error("Could not parse number after -threads. Defaulting to all threads.");
					},
				});
				goto next_arg;
			}
		}
		if (args[i][0] == '-') {
			immediate_reporter.warning("Unknown command line parameter: {}", args[i]);
		} else {
			if (input_source_path.count) {
				with(ConsoleColor::red, println("No multiple input files allowed"));
				return {};
			} else {
				input_source_path = normalize_path(make_absolute_path(args[i]));
			}
		}
	next_arg:;
	}

	if (!input_source_path.count) {
		with(ConsoleColor::red, println("No input file was specified"));
		return false;
	} 

	return true;
}

void init_builtin_types() {
	#define x(name) \
		{ \
			auto type = get_builtin_type(BuiltinType::name); \
			type->type_kind = BuiltinType::name; \
			type->type = get_builtin_type(BuiltinType::Type); \
		}
	ENUMERATE_BUILTIN_TYPES(x)
	#undef x
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

bool find_main_and_run() {
	for (auto node : global_block.children) {
		if (auto definition = as<Definition>(node)) {
			if (definition->name == u8"main"s) {
				if (!definition->initial_value) {
					immediate_reporter.error(definition->location, "main must be a lambda");
					return false;
				}

				if (definition->mutability != Mutability::constant) {
					immediate_reporter.error(definition->location, "main must be constant");
					return false;
				}

				auto lambda = as<Lambda>(definition->initial_value);
				if (!lambda) {
					immediate_reporter.error(definition->location, "main must be a lambda");
					return false;
				}

				if (!types_match(lambda->head.return_type, get_builtin_type(BuiltinType::None)) &&
					!::is_concrete_integer(lambda->head.return_type)) 
				{
					immediate_reporter.error(definition->location, "main must return integer or None, not {}.", lambda->head.return_type);
					return false;
				}


				auto call = Call::create();
				call->callable = lambda;
				call->type = lambda->head.return_type;
				call->call_kind = CallKind::lambda;
				switch (interpret_mode) {
					case InterpretMode::bytecode: {
						dbgln("\nBytecode:\n");
						Bytecode::Builder builder;
						for (auto definition : global_block.definition_list) {
							builder.append_global_definition(definition);
						}
						visit(&global_block, Combine {
							[&] (auto) {},
							[&] (Lambda *lambda) {
								if (lambda->body && !lambda->head.is_template) {
									builder.append_lambda(lambda);
								}
							},
						});
						auto bytecode = builder.build(call);
						if (is_debugging) {
							println("\nFinal instructions:\n");
							print_instructions(bytecode.instructions);
						}
						
						//target_x64::emit(u8"output.exe"s, bytecode);

						if (run_compiled_code) {
							timed_block("executing main");
							auto result = Bytecode::Interpreter{}.run(&bytecode, builder.entry_point(), run_interactive);
							if (!result)
								return false;
							println("main returned {}", result.value());
						}
						break;
					}
					case InterpretMode::ast: {
						if (run_compiled_code) {
							auto context = NodeInterpreter::create(call);
							auto result = context->run();
							if (result.is_value()) {
								println("main returned {}", result.value());
							} else {
								with(ConsoleColor::red, println("main failed to execute"));
							}
							break;
						}
					}
				}
				return true;
			}
		}
	}

	immediate_reporter.error("main lambda not found");
	return false;
}

s32 tl_main(Span<Span<utf8>> args) {
	debug_init();

	set_console_encoding(Encoding::utf8);

	defer {
		if (enable_time_log) {
			for (auto time : timed_results) {
				println("{} took {} ms", time.name, time.seconds * 1000);
			}
		}

		if (print_stats) {
			println("Fiber allocations: {}", allocated_fiber_count);
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

	init_globals();
	init_builtin_types();
	Typechecker::init_binary_typecheckers();
	
	timed_function();

	compiler_path = args[0];
	compiler_bin_directory = parse_path(compiler_path).directory;
	compiler_root_directory = format(u8"{}\\..", compiler_bin_directory);
	generated_source_directory = format(u8"{}\\generated", compiler_root_directory);

	for_each_file(generated_source_directory, {}, [&](String path) {
		return ForEach_erase;
	});

	if (!parse_arguments(args)) {
		immediate_reporter.error("Failed to parse arguments.");
		return 1;
	}
	
	auto cpu_info = get_cpu_info();

	u32 thread_count;
	if (requested_thread_count == 0) {
		thread_count = cpu_info.logical_processor_count;
	} else {
		thread_count = min(requested_thread_count, cpu_info.logical_processor_count);
	}

	
	TaskQueueThreadPool thread_pool;
	thread_pool.init(thread_count - 1);
	defer { thread_pool.deinit(); };

	imports.use_unprotected().add_file({.path = input_source_path, .location = {}});
	imports.use_unprotected().add_file({.path = normalize_path(make_absolute_path(format(u8"{}\\import\\base.sp", compiler_root_directory))), .location = {}});

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

		if (imports.use_unprotected().files_to_import.count == 0) {
			break;
		}
	}

	if (failed) {
		LOG_ERROR_PATH("Parsing failed");
		return 1;
	}

	{
		timed_block("typecheck");


		failed = false;

		for (auto &node : global_block.children) {
			typecheck_entries.add({.node = &node});
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

				if (result != YieldResult::wait) {
					entry.typechecker->retire();

					if (result == YieldResult::fail) {
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
							flip_order(cycle); // reverse to get correct order
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

					if (auto definition = as<Definition>(*entry.node); definition && definition->initial_value) {
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

					immediate_reporter.info((*entry.node)->location, "{} depends on {}.", (*entry.node)->location, (*next_entry.node)->location);
				}
			}
		}

		for (auto &report : deferred_reports.use_unprotected()) {
			report.print();
		}

		if (failed) {
			LOG_ERROR_PATH("Typechecking failed.");
			return 1;
		}
	}

	if (should_print_ast) {
		print_ast(&global_block);
	}

	if (!find_main_and_run()) {
		return 1;
	}

	with(ConsoleColor::green, println("Build success"));

	return 0;
}