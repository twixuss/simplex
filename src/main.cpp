#include <type_traits>
#include <xtr1common>
#include <concepts>

#undef assert

namespace tl {
template <class, class Size>
struct Span;
bool debugger_attached();
}
using String = tl::Span<char8_t, unsigned long long>;

void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function);
template <class ...Args>
void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, char const *format, Args ...args);

void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location);
template <class ...Args>
void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, char const *format, Args ...args);

void print_crash_info();

#define ASSERTION_FAILURE(cause_string, expression, ...) (\
	::assertion_failure(cause_string, expression, __FILE__, __LINE__, __FUNCSIG__ __VA_OPT__(,) __VA_ARGS__), \
	print_crash_info(), \
	(BUILD_DEBUG || debugger_attached()) ? (debug_break(), 0) : 0, \
	exit(-1) \
)

#if !BUILD_DEBUG
#define assert(...)
#endif
#define TL_DEBUG BUILD_DEBUG
#define TL_IMPL
#define TL_DEFAULT_HASH_MAP ContiguousHashMap
#include <tl/main.h>
#include <tl/file.h>
#include <tl/thread.h>
#include <tl/string.h>
#include <tl/cpu.h>
#include <tl/hash_map.h>
#include <tl/reusable_fiber.h>
#include <tl/time.h>
#include <tl/debug.h>
#include <tl/macros.h>
#include <tl/bits.h>
#include <tl/block_list.h>

#pragma warning(error: 4996)

#define ENABLE_STRING_HASH_COUNT 0
#define ENABLE_NOTE_LEAK 0

using namespace tl;

constexpr u64 read_u64(utf8 *data) {
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

bool constant_name_inlining = true;
bool print_uids = false;
bool report_yields = false;
bool enable_time_log = false;
bool is_debugging = false;
bool print_tokens = false;
bool print_wait_failures = false;
bool enable_log_error_path = false;

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

struct SourceLocation {
	String file;
	u32 line_number = 0;
	u32 column_number = 0;
	List<String> lines;
};

GHashMap<utf8 *, String> content_start_to_file_name;

SourceLocation get_source_location(String location) {
	SourceLocation result;

	auto cursor = location.data;
	result.column_number = 0;
	while (*cursor != '\n' && *cursor != '\0') {
		++result.column_number;
		--cursor;
	}

	if (location == u8"\n"s) {
		result.lines.add({location});
	} else {
		String combined_lines;
		combined_lines.data = cursor + 1;
		combined_lines.set_end(location.end());

		while (*combined_lines.end() != '\n' && *combined_lines.end() != '\0')
			++combined_lines.count;

		split(combined_lines, u8'\n', [&](String line){ result.lines.add({ line }); });
	}

	assert(result.lines.front().begin() <= location.begin());
	assert(result.lines.back().end() >= location.end());

	result.line_number = 1;
	while (*cursor != '\0') {
		if (*cursor == '\n') 
			++result.line_number;
		--cursor;
	}

	auto found_file_name = content_start_to_file_name.find(cursor + 1);
	assert(found_file_name);
	result.file = found_file_name->value;

	return result;
}

inline umm append(StringBuilder &builder, SourceLocation location) {
	return append_format(builder, "{}:{}:{}", location.file, location.line_number, location.column_number);
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

enum class ReportKind : u8 {
	info,
	warning,
	error,
	help,
};

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

		auto indent = [&] {
			for (u32 i = indentation; i--;) {
				tl::print(u8"|   "s);
			}
		};

		if (location.data) {
			auto source_location = get_source_location(location);

			indent();
			println("{}: ", source_location);

			indent();
			print_report_kind(kind);
			println(": {}",  message);

			auto max_line_number = source_location.line_number + source_location.lines.count - 1;
			auto line_number_width = log10(max_line_number);
			auto line_number_alignment = align_right(line_number_width, ' ');

			auto output_line = [&](u32 line_number, String line, String highlight) {
				auto prefix = format(u8" {} | {}", Format(line_number, line_number_alignment), String(line.begin(), highlight.begin()));
				auto postfix = String(highlight.end(), line.end());

				indent();
				print_replacing_tabs_with_spaces(prefix);
				with(get_color(kind), print_replacing_tabs_with_spaces(highlight));
				print_replacing_tabs_with_spaces(postfix);
				println();
				if (!is_stdout_console()) {
					for (auto c : prefix)    for (umm i = 0; i < (c=='\t'?4:1);++i) tl::print(' ');
					for (auto c : highlight) for (umm i = 0; i < (c=='\t'?4:1);++i) tl::print('~');
					for (auto c : postfix)   for (umm i = 0; i < (c=='\t'?4:1);++i) tl::print(' ');
					println();
				}
			};

			if (source_location.lines.count == 1) {
				output_line(source_location.line_number, source_location.lines[0], location);
			} else {
				assert(source_location.lines.count > 1);

				output_line(source_location.line_number, source_location.lines[0], String{location.begin(), source_location.lines[0].end()});

				for (umm i = 1; i < source_location.lines.count - 1; ++i) {
					output_line(source_location.line_number + i, source_location.lines[i], source_location.lines[i]);
				}
				
				output_line(source_location.line_number + source_location.lines.count - 1, source_location.lines.back(), String{source_location.lines.back().begin(), location.end()});
			}
		} else {
			indent();
			print_report_kind(kind);
			println(": {}", message);
		}
		println();
	}
};

bool break_on_error = false;

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

void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function) {
	assertion_failure_impl(cause_string, expression, file, line, function, {}, {});
}

template <class ...Args>
void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, char const *format, Args ...args) {
	assertion_failure_impl(cause_string, expression, file, line, function, location, tformat(format, args...));
}

template <class ...Args>
void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, char const *format, Args ...args) {
	assertion_failure_impl(cause_string, expression, file, line, function, {}, tformat(format, args...));
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

struct Lexer {
	static Lexer create(String source) {
		Lexer result;
		result.source = source;
		result.cursor = source.data;
		
		for (umm i = 0; i < source.count; ++i) {
			auto c = source.data[i];
			if (c <= 0x08 || (0x0b <= c && c <= 0x0c) || (0x0e <= c && c <= 0x1f)) {
				immediate_reporter.error("Invalid character at byte {}: {} (0x{})", i, escape_character(c).span(), FormatInt{.value=(u32)c,.radix=16});
				return {};
			}
		}

		return result;
	}
	
	Token next_token() {
		Token eof;
		eof.kind = Token_eof;
		eof.string = {source.end() - 1, source.end()};

	restart:
		if (cursor >= source.end()) {
			return eof;
		}

		while (true) {
			if (*cursor == ' ' || *cursor == '\t' || *cursor == '\r')
				next();
			else
				break;
		}

		if (cursor == source.end())
			return eof;

		Token token;
		token.string.data = cursor;

		// ("&", "=")
		// "&"
		// "&="
#define CASE_SINGLE_OR_DOUBLE(a, b)                                   \
	case a: {                                                         \
		next();                                                       \
		if (*cursor == b) {                                           \
			next();                                                   \
			token.kind = (TokenKind)const_string_to_token_kind(a, b); \
			token.string.count = 2;                                   \
		} else {                                                      \
			token.kind = (TokenKind)a;                                \
			token.string.count = 1;                                   \
		}                                                             \
		return token;                                                 \
	}

		// ("&", "=")
		// "&"
		// "&&"
		// "&="
#define CASE_SINGLE_OR_TWO_DOUBLES(a, b)                                  \
	case a: {                                                             \
		next();                                                           \
		switch (*cursor) {                                                \
			case a:                                                       \
				next();                                                   \
				token.kind = (TokenKind)const_string_to_token_kind(a, a); \
				token.string.count = 2;                                   \
				break;                                                    \
			case b:                                                       \
				next();                                                   \
				token.kind = (TokenKind)const_string_to_token_kind(a, b); \
				token.string.count = 2;                                   \
				break;                                                    \
			default: {                                                    \
				token.kind = (TokenKind)a;                                \
				token.string.count = 1;                                   \
				break;                                                    \
			}                                                             \
		}                                                                 \
		return token;                                                     \
	}
		// ("<", "=")
		// "<"
		// "<="
		// "<<"
		// "<<="
#define CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE(a, b)                               \
	case a: {                                                                    \
		next();                                                                  \
		switch (*cursor) {                                                       \
			case b:                                                              \
				token.kind = (TokenKind)const_string_to_token_kind(a, b);        \
				next();                                                          \
				token.string.count = 2;                                          \
				break;                                                           \
			case a: {                                                            \
				next();                                                          \
				if (*cursor == b) {                                              \
					next();                                                      \
					token.kind = (TokenKind)const_string_to_token_kind(a, a, b); \
					token.string.count = 3;                                      \
				} else {                                                         \
					token.kind = (TokenKind)const_string_to_token_kind(a, a);    \
					token.string.count = 2;                                      \
				}                                                                \
				break;                                                           \
			}                                                                    \
			default: {                                                           \
				token.kind = (TokenKind)a;                                       \
				token.string.count = 1;                                          \
				break;                                                           \
			}                                                                    \
		}                                                                        \
		return token;                                                            \
	}
		switch (*cursor) {
			case '(': case ')':
			case '[': case ']':
			case '{': case '}':
			case '`': case '~':
			case '@':
			case '$': case ';':
			case ':': case ',':
			case '?':
			case '\\': case '\n': {
				token.kind = (TokenKind)*cursor;
				token.string.count = 1;
				next();
				return token;
			}
			CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE('>', '=')
			CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE('<', '=')
			CASE_SINGLE_OR_DOUBLE('.', '.');
			case '+':
			case '-':
			case '*':
			case '%':
			case '^':
			case '!': {
				char a = *cursor;
				next();

				u8 shift_table[] = {0, 8};
				u16 or_table[] = {0, '=' << 8};
				bool is_equals = *cursor == '=';

				cursor += is_equals;
				token.kind = (TokenKind)(a | or_table[is_equals]);
				token.string.count = is_equals + 1;

				return token;
			}

			CASE_SINGLE_OR_TWO_DOUBLES('=', '>');
			CASE_SINGLE_OR_TWO_DOUBLES('&', '=');
			CASE_SINGLE_OR_TWO_DOUBLES('|', '=');
			case '/': {
				token.kind = (TokenKind)*cursor;
				next();
				if (*cursor == '/') {
					while (*cursor != '\n' && cursor != source.end()) {
						next();
					}
					goto restart;
				} else if (*cursor == '*') {
					while (true) {
						if (String(cursor, 2) == "*/") {
							cursor += 2;
							break;
						}
						next();
						if (cursor > source.end()) {
							immediate_reporter.error(token.string.take(2), "Unclosed comment");
							return {};
						}
					}
					goto restart;
				} else {
					token.kind = (TokenKind)'/';
					token.string.count = 1;
					return token;
				}
				break;
			}
			case '"': {
				token.kind = Token_string;
				next();
				while (true) {
					if (*cursor == '"' && cursor[-1] != '\\') {
						break;
					}
					next();
					if (cursor > source.end()) {
						immediate_reporter.error(token.string.take(2), "Unclosed string literal");
						return {};
					}
				}

				next();

				token.string.set_end(cursor);
				return token;
			}
			case '#': {
				token.kind = Token_directive;

				next();
				while (true) {
					switch (*cursor) {
						ENUMERATE_CHARS_ALPHA(PASTE_CASE)
						ENUMERATE_CHARS_DIGIT(PASTE_CASE)
						case '_': {
							next();
							break;
						}
						default:
							goto directive_loop_end;
					}
				}
			directive_loop_end:;

				token.string.set_end(cursor);
				return token;
			}

			ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
				token.kind = Token_number;

				next();
				
				if (*cursor == 'x') {
					next();

					while (true) {
						switch (*cursor) {
							ENUMERATE_CHARS_HEX(PASTE_CASE) {
								next();
								break;
							}
							default:
								goto number_loop_end;
						}
					}

				} else {
					while (true) {
						switch (*cursor) {
							ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
								next();
								break;
							}
							default:
								goto number_loop_end;
						}
					}
				}
			number_loop_end:;

				token.string.set_end(cursor);
				return token;
			}
			default: {
				token.kind = Token_name;

				while (true) {
					if ((0x00 <= *cursor && *cursor <= 0x2f) ||
						(0x3a <= *cursor && *cursor <= 0x40) ||
						(0x5b <= *cursor && *cursor <= 0x5e) ||
						(0x7c <= *cursor && *cursor <= 0x7f) ||
						(*cursor == 0x60)
						) {
						goto name_loop_end;
					}

					next();
				}
			name_loop_end:;

				token.string.set_end(cursor);
				if (token.string.count == 0) {
					immediate_reporter.error({token.string.data, 1}, "Invalid character ({}).", (u32)*cursor);
					return {};
				}

				static constexpr umm max_keyword_size = []{
					umm max = 0;
					#define x(name) max = tl::max(max, u8###name##s.count);
					ENUMERATE_KEYWORDS(x)
					#undef x
					return max; 
				}();

				// NOTE:
				// This relies on compiler being able to optimize out branches.
				// Based on string size, check only keywords with that size.
				switch (token.string.count) {
					#if 0
					// Preprocessor way with switch

					#define x(name) \
						case const_string_to_token_kind(#name##s): { token.kind = Token_##name; break; }

					#define CASE(N) \
						case N: { \
							umm const COUNT = N; \
							auto token_as_int = chars_as_int<N>(token.string.data); \
							switch (token_as_int) { \
								ENUMERATE_KEYWORDS_L##N(x) \
							} \
							break; \
						}
					CASE(1)
					CASE(2)
					CASE(3)
					CASE(4)
					CASE(5)
					CASE(6)
					CASE(7)
					CASE(8)
					static_assert(max_keyword_size <= 9, "");
					#undef CASE
					#undef x

					#elif 1
					// Manual switch
					case 2: {
						u16 token_as_int = *(u16 *)token.string.data;
						switch (token_as_int) {
							case const_string_to_token_kind("U8"s): { token.kind = Token_U8; break; }
							case const_string_to_token_kind("S8"s): { token.kind = Token_S8; break; }
							case const_string_to_token_kind("if"s): { token.kind = Token_if; break; }
							case const_string_to_token_kind("as"s): { token.kind = Token_as; break; }
						}
						break;
					}
					case 3: {
						u32 token_as_int = *(u32 *)token.string.data & 0xff'ff'ff;
						switch (token_as_int) {
							case const_string_to_token_kind("U16"s): { token.kind = Token_U16; break; }
							case const_string_to_token_kind("U32"s): { token.kind = Token_U32; break; }
							case const_string_to_token_kind("U64"s): { token.kind = Token_U64; break; }
							case const_string_to_token_kind("S16"s): { token.kind = Token_S16; break; }
							case const_string_to_token_kind("S32"s): { token.kind = Token_S32; break; }
							case const_string_to_token_kind("S64"s): { token.kind = Token_S64; break; }
							case const_string_to_token_kind("let"s): { token.kind = Token_let; break; }
							case const_string_to_token_kind("var"s): { token.kind = Token_var; break; }
						}
						break;
					}
					case 4: {
						u32 token_as_int = *(u32 *)token.string.data;
						switch (token_as_int) {
							case const_string_to_token_kind("Type"s): { token.kind = Token_Type; break; }
							case const_string_to_token_kind("Bool"s): { token.kind = Token_Bool; break; }
							case const_string_to_token_kind("None"s): { token.kind = Token_None; break; }
							case const_string_to_token_kind("none"s): { token.kind = Token_none; break; }
							case const_string_to_token_kind("then"s): { token.kind = Token_then; break; }
							case const_string_to_token_kind("else"s): { token.kind = Token_else; break; }
							case const_string_to_token_kind("true"s): { token.kind = Token_true; break; }
						}
						break;
					}
					case 5: {
						u64 token_as_int = *(u64 *)token.string.data & 0xff'ff'ff'ff'ff;
						switch (token_as_int) {
							case const_string_to_token_kind("const"s): { token.kind = Token_const; break; }
							case const_string_to_token_kind("false"s): { token.kind = Token_false; break; }
							case const_string_to_token_kind("while"s): { token.kind = Token_while; break; }
							case const_string_to_token_kind("break"s): { token.kind = Token_break; break; }
							case const_string_to_token_kind("match"s): { token.kind = Token_match; break; }
						}
						break;
					}
					case 6: {
						u64 token_as_int = *(u64 *)token.string.data & 0xff'ff'ff'ff'ff'ff;
						switch (token_as_int) {
							case const_string_to_token_kind("String"s): { token.kind = Token_String; break; }
							case const_string_to_token_kind("return"s): { token.kind = Token_return; break; }
							case const_string_to_token_kind("typeof"s): { token.kind = Token_typeof; break; }
							case const_string_to_token_kind("inline"s): { token.kind = Token_inline; break; }
							case const_string_to_token_kind("struct"s): { token.kind = Token_struct; break; }
							case const_string_to_token_kind("import"s): { token.kind = Token_import; break; }
						}
						break;
					}
					case 8: {
						u64 token_as_int = *(u64 *)token.string.data;
						switch (token_as_int) {
							case const_string_to_token_kind("continue"s): { token.kind = Token_continue; break; }
							case const_string_to_token_kind("noinline"s): { token.kind = Token_noinline; break; }
						}
						break;
					}
					#elif 0
					// Manual ifs
					case 2: {
						if (false) {}
						else if (token.string == "U8") { token.kind = Token_U8; }
						else if (token.string == "S8") { token.kind = Token_S8; }
						else if (token.string == "if") { token.kind = Token_if; }
						else if (token.string == "as") { token.kind = Token_as; }
						break;
					}
					case 3: {
						if (false) {}
						else if (token.string == "U16") { token.kind = Token_U16; }
						else if (token.string == "U32") { token.kind = Token_U32; }
						else if (token.string == "U64") { token.kind = Token_U64; }
						else if (token.string == "S16") { token.kind = Token_S16; }
						else if (token.string == "S32") { token.kind = Token_S32; }
						else if (token.string == "S64") { token.kind = Token_S64; }
						else if (token.string == "let") { token.kind = Token_let; }
						else if (token.string == "var") { token.kind = Token_var; }
						break;
					}
					case 4: {
						if (false) {}
						else if (token.string == "Type") { token.kind = Token_Type; }
						else if (token.string == "Bool") { token.kind = Token_Bool; }
						else if (token.string == "None") { token.kind = Token_None; }
						else if (token.string == "then") { token.kind = Token_then; }
						else if (token.string == "else") { token.kind = Token_else; }
						else if (token.string == "true") { token.kind = Token_true; }
						break;
					}
					case 5: {
						if (false) {}
						else if (token.string == "const") { token.kind = Token_const; }
						else if (token.string == "false") { token.kind = Token_false; }
						else if (token.string == "while") { token.kind = Token_while; }
						else if (token.string == "break") { token.kind = Token_break; }
						else if (token.string == "match") { token.kind = Token_match; }
						break;
					}
					case 6: {
						if (false) {}
						else if (token.string == "String") { token.kind = Token_String; }
						else if (token.string == "return") { token.kind = Token_return; }
						else if (token.string == "typeof") { token.kind = Token_typeof; }
						else if (token.string == "inline") { token.kind = Token_inline; }
						else if (token.string == "struct") { token.kind = Token_struct; }
						break;
					}
					case 8: {
						if (false) {}
						else if (token.string == "continue") { token.kind = Token_continue; }
						else if (token.string == "noinline") { token.kind = Token_noinline; }
						break;
					}
					#else
					// Preprocessor way with ifs


					#define x(name) \
						else if (u8###name##s.count == COUNT && token.string == #name) { \
							token.kind = Token_##name; \
						}

					#define CASE(N) \
						case N: { \
							umm const COUNT = N; \
							if (false) {} \
							ENUMERATE_KEYWORDS(x); \
							break; \
						}
					CASE(1)
					CASE(2)
					CASE(3)
					CASE(4)
					CASE(5)
					CASE(6)
					CASE(7)
					CASE(8)
					CASE(9)
					static_assert(max_keyword_size <= 9, "");
					#undef CASE
					#undef x
					#endif
				}

				return token;
			}
		} 
	}

private:
	String source;
	utf8 *cursor;
	
	void print_invalid_character_error() {
		immediate_reporter.error({cursor, 1}, "Invalid uft8 character.");
	}
	
	void next() {
		assert(cursor <= source.end());
		++cursor;
	}

};

using Tokens = GList<Token>;

// don't make a giant list, cmon
Optional<Tokens> source_to_tokens(String source, String path) {
	timed_function();

	Tokens tokens;

	Lexer lexer = Lexer::create(source);

	while (1) {
		Token token = lexer.next_token();
		if (token.kind == Token_eof)
			break;
		tokens.add(token);
	}

	if (print_tokens) {
		println("\nTokens of \"{}\":", path);
		for (umm i = 0; i < tokens.count; ++i) {
			print_token(i, tokens[i]);
		}
	}

	// NOTE: add a bunch of 'eof' tokens to the end, so peeking does not go out of bounds.
	Token eof;
	eof.string = {source.end() - 1, source.end()};
	for (int i = 0; i < 4; ++i)
		tokens.add(eof);

	return tokens;
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
		inline static constexpr NodeKind kind = NodeKind::##name; \
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

		atomic_update(&cursor, [=, &target](u8 *cursor) {
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

enum class Mutability : u8 {
	readonly,  // can not be modified by anyone.
	immutable, // can not be modified directly, can be modified by someone else (e.g. other thread)
	constant,  // known at compile time. can be casted to readonly
	variable,  // can be modified by anyone.
};

inline umm append(StringBuilder &builder, Mutability mutability) {
	switch (mutability) {
		case Mutability::constant: return append(builder, "const");
		case Mutability::readonly: return append(builder, "let");
		case Mutability::variable: return append(builder, "var");
	}
	return append_format(builder, "(unknown Mutability {})", (u32)mutability);
}

template <class T>
struct Meaning {
	T value = {};
};

inline umm append(StringBuilder &builder, Meaning<Mutability> mutability) {
	switch (mutability.value) {
		case Mutability::constant: return append(builder, "constant");
		case Mutability::readonly: return append(builder, "read-only");
		case Mutability::variable: return append(builder, "variable");
	}
	return append_format(builder, "(unknown Mutability {})", (u32)mutability.value);
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
		memcpy(this, &that, sizeof Value);
	}
	Value(Value &&that) { 
		memcpy(this, &that, sizeof Value);
		memset(&that, 0, sizeof Value);
	}
	~Value() {
		memset(this, 0, sizeof Value);
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

	void add(Node *child);
	void free_impl() {
		tl::free(children);
		tl::free(definition_list);
		tl::free(definition_map);
	}
};
DEFINE_EXPRESSION(Call) {
	Expression *callable = 0;
	GList<Expression *> arguments;

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
		parameters_block.container = this;
	}
	Block parameters_block;
	Expression *parsed_return_type = 0;
	Expression *return_type = 0;
};
DEFINE_EXPRESSION(Lambda) {
	Definition *definition = 0;
	Expression *body = 0;
	LambdaHead head;

	GList<Return *> returns;
	
	u64 first_instruction_index = -1;

	Inline inline_status = {};

	String extern_library = {};

	bool is_intrinsic : 1 = false;
	bool is_extern    : 1 = false;
};
DEFINE_EXPRESSION(Name) {
	String name;
	List<Definition *> possible_definitions;

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
	List<Definition *> members;
	s64 size = -1;
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

void Block::add(Node *child) {
	children.add(child);
	if (auto definition = as<Definition>(child)) {
		definition_list.add(definition);
		definition_map.get_or_insert(definition->name).add(definition);
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

#define VISIT(node)                                      \
	do {                                                 \
		auto **x = node;                                 \
		if (visit((Node **)x, visitor) == ForEach_break) \
			return ForEach_break;                        \
	} while (0)

#define VISIT_STATIC(node)                                 \
	do {                                                   \
		auto x = &node;                                    \
		auto result = visit((Node **)&x, visitor);         \
		assert(x == &node, "Unexpected node replacement"); \
		if (result == ForEach_break)                       \
			return ForEach_break;                          \
	} while (0)

ForEachDirective visit(Node **node, auto &&visitor);
ForEachDirective visit_impl(Block **node, auto &&visitor) {
	auto block = *node;
	for (auto &child : block->children) {
		VISIT(&child);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Return **node, auto &&visitor) {
	auto ret = *node;
	VISIT(&ret->value);
	return ForEach_continue;
}
ForEachDirective visit_impl(IfStatement **node, auto &&visitor) {
	auto If = *node;
	VISIT(&If->condition);
	VISIT(&If->true_branch);
	if (If->false_branch)
		VISIT(&If->false_branch);
	return ForEach_continue;
}
ForEachDirective visit_impl(IfExpression **node, auto &&visitor) {
	auto If = *node;
	VISIT(&If->condition);
	VISIT(&If->true_branch);
	VISIT(&If->false_branch);
	return ForEach_continue;
}
ForEachDirective visit_impl(Definition **node, auto &&visitor) {
	auto definition = *node;
	if (definition->parsed_type) {
		VISIT(&definition->parsed_type);
	}
	if (definition->initial_value) {
		VISIT(&definition->initial_value);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Call **node, auto &&visitor) { 
	auto call = *node;
	VISIT(&call->callable);
	for (auto &argument : call->arguments) {
		VISIT(&argument);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Lambda **node, auto &&visitor) {
	auto lambda = *node;
	VISIT_STATIC(lambda->head);
	if (lambda->body) {
		VISIT(&lambda->body);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(LambdaHead **node, auto &&visitor) {
	auto head = *node;
	VISIT_STATIC(head->parameters_block);
	if (head->parsed_return_type) {
		VISIT(&head->parsed_return_type);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Binary **node, auto &&visitor) {
	auto binary = *node;
	VISIT(&binary->left);
	VISIT(&binary->right);
	return ForEach_continue;
}
ForEachDirective visit_impl(Unary **node, auto &&visitor) {
	auto unary = *node;
	VISIT(&unary->expression);
	return ForEach_continue;
}
ForEachDirective visit_impl(Match **node, auto &&visitor) {
	auto match = *node;
	VISIT(&match->expression);
	for (auto &Case : match->cases) {
		if (Case.from) {
			VISIT(&Case.from);
		}
		VISIT(&Case.to);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(While **node, auto &&visitor) {
	auto While = *node;
	VISIT(&While->condition);
	VISIT(&While->body);
	return ForEach_continue;
}
ForEachDirective visit_impl(Name **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(IntegerLiteral **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(BooleanLiteral **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(NoneLiteral **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(StringLiteral  **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(BuiltinTypeName **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(Continue **node, auto &&visitor) { return ForEach_continue; }
ForEachDirective visit_impl(Break **node, auto &&visitor) {
	auto Break = *node;
	if (Break->value) {
		VISIT(&Break->value);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Struct **node, auto &&visitor) {
	auto Struct = *node;
	for (auto &member : Struct->members) {
		VISIT(&member);
	}
	return ForEach_continue; 
}
ForEachDirective visit_impl(ArrayType **node, auto &&visitor) {
	auto arr = *node;
	VISIT(&arr->count_expression);
	VISIT(&arr->element_type);
	return ForEach_continue;
}
ForEachDirective visit_impl(Subscript **node, auto &&visitor) {
	auto subscript = *node;
	VISIT(&subscript->subscriptable);
	VISIT(&subscript->index);
	return ForEach_continue;
}
ForEachDirective visit_impl(ArrayConstructor **node, auto &&visitor) {
	auto arr = *node;
	for (auto &element : arr->elements) {
		VISIT(&element);
	}
	return ForEach_continue;
}
ForEachDirective visit_impl(Import **node, auto &&visitor) {
	return ForEach_continue;
}

ForEachDirective visit(Node **node, auto &&visitor) {

	auto visitor_wrapper = [&] (auto node) {
		if constexpr (requires { { visitor(*node) }; }) {
			if constexpr (requires { { visitor(*node) } -> std::same_as<ForEachDirective>; }) {
				if (visitor(*node) == ForEach_break)
					return ForEach_break;
			} else if constexpr (std::is_convertible_v<decltype(visitor(*node)), Node *>) {
				// Sketchy
				*node = autocast visitor(*node);
			} else if constexpr (requires { { visitor(*node) } -> std::same_as<void>; }) {
				visitor(*node);
			} else {
				static_error_v(node, "Unexpected return type in visitor");
			}
		} else {
			static_error_v(node, "Unexpected argument type in visitor");
		}
		return ForEach_continue;
	};

	// Visit the actual node. It might be replaced, so do two separate switches.
	switch ((*node)->kind) {
#define x(name)                                              \
	case NodeKind::name:                                     \
		if (visitor_wrapper((name **)node) == ForEach_break) \
			return ForEach_break;                            \
		break;

		ENUMERATE_NODE_KIND(x)
#undef x
		default: invalid_code_path();
	}

	switch ((*node)->kind) {
#define x(name) case NodeKind::name: return visit_impl((name **)node, visitor);
		ENUMERATE_NODE_KIND(x)
#undef x
		default: invalid_code_path();
	}
	return ForEach_continue;
}
ForEachDirective visit(Node *node, auto &&visitor) {
	auto old_node = node;
	auto result = visit(&node, visitor);
	assert(node == old_node);
	return result;
}

#undef VISIT
#undef VISITOR

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
u64 get_size_impl(Block *node) { invalid_code_path("get_size({}) is invalid", "Block"); }
u64 get_size_impl(Call *node) { invalid_code_path("get_size({}) is invalid", "Call"); }
u64 get_size_impl(Definition *node) { invalid_code_path("get_size({}) is invalid", "Definition"); }
u64 get_size_impl(IntegerLiteral *node) { invalid_code_path("get_size({}) is invalid", "IntegerLiteral"); }
u64 get_size_impl(BooleanLiteral *node) { invalid_code_path("get_size({}) is invalid", "BooleanLiteral"); }
u64 get_size_impl(NoneLiteral *node) { invalid_code_path("get_size({}) is invalid", "NoneLiteral"); }
u64 get_size_impl(StringLiteral *node) { invalid_code_path("get_size({}) is invalid", "StringLiteral"); }
u64 get_size_impl(Lambda *node) { invalid_code_path("get_size({}) is invalid", "Lambda"); }
u64 get_size_impl(LambdaHead *node) { invalid_code_path("get_size({}) is invalid", "LambdaHead"); }
u64 get_size_impl(Name *node) { invalid_code_path("get_size({}) is invalid", "Name"); }
u64 get_size_impl(IfStatement *node) { invalid_code_path("get_size({}) is invalid", "IfStatement"); }
u64 get_size_impl(IfExpression *node) { invalid_code_path("get_size({}) is invalid", "IfExpression"); }
u64 get_size_impl(BuiltinTypeName *node) {
	return get_size(node->type_kind);
}
u64 get_size_impl(Binary *node) { invalid_code_path("get_size({}) is invalid", "Binary"); }
u64 get_size_impl(Match *node) { invalid_code_path("get_size({}) is invalid", "Match"); }
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
u64 get_size_impl(Subscript *node) { invalid_code_path("get_size({}) is invalid", "Subscript"); }
u64 get_size_impl(ArrayConstructor *node) { invalid_code_path("get_size({}) is invalid", "ArrayConstructor"); }

u64 get_size(Type type) {
	type = direct(type);
	switch (type->kind) {
#define x(name) case NodeKind::name: return get_size_impl((name *)type);
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path("invalid type->kind {}", type->kind);
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
			print_ast(argument);
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
void print_ast(Node *node) {
	switch (node->kind) {
#define x(name) case NodeKind::name: return print_ast_impl((##name *)node);
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

List<String> leaks;

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

struct FileToParse {
	String location;
	String path;
};
LockProtected<GList<FileToParse>, SpinLock> files_to_parse;

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

ReusableFiber get_new_fiber() {
	return locked_use_ret(fibers_to_reuse) {
		if (auto popped = fibers_to_reuse.pop()) {
			return popped.value();
		} else {
			return create_reusable_fiber();
		}
	};
}

void add_fiber_to_reuse(ReusableFiber fiber) {
	locked_use(fibers_to_reuse) {
		fibers_to_reuse.add(fiber);
	};
}

struct Parser {
	Token *token = 0;
	Block *current_block = &global_block;
	While *current_loop = 0;
	Expression *current_container = 0;
	Reporter reporter;
	ReusableFiber fiber = {};
	Fiber parent_fiber = {};
	List<Node *> result_nodes;
	bool success = false;
	List<utf8> extern_library = {};

	List<utf8> unescape_string(String string) {
		if (auto result = ::unescape_string(string.skip(1).skip(-1))) {
			return result.string;
		} else {
			reporter.error(result.failed_at, "Failed to unespace this string: {}", result.fail_reason);
			yield(false);
			return {};
		}
	}

	// Parses parse_expression_2 with binary operators and definitions.
	Expression *parse_expression(bool whitespace_is_skippable_before_binary_operator = false, int right_precedence = 0) {
		switch (token->kind) {
			case Token_var:
			case Token_let:
			case Token_const: {
				auto definition = Definition::create();
				definition->container = current_container;
				definition->mutability = to_mutability(token->kind).value();
				
				next();
				skip_lines();

				expect(Token_name);

				definition->name = token->string;
				definition->location = token->string;

				next();
				skip_lines();

				expect({':', '='});

				if (token->kind == ':') {
					next();
					skip_lines();

					definition->parsed_type = parse_expression_2(); // NOTE: don't parse '='

					switch (definition->parsed_type->kind) {
						case NodeKind::Name:
						case NodeKind::BuiltinTypeName:
						case NodeKind::Unary:
							break;
						default:
							reporter.error(definition->parsed_type->location, "{} is not allowed in type context.", definition->parsed_type->kind);
							yield(false);
					}
				}

				if (token->kind == '=') {
					next();
					skip_lines();

					definition->initial_value = parse_expression();

					if (definition->mutability == Mutability::constant) {
						if (auto lambda = as<Lambda>(definition->initial_value)) {
							lambda->definition = definition;
						} else if (auto Struct = as<::Struct>(definition->initial_value)) {
							Struct->definition = definition;
						}
					}
				} else {
					//if (definition->mutability != Mutability::variable) {
					//	reporter.error(definition->location, "Definitions can't be marked as {} and have no initial expression.", definition->mutability);
					//	reporter.help(definition->location, "You can either change {} to {}, or provide an initial expression.", definition->mutability, Mutability::variable);
					//	yield(false);
					//}

					expect({Token_eol, Token_eof});
				}
				return definition;
			}
		}


		//null denotation
		auto left = parse_expression_2();

		// left binding power
		Optional<BinaryOperation> operation;
		while (1) {

			if (whitespace_is_skippable_before_binary_operator) {
				skip_lines();
			}

			constexpr bool enable_custom_infix = false;

			if (enable_custom_infix && token->kind == Token_name) {

				// Custom binary operator

				if (right_precedence < custom_precedence) {
					auto location = token->string;

					next();
					skip_lines();

					auto right = parse_expression(whitespace_is_skippable_before_binary_operator, custom_precedence);

					auto name = Name::create();
					name->name = location;
					name->location = location;

					auto call = Call::create();
					call->location = {left->location.begin(), right->location.end()};
					call->callable = name;
					call->arguments.add({left, right});
					left = call;
					continue;
				}
			} else {
				operation = as_binary_operation(token->kind);
				if (operation && right_precedence < get_precedence(operation.value())) {
					auto binop = Binary::create();
					binop->location = token->string;
					binop->left = left;
					binop->operation = operation.value();

					next();
					skip_lines();

					binop->right = parse_expression(whitespace_is_skippable_before_binary_operator, get_precedence(binop->operation) - is_right_associative(operation.value()));
					binop->location = { binop->left->location.begin(), binop->right->location.end() };

					left = binop;
					continue;
				}
			}
			break;
		}
		return left;
	}
	// Parses parse_expression_1 plus parentheses or brackets after, e.g. calls, subscripts.
	Expression *parse_expression_2() {
		auto node = parse_expression_1();

		while (token->kind == '(' || token->kind == '[') {
			switch (token->kind) {
				case '(': {
					auto call = Call::create();
					call->callable = node;
					call->location = node->location;

					next();
					skip_lines();
					if (token->kind != ')') {
						while (true) {
							auto argument = parse_expression();

							call->arguments.add(argument);

							skip_lines();
							if (token->kind == ',') {
								next();
								skip_lines();
								if (token->kind == ')') {
									break;
								}
								continue;
							}
							if (token->kind == ')') {
								break;
							}

							reporter.error(token->string, "Unexpected token `{}` when parsing call argument list. Expected `,` or `)`.", token->string);
							yield(false);
						}
					}

					call->location = {call->location.begin(), token->string.end()};

					next();
					node = call;
					break;
				}
				case '[': {
					auto subscript = Subscript::create();
					subscript->subscriptable = node;
					subscript->location = node->location;

					next();
					skip_lines();

					subscript->index = parse_expression();

					skip_lines();
					expect(']');

					subscript->location = {subscript->location.begin(), token->string.end()};

					next();
					node = subscript;
					break;
				}
				default: invalid_code_path("unreachable");
			}
		}
		return node;
	}
	// Parses parse_expression_0 plus member access.
	Expression *parse_expression_1() {
		auto expression = parse_expression_0();
		while (token->kind == '.') {
			auto binary = Binary::create();
			binary->location = token->string;
			binary->operation = BinaryOperation::dot;
			binary->left = expression;
			next();
			binary->right = parse_expression_0();
			if (!as<Name>(binary->right)) {
				reporter.error(binary->right->location, "Only names can follow a dot.");
				yield(false);
			}
			binary->location = {binary->left->location.begin(), binary->right->location.end()};
			expression = binary;
		}
		return expression;
	}
	// Parses single-part expressions
	Expression *parse_expression_0() {
		switch (token->kind) {
			case '(': {
				auto lambda = Lambda::create();
				lambda->location = token->string;
				lambda->head.parameters_block.parent = current_block;

				scoped_replace(current_block, &lambda->head.parameters_block);
				scoped_replace(current_loop, 0);
				scoped_replace(current_container, lambda);
				assert(current_container->kind == NodeKind::Lambda);

				next();
				skip_lines();
				if (token->kind != ')') {
					while (true) {
						expect({Token_name, Token_var, Token_let, Token_const});

						auto mutability = Mutability::readonly;

						switch (token->kind) {
							case Token_var:
							case Token_let:
							case Token_const:
								mutability = to_mutability(token->kind).value();
								next();
								skip_lines();
								expect(Token_name);
								break;
						}

						List<Definition *> parameter_group;

						auto create_and_add_parameter = [&] {
							auto parameter = Definition::create();
							parameter->name = token->string;
							parameter->location = token->string;
							parameter_group.add(parameter);
						};

						create_and_add_parameter();

						next();
						skip_lines();

						while (token->kind == ',') {
							next();
							skip_lines();
							expect(Token_name);
							create_and_add_parameter();
							next();
						}

						expect(':');

						next();
						skip_lines();

						auto parsed_type = parse_expression();

						for (auto parameter : parameter_group) {
							parameter->container = lambda;
							parameter->parsed_type = parsed_type;
							parameter->is_parameter = true;
							parameter->mutability = mutability;
							lambda->head.parameters_block.add(parameter);
						}

						skip_lines();
						if (token->kind == ',') {
							next();
							skip_lines();
							if (token->kind == ')') {
								break;
							}
							continue;
						}
						if (token->kind == ')') {
							break;
						}
					}
				}
				
				next();
				skip_lines();

				bool body_required = true;

				if (token->kind == ':') {
					next();
					skip_lines();

					lambda->head.parsed_return_type = parse_expression_0();
					body_required = false;
				}
				
				lambda->head.location = lambda->location = { lambda->location.begin(), token[-1].string.end() };

				constexpr auto arrow = const_string_to_token_kind("=>"s);
				if (lambda->head.parsed_return_type) {
					if (token->kind == arrow) {
						next();
						body_required = true;
					}
				} else {
					if (token->kind != arrow) {
						reporter.error(token->string, "Expected : or => after )");
						yield(false);
					}
					next();
					body_required = true;
				}

				if (body_required) {
					skip_lines();

					while (token->kind == Token_directive) {
						if (token->string == u8"#intrinsic"s) {
							lambda->is_intrinsic = true;
						} else if (token->string == u8"#extern"s) {
							lambda->is_extern = true;
							lambda->extern_library = extern_library;
						} else {
							reporter.error(token->string, "Unknown lambda directive '{}'.", token->string);
							yield(false);
						}
						next();
					}

					if (lambda->is_intrinsic || lambda->is_extern) {
						return lambda;
					}

					lambda->body = parse_expression();

					return lambda;
				}

				NOTE_LEAK(lambda, u8"the rest of the lambda is unused.can't just free lambda because head is in it"s);
				return &lambda->head;
			}
			case '{': {
				auto block = Block::create();
				block->location = token->string;

				block->parent = current_block;
				scoped_replace(current_block, block);

				block->container = current_container;

				next();

				if (token->kind == ':') {
					next();
					expect(Token_name);

					block->tag = token->string;
					next();
				}

				while (true) {
					skip_lines();
					while (token->kind == ';') {
						next();
						skip_lines();
					}

					if (token->kind == '}') {
						break;
					}

					auto child = parse_statement();

					block->add(child);
				}
				block->location = {block->location.begin(), token->string.end()};
				next();

				for (auto child : block->children.skip(-1)) {
					ensure_allowed_in_statement_context(child);
				}

				if (is_substitutable(block)) {
					if (auto expression = as<Expression>(block->children[0])) {
						block->free();
						return expression;
					}
				}

				return block;
			}
			case '[': {
				auto Array = ArrayType::create();
				Array->location = token->string;
				next();
				Array->count_expression = parse_expression();
				expect(']');
				next();
				Array->element_type = parse_expression_0();
				Array->location = {Array->location.begin(), Array->element_type->location.end()};
				return Array;
			}
			case '.': {
				auto constructor = ArrayConstructor::create();
				constructor->location = token->string;
				next();
				expect('[');
				next();
				while (true) {
					skip_lines();
					constructor->elements.add(parse_expression());
					skip_lines();

					expect({',', ']'});

					if (token->kind == ']') {
						break;
					}
					next();
				}
				constructor->location = {constructor->location.begin(), token->string.end()};
				next();
				return constructor;
			}
			case Token_none: {
				auto none = NoneLiteral::create();
				none->location = token->string;
				next();
				return none;
			}
			case Token_name: {
				auto name = Name::create();
				name->location = token->string;
				name->name = token->string;

				next();
				if (token->kind == Token_name) {
					reporter.error({token[-1].string.begin(), token->string.end()}, "Two consecutive names is invalid syntax.");
					yield(false);
				}

				return name;
			}
			case Token_number: {
				auto literal = IntegerLiteral::create();
				literal->location = token->string;

				auto hex_digit_to_int = [](utf8 c) -> u8 {
					u32 u = c;
					if (u - '0' < 10) {
						return u - '0';
					}
					if (u - 'A' < 6) {
						return u - 'A' + 10;
					}
					if (u - 'a' < 6) {
						return u - 'a' + 10;
					}
					invalid_code_path();
				};

				if (token->string.count >= 2 && token->string.data[1] == 'x') {
					u64 result = 0;
					for (int i = 2; i < token->string.count; ++i) {
						result = (result << 4) | hex_digit_to_int(token->string.data[i]);
					}
					literal->value = result;
				} else {
					auto parsed = parse_u64(token->string);
					if (!parsed) {
						reporter.error(token->string, "Could not parse number {}", token->string);
						yield(false);
					}

					literal->value = parsed.value();
				}

				next();

				return literal;
			}
			case Token_false:
			case Token_true: {
				auto literal = BooleanLiteral::create();
				literal->location = token->string;
				literal->value = token->kind == Token_true;
				next();
				return literal;
			}
			case Token_string: {
				auto literal = StringLiteral::create();
				literal->location = token->string;
				literal->value = unescape_string(token->string);
				next();
				return literal;
			}
			case Token_if: {
				auto If = IfExpression::create();
				If->location = token->string;
				next();
				skip_lines();

				If->condition = parse_expression();

				skip_lines();
				if (token->kind == Token_then) {
					next();
					skip_lines();
				}

				If->true_branch = parse_expression();

				skip_lines();
				if (token->kind == ';') {
					next();
					skip_lines();
				}
				expect(Token_else);
				next();
				skip_lines();

				If->false_branch = parse_expression();
					
				If->location = {If->location.begin(), If->false_branch->location.end()};
				return If;
			}
			case Token_match: {
				auto match = Match::create();
				match->location = token->string;
				next();
				skip_lines();

				match->expression = parse_expression();

				skip_lines();
				expect('{');

				next();
				skip_lines();

				while (true) {
					Expression *from = 0;
					if (token->kind == Token_else) {
						next();
					} else {
						from = parse_expression();
					}

					skip_lines();
					expect(const_string_to_token_kind("=>"s));
					next();
					skip_lines();

					auto to = parse_expression();

					auto &Case = match->cases.add({from, to});
					if (!from) {
						if (match->default_case) {
							reporter.error(to->location, "Match expression can not have multiple default cases.");
							yield(false);
						}
						match->default_case = &Case;
					}

					skip_lines();
					while (token->kind == ';') {
						next();
						skip_lines();
					}
					if (token->kind == '}')
						break;
				}
				next();

				return match;
			}
			case Token_inline:
			case Token_noinline: {
				auto inline_token = token;
				auto status = token->kind == Token_inline ? Inline::always : Inline::never;
				next();
				auto expr = parse_expression_2();
				if (auto lambda = as<Lambda>(expr)) {
					lambda->inline_status = status;
					return lambda;
				} else if (auto call = as<Call>(expr)) {
					call->inline_status = status;
					return call;
				}

				reporter.error(inline_token->string, "{} keyword must precede a lambda or a call, not a {}", inline_token->string, expr->kind);
				yield(false);
				return 0;
			}
			case Token_struct: {
				auto Struct = Struct::create();
				scoped_replace(current_container, Struct);

				Struct->location = token->string;
				next();
				expect('{');
				next();
				skip_lines();

				while (token->kind != '}') {
					expect(Token_name);
					auto name = token->string;
					next();

					expect(':');
					next();
					auto type = parse_expression();

					expect('\n');
					skip_lines();

					auto definition = Definition::create();
					definition->location = name;
					definition->name = name;
					definition->container = Struct;
					definition->mutability = Mutability::variable;
					definition->parsed_type = type;
					Struct->members.add(definition);
				}
				next();

				return Struct;
			}
#define x(name) case Token_##name:
			ENUMERATE_CONCRETE_BUILTIN_TYPES(x)
#undef x
			{
				auto type = BuiltinTypeName::create();
				type->location = token->string;
				type->type_kind = to_builtin_type_kind(token->kind);
				next();
				return type;
			}

			default: {
				if (auto operation = as_unary_operation(token->kind)) {
					auto unop = Unary::create();
					unop->operation = operation.value();
					unop->location = token->string;
					next();
					skip_lines();

					if (operation.value() == UnaryOperation::star) {
						switch (token->kind) {
							case Token_var: 
							case Token_let: 
							case Token_const: {
								unop->mutability = to_mutability(token->kind).value();
								next();
								skip_lines();
								break;
							}
						}
					}

					unop->expression = parse_expression_2();
					unop->location = {unop->location.begin(), unop->expression->location.end()};
					return unop;
				}

				reporter.error(token->string, "Unexpected token {} when parsing expression.", token->kind);
				yield(false);
			}
		}

		invalid_code_path("node was not returned");
	}
	Node *parse_statement() {
		switch (token->kind) {
			case Token_return: {
				auto return_ = Return::create();
				return_->location = token->string;

				if (!current_container) {
					reporter.error(return_->location, "Return statement can not appear outside of a lambda.");
					yield(false);
				}

				if (auto lambda = as<Lambda>(current_container)) {
					return_->lambda = lambda;
					lambda->returns.add(return_);
				} else {
					reporter.error(return_->location, "Return statement can only appear in a lambda. But current container is {}.", current_container->kind);
					yield(false);
				}

				next();

				if (token->kind != '\n') {
					return_->value = parse_expression();
				}

				return return_;
			}
			case Token_while: {
				auto While = While::create();
				While->location = token->string;
				next();

				scoped_replace(current_loop, While);

				While->condition = parse_expression();

				skip_lines();
				if (token->kind == Token_then) {
					next();
					skip_lines();
				}

				While->body = parse_statement();

				return While;
			}
			case Token_continue: {
				if (!current_loop) {
					reporter.error(token->string, "`continue` must be inside a loop.");
					yield(false);
				}

				auto Continue = Continue::create();
				Continue->location = token->string;
				next();
				return Continue;
			}
			case Token_break: {
				auto Break = Break::create();
				Break->location = token->string;
				next();

				if (!current_block) {
					reporter.error(Break->location, "`break` must be inside a block.");
					yield(false);
				}

				if (token->kind == ':') {
					next();
					expect(Token_name);

					auto tag = token->string;
					next();
					Break->value = parse_expression();

					bool blocks_are_valid = true;

					auto block = current_block;
					while (1) {
						if (block->tag == tag) {
							if (blocks_are_valid) {
								Break->tag_block = block;
								Break->tag_block->breaks.add(Break);
							} else {
								reporter.error(Break->location, "Block with name {} is outside of current container.", tag);
								reporter.info(block->location, "Here is the block:");
								reporter.info(current_container->location, "Here is current container:");
								yield(false);
							}
							break;
						}

						block = block->parent;
						if (block) {
							if (block->container != current_container) {
								blocks_are_valid = false;
							}
						}

						if (!block) {
							reporter.error(Break->location, "Could not find block with name {}.", tag);
							yield(false);
						}
					}
				} else {
					if (!current_loop) {
						reporter.error(Break->location, "Empty `break` must be inside a loop.");
						yield(false);
					}
				}


				return Break;
			}
			case Token_directive: {
				if (token->string == "#extern") {
					auto extern_location = token->string;
					next();
					expect(Token_string);
					// Don't free extern_library. Lambdas point to it.
					extern_library = unescape_string(token->string);
					next();
					skip_lines();
					if (token->kind == Token_eof) {
						reporter.error(extern_location, "At least one definition must follow `extern` directive.");
						yield(false);
					}
					return parse_statement();
				}

				reporter.error(token->string, "Unknown directive.");
				yield(false);
				break;
			}
			case Token_if: {
				auto location = token->string;
				next();
				skip_lines();

				auto condition = parse_expression();

				skip_lines();
				if (token->kind == Token_then) {
					next();
					skip_lines();
				}

				auto true_branch = parse_statement();
				Node *false_branch = 0;

				skip_lines();
				if (token->kind == ';') {
					next();
					skip_lines();
				}
				if (token->kind == Token_else) {
					next();
					skip_lines();

					false_branch = parse_statement();

					location = {location.begin(), false_branch->location.end()};
				} else {
					location = {location.begin(), true_branch->location.end()};
				}

				if (false_branch) {
					if (auto true_expression = as<Expression>(true_branch)) {
						if (auto false_expression = as<Expression>(false_branch)) {
							auto If = IfExpression::create();
							If->location = location;
							If->condition = condition;
							If->true_branch = true_expression;
							If->false_branch = false_expression;
							return If;
						}
					}
				}

				auto If = IfStatement::create();
				If->location = location;
				If->condition = condition;
				If->true_branch = true_branch;
				If->false_branch = false_branch;
				return If;
			}
			case Token_import: {
				next();

				expect(Token_string);
				
				auto import = Import::create();
				import->path = unescape_string(token->string);
				
				next();
	
				auto full_path = tformat(u8"{}\\import\\{}.sp", compiler_root_directory, import->path);
				locked_use(files_to_parse) {
					files_to_parse.add({.location = import->location, .path = full_path});
				};

				return import;
			}
		}

		auto expression = parse_expression();

		return expression;
	}

	void yield(bool result) {
		if (result)
			success = true;
		tl::yield_reuse(parent_fiber, fiber);
	}
	void main() {
		scoped_replace(debug_current_location, {});

		while (true) {
			auto saved = token;
			skip_lines();
			while (token->kind == ';') {
				next();
				skip_lines();
			}

			if (token->kind == Token_eof) {
				break;
			}

			auto child = parse_statement();

			switch (child->kind) {
				case NodeKind::Block: reporter.error(child->location, "Blocks are not allowed in global scope."); yield(false); return;
			}

			ensure_allowed_in_statement_context(child);

			result_nodes.add(child);
		}

		yield(true);
	}
	static void fiber_main(void *param) {
		auto parser = (Parser *)param;
		parser->main();
	}

	void ensure_allowed_in_statement_context(Node *node) {
		switch (node->kind) {
			#define x(name) case NodeKind::name:
			ENUMERATE_STATEMENT_KIND(x)
			#undef x
			case NodeKind::Definition:
			case NodeKind::Block:
			case NodeKind::Call:
			case NodeKind::IfExpression:
			case NodeKind::Match:
				return;
			case NodeKind::Binary: {
				auto binary = (Binary *)node;
				switch (binary->operation) {
					case BinaryOperation::ass:
					case BinaryOperation::addass:
					case BinaryOperation::subass:
					case BinaryOperation::mulass:
					case BinaryOperation::divass:
					case BinaryOperation::modass:
					case BinaryOperation::borass:
					case BinaryOperation::banass:
					case BinaryOperation::bxoass:
					case BinaryOperation::bslass:
					case BinaryOperation::bsrass:
						return;
				}

				reporter.error(node->location, "Binary {} is not allowed in statement context.", binary->operation);
				yield(false);
			}
		}

		reporter.error(node->location, "{} is not allowed in statement context.", node->kind);
		yield(false);
	}
	bool next() {
		++token;
		debug_current_location = token->string;
		return token->kind != Token_eof;
	}
	void expect(std::underlying_type_t<TokenKind> expected_kind) {
		if (token->kind != expected_kind) {
			reporter.error(token->string, "Expected {}, but got {}", (TokenKind)expected_kind, *token);
			yield(false);
		}
	}
	void expect_not(std::underlying_type_t<TokenKind> unexpected_kind) {
		if (token->kind == unexpected_kind) {
			reporter.error(token->string, "Unexpected {}", (TokenKind)unexpected_kind);
			yield(false);
		}
	}
	void expect(std::initializer_list<std::underlying_type_t<TokenKind>> expected_kinds) {
		for (auto expected_kind : expected_kinds) {
			if (token->kind == expected_kind) {
				return;
			}
		}

		StringBuilder builder;
		append(builder, "Expected ");
		for (auto expected_kind : expected_kinds) {
			append_format(builder, "{} or ", (TokenKind)expected_kind);
		}
		append_format(builder, "but got {}.\0"s, *token);
		reporter.error(token->string, (char *)to_string(builder).data);
		yield(false);
	}
	void expect_not(std::initializer_list<std::underlying_type_t<TokenKind>> unexpected_kinds) {
		for (auto unexpected_kind : unexpected_kinds) {
			expect_not(unexpected_kind);
		}
	}
	void skip_lines() {
		while (token->kind == '\n')
			++token;
		debug_current_location = token->string;
	}
};

Optional<List<Node *>> tokens_to_nodes(Fiber parent_fiber, Span<Token> tokens) {
	timed_function();

	Parser parser = {};
	parser.parent_fiber = parent_fiber;
	parser.token = tokens.data;
	defer { parser.reporter.print_all(); };

	parser.fiber = get_new_fiber();
	set_start(parser.fiber, Parser::fiber_main, &parser);
	yield(parser.fiber);

	add_fiber_to_reuse(parser.fiber);
	parser.fiber = {};

	if (parser.success)
		return parser.result_nodes;

	return {};
}

bool read_file_and_parse_into_global_block(Fiber parent_fiber, String location, String path) {
	if (!file_exists(path)) {
		immediate_reporter.error(location, "File {} does not exist", path);
		return false;
	}

	// Will be used after function exits, don't free.
	auto source_buffer = read_entire_file(path, {.extra_space_before = 1, .extra_space_after = 1});

	auto source = (String)source_buffer.subspan(1, source_buffer.count - 2);
	
	content_start_to_file_name.get_or_insert(source.data) = path;

	auto tokens = source_to_tokens(source, path);
	if (!tokens) {
		LOG_ERROR_PATH("Failed to tokenize this file: {}", path);
		return false;
	}
	defer { free(tokens.value()); };
	auto nodes = tokens_to_nodes(parent_fiber, tokens.value());
	if (!nodes) {
		LOG_ERROR_PATH("Failed to parse this file: {}", path);
		return false;
	}

	for (auto &node : nodes.value()) {
		global_block.add(node);
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

struct NodeInterpreter {
	struct Scope {
		// This needs to be pointer-stable
		BucketHashMap<Definition *, Value> variables;
	};

	static NodeInterpreter *create(Fiber parent_fiber, Node *node) {
		auto context = DefaultAllocator{}.allocate<NodeInterpreter>();
		context->parent_fiber = parent_fiber;
		context->fiber = get_new_fiber();
		set_start(context->fiber, [](void *param) { ((NodeInterpreter *)param)->fiber_main(); }, context);
		context->node_to_execute = node;
		return context;
	}

	Result<Value, YieldResult> run() {
		tl::yield(fiber);
		add_fiber_to_reuse(fiber);
		fiber = {};
		if (yield_result == YieldResult::success) {
			return result_value;
		}
		return yield_result;
	}

private:

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

	static constexpr umm recursion_limit = 256;
	umm recursion_level = 0;

	Node *current_node = 0;
	Block *current_block = 0;
	Block *currently_breaking_from = 0;

	List<Scope> scope_stack;
	Value return_value;

	ReusableFiber fiber;
	Fiber parent_fiber;

	Node *node_to_execute;
	Value result_value;
	YieldResult yield_result;

	HashMap<String, HMODULE> loaded_extern_libraries;

	void fiber_main() {
		yield_result = YieldResult::fail;

		auto &scope = scope_stack.add(); // global scope

		result_value = execute(node_to_execute);
		yield(YieldResult::success);
	}

	void yield(YieldResult result) {
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

	Value load_address(Expression *expression) {
		scoped_replace(debug_current_location, expression->location);
		scoped_replace(current_node, expression);

		Value result = {};

		switch (expression->kind) {
#define x(name) case NodeKind::name: result = load_address_impl((##name *)expression); break;
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
	Value load_address_impl(IntegerLiteral *) { invalid_code_path(); }
	Value load_address_impl(BooleanLiteral *) { invalid_code_path(); }
	Value load_address_impl(NoneLiteral *) { invalid_code_path(); }
	Value load_address_impl(StringLiteral *) { invalid_code_path(); }
	Value load_address_impl(Definition *definition) {
		for (auto &scope : reversed(scope_stack)) {
			if (auto found = scope.variables.find(definition)) {
				return Value(&found->value);
			}
		}
		invalid_code_path();
	}
	Value load_address_impl(Block *block) {
		assert(block->children.count > 0);
		scoped_replace(current_block, block);
		for (auto child : block->children.skip(-1)) {
			EXECUTE_DEFN(ignored, child);
		}
		auto last_expression = as<Expression>(block->children.back());
		assert(last_expression);
		return load_address(last_expression);
	}
	Value load_address_impl(Lambda *) { invalid_code_path(); }
	Value load_address_impl(LambdaHead *) { invalid_code_path(); }
	Value load_address_impl(Name *name) {
		auto definition = name->definition();
		assert(definition);
		return load_address(definition);
	}
	Value load_address_impl(Call *) { invalid_code_path(); }
	Value load_address_impl(IfExpression *) { invalid_code_path(); }
	Value load_address_impl(BuiltinTypeName *) { invalid_code_path(); }
	Value load_address_impl(Binary *binary) {
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
	Value load_address_impl(Match *) { invalid_code_path(); }
	Value load_address_impl(Unary *unary) {
		if (unary->operation == UnaryOperation::dereference) {
			auto pointer = execute(unary->expression);
			assert(pointer.kind == ValueKind::pointer);
			return pointer;
		}
		invalid_code_path();
	}
	Value load_address_impl(Struct *) { invalid_code_path(); }
	Value load_address_impl(ArrayType *) { invalid_code_path(); }
	Value load_address_impl(Subscript *subscript) {
		LOAD_ADDRESS_DEFN(arr, subscript->subscriptable);
		EXECUTE_DEFN(index, subscript->index);
		return Value(&element_at(arr.pointer->elements, index));
	}
	Value load_address_impl(ArrayConstructor *) { invalid_code_path(); }
	Value load_address_impl(Import *) { invalid_code_path(); }

	Value execute(Node *node) {
		scoped_replace(debug_current_location, node->location);
		scoped_replace(current_node, node);

		switch (node->kind) {
#define x(name) case NodeKind::name: return execute_impl((##name *)node);
			ENUMERATE_NODE_KIND(x)
#undef x
		}
		invalid_code_path();
	}
	Value execute_impl(IntegerLiteral *literal) {
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
				case BuiltinType::UnsizedInteger: return Value(unsized_integer_tag, (s64)literal->value);
			}
		}

		immediate_reporter.error(literal->location, "Could not execute this literal because it does not have a concrete type, its type is {}. This is probably a bug in the compiler.", literal->type);
		yield(YieldResult::fail);
		return {};
	}
	Value execute_impl(BooleanLiteral *literal) {
		return Value(literal->value);
	}
	Value execute_impl(NoneLiteral *literal) {
		return Value(ValueKind::none);
	}
	Value execute_impl(StringLiteral *literal) {
		return Value(literal->value);
	}
	Value execute_impl(Definition *definition) {
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
	Value execute_impl(Return *return_) {
		if (return_->value) {
			EXECUTE_INTO(return_value, return_->value);
		}
		return Value(ValueKind::return_);
	}
	Value execute_impl(Block *block) {
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
	Value execute_impl(Lambda *lambda) {
		return Value(lambda);
	}
	Value execute_impl(LambdaHead *head) {
		return Value(Type(head));
	}
	Value execute_impl(Name *name) {
		auto definition = name->definition();
		assert(definition);

		if (!definition->container) {
			// Globals may need to be waited for.
			auto &scope = scope_stack[0];
			if (auto found = scope.variables.find(definition)) {
				return found->value;
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
				return found->value;
			}
		}
		invalid_code_path();
	}
	Value execute_impl(Call *call) {
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
				EXECUTE_DEFN(argument_value, argument);
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
							immediate_reporter.error(arguments[0]->location, "Unknown type {} in println", argument_values[0].kind);
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

				HMODULE module = 0;
				if (auto found = loaded_extern_libraries.find(lambda->extern_library)) {
					module = found->value;
				} else {
					module = LoadLibraryA((char *)null_terminate(lambda->extern_library).data);
					if (!module) {
						immediate_reporter.error(call->location, "Failed to load extern library {}", lambda->extern_library);
						immediate_reporter.info(lambda->location, "Here is the definition:");
						yield(YieldResult::fail);
					}

					loaded_extern_libraries.insert(lambda->extern_library, module);
				}

				assert(lambda->definition);

				auto func = (u64(*)(u64,u64,u64,u64,u64,u64,u64,u64))GetProcAddress(module, (char *)null_terminate(lambda->definition->name).data);
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
							immediate_reporter.error(argument->location, "Can't pass value of type {} to extern function.", argument->type);
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
				EXECUTE_DEFN(argument_value, argument);
				argument_values.add(argument_value);
			}

			return Value(struct_tag, argument_values);
		}
	}
	Value execute_impl(IfStatement *If) {
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
	Value execute_impl(IfExpression *If) {
		EXECUTE_DEFN(condition, If->condition);
		assert(condition.kind == ValueKind::Bool);
		EXECUTE_DEFN(value, condition.Bool ? If->true_branch : If->false_branch);
		return value;
	}
	Value execute_impl(While *While) {
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
	Value execute_impl(Continue *Continue) {
		return Value(ValueKind::continue_);
	}
	Value execute_impl(Break *Break) {
		if (!Break->value) {
			return Value(ValueKind::break_);
		}

		assert(Break->tag_block);

		EXECUTE_DEFN(value, Break->value);
		currently_breaking_from = Break->tag_block;
		return value;
	}
	Value execute_impl(BuiltinTypeName *type) { 
		return Value(Type(type));
	}
	Value execute_impl(Binary *binary) {
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

		immediate_reporter.error(binary->location, "Invalid binary operation: {} {} {}", left.kind, binary->operation, right.kind);
		yield(YieldResult::fail);
		return {};
	}
	Value execute_impl(Match *match) {
		EXECUTE_DEFN(value, match->expression);
		assert(value.kind == ValueKind::S64, "Only this is implemented");

		for (auto Case : match->cases) {
			EXECUTE_DEFN(from, Case.from);
			assert(from.kind == ValueKind::S64, "Only this is implemented");
			if (value.S64 == from.S64) {
				return execute(Case.to);
			}
		}

		invalid_code_path(match->location, "match did not match value {}", value);
	}
	Value execute_impl(Unary *unary) {
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
	Value execute_impl(Struct *Struct) { 
		return Value(Type(Struct));
	}
	Value execute_impl(ArrayType *array) {
		return Value((Type)array);
	}
	Value execute_impl(Subscript *node) {
		EXECUTE_DEFN(array, node->subscriptable);
		EXECUTE_DEFN(index, node->index);
		return element_at(array.elements, index);
	}
	Value execute_impl(ArrayConstructor *node) {
		Value result;
		result.kind = ValueKind::array;
		result.elements = {};
		result.elements.resize(node->elements.count);
		for (umm i = 0; i < node->elements.count; ++i) {
			EXECUTE_INTO(result.elements[i], node->elements[i]);
		}
		return result;
	}
	Value execute_impl(Import *import) { invalid_code_path(); }
};

#undef PERFORM_WITH_BREAKS
#undef EXECUTE_INTO
#undef EXECUTE_DEFN
#undef LOAD_ADDRESS_INTO
#undef LOAD_ADDRESS_DEFN

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
		if (auto definition = as<Definition>(directed)) {
			head = direct_as<LambdaHead>(definition->type);
		}
	}
	return {lambda, head, struct_};
}

#define PASSTHROUGH(...) __VA_ARGS__

#define ENUMERATE_COMPARISONS \
	x(equals) \
	x(not_equals) \
	x(signed_less) \
	x(signed_greater) \
	x(signed_less_equals) \
	x(signed_greater_equals) \
	x(unsigned_less) \
	x(unsigned_greater) \
	x(unsigned_less_equals) \
	x(unsigned_greater_equals) \

enum class Comparison : u8 {
#define x(name) name,
	ENUMERATE_COMPARISONS
#undef x
};

inline umm append(StringBuilder &builder, Comparison c) {
	switch (c) {
		#define x(name) case Comparison::name: return append(builder, #name);
		#define y(name, value) x(name)
		ENUMERATE_COMPARISONS
		#undef y
		#undef x
	}
	return append_format(builder, "(unknown Comparison {})", (u64)c);
}

namespace Bytecode {

#define ENUMERATE_1248 \
	x(1) \
	x(2) \
	x(4) \
	x(8) \

#define ENUMERATE_NAMED_BYTECODE_REGISTERS \
	x(base           , 248) /* NOTE: all registers before this one can be allocated. */ \
	x(stack          , 249) \
	x(returns        , 250) \
	x(arguments      , 251) \
	x(temporary      , 252) \
	x(locals         , 253) \
	x(global_readonly, 254) \
	x(global_mutable , 255) \

enum class Register : u8 {
#define x(name, value) name = value,
	ENUMERATE_NAMED_BYTECODE_REGISTERS
#undef x
};

struct Address {
	Optional<Register> base = {};
	Register element_index = {};
	u8 element_size = {};
	s64 offset = {};
};

struct Site {
	Site() { memset(this, 0, sizeof(*this)); }
	Site(Register r) : _is_address(false), r(r) {}
	Site(Address a) : _is_address(true), a(a) {}
	~Site() {}

	bool is_register() { return !_is_address; }
	bool is_address() { return _is_address; }

	Register &get_register() { assert(!_is_address); return r; }
	Address &get_address() { assert(_is_address); return a; }

private:
	bool _is_address;
	union {
		Register r;
		Address a;
	};
};

struct InputValue {
	InputValue() { memset(this, 0, sizeof *this); }
	InputValue(Register r) : kind(Kind::Register), r(r) {}
	InputValue(Address a) : kind(Kind::Address), a(a) {}
	InputValue(s64 c) : kind(Kind::Constant), c(c) {}
	InputValue(Site s) : kind(s.is_register() ? Kind::Register : Kind::Address) {
		if (s.is_register())
			r = s.get_register();
		else
			a = s.get_address();
	}
	~InputValue() {}

	bool is_register() { return kind == Kind::Register; }
	bool is_address() { return kind == Kind::Address; }
	bool is_constant() { return kind == Kind::Constant; }

	Register &get_register() { assert(is_register()); return r; }
	Address &get_address() { assert(is_address()); return a; }
	s64 &get_constant() { assert(is_constant()); return c; }

private:
	enum class Kind {
		Register,
		Address,
		Constant,
	};

	Kind kind;
	union {
		Register r;
		Address a;
		s64 c;
	};
};

#define ENUMERATE_INTRINSICS \
	x(println_S64) \
	x(println_String) \
	x(panic) \
	x(debug_break) \

enum class Intrinsic : u8 {
#define x(name) name,
	ENUMERATE_INTRINSICS
#undef x
};

inline umm append(StringBuilder &builder, Intrinsic i) {
	switch (i) {
		#define x(name) case Intrinsic::name: return append(builder, #name);
		#define y(name, value) x(name)
		ENUMERATE_INTRINSICS
		#undef y
		#undef x		

	}
	return append_format(builder, "(unknown Intrinsic {})", (u64)i);
}

#define xor xor_
#define and and_
#define or or_

/*
#define y(type, name)
#define x(name, fields)
ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
#undef y
*/
#define ENUMERATE_BYTECODE_INSTRUCTION_KIND \
	x(nop,  ()) \
	x(push, (y(InputValue, s))) \
	x(pop,  (y(Site, d))) \
	x(copy, (y(Site, d) y(InputValue, s) y(u64, size))) \
	x(set,  (y(Address, d) y(u8, value) y(u64, size))) \
	x(lea,  (y(Site, d) y(Address, s))) \
	x(add1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(div1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mod1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or1,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra1, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp1, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(add2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(div2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mod2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or2,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra2, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp2, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(add4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(div4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mod4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or4,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra4, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp4, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(add8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sub8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mul8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(div8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(mod8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(xor8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(and8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(or8,  (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sll8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(srl8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(sra8, (y(Site, d) y(InputValue, a) y(InputValue, b))) \
	x(cmp8, (y(Site, d) y(InputValue, a) y(InputValue, b) y(Comparison, cmp))) \
	x(sex21,  (y(Site, d) y(InputValue, a))) \
	x(sex41,  (y(Site, d) y(InputValue, a))) \
	x(sex42,  (y(Site, d) y(InputValue, a))) \
	x(sex81,  (y(Site, d) y(InputValue, a))) \
	x(sex82,  (y(Site, d) y(InputValue, a))) \
	x(sex84,  (y(Site, d) y(InputValue, a))) \
	x(call, (y(InputValue, d))) \
	x(callext, (y(Lambda *, lambda) y(String, lib) y(String, name))) \
	x(ret,  ()) \
	x(jmp,  (y(InputValue, d))) \
	x(jz,   (y(Site, s) y(InputValue, d))) \
	x(jnz,  (y(Site, s) y(InputValue, d))) \
	x(intrinsic, (y(Intrinsic, i))) \

enum class InstructionKind : u8 {
#define x(name, fields) name,
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
};

struct Instruction {
	InstructionKind kind;

	// `Invalid` is here to prevent me from leaving an uninitialized member.
#define y(type, name) type name = Invalid();
#define x(name, fields)      \
	template <class Invalid> \
	struct name##_x {        \
		PASSTHROUGH fields   \
	};
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
#undef y

#define x(name, fields) using name##_t = name##_x<void>;
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x

#define x(name, fields) name##_t &name() { assert(kind == InstructionKind::name); return v_##name; }
	ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x

	union {
#define x(name, fields) name##_t v_##name;
		ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
	};

	~Instruction() {}

	void visit_addresses(auto &&visitor) {
		switch (kind) {
#define y(type, name) visit_address(i.name, visitor);
#define x(name, fields)           \
	case InstructionKind::name: { \
		auto &i = v_##name;       \
		PASSTHROUGH fields;       \
		break;                    \
	}
		ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
#undef y
		}
	}

private:
	static void visit_address(auto, auto &&visitor) {}
	static void visit_address(Address &a, auto &&visitor) { visitor(a); }
	static void visit_address(Site &s, auto &&visitor) { if (s.is_address()) visitor(s.get_address()); }
	static void visit_address(InputValue &v, auto &&visitor) { if (v.is_address()) visitor(v.get_address()); }
};


struct Callback {
	void *start_address;
};

u64 ffi_callback(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda);

Callback generate_callback(Lambda *lambda) {
	// arg0 - rcx
	// arg1 - rdx
	// arg2 - r8
	// arg3 - r9
	assert(lambda->head.parameters_block.definition_list.count == 4, "Other count of arguments not implemented");
	List<u8> bytes;
	
	// sub rsp, 40 // 8 bytes for lambda and 32 bytes for shadow space
	bytes.add({0x48, 0x83, 0xec, 40});

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
	bytes.add({0x48, 0x83, 0xc4, 40});

	// ret
	bytes.add({0xc3});


	void *page = VirtualAlloc(0, bytes.count, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);

	memcpy(page, bytes.data, bytes.count);
	memset((char *)page + bytes.count, 0xcc, ceil(bytes.count, (umm)4096) - bytes.count);

	DWORD old_protect;
	if (!VirtualProtect(page, bytes.count, PAGE_EXECUTE_READ, &old_protect)) {
		immediate_reporter.error(lambda->location, "FATAL: VirtualProtect failed: {}", win32_error());
		exit(-1);
	}

	return {page};
}


struct Bytecode {
	List<Instruction> instructions;
	List<u8> global_readonly_data;
	List<u8> global_mutable_data;
};

struct Builder {
	static constexpr s64 register_size = 8;
	static constexpr s64 pointer_size = 8;

	// return value
	// arg3
	// arg2
	// arg1
	// arg0
	// return address
	// old base <- base

#define MI(name, ...)                  \
	Instruction {                      \
		.kind = InstructionKind::name, \
		.v_##name = { __VA_ARGS__ }    \
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

	Builder() {
		for (umm i = 0; i < (umm)Register::base; ++i) {
			available_registers.set(i, true);
		}
	}

	Bytecode output_bytecode;

	Bytecode build(Expression *expression) {
		entry_point_instruction_index = output_bytecode.instructions.count;
		tmpval(destination, get_size(expression->type));
		output(destination, expression);
		for (auto [index, lambda] : calls_to_patch) {
			auto &i = output_bytecode.instructions[index];

			if (lambda->is_extern) {
				immediate_reporter.error(lambda->location, "External calls not implemented yet");
				// TODO: better failing
				invalid_code_path();
			}

			i.call().d = lambda->first_instruction_index;
		}
		
		for (auto relocation : lambda_relocations) {
			*(u64 *)&(*relocation.section)[relocation.offset] = relocation.lambda->first_instruction_index;
		}

		return output_bytecode;
	}


	void append_global_definition(Definition *definition) {
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

			Result<Value, Node *> get_constant_value(Node *);
			auto value = get_constant_value(definition->initial_value).value();

			write(section, value, definition->initial_value->type);
		} else {
			section.resize(section.count + get_size(definition->type));
		}
	}

	void append_lambda(Lambda *lambda) {
		scoped_replace(locals_size, 0);
		scoped_replace(max_temporary_size, 0);
		jumps_to_ret.clear();

		auto first_instruction_index = output_bytecode.instructions.count;

		auto head = &lambda->head;

		I(push, Register::base);
		I(copy, Register::base, Register::stack, register_size);
		auto reserver_index = output_bytecode.instructions.count;
		I(sub8, Register::stack, Register::stack, 0);

		u64 return_value_size = get_size(head->return_type);
		u64 total_parameters_size = 0;

		for (auto parameter : head->parameters_block.definition_list) {
			parameter->offset = total_parameters_size;
			total_parameters_size += get_size(parameter->type);
		}

		auto return_value_destination = Address { .base = Register::returns };

		I(set, .d = return_value_destination, .value = 0, .size = align_size(return_value_size));

		output(return_value_destination, lambda->body);

		lambda->first_instruction_index = first_instruction_index;
		lambda_infos.insert(lambda, {
			.locals_size = locals_size,
			.temporary_size = max_temporary_size,
		});

		u64 reserved_stack_size = locals_size + max_temporary_size;

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
		for (auto [index, instr] : enumerate(lambda_instructions)) {
			dbgln("{}: {}", first_instruction_index + index, instr);
		}

		for (auto &i : lambda_instructions) {
			i.visit_addresses([&] (Address &a) {
				if (a.base) {
					switch(a.base.value()) {
						case Register::locals: {
							a.base = Register::base;
							a.offset -= locals_size + max_temporary_size;
							break;
						}
						case Register::temporary: {
							a.base = Register::base;
							a.offset -= max_temporary_size;
							break;
						}
						case Register::arguments: {
							a.base = Register::base;
							a.offset += 16; // saved base + return address
							break;
						}
						case Register::returns: {
							a.base = Register::base;
							a.offset += total_parameters_size + 16; // parameters + saved base + return address
							break;
						}
					}
				}
			});

			if (i.kind == InstructionKind::copy) {
				REDECLARE_REF(i, i.copy());
				if (i.s.is_constant()) {
					assert(i.size <= 8);
				}
			}
		}

		dbgln();
	}

	umm entry_point() { return entry_point_instruction_index; }

	struct LambdaInfo {
		umm locals_size = 0;
		umm temporary_size = 0;
	};

	struct BlockInfo {
		Site destination;
		List<umm> break_jump_indices;
	};

	BitSet<(umm)Register::base> available_registers;
	u64 temporary_offset = 0;
	u64 max_temporary_size = 0;
	umm entry_point_instruction_index = -1;
	umm locals_size = 0;
	List<std::tuple<umm, Lambda *>> calls_to_patch;
	List<umm> jumps_to_ret;
	BucketHashMap<Lambda *, LambdaInfo> lambda_infos;
	BucketHashMap<Block *, BlockInfo> block_infos;
	BucketHashMap<While *, List<umm>> continue_jump_indices;
	BucketHashMap<While *, List<umm>> loop_break_indices;
	struct LambdaRelocation {
		List<u8> *section;
		u64 offset = 0;
		Lambda *lambda = 0;
	};
	List<LambdaRelocation> lambda_relocations;

	ContiguousHashMap<String, umm> string_literal_offsets;

	void write(List<u8> &data, Value value, Type type) {
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
				data.add(Span((u8 *)&value.S64, get_size(type)));
				break;
			}
			case ValueKind::lambda: {
				lambda_relocations.add({&data, data.count, value.lambda});
				u64 index = 0;
				data.add(value_as_bytes(index));
				break;
			}
			default:
				invalid_code_path("Writing {} to section is not handled.", value.kind);
		}
	}

	u64 align_size(u64 x) { return ceil<u64>(max<u64>(1, x), 8); }
	s64 align_size(s64 x) { return ceil<s64>(max<s64>(1, x), 8); }

	Register allocate_register() {
		return (Register)available_registers.pop().value();
	}
	void deallocate(Register r) {
		assert(!available_registers.get((umm)r));
		available_registers.set((umm)r, true);
	}

	Address allocate_temporary(u64 size) {
		Address result;
		result.base = Register::temporary;
		result.offset = temporary_offset;
		temporary_offset += size;
		max_temporary_size = max(max_temporary_size, temporary_offset);
		return result;
	}

	Site create_destination(u64 size) {
		if (size <= 8) {
			return allocate_register();
		} else {
			return allocate_temporary(size);
		}
	}
	void deallocate(Site d) {
		if (d.is_register()) {
			deallocate(d.get_register());
		}
	}

	void output(Site destination, Expression *expression) {
		switch (expression->kind) {
#define x(name) case NodeKind::name: return output_impl(destination, (name *)expression);
			ENUMERATE_EXPRESSION_KIND(x)
#undef x
		}
		invalid_code_path("invalid expression kind {}", expression->kind);
	}

	void output(Statement *statement) {
		switch (statement->kind) {
#define x(name) case NodeKind::name: return output_impl((name *)statement);
			ENUMERATE_STATEMENT_KIND(x)
#undef x
		}
		invalid_code_path("invalid statement kind {}", statement->kind);
	}

	void output_discard(Node *node) {
		if (auto definition = as<Definition>(node)) {
			output_local_definition({}, definition);
		} else if (auto expression = as<Expression>(node)) {
			tmpval(destination, get_size(expression->type));
			output(destination, expression);
		} else {
			auto statement = as<Statement>(node);
			assert(statement);
			output(statement);
		}
	}

	void output_local_definition(Optional<Site> destination, Definition *definition) {
		assert(definition->mutability != Mutability::constant);
		auto offset = locals_size;
		definition->offset = locals_size;
		auto definition_size = get_size(definition->type);
		locals_size = ceil<umm>(locals_size + definition_size, 8);
		auto address = Address{.base = Register::locals, .offset = (s64)offset};
		if (definition->initial_value) {
			output(address, definition->initial_value);
		} else {
			if (direct_as<Struct>(definition->type)) {
				immediate_reporter.warning(definition->location, "default struct values not implemented. initializing with zero");
			}
			I(set, .d = address, .value = 0, .size = definition_size);
		}
		if (destination) {
			I(copy, .d = destination.value(), .s = address, .size = definition_size);
		}
	}

	Address get_definition_address(Definition *definition) {
		assert(definition->offset != invalid_definition_offset);
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

	void output_impl(Site destination, Block *block) {
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
	} 
	void output_impl(Site destination, Call *call) {
		auto [lambda, head, Struct] = get_lambda_and_head_or_struct(call->callable);
		switch (call->call_kind) {
			case CallKind::lambda: {
				assert(lambda);
				assert(head);

				s64 total_arguments_size = 0;
		
				s64 return_value_size = get_size(head->return_type);

				List<s64, TemporaryAllocator> argument_offsets;
				argument_offsets.reserve(call->arguments.count);

				for (auto argument : call->arguments) {
					s64 argument_size = get_size(argument->type);
					argument_offsets.add(total_arguments_size);
					total_arguments_size += align_size(argument_size);
				}

				auto registers_to_save = ~available_registers;
				if (destination.is_register())
					registers_to_save.set((umm)destination.get_register(), false);

				for_each(registers_to_save, [&] (umm r) {
					I(push, (Register)r);
				});
				defer {
					for_each<ForEach_reverse>(registers_to_save, [&] (umm r) {
						I(pop, (Register)r);
					});
				};

				I(sub8, .d = Register::stack, .a = Register::stack, .b = total_arguments_size + align_size(return_value_size));
				defer {
					I(add8, .d = Register::stack, .a = Register::stack, .b = total_arguments_size + align_size(return_value_size));
				};

				for (umm i = 0; i < call->arguments.count; ++i) {
					auto offset = argument_offsets[i];
					auto argument = call->arguments[i];

					Site destination = Address{.base = Register::stack, .offset = offset};

					output(destination, argument);
				}

				if (lambda->is_intrinsic) {
					auto definition = lambda->definition;
					assert(definition, lambda->location, "Intrinsic function is expected to have a definition, but for some reason this doesn't");
					if (definition->name == "println") {
						auto &parameters = lambda->head.parameters_block.definition_list;
						assert(parameters.count == 1, lambda->location, "Intrinsic function 'println' is expected to have exactly one parameter");
						auto parameter_type = parameters[0]->type;
						if (types_match(parameter_type, BuiltinType::S64)) {
							I(intrinsic, Intrinsic::println_S64);
						} else if (types_match(parameter_type, BuiltinType::String)) {
							I(intrinsic, Intrinsic::println_String);
						} else {
							invalid_code_path(definition->location, "Unsupported parameter type '{}' in 'println' intrinsic", parameter_type);
						}
					} else {
						invalid_code_path(definition->location, "Unknown intrinsic name '{}'", definition->name);
					}
				} else if (lambda->is_extern) {
					assert(lambda->definition);
					assert(lambda->definition->name.count);
					I(callext, .lambda = lambda, .lib = lambda->extern_library, .name = lambda->definition->name);
				} else {
					calls_to_patch.add({output_bytecode.instructions.count, lambda});
					I(call, .d = -1);
				}
				I(copy, .d = destination, .s = Address{.base = Register::stack, .offset = total_arguments_size}, .size = (u64)return_value_size);
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

					output(member_address, argument);
				}
				break;
			}
			default: 
				not_implemented();
		}
	} 
	void output_impl(Site destination, Definition *definition) {
		output_local_definition(destination, definition);
	}
	void output_impl(Site destination, IntegerLiteral *literal) {
		I(copy, .d = destination, .s = (s64)literal->value, .size = get_size(literal->type));
	} 
	void output_impl(Site destination, BooleanLiteral *literal) {
		I(copy, .d = destination, .s = (s64)literal->value, .size = 1);
	} 
	void output_impl(Site destination, NoneLiteral *literal) {
		I(copy, .d = destination, .s = 0, .size = get_size(literal->type));
	} 
	void output_impl(Site destination, StringLiteral *literal) {
		umm offset = -1;
		auto found = string_literal_offsets.find(literal->value);
		if (found) {
			offset = found->value;
		} else {
			offset = output_bytecode.global_readonly_data.count;
			output_bytecode.global_readonly_data.add((Span<u8>)literal->value);
			output_bytecode.global_readonly_data.add('\0');
			string_literal_offsets.insert(literal->value, offset);
		}

		assert(destination.is_address(), "not implemented");

		I(lea, .d = destination, .s = Address{ .base = Register::global_readonly, .offset = (s64)offset });
		destination.get_address().offset += 8;
		I(copy, .d = destination, .s = (s64)literal->value.count, .size = 8);
	} 
	void output_impl(Site destination, Lambda *node) { not_implemented(); } 
	void output_impl(Site destination, LambdaHead *node) { not_implemented(); } 
	void output_impl(Site destination, Name *name) {
		auto definition = name->definition();
		I(copy, .d = destination, .s = get_definition_address(definition), .size = get_size(name->type));
	} 
	void output_impl(Site destination, IfExpression *If) {
		tmpreg(cr);
		output(cr, If->condition);
		auto jz_index = output_bytecode.instructions.count;
		I(jz, cr, 0);
		output(destination, If->true_branch);
		auto jmp_index = output_bytecode.instructions.count;
		I(jmp, 0);
		output_bytecode.instructions[jz_index].jz().d = output_bytecode.instructions.count;
		output(destination, If->false_branch);
		output_bytecode.instructions[jmp_index].jmp().d = output_bytecode.instructions.count;
	} 
	void output_impl(Site destination, BuiltinTypeName *node) {
		I(copy, destination, (s64)node->type_kind, 8);
	} 
	void output_impl(Site destination, Binary *binary) {
		if (binary->operation == BinaryOperation::dot) {
			auto Struct = direct_as<::Struct>(binary->left->type);
			auto member_size = get_size(binary->type);
			auto member = as<Name>(binary->right)->definition();
			if (Struct) {
				tmpaddr(struct_addr, Struct->size);
				output(struct_addr, binary->left);
			
				auto member_address = struct_addr;
				member_address.offset += member->offset;
				I(copy, destination, member_address, member_size);
			} else {
				assert(as_pointer(binary->left->type));
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
				auto left_size = get_size(binary->left->type);
				auto right_size = get_size(binary->right->type);
				assert(result_size == left_size);
				assert(result_size == right_size);
				assert(result_size <= 8);

				tmpreg(left);
				output(left, binary->left);
				tmpreg(right);
				output(right, binary->right);

#define x(n)                                                                                       \
	case n: {                                                                                      \
		switch (binary->operation) {                                                               \
			case BinaryOperation::add: I(add##n, .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::sub: I(sub##n, .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::mul: I(mul##n, .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::div: I(div##n, .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::mod: I(mod##n, .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::bxo: I(xor##n, .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::ban: I(and##n, .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::bor: I(or##n,  .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::bsl: I(sll##n, .d = destination, .a = left, .b = right);  break; \
			case BinaryOperation::bsr: {                                                           \
				if (is_signed_integer(binary->left->type))                                         \
					I(sra##n, .d = destination, .a = left, .b = right);                            \
				else                                                                               \
					I(srl##n, .d = destination, .a = left, .b = right);                            \
				break;                                                                             \
			}                                                                                      \
			default: not_implemented();                                                            \
		}                                                                                          \
		break;                                                                                     \
	}

				switch (result_size) {
					ENUMERATE_1248
				}
#undef x

				break;
			}
			case BinaryOperation::equ:
			case BinaryOperation::neq:
			case BinaryOperation::les:
			case BinaryOperation::grt:
			case BinaryOperation::leq:
			case BinaryOperation::grq: {
				auto left_size = get_size(binary->left->type);
				auto right_size = get_size(binary->right->type);
				assert(right_size == left_size);
				assert(right_size <= 8);

				tmpreg(left);
				output(left, binary->left);
				tmpreg(right);
				output(right, binary->right);

				if (::is_signed_integer(binary->left->type)) {
					switch (binary->operation) {
						case BinaryOperation::equ: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::equals    );  break;
						case BinaryOperation::neq: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::not_equals);  break;
						case BinaryOperation::les: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less          );  break;
						case BinaryOperation::grt: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater       );  break;
						case BinaryOperation::leq: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_less_equals   );  break;
						case BinaryOperation::grq: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::signed_greater_equals);  break;
						default: not_implemented();
					}
				} else {
					switch (binary->operation) {
						case BinaryOperation::equ: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::equals    );  break;
						case BinaryOperation::neq: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::not_equals);  break;
						case BinaryOperation::les: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less          );  break;
						case BinaryOperation::grt: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater       );  break;
						case BinaryOperation::leq: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_less_equals   );  break;
						case BinaryOperation::grq: I(cmp8, .d = destination, .a = left, .b = right, .cmp = Comparison::unsigned_greater_equals);  break;
						default: not_implemented();
					}
				}
				break;
			}
			case BinaryOperation::lan: {
				output(destination, binary->left);
				auto jz_index = output_bytecode.instructions.count;
				I(jz, destination, 0);
				output(destination, binary->right);
				output_bytecode.instructions[jz_index].jz().d = output_bytecode.instructions.count;
				break;
			}
			case BinaryOperation::lor: {
				output(destination, binary->left);
				auto jnz_index = output_bytecode.instructions.count;
				I(jnz, destination, 0);
				output(destination, binary->right);
				output_bytecode.instructions[jnz_index].jnz().d = output_bytecode.instructions.count;
				break;
			}
			case BinaryOperation::as: {
				auto source_type = direct(binary->left->type);
				auto target_type = direct(binary->right);
				// From none
				if (types_match(source_type, BuiltinType::None)) {
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						I(copy, destination, 0, pointer_size);
						break;
					}
				}

				// From lambda
				if (auto lambda = direct_as<Lambda>(binary->left)) {
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						// TODO: only works with direct lambdas.
						auto callback = generate_callback(lambda);
						I(copy, destination, (s64)callback.start_address, pointer_size);
						break;
					}
				}

				// From string
				if (types_match(source_type, BuiltinType::String)) {
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						tmpaddr(tmpstr, get_size(BuiltinType::String));
						output(tmpstr, binary->left);
						I(copy, destination, tmpstr, pointer_size);
						break;
					}
				}

				// From pointer
				if (auto left_pointer = as_pointer(source_type)) {
					// To pointer
					if (auto right_pointer = as_pointer(target_type)) {
						output(destination, binary->left);
						break;
					}
					// To integer
					if (is_concrete_integer(target_type)) {
						output(destination, binary->left);
						break;
					}
				}
				

				// From integer
				if (is_concrete_integer(source_type)) {
					// To integer
					if (is_concrete_integer(target_type)) {
						auto target_size = get_size(target_type);
						auto source_size = get_size(source_type);
						if (target_size == source_size) {
							output(destination, binary->left);
							// Same size, noop
						} else if (target_size >= source_size) {
							output(destination, binary->left);
							// Sign or zero extend
							if (is_signed_integer(source_type)) {
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
							output(tmp, binary->left);
							// Truncate
							I(copy, destination, tmp, target_size);
						}
						break;
					}
				}

				invalid_code_path();
				break;
			}
			default: not_implemented();
		} 
	}
	void output_impl(Site destination, Match *match) {
		// FIXME: match without a default case can't always yield a value, so I think 
		// there should be separation between match expression and match statement.
		// Or just insert a default case that will panic?

		auto matchee_size = get_size(match->expression->type);
		assert(matchee_size <= 8);
		tmpreg(matchee);
		output(matchee, match->expression);

		tmpreg(does_match);

		List<umm> jump_to_end_indices;

		for (auto &Case : match->cases) {
			if (!Case.from)
				continue;

			umm prev_case_jump_over_index = 0;
			{
				tmpreg(from);
				output(from, Case.from);
				auto size = get_size(match->expression->type);
				switch (size) {
					case 1: I(cmp1, does_match, matchee, from, Comparison::equals); break;
					case 2: I(cmp2, does_match, matchee, from, Comparison::equals); break;
					case 4: I(cmp4, does_match, matchee, from, Comparison::equals); break;
					case 8: I(cmp8, does_match, matchee, from, Comparison::equals); break;
					default: invalid_code_path("`match` only works on sizes 1, 2, 4 or 8, but not {}", size);
				}
				prev_case_jump_over_index = output_bytecode.instructions.count;
				I(jz, does_match, 0);
			}
			output(destination, Case.to);
			jump_to_end_indices.add(output_bytecode.instructions.count);
			I(jmp, 0);
			output_bytecode.instructions[prev_case_jump_over_index].jz().d = output_bytecode.instructions.count;
		}
		
		if (match->default_case) {
			output(destination, match->default_case->to);
		} else {
			if (!types_match(match->type, BuiltinType::None)) {
				I(sub8, Register::stack, Register::stack, 16);
				StringLiteral message;
				message.value = tformat(u8"{}: failed to execute `match` expression with no default case", get_source_location(match->location));
				message.location = match->location;
				message.type = get_builtin_type(BuiltinType::String);
				output(Address{.base = Register::stack}, &message);
				I(intrinsic, Intrinsic::println_String);
				I(add8, Register::stack, Register::stack, 16);
				I(intrinsic, Intrinsic::panic);
			}
		}

		for (auto i : jump_to_end_indices) {
			output_bytecode.instructions[i].jmp().d = output_bytecode.instructions.count;
		}
	}
	void output_impl(Site destination, Unary *unary) {
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
			default: not_implemented();
		}
	} 
	void output_impl(Site destination, Struct *node) { not_implemented(); } 
	void output_impl(Site destination, ArrayType *node) { not_implemented(); } 
	void output_impl(Site destination, Subscript *subscript) {
		auto array_type = as<ArrayType>(subscript->subscriptable->type);
		assert(array_type);

		auto element_size = get_size(array_type->element_type);

		tmpaddr(array_address, get_size(array_type));
		output(array_address, subscript->subscriptable);

		tmpreg(index_reg);
		output(index_reg, subscript->index);

		auto element_address = array_address;
		element_address.element_index = index_reg;
		element_address.element_size = element_size;

		I(copy, destination, element_address, element_size);
	} 
	void output_impl(Site destination, ArrayConstructor *arr) {
		auto element_size = get_size(as<ArrayType>(arr->type)->element_type);
		assert(destination.is_address());
		auto element_destination = destination.get_address();
		for (auto element : arr->elements) {
			output(element_destination, element);
			element_destination.offset += element_size;
		}
	} 
	void output_impl(Return *ret) {
		if (ret->value) {
			Site return_value_destination = Address { .base = Register::returns };
			output(return_value_destination, ret->value);
		}
		jumps_to_ret.add(output_bytecode.instructions.count);
		I(jmp, 0);
	} 
	void output_impl(While *While) {
		tmpreg(cr);
		auto condition_index = output_bytecode.instructions.count;
		output(cr, While->condition);
		auto jz_index = output_bytecode.instructions.count;
		I(jz, cr, 0);
		output_discard(While->body);
		I(jmp, (s64)condition_index);
		output_bytecode.instructions[jz_index].jz().d = output_bytecode.instructions.count;
		for (auto i : continue_jump_indices.get_or_insert(While)) {
			output_bytecode.instructions[i].jmp().d = condition_index;
		}
		for (auto i : loop_break_indices.get_or_insert(While)) {
			output_bytecode.instructions[i].jmp().d = output_bytecode.instructions.count;
		}
	} 
	void output_impl(Continue *node) {
		continue_jump_indices.get_or_insert(node->loop).add(output_bytecode.instructions.count);
		I(jmp, 0);
	} 
	void output_impl(Break *node) {
		if (node->tag_block) {
			auto &info = block_infos.find(node->tag_block)->value;
			output(info.destination, node->value);
			info.break_jump_indices.add(output_bytecode.instructions.count);
			I(jmp, 0);
		} else {
			assert(node->loop);
			loop_break_indices.get_or_insert(node->loop).add(output_bytecode.instructions.count);
			I(jmp, 0);
		}
	}
	void output_impl(IfStatement *If) {
		tmpreg(cr);
		output(cr, If->condition);
		auto jz_index = output_bytecode.instructions.count;
		I(jz, cr, 0);
		output_discard(If->true_branch);
		if (If->false_branch) {
			auto jmp_index = output_bytecode.instructions.count;
			I(jmp, 0);
			output_bytecode.instructions[jz_index].jz().d = output_bytecode.instructions.count;
			output_discard(If->false_branch);
			output_bytecode.instructions[jmp_index].jmp().d = output_bytecode.instructions.count;
		} else {
			output_bytecode.instructions[jz_index].jz().d = output_bytecode.instructions.count;
		}
	} 
	void output_impl(Import *import) { invalid_code_path(); } 

	void load_address(Site destination, Expression *expression) {
		switch (expression->kind) {
#define x(name) case NodeKind::name: load_address_impl(destination, (name *)expression); break;
			ENUMERATE_EXPRESSION_KIND(x)
#undef x
		}
	}

	void load_address_impl(Site destination, Block *block) {
		for (auto child : block->children.skip(-1)) {
			output_discard(child);
		}
		return load_address(destination, as<Expression>(block->children.back()));
	}
	void load_address_impl(Site destination, Call *node) { not_implemented(); }
	void load_address_impl(Site destination, Definition *node) { not_implemented(); }
	void load_address_impl(Site destination, IntegerLiteral *node) { not_implemented(); }
	void load_address_impl(Site destination, BooleanLiteral *node) { not_implemented(); }
	void load_address_impl(Site destination, NoneLiteral *node) { not_implemented(); }
	void load_address_impl(Site destination, StringLiteral *node) { not_implemented(); }
	void load_address_impl(Site destination, Lambda *node) { not_implemented(); }
	void load_address_impl(Site destination, LambdaHead *node) { not_implemented(); }
	void load_address_impl(Site destination, Name *name) {
		auto definition = name->definition();
		I(lea, destination, get_definition_address(definition));
	}
	void load_address_impl(Site destination, IfExpression *node) { not_implemented(); }
	void load_address_impl(Site destination, BuiltinTypeName *node) { not_implemented(); }
	void load_address_impl(Site destination, Binary *binary) {
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
	void load_address_impl(Site destination, Match *node) { not_implemented(); }
	void load_address_impl(Site destination, Unary *unary) {
		switch (unary->operation) {
			case UnaryOperation::dereference: {
				output(destination, unary->expression);
				break;
			}
			default: not_implemented();
		}
	}
	void load_address_impl(Site destination, Struct *node) { not_implemented(); }
	void load_address_impl(Site destination, ArrayType *node) { not_implemented(); }
	void load_address_impl(Site destination, Subscript *node) { not_implemented(); }
	void load_address_impl(Site destination, ArrayConstructor *node) { not_implemented(); }

#undef MI
#undef I
#undef tmpreg
#undef tmpval
};

struct Interpreter {
	inline static Interpreter *current_interpreter;
	Bytecode *bytecode;

	u64 run(Bytecode *bytecode, umm entry_index) {
		scoped_replace(current_interpreter, this);
		this->bytecode = bytecode;
		current_instruction_index = entry_index;

		reg(Register::stack) = (s64)stack.end();
		reg(Register::global_mutable) = (s64)bytecode->global_mutable_data.data;
		reg(Register::global_readonly) = (s64)bytecode->global_readonly_data.data;

		run_while([&] { return current_instruction_index < bytecode->instructions.count; });

		return val8(Address { .offset = (s64)(stack.end() - 8)});
	}
	void run_while(auto predicate) {
		while (predicate()) {
			auto i = bytecode->instructions[current_instruction_index];
			switch (i.kind) {
#define x(name, fields) case InstructionKind::name: execute(i.v_##name); break;
				ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
			}
			++current_instruction_index;
		}
	}

	umm current_instruction_index = (umm)-1;
	s64 registers[256] = {};
	Array<u8, 1*KiB> stack = {};
	List<u64> debug_stack;

	struct Library {
		HMODULE module;
		HashMap<String, void *> functions;
	};
	HashMap<String, Library> libraries;

	s64 &reg(Register r) {
		return registers[to_underlying(r)];
	}
	s64 &val8(Register r) {
		return registers[to_underlying(r)];
	}
	s64 &val8(Address a) {
		s64 x = a.offset;
		if (a.base) {
			x += reg(a.base.value());
		}
		if (a.element_size) {
			x += reg(a.element_index) * a.element_size;
		}

		return *(s64 *)x;
	}
	s64 &val8(Site s) {
		if (s.is_register()) {
			return reg(s.get_register());
		} else {
			return val8(s.get_address());
		}
	}
	s64 &val8(InputValue v) {
		if (v.is_register()) {
			return reg(v.get_register());
		} else if (v.is_address()) {
			return val8(v.get_address());
		} else {
			return v.get_constant();
		}
	}
	s32 &val4(Register   x) { return (s32 &)val8(x); }
	s32 &val4(Address    x) { return (s32 &)val8(x); }
	s32 &val4(Site       x) { return (s32 &)val8(x); }
	s32 &val4(InputValue x) { return (s32 &)val8(x); }
	s16 &val2(Register   x) { return (s16 &)val8(x); }
	s16 &val2(Address    x) { return (s16 &)val8(x); }
	s16 &val2(Site       x) { return (s16 &)val8(x); }
	s16 &val2(InputValue x) { return (s16 &)val8(x); }
	s8  &val1(Register   x) { return (s8  &)val8(x); }
	s8  &val1(Address    x) { return (s8  &)val8(x); }
	s8  &val1(Site       x) { return (s8  &)val8(x); }
	s8  &val1(InputValue x) { return (s8  &)val8(x); }

#define E(name, ...) execute(Instruction{.kind = InstructionKind::name, .v_##name = { __VA_ARGS__ }}.v_##name)

	void execute(Instruction::pop_t i) {
		E(copy, .d = i.d, .s = Address{.base = Register::stack}, .size = 8);
		E(add8, .d = Register::stack, .a = Register::stack, .b = 8);
	}

	void execute(Instruction::nop_t i) {}
	void execute(Instruction::push_t i) {
		E(sub8, .d = Register::stack, .a = Register::stack, .b = 8);
		E(copy, .d = Address{.base = Register::stack}, .s = i.s, .size = 8);
	}
	void execute(Instruction::copy_t i) {
		auto d = &val8(i.d);
		auto s = &val8(i.s);
		memcpy(d, s, i.size);
	}
	void execute(Instruction::set_t i) {
		auto d = &val8(i.d);
		memset(d, i.value, i.size);
	}
	void execute(Instruction::lea_t i) { val8(i.d) = (s64) &val8(i.s); }
	void execute(Instruction::add1_t i) { val1(i.d) = val1(i.a) + val1(i.b); }
	void execute(Instruction::sub1_t i) { val1(i.d) = val1(i.a) - val1(i.b); }
	void execute(Instruction::mul1_t i) { val1(i.d) = val1(i.a) * val1(i.b); }
	void execute(Instruction::div1_t i) { val1(i.d) = val1(i.a) / val1(i.b); }
	void execute(Instruction::mod1_t i) { val1(i.d) = val1(i.a) % val1(i.b); }
	void execute(Instruction::xor1_t i) { val1(i.d) = val1(i.a) ^ val1(i.b); }
	void execute(Instruction::and1_t i) { val1(i.d) = val1(i.a) & val1(i.b); }
	void execute(Instruction::or1_t i)  { val1(i.d) = val1(i.a) | val1(i.b); }
	void execute(Instruction::sll1_t i) { val1(i.d) = val1(i.a) << val1(i.b); }
	void execute(Instruction::srl1_t i) { val1(i.d) = (u8)val1(i.a) >> (u8)val1(i.b); }
	void execute(Instruction::sra1_t i) { val1(i.d) = (s8)val1(i.a) >> (s8)val1(i.b); }
	void execute(Instruction::cmp1_t i) {
		switch (i.cmp) {
			case Comparison::equals:                  val1(i.d) =     val1(i.a) ==     val1(i.b); break;
			case Comparison::not_equals:              val1(i.d) =     val1(i.a) !=     val1(i.b); break;
			case Comparison::signed_less:             val1(i.d) = (s8)val1(i.a) <  (s8)val1(i.b); break;
			case Comparison::signed_greater:          val1(i.d) = (s8)val1(i.a) >  (s8)val1(i.b); break;
			case Comparison::signed_less_equals:      val1(i.d) = (s8)val1(i.a) <= (s8)val1(i.b); break;
			case Comparison::signed_greater_equals:   val1(i.d) = (s8)val1(i.a) >= (s8)val1(i.b); break;
			case Comparison::unsigned_less:           val1(i.d) = (u8)val1(i.a) <  (u8)val1(i.b); break;
			case Comparison::unsigned_greater:        val1(i.d) = (u8)val1(i.a) >  (u8)val1(i.b); break;
			case Comparison::unsigned_less_equals:    val1(i.d) = (u8)val1(i.a) <= (u8)val1(i.b); break;
			case Comparison::unsigned_greater_equals: val1(i.d) = (u8)val1(i.a) >= (u8)val1(i.b); break;
		}
	}
	void execute(Instruction::add2_t i) { val2(i.d) = val2(i.a) + val2(i.b); }
	void execute(Instruction::sub2_t i) { val2(i.d) = val2(i.a) - val2(i.b); }
	void execute(Instruction::mul2_t i) { val2(i.d) = val2(i.a) * val2(i.b); }
	void execute(Instruction::div2_t i) { val2(i.d) = val2(i.a) / val2(i.b); }
	void execute(Instruction::mod2_t i) { val2(i.d) = val2(i.a) % val2(i.b); }
	void execute(Instruction::xor2_t i) { val2(i.d) = val2(i.a) ^ val2(i.b); }
	void execute(Instruction::and2_t i) { val2(i.d) = val2(i.a) & val2(i.b); }
	void execute(Instruction::or2_t i)  { val2(i.d) = val2(i.a) | val2(i.b); }
	void execute(Instruction::sll2_t i) { val2(i.d) = val2(i.a) << val2(i.b); }
	void execute(Instruction::srl2_t i) { val2(i.d) = (u16)val2(i.a) >> (u16)val2(i.b); }
	void execute(Instruction::sra2_t i) { val2(i.d) = (s16)val2(i.a) >> (s16)val2(i.b); }
	void execute(Instruction::cmp2_t i) {
		switch (i.cmp) {
			case Comparison::equals:                  val2(i.d) =      val2(i.a) ==      val2(i.b); break;
			case Comparison::not_equals:              val2(i.d) =      val2(i.a) !=      val2(i.b); break;
			case Comparison::signed_less:             val2(i.d) = (s16)val2(i.a) <  (s16)val2(i.b); break;
			case Comparison::signed_greater:          val2(i.d) = (s16)val2(i.a) >  (s16)val2(i.b); break;
			case Comparison::signed_less_equals:      val2(i.d) = (s16)val2(i.a) <= (s16)val2(i.b); break;
			case Comparison::signed_greater_equals:   val2(i.d) = (s16)val2(i.a) >= (s16)val2(i.b); break;
			case Comparison::unsigned_less:           val2(i.d) = (u16)val2(i.a) <  (u16)val2(i.b); break;
			case Comparison::unsigned_greater:        val2(i.d) = (u16)val2(i.a) >  (u16)val2(i.b); break;
			case Comparison::unsigned_less_equals:    val2(i.d) = (u16)val2(i.a) <= (u16)val2(i.b); break;
			case Comparison::unsigned_greater_equals: val2(i.d) = (u16)val2(i.a) >= (u16)val2(i.b); break;
		}
	}
	void execute(Instruction::add4_t i) { val4(i.d) = val4(i.a) + val4(i.b); }
	void execute(Instruction::sub4_t i) { val4(i.d) = val4(i.a) - val4(i.b); }
	void execute(Instruction::mul4_t i) { val4(i.d) = val4(i.a) * val4(i.b); }
	void execute(Instruction::div4_t i) { val4(i.d) = val4(i.a) / val4(i.b); }
	void execute(Instruction::mod4_t i) { val4(i.d) = val4(i.a) % val4(i.b); }
	void execute(Instruction::xor4_t i) { val4(i.d) = val4(i.a) ^ val4(i.b); }
	void execute(Instruction::and4_t i) { val4(i.d) = val4(i.a) & val4(i.b); }
	void execute(Instruction::or4_t i)  { val4(i.d) = val4(i.a) | val4(i.b); }
	void execute(Instruction::sll4_t i) { val4(i.d) = val4(i.a) << val4(i.b); }
	void execute(Instruction::srl4_t i) { val4(i.d) = (u32)val4(i.a) >> (u32)val4(i.b); }
	void execute(Instruction::sra4_t i) { val4(i.d) = (s32)val4(i.a) >> (s32)val4(i.b); }
	void execute(Instruction::cmp4_t i) {
		switch (i.cmp) {
			case Comparison::equals:                  val4(i.d) =      val4(i.a) ==      val4(i.b); break;
			case Comparison::not_equals:              val4(i.d) =      val4(i.a) !=      val4(i.b); break;
			case Comparison::signed_less:             val4(i.d) = (s32)val4(i.a) <  (s32)val4(i.b); break;
			case Comparison::signed_greater:          val4(i.d) = (s32)val4(i.a) >  (s32)val4(i.b); break;
			case Comparison::signed_less_equals:      val4(i.d) = (s32)val4(i.a) <= (s32)val4(i.b); break;
			case Comparison::signed_greater_equals:   val4(i.d) = (s32)val4(i.a) >= (s32)val4(i.b); break;
			case Comparison::unsigned_less:           val4(i.d) = (u32)val4(i.a) <  (u32)val4(i.b); break;
			case Comparison::unsigned_greater:        val4(i.d) = (u32)val4(i.a) >  (u32)val4(i.b); break;
			case Comparison::unsigned_less_equals:    val4(i.d) = (u32)val4(i.a) <= (u32)val4(i.b); break;
			case Comparison::unsigned_greater_equals: val4(i.d) = (u32)val4(i.a) >= (u32)val4(i.b); break;
		}
	}
	void execute(Instruction::add8_t i) { val8(i.d) = val8(i.a) + val8(i.b); }
	void execute(Instruction::sub8_t i) { val8(i.d) = val8(i.a) - val8(i.b); }
	void execute(Instruction::mul8_t i) { val8(i.d) = val8(i.a) * val8(i.b); }
	void execute(Instruction::div8_t i) { val8(i.d) = val8(i.a) / val8(i.b); }
	void execute(Instruction::mod8_t i) { val8(i.d) = val8(i.a) % val8(i.b); }
	void execute(Instruction::xor8_t i) { val8(i.d) = val8(i.a) ^ val8(i.b); }
	void execute(Instruction::and8_t i) { val8(i.d) = val8(i.a) & val8(i.b); }
	void execute(Instruction::or8_t i)  { val8(i.d) = val8(i.a) | val8(i.b); }
	void execute(Instruction::sll8_t i) { val8(i.d) = val8(i.a) << val8(i.b); }
	void execute(Instruction::srl8_t i) { val8(i.d) = (u64)val8(i.a) >> (u64)val8(i.b); }
	void execute(Instruction::sra8_t i) { val8(i.d) = (s64)val8(i.a) >> (s64)val8(i.b); }
	void execute(Instruction::cmp8_t i) {
		switch (i.cmp) {
			case Comparison::equals:                  val8(i.d) =      val8(i.a) ==      val8(i.b); break;
			case Comparison::not_equals:              val8(i.d) =      val8(i.a) !=      val8(i.b); break;
			case Comparison::signed_less:             val8(i.d) = (s64)val8(i.a) <  (s64)val8(i.b); break;
			case Comparison::signed_greater:          val8(i.d) = (s64)val8(i.a) >  (s64)val8(i.b); break;
			case Comparison::signed_less_equals:      val8(i.d) = (s64)val8(i.a) <= (s64)val8(i.b); break;
			case Comparison::signed_greater_equals:   val8(i.d) = (s64)val8(i.a) >= (s64)val8(i.b); break;
			case Comparison::unsigned_less:           val8(i.d) = (u64)val8(i.a) <  (u64)val8(i.b); break;
			case Comparison::unsigned_greater:        val8(i.d) = (u64)val8(i.a) >  (u64)val8(i.b); break;
			case Comparison::unsigned_less_equals:    val8(i.d) = (u64)val8(i.a) <= (u64)val8(i.b); break;
			case Comparison::unsigned_greater_equals: val8(i.d) = (u64)val8(i.a) >= (u64)val8(i.b); break;
		}
	}
	void execute(Instruction::sex21_t i) { val8(i.d) = (s16)(s8)val1(i.a); }
	void execute(Instruction::sex41_t i) { val8(i.d) = (s32)(s8)val1(i.a); }
	void execute(Instruction::sex42_t i) { val8(i.d) = (s32)(s16)val2(i.a); }
	void execute(Instruction::sex81_t i) { val8(i.d) = (s64)(s8)val1(i.a); }
	void execute(Instruction::sex82_t i) { val8(i.d) = (s64)(s16)val2(i.a); }
	void execute(Instruction::sex84_t i) { val8(i.d) = (s64)(s32)val4(i.a); }

	void execute(Instruction::call_t i) {
		E(push, (s64)current_instruction_index);

		// NOTE: offset by -1 because it will be incremented in the main loop
		current_instruction_index = val8(i.d) - 1;

		debug_stack.add(val8(Register::stack));
	}
	void execute(Instruction::callext_t i) {
		auto &lib = libraries.get_or_insert(i.lib);
		if (!lib.module) {
			auto lib_name = format("{}.dll\0"s, i.lib);
			defer { free(lib_name); };
			lib.module = GetModuleHandleA(lib_name.data);
			if (!lib.module) {
				lib.module = LoadLibraryA(lib_name.data);
			}
			assert(lib.module, "{}.dll was not found", i.lib);
		}

		auto &fn = lib.functions.get_or_insert(i.name);
		if (!fn) {
			auto fn_name = null_terminate(i.name);
			defer { free(fn_name); };
			fn = GetProcAddress(lib.module, (char *)fn_name.data);
			assert(fn, "{}.dll does not contain {}", i.lib, i.name);
		}

		s64 parameter_count = i.lambda->head.parameters_block.definition_list.count;

		s64 result = 0;

		#if 0
		system("cls");
		println("switch (parameter_count) {");
		for (int i = 0; i < 16; ++i) {
			println("    case {}: {{", i);
			for (int j = 0; j < i; ++j) {
				println("        s64 arg{} = val8(Address{{.base = Register::stack, .offset = {}}});", j, j*8);
			}
			print("        result = ((s64(*)(");
			for (int j = 0; j < i; ++j) {
				if (j) print(',');
				print("s64");
			}
			print("))fn)(");
			for (int j = 0; j < i; ++j) {
				if (j) print(',');
				print("arg{}", j);
			}
			println(");");
			println("        break;");
			println("    }");
		}
		println("    default: not_implemented();");
		println("}");
		exit(1);
		#endif
		switch (parameter_count) {
			case 0: {
				result = ((s64(*)())fn)();
				break;
			}
			case 1: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				result = ((s64(*)(s64))fn)(arg0);
				break;
			}
			case 2: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				result = ((s64(*)(s64,s64))fn)(arg0,arg1);
				break;
			}
			case 3: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				result = ((s64(*)(s64,s64,s64))fn)(arg0,arg1,arg2);
				break;
			}
			case 4: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				result = ((s64(*)(s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3);
				break;
			}
			case 5: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				result = ((s64(*)(s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4);
				break;
			}
			case 6: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5);
				break;
			}
			case 7: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6);
				break;
			}
			case 8: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7);
				break;
			}
			case 9: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
				s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8);
				break;
			}
			case 10: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
				s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
				s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9);
				break;
			}
			case 11: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
				s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
				s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
				s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10);
				break;
			}
			case 12: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
				s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
				s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
				s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
				s64 arg11 = val8(Address{.base = Register::stack, .offset = 88});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11);
				break;
			}
			case 13: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
				s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
				s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
				s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
				s64 arg11 = val8(Address{.base = Register::stack, .offset = 88});
				s64 arg12 = val8(Address{.base = Register::stack, .offset = 96});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12);
				break;
			}
			case 14: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
				s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
				s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
				s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
				s64 arg11 = val8(Address{.base = Register::stack, .offset = 88});
				s64 arg12 = val8(Address{.base = Register::stack, .offset = 96});
				s64 arg13 = val8(Address{.base = Register::stack, .offset = 104});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13);
				break;
			}
			case 15: {
				s64 arg0 = val8(Address{.base = Register::stack, .offset = 0});
				s64 arg1 = val8(Address{.base = Register::stack, .offset = 8});
				s64 arg2 = val8(Address{.base = Register::stack, .offset = 16});
				s64 arg3 = val8(Address{.base = Register::stack, .offset = 24});
				s64 arg4 = val8(Address{.base = Register::stack, .offset = 32});
				s64 arg5 = val8(Address{.base = Register::stack, .offset = 40});
				s64 arg6 = val8(Address{.base = Register::stack, .offset = 48});
				s64 arg7 = val8(Address{.base = Register::stack, .offset = 56});
				s64 arg8 = val8(Address{.base = Register::stack, .offset = 64});
				s64 arg9 = val8(Address{.base = Register::stack, .offset = 72});
				s64 arg10 = val8(Address{.base = Register::stack, .offset = 80});
				s64 arg11 = val8(Address{.base = Register::stack, .offset = 88});
				s64 arg12 = val8(Address{.base = Register::stack, .offset = 96});
				s64 arg13 = val8(Address{.base = Register::stack, .offset = 104});
				s64 arg14 = val8(Address{.base = Register::stack, .offset = 112});
				result = ((s64(*)(s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64,s64))fn)(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14);
				break;
			}
			default: not_implemented();
		}
		val8(Address{.base = Register::stack, .offset = 8 * parameter_count}) = result;
	}
	void execute(Instruction::ret_t i) {
		{
			auto expected = debug_stack.pop().value();
			auto actual = val8(Register::stack);
			assert(actual == expected);
		}
		auto prev_instruction_index = current_instruction_index;
		current_instruction_index = val8(Address{.base = Register::stack});
		E(add8, .d = Register::stack, .a = Register::stack, .b = 8);
	}
	void execute(Instruction::jmp_t i) { current_instruction_index = val8(i.d) - 1; }
	void execute(Instruction::jz_t  i) { if (val8(i.s) == 0) current_instruction_index = val8(i.d) - 1; }
	void execute(Instruction::jnz_t i) { if (val8(i.s) != 0) current_instruction_index = val8(i.d) - 1; }
	void execute(Instruction::intrinsic_t i) {
		switch (i.i) {
			case Intrinsic::println_S64: {
				println(val8(Address { .base = Register::stack }));
				break;
			}
			case Intrinsic::println_String: {
				auto data  = val8(Address { .base = Register::stack });
				auto count = val8(Address { .base = Register::stack, .offset = 8 });
				println(String((utf8 *)data, count));
				break;
			}
			case Intrinsic::panic: {
				immediate_reporter.error("PANIC");
				invalid_code_path();
				break;
			}
			case Intrinsic::debug_break: {
				debug_break();
				break;
			}
		}
	}

	u64 ffi_callback(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda) {
		auto ret_size = get_size(lambda->head.return_type);
		auto arg0_size = get_size(lambda->head.parameters_block.definition_list[0]->type);
		auto arg1_size = get_size(lambda->head.parameters_block.definition_list[1]->type);
		auto arg2_size = get_size(lambda->head.parameters_block.definition_list[2]->type);
		auto arg3_size = get_size(lambda->head.parameters_block.definition_list[3]->type);
		E(sub8, Register::stack, Register::stack, (s64)(ret_size + arg0_size + arg1_size + arg2_size + arg3_size));
		E(copy, .d = Address{.base = Register::stack, .offset = 0                                       }, .s = (s64)arg0, .size = arg0_size);
		E(copy, .d = Address{.base = Register::stack, .offset = (s64)(arg0_size                        )}, .s = (s64)arg1, .size = arg1_size);
		E(copy, .d = Address{.base = Register::stack, .offset = (s64)(arg0_size + arg1_size            )}, .s = (s64)arg2, .size = arg2_size);
		E(copy, .d = Address{.base = Register::stack, .offset = (s64)(arg0_size + arg1_size + arg2_size)}, .s = (s64)arg3, .size = arg3_size);
		E(call, (s64)lambda->first_instruction_index);
		
		// NOTE: `call` does an offset by -1 because the main loop always increments the index, but we are not in a loop yet, so offset the offset.
		++current_instruction_index;

		umm target_stack_count = debug_stack.count;
		run_while([&] { return debug_stack.count >= target_stack_count; });

		// NOTE: now current_instruction_index is one past `callext` instruction and will be incremented again at the end of main loop iteration, which will make it
		// skip one instruction, so offset it here as well.
		--current_instruction_index;

		E(add8, Register::stack, Register::stack, (s64)(ret_size + arg0_size + arg1_size + arg2_size + arg3_size));
		
		auto result = val8(Address{.base = Register::stack, .offset = -(s64)ret_size});
		return result;
	}

#undef E
};

u64 ffi_callback(u64 arg0, u64 arg1, u64 arg2, u64 arg3, Lambda *lambda) {
	return Interpreter::current_interpreter->ffi_callback(arg0, arg1, arg2, arg3, lambda);
}

inline umm append(StringBuilder &builder, Register r) {
	switch (r) {
#define x(name, value) case Register::name: return append(builder, #name);
		ENUMERATE_NAMED_BYTECODE_REGISTERS
#undef x
	}
	return append_format(builder, "r{}", (u64)r);
}

inline umm append(StringBuilder &builder, Address a) {
	umm result = 0;
	result += append(builder, '[');
	if (a.base)
		if (a.element_size)
			if (a.offset)
				result += append_format(builder, "{}+{}*{}{}{}", a.base.value(), a.element_index, a.element_size, a.offset < 0 ? "-" : "+", abs(a.offset));
			else
				result += append_format(builder, "{}+{}*{}", a.base.value(), a.element_index, a.element_size);
		else
			if (a.offset)
				result += append_format(builder, "{}{}{}", a.base.value(), a.offset < 0 ? "-" : "+", abs(a.offset));
			else
				result += append_format(builder, "{}", a.base.value());
	else
		if (a.element_size)
			if (a.offset)
				result += append_format(builder, "{}*{}{}{}", a.element_index, a.element_size, a.offset < 0 ? "-" : "+", abs(a.offset));
			else
				result += append_format(builder, "{}*{}", a.element_index, a.element_size);
		else
			if (a.offset)
				result += append_format(builder, "{}{}", a.offset < 0 ? "-" : "", abs(a.offset));
			else
				result += append_format(builder, "0");

	result += append(builder, ']');
	return result;
}

inline umm append(StringBuilder &builder, Site s) {
	if (s.is_register())
		return append(builder, s.get_register());
	else
		return append(builder, s.get_address());
}

inline umm append(StringBuilder &builder, InputValue v) {
	if (v.is_register())
		return append(builder, v.get_register());
	else if (v.is_address())
		return append(builder, v.get_address());
	else
		return append(builder, v.get_constant());
}

inline umm append(StringBuilder &builder, InstructionKind i) {
	switch (i) {
#define x(name, fields) case InstructionKind::name: return append(builder, #name);
		ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
	}
	return append_format(builder, "(unknown InstructionKind {})", (u64)i);
}

#define y(type, name) \
	if (need_comma) { result += append(builder, ", "); } \
	need_comma = true; \
	result += append(builder, i.name); \

#define x(name, fields) \
inline umm append(StringBuilder &builder, Instruction::name##_t i) { \
	umm result = 0; \
	result += append(builder, InstructionKind::name); \
	result += append(builder, ' '); \
	bool need_comma = false; \
	PASSTHROUGH fields; \
	return result; \
}
ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
#undef y

inline umm append(StringBuilder &builder, Instruction i) {
	switch (i.kind) {
#define x(name, fields) case InstructionKind::name: return append(builder, i.v_##name);
		ENUMERATE_BYTECODE_INSTRUCTION_KIND
#undef x
	}
	return append_format(builder, "(unknown InstructionKind {})", (u64)i.kind);
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
		MUST_BE_CONSTANT(argument);
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
CheckResult is_constant(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return is_constant_impl((##name *)expression);
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
CheckResult is_mutable(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return is_mutable_impl((##name *)expression);
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
Result<Value, Node *> get_constant_value_impl(Call *node) { return node; }
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
Result<Value, Node *> get_constant_value(Node *node) {
	scoped_replace(debug_current_location, node->location);
	switch (node->kind) {
#define x(name) case NodeKind::name: return get_constant_value_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
	invalid_code_path("get_constant_value: Invalid node kind {}", node->kind);
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
#define DEEP_COPY_INPLACE(x) deep_copy_impl(&from->x, &to->x)
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
	[[nodiscard]] T *copy_base(T *from) {
		auto to = T::create();
		copied_nodes.get_or_insert(from) = to;
		to->location = from->location;
		return to;
	}

	template <class T>
	[[nodiscard]] T *deep_copy(T *from) {
		auto to = copy_base(from);
		deep_copy_impl(from, to);
		if (auto to_expression = as<Expression>(to)) {
			auto from_expression = as<Expression>(from);
			assert(from_expression);
			assert((bool)from_expression->type == (bool)to_expression->type);
		}
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

	[[nodiscard]] void deep_copy_impl(Block *from, Block *to) {
		for (auto from_child : from->children) {
			auto to_child = deep_copy(from_child);
			to->add(to_child);
		}

		if (to->children.count) {
			if (auto last_expression = as<Expression>(to->children.back())) {
				to->type = last_expression->type;
			}
		}

		if (!to->type)
			to->type = get_builtin_type(BuiltinType::None);
	} 
	[[nodiscard]] void deep_copy_impl(Call *from, Call *to) {
		DEEP_COPY(callable);
		COPY_LIST(arguments, DEEP_COPY);
		COPY(inline_status);
		COPY(call_kind);
		LOOKUP_COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(Definition *from, Definition *to) {
		COPY(name);
		if (from->parsed_type)
			DEEP_COPY(parsed_type);
		if (from->initial_value)
			DEEP_COPY(initial_value);

		COPY(is_parameter);
		COPY(mutability);

		LOOKUP_COPY(container);

		if (from->parsed_type) {
			to->type = to->parsed_type;
		} else {
			to->type = to->initial_value->type;
		}
	} 
	[[nodiscard]] void deep_copy_impl(IntegerLiteral *from, IntegerLiteral *to) {
		COPY(value);
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(BooleanLiteral *from, BooleanLiteral *to) {
		COPY(value);
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(NoneLiteral *from, NoneLiteral *to) {
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(StringLiteral *from, StringLiteral *to) {
		COPY(value);
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(Lambda *from, Lambda *to) {
		COPY(inline_status);
		COPY(is_intrinsic);
		DEEP_COPY_INPLACE(head);
		DEEP_COPY(body);
		LOOKUP_COPY(definition);

		// returns will be updated by deep_copy_impl(Return)
		
		LOOKUP_COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(LambdaHead *from, LambdaHead *to) {
		DEEP_COPY_INPLACE(parameters_block);
		if (from->parsed_return_type) {
			DEEP_COPY(parsed_return_type);
		}
		LOOKUP_COPY(return_type);
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(Name *from, Name *to) {
		COPY(name);
		COPY_LIST(possible_definitions, LOOKUP_COPY);
		LOOKUP_COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(IfStatement *from, IfStatement *to) {
		DEEP_COPY(condition);
		DEEP_COPY(true_branch);
		if (from->false_branch)
			DEEP_COPY(false_branch);
	} 
	[[nodiscard]] void deep_copy_impl(IfExpression *from, IfExpression *to) {
		DEEP_COPY(condition);
		DEEP_COPY(true_branch);
		DEEP_COPY(false_branch);
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(BuiltinTypeName *from, BuiltinTypeName *to) {
		COPY(type_kind);
		to->type = get_builtin_type(BuiltinType::Type);
	} 
	[[nodiscard]] void deep_copy_impl(Binary *from, Binary *to) {
		DEEP_COPY(left);
		DEEP_COPY(right);
		COPY(operation);
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(Match *from, Match *to) {
		DEEP_COPY(expression);
		to->cases.resize(from->cases.count);
		for (umm i = 0; i < to->cases.count; ++i) {
			if (from->cases[i].from)
				DEEP_COPY(cases[i].from);
			DEEP_COPY(cases[i].to);
		}
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(Unary *from, Unary *to) {
		DEEP_COPY(expression);
		COPY(operation);
		COPY(mutability);
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(Return *from, Return *to) {
		DEEP_COPY(value);
		LOOKUP_COPY(lambda);
		to->lambda->returns.add(to);
	}
	[[nodiscard]] void deep_copy_impl(While *from, While *to) {
		DEEP_COPY(condition);
		DEEP_COPY(body);
	} 
	[[nodiscard]] void deep_copy_impl(Continue *from, Continue *to) {
		LOOKUP_COPY(loop);
	} 
	[[nodiscard]] void deep_copy_impl(Break *from, Break *to) {
		LOOKUP_COPY(tag_block);
		LOOKUP_COPY(loop);
	}
	[[nodiscard]] void deep_copy_impl(Struct *from, Struct *to) { not_implemented(); }
	[[nodiscard]] void deep_copy_impl(ArrayType *from, ArrayType *to) { not_implemented(); }
	[[nodiscard]] void deep_copy_impl(Subscript *from, Subscript *to) { not_implemented(); }
	[[nodiscard]] void deep_copy_impl(ArrayConstructor *from, ArrayConstructor *to) {
		COPY_LIST(elements, DEEP_COPY);
		COPY(type);
	}
	[[nodiscard]] void deep_copy_impl(Import *from, Import *to) {
		COPY(path);
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

inline bool do_all_paths_return(Node *node) {
	switch (node->kind) {
		case NodeKind::Return: return true;
		case NodeKind::Block: {
			auto block = (Block *)node;
			for (auto child : block->children) {
				if (do_all_paths_return(child)) {
					return true;
				}
			}
			return false;
		}
		case NodeKind::IfStatement: {
			auto If = (IfStatement *)node;
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
		case NodeKind::IfExpression: {
			auto If = (IfExpression *)node;
			if (do_all_paths_return(If->condition)) {
				return true;
			}

			if (do_all_paths_return(If->true_branch) && do_all_paths_return(If->false_branch)) {
				return true;
			}

			return false;
		}
		case NodeKind::Binary: {
			auto bin = (Binary *)node;
			if (do_all_paths_return(bin->left)) {
				return true;
			}
			if (do_all_paths_return(bin->right)) {
				return true;
			}
			return false;
		}
		case NodeKind::Unary: {
			auto un = (Unary *)node;
			if (do_all_paths_return(un->expression)) {
				return true;
			}
			return false;
		}
		case NodeKind::Call: {
			auto call = (Call *)node;
			if (do_all_paths_return(call->callable)) {
				return true;
			}
			for (auto argument : call->arguments) {
				if (do_all_paths_return(argument)) {
					return true;
				}
			}
			return false;
		}
		case NodeKind::Definition: {
			auto definition = (Definition *)node;
			if (definition->initial_value) {
				if (do_all_paths_return(definition->initial_value)) {
					return true;
				}
			}
			return false;
		}
		case NodeKind::Match: {
			auto match = (Match *)node;
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
		case NodeKind::While: {
			return false;
		}
		case NodeKind::ArrayType:{
			auto arr = (ArrayType *)node;
			if (do_all_paths_return(arr->count_expression)) {
				return true;
			}
			if (do_all_paths_return(arr->element_type)) {
				return true;
			}
			return false;
		}
		case NodeKind::Subscript:{
			auto sub = (Subscript *)node;
			if (do_all_paths_return(sub->subscriptable)) {
				return true;
			}
			if (do_all_paths_return(sub->index)) {
				return true;
			}
			return false;
		}
		case NodeKind::ArrayConstructor:{
			auto arr = (ArrayConstructor *)node;
			for (auto element : arr->elements) {
				if (do_all_paths_return(element)) {
					return true;
				}
			}
			return false;
		}
		case NodeKind::BuiltinTypeName:
		case NodeKind::Break:
		case NodeKind::Continue:
		case NodeKind::Name:
		case NodeKind::IntegerLiteral:
		case NodeKind::BooleanLiteral:
		case NodeKind::NoneLiteral:
		case NodeKind::StringLiteral:
		case NodeKind::Lambda:
		case NodeKind::LambdaHead:
			return false;
	}
	not_implemented("do_all_paths_return not implemented for {}", node->kind);
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

LockProtected<HashMap<VectorizedLambdaKey, Definition *, VectorizedLambdaKeyHashTraits>, SpinLock> vectorized_lambdas;

enum class FailStrategy {
	yield,
	unwind,
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

	YieldResult continue_typechecking(Fiber parent_fiber, TypecheckEntry *entry) {
		assert(parent_fiber.handle);

		assert(debug_thread_id == 0);
		debug_thread_id = get_current_thread_id();
		defer { debug_thread_id = 0; };

		debug_start();

		{
			scoped_replace(this->parent_fiber, parent_fiber);
			scoped_replace(this->entry, entry);

			tl::yield(fiber);
		}

		if (yield_result != YieldResult::wait) {
			locked_use(deferred_reports) {
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
	jmp_buf unwind_point = {};

	FailStrategy fail_strategy = FailStrategy::yield;
	#define fail()                                      \
		do {                                            \
			if (fail_strategy == FailStrategy::yield) { \
				yield(YieldResult::fail);               \
			}                                           \
			return 0;                                   \
		} while (0)
	#define with_unwind_strategy(x) ([&]()->decltype(auto){ scoped_replace(fail_strategy, FailStrategy::unwind); return x; }())

	#define typecheck_or_unwind(...) do { if (!typecheck(__VA_ARGS__)) return 0; } while (0)

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
			longjmp(unwind_point, 1);
		}
	}

	void fiber_main() {
		while (1) {
			setjmp(unwind_point);
			assert(initial_node);
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
			parameter->initial_value = argument;
			parameter->is_parameter = false;
			parameter->container = current_container;
			result_block->add(parameter);
		}

		if (auto body_block = as<Block>(copied_lambda->body)) {
			body_block->tag = format(u8"_{}", body_block->uid);
			visit(body_block, Combine{
				[&](Node *) {},
				[&](Return *ret) -> Statement * {
					if (ret->lambda == copied_lambda) {
						auto Break = Break::create();
						Break->value = ret->value;
						assert(body_block);
						Break->tag_block = body_block;
						body_block->breaks.add(Break);
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
		auto context = NodeInterpreter::create(fiber, node);
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

	Definition *get_vectorized_lambda_definition(Lambda *original_lambda, u64 vector_size) {
		return locked_use(vectorized_lambdas) {
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
				call->arguments.set(subscript);
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

			global_block.add(new_lambda_definition);

			vectorized_lambdas.insert({ original_lambda, vector_size }, new_lambda_definition);
			return new_lambda_definition;
		};
	}

	Expression *typecheck_lambda_call(Call *call, Lambda *lambda, LambdaHead *head, bool apply = true) {

		auto &arguments = call->arguments;
		auto &callable = call->callable;

		if (!yield_while_null(call->location, &head->return_type)) {
			reporter.error(head->location, "Could not wait for lambda's return type");
			fail();
		}

		auto &parameters = head->parameters_block.definition_list;

		if (arguments.count != parameters.count) {
			reporter.error(call->location, "Too {} arguments. Expected {}, but got {}.", arguments.count > parameters.count ? "many"s : "few"s, parameters.count, arguments.count);
			reporter.info(head->location, "Lambda is here:");
			fail();
		}



		if (lambda) {
			if (arguments.count == 1) {
				if (auto array = as<ArrayType>(arguments[0]->type)) {
					if (types_match(array->element_type, parameters[0]->type)) {
						auto vectorized_lambda_definition = get_vectorized_lambda_definition(lambda, array->count.value());

						auto name = Name::create();
						name->location = callable->location;
						name->name = vectorized_lambda_definition->name;
						name->possible_definitions.set(vectorized_lambda_definition);
						name->type = vectorized_lambda_definition->type;

						callable = name;
						lambda = as<Lambda>(vectorized_lambda_definition->initial_value);
						head = &lambda->head;
					}
				}
			}
		}



		for (umm i = 0; i < arguments.count; ++i) {
			auto &argument = arguments[i];
			auto &parameter = head->parameters_block.definition_list[i];
			if (!implicitly_cast(&argument, parameter->type, &reporter, apply)) {
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

		if (arguments.count != members.count) {
			reporter.error(call->location, "Too {} arguments. Expected {}, but got {}.", arguments.count > members.count ? "many"s : "few"s, members.count, arguments.count);
			fail();
		}

		for (umm i = 0; i < arguments.count; ++i) {
			auto &argument = arguments[i];
			auto &member = members[i];
			if (!implicitly_cast(&argument, member->type, true)) {
				fail();
			}
		}

		call->type = call->callable;
		return call;
	};

	bool typecheck_binary_dot(Binary *binary, Reporter &reporter) {
		typecheck_or_unwind(&binary->left);
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

	//
	// These `typecheck` overloads automatically substitute old node with new one.
	//
	[[nodiscard]] bool typecheck(Node **node) {
		*node = typecheck(*node, true);
		return *node != 0;
	}
	template <CNode T>
	[[nodiscard]] bool typecheck(T **node) { 
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
	[[nodiscard]] bool typecheck(T &node) {
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
#define x(name) case NodeKind::name: new_node = typecheck_impl((##name *)node, can_substitute); break;
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
			typecheck_or_unwind(&definition->parsed_type);
		} else {
			assert(definition->initial_value);
		}

		if (definition->initial_value) {
			typecheck_or_unwind(&definition->initial_value);

			if (definition->mutability == Mutability::constant) {

				if (definition->parsed_type) {
					if (!implicitly_cast(&definition->initial_value, definition->parsed_type, true)) {
						fail();
					}
				}

				auto constant_check = is_constant(definition->initial_value);
				if (!constant_check) {
					reporter.error(definition->location, "Initial value is not constant.");
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
		} else {
			if (definition->is_parameter) {
				// Parameters can be immutable and have no initial expression
			} else if (definition->container && definition->container->kind == NodeKind::Struct) {
				// Struct members can be immutable and have no initial expression
			} else {
				assert(definition->mutability == Mutability::variable);
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
		typecheck_or_unwind(head->parameters_block);

		if (head->parsed_return_type) {
			typecheck_or_unwind(&head->parsed_return_type);
			head->return_type = head->parsed_return_type;
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
		typecheck_or_unwind(lambda->head);

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

			typecheck_or_unwind(&lambda->body);

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
						if (print_wait_failures) {
							reporter.error(name->location, "Couldn't wait for definition type.");
						}
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
		if (auto binary = as<Binary>(call->callable)) {
			if (binary->operation == BinaryOperation::dot) {
				if (auto lambda_name = as<Name>(binary->right)) {
					Reporter reporter2;
					if (with_unwind_strategy(typecheck_binary_dot(binary, reporter2))) {
						goto typecheck_dot_succeeded;
					} else {
						Definition *lambda_definition = 0;
						for (auto block = current_block; block; block = block->parent) {
							if (auto found_definitions = block->definition_map.find(lambda_name->name)) {
								auto [name, definitions] = *found_definitions;
								assert(definitions.count != 0);
								if (definitions.count > 1) {
									reporter.error(binary->right->location, "Function overloading not implemented yet.");
									fail();
								}
								lambda_definition = definitions[0];
								break;
							}
						}

						auto lambda = lambda_definition->initial_value ? as<Lambda>(lambda_definition->initial_value) : 0;
						if (!lambda) {
							reporter.error(binary->right->location, "This is not a lambda.");
							fail();
						}


						call->callable = binary->right;
						
						// Attempt passing `this` as follows (return on first successful attempt):
						//     1. As-is.
						//     2. By pointer.

						call->arguments.insert_at(binary->left, 0);

						Reporter as_is_reporter;
						{
							scoped_exchange(reporter, as_is_reporter);
							auto result = with_unwind_strategy(typecheck_impl(call, can_substitute));
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
							call->arguments[0] = first_argument_address;

							auto result = with_unwind_strategy(typecheck_impl(call, can_substitute));
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
					}
				}
			}
		}


		typecheck_or_unwind(&call->callable);

	typecheck_dot_succeeded:
		auto &arguments = call->arguments;
		for (auto &argument : arguments) {
			typecheck_or_unwind(&argument);
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
				scoped_replace(fail_strategy, FailStrategy::unwind);
				scoped_exchange(reporter, overload.reporter);
				if (typecheck_lambda_call(call, overload.lambda, overload.lambda_head, false)) {
					matching_overloads.add(&overload);
				}
			}

			if (matching_overloads.count == 1) {
				auto matching_overload = matching_overloads[0];
				name->possible_definitions.set(matching_overload->definition);
				return typecheck_lambda_call(call, matching_overload->lambda, matching_overload->lambda_head);
			}
			if (matching_overloads.count == 0) {
				reporter.error(call->location, "No matching overload was found.");
				for (auto [i, overload] : enumerate(overloads)) {
					reporter.info("Overload #{}:", i);
					reporter.reports.add(overload.reporter.reports);
				}
				fail();
			}

			reporter.error(call->location, "Multiple matching overload were found:");
			for (auto [i, overload] : enumerate(matching_overloads)) {
				reporter.info(overload->definition->location, "Overload #{}:", i);
			}
			fail();
		}

		reporter.error(call->callable->location, "Only lambdas / structs can be called for now");
		fail();
		return 0;
	}
	[[nodiscard]] Node *typecheck_impl(IfStatement *If, bool can_substitute) {
		typecheck_or_unwind(&If->condition);

		typecheck_or_unwind(&If->true_branch);

		if (If->false_branch) {
			typecheck_or_unwind(&If->false_branch);
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
		typecheck_or_unwind(&If->condition);

		typecheck_or_unwind(&If->true_branch);
		typecheck_or_unwind(&If->false_branch);

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
			if (!with_unwind_strategy(typecheck_binary_dot(binary, reporter))) {
				fail();
			}
			return binary;
		} else {
			typecheck_or_unwind(&binary->left);
			typecheck_or_unwind(&binary->right);

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

		no_binop:
			reporter.error(binary->location, "No binary operation {} defined for types {} and {}.", binary->operation, binary->left->type, binary->right->type);
			fail();
			return 0;
		}
	}
	[[nodiscard]] Match *typecheck_impl(Match *match, bool can_substitute) {
		typecheck_or_unwind(&match->expression);

		make_concrete(match->expression);

		for (auto &Case : match->cases) {
			if (Case.from) {
				typecheck_or_unwind(&Case.from);

				if (!is_constant(Case.from)) {
					reporter.error(Case.from->location, "Match case expression must be constant.");
					fail();
				}

				if (!implicitly_cast(&Case.from, match->expression->type, true))
					fail();
			}

			typecheck_or_unwind(&Case.to);
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
		typecheck_or_unwind(&unary->expression);
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
			default:
				not_implemented();
				break;
		}

		return unary;
	}
	[[nodiscard]] Return *typecheck_impl(Return *return_, bool can_substitute) {
		if (return_->value)
			typecheck_or_unwind(&return_->value);

		return return_;
	}
	[[nodiscard]] While *typecheck_impl(While *While, bool can_substitute) {
		typecheck_or_unwind(&While->condition);

		scoped_replace(current_loop, While);

		if (auto builtin_type = direct_as<BuiltinTypeName>(While->condition->type); !builtin_type || builtin_type->type_kind != BuiltinType::Bool) {
			reporter.error(While->condition->location, "Condition type must be Bool.");
			fail();
		}

		typecheck_or_unwind(&While->body);

		return While;
	}
	[[nodiscard]] Continue *typecheck_impl(Continue *Continue, bool can_substitute) {
		assert(current_loop);
		Continue->loop = current_loop;
		return Continue;
	}
	[[nodiscard]] Break *typecheck_impl(Break *Break, bool can_substitute) {
		if (Break->value) {
			typecheck_or_unwind(&Break->value);
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
			typecheck_or_unwind(&member);
			member->offset = struct_size;
			struct_size += get_size(member->type);
		}
		Struct->type = get_builtin_type(BuiltinType::Type);
		Struct->size = struct_size;
		return Struct;
	}
	[[nodiscard]] ArrayType *typecheck_impl(ArrayType *arr, bool can_substitute) {
		typecheck_or_unwind(&arr->count_expression);
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
		
		typecheck_or_unwind(&arr->element_type);
		if (!is_type(arr->element_type)) {
			reporter.error(arr->element_type->location, "This must be a type.");
			reporter.info(arr->location, "Because this is an array.");
			fail();
		}

		arr->type = get_builtin_type(BuiltinType::Type);
		return arr;
	}
	[[nodiscard]] Expression *typecheck_impl(Subscript *Subscript, bool can_substitute) {
		typecheck_or_unwind(&Subscript->subscriptable);
		auto array_type = direct_as<ArrayType>(Subscript->subscriptable->type);
		if (!array_type) {
			reporter.error(Subscript->subscriptable->location, "This must be an array.");
			fail();
		}

		typecheck_or_unwind(&Subscript->index);
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
			typecheck_or_unwind(&element);
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
#if ENABLE_NOTE_LEAK
	construct(leaks);
#endif

	//GlobalAllocator::init();
}

enum class InterpretMode {
	bytecode,
	ast,
};

struct ParsedArguments {
	String source_name;
	u32 thread_count = 0;
	bool print_ast = false;
	InterpretMode interpret_mode = {};
};

Optional<ParsedArguments> parse_arguments(Span<Span<utf8>> args) {
	ParsedArguments result;

	for (umm i = 1; i < args.count; ++i) {

		if (starts_with(args[i], u8"-t"s)) {
			auto n = args[i];
			n.set_begin(n.begin() + 2);
			if (auto number = parse_u64(n)) {
				result.thread_count = (u32)number.value();
			} else {
				immediate_reporter.error("Could not parse number after -t. Defaulting to all threads.");
			}
		} else if (args[i] == "-print-tokens") {
			print_tokens = true;
		} else if (args[i] == "-print-ast") {
			result.print_ast = true;
		} else if (args[i] == "-print-uids") {
			print_uids = true;
		} else if (args[i] == "-no-constant-name-inlining") {
			constant_name_inlining = false;
		} else if (args[i] == "-report-yields") {
			report_yields = true;
		} else if (args[i] == "-log-time") {
			enable_time_log = true;
		} else if (args[i] == "-debug") {
			is_debugging = true;
		} else if (args[i] == "-run-bytecode") {
			result.interpret_mode = InterpretMode::bytecode;
		} else if (args[i] == "-run-ast") {
			result.interpret_mode = InterpretMode::ast;
		} else if (args[i] == "-limit-time") {
			create_thread([] {
				int seconds_limit = 1;
				sleep_milliseconds(seconds_limit * 1000);
				immediate_reporter.error("Time limit of {} seconds exceeded.", seconds_limit);
				exit(-1);
			});
		} else if (args[i] == "-print-wait-failures") {
			print_wait_failures = true;
		} else if (args[i] == "-log-error-path") {
			enable_log_error_path = true;
		} else if (args[i][0] == '-') {
			immediate_reporter.warning("Unknown command line parameter: {}", args[i]);
		} else {
			if (result.source_name.count) {
				with(ConsoleColor::red, println("No multiple input files allowed"));
				return {};
			} else {
				result.source_name = args[i];
			}
		}
	}

	if (!result.source_name.count) {
		with(ConsoleColor::red, println("No input file was specified"));
		return {};
	} 

	return result;
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

s32 tl_main(Span<Span<utf8>> args) {

	debug_init();

	auto main_fiber = init_fiber(0);

	set_console_encoding(Encoding::utf8);

	defer {
		if (enable_time_log) {
			for (auto time : timed_results) {
				println("{} took {} ms", time.name, time.seconds * 1000);
			}
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

	auto maybe_arguments = parse_arguments(args);
	if (!maybe_arguments) {
		immediate_reporter.error("Failed to parse arguments.");
		return 1;
	}
	auto arguments = maybe_arguments.value();
	
	auto cpu_info = get_cpu_info();

	u32 thread_count;
	if (arguments.thread_count == 0) {
		thread_count = cpu_info.logical_processor_count;
	} else {
		thread_count = min(arguments.thread_count, cpu_info.logical_processor_count);
	}

	
	// NOTE: lock is only needed initially for insertion. There's no modification afterwards so 
	// no need for LockProtected
	static SpinLock thread_id_to_fiber_lock;
	static HashMap<u32, Fiber> thread_id_to_fiber;
	
	thread_id_to_fiber.insert(get_current_thread_id(), main_fiber);

	static auto init_worker_fiber = [] {
		auto worker_fiber = init_fiber(0);
		auto thread_id = get_current_thread_id();

		scoped(thread_id_to_fiber_lock);
		thread_id_to_fiber.insert(thread_id, worker_fiber);
	};

	TaskQueueThreadPool thread_pool;
	thread_pool.init(thread_count - 1, {.worker_initter = init_worker_fiber});
	defer { thread_pool.deinit(); };


	auto source_contents_buffer = read_entire_file(arguments.source_name, {.extra_space_before = 1, .extra_space_after = 1});
	if (!source_contents_buffer.data) {
		immediate_reporter.error("Could not read input file '{}'", arguments.source_name);
		return 1;
	}
	defer { free(source_contents_buffer); };

	files_to_parse.use_unprotected().add({.location = {}, .path = arguments.source_name});

	static bool failed = false;

	while (1) {
		while (1) {
			auto popped = locked_use(files_to_parse) { return files_to_parse.pop(); };
			if (!popped) {
				break;
			}

			thread_pool += [to_parse = popped.value()] {
				auto fiber = thread_id_to_fiber.find(get_current_thread_id())->value;
				bool success = read_file_and_parse_into_global_block(fiber, to_parse.location, to_parse.path);
				atomic_or(&failed, !success);
			};
		}
	
		thread_pool.wait_for_completion(WaitForCompletionOption::do_my_task);

		if (files_to_parse.use_unprotected().count == 0) {
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

				auto worker_fiber = thread_id_to_fiber.find(get_current_thread_id())->value;

				auto result = entry.typechecker->continue_typechecking(worker_fiber, &entry);

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

	if (arguments.print_ast) {
		print_ast(&global_block);
	}

	for (auto node : global_block.children) {
		if (auto definition = as<Definition>(node)) {
			if (definition->name == u8"main"s) {
				if (!definition->initial_value) {
					immediate_reporter.error(definition->location, "main must be a lambda");
					return 1;
				}

				if (definition->mutability != Mutability::constant) {
					immediate_reporter.error(definition->location, "main must be constant");
					return 1;
				}

				auto lambda = as<Lambda>(definition->initial_value);
				if (!lambda) {
					immediate_reporter.error(definition->location, "main must be a lambda");
					return 1;
				}

				if (!types_match(lambda->head.return_type, get_builtin_type(BuiltinType::None)) &&
					!::is_concrete_integer(lambda->head.return_type)) 
				{
					immediate_reporter.error(definition->location, "main must return integer or None, not {}.", lambda->head.return_type);
					return 1;
				}

				timed_block("executing main");

				auto call = Call::create();
				call->callable = lambda;
				call->type = lambda->head.return_type;
				call->call_kind = CallKind::lambda;
				switch (arguments.interpret_mode) {
					case InterpretMode::bytecode: {
						dbgln("\nBytecode:\n");
						Bytecode::Builder builder;
						for (auto definition : global_block.definition_list) {
							builder.append_global_definition(definition);
						}
						visit(&global_block, Combine {
							[&] (auto) {},
							[&] (Lambda *lambda) {
								if (lambda->body) {
									builder.append_lambda(lambda);
								}
							},
						});
						auto bytecode = builder.build(call);
						dbgln("\nFinal instructions:\n");
						for (auto [index, instruction] : enumerate(bytecode.instructions)) {
							dbgln("{}: {}", index, instruction);
						}
						dbgln();
						auto result = Bytecode::Interpreter{}.run(&bytecode, builder.entry_point());
						println("main returned {}", result);
						break;
					}
					case InterpretMode::ast: {
						auto context = NodeInterpreter::create(main_fiber, call);
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
		}
	}

	with(ConsoleColor::green, println("Build success"));

	return 0;
}