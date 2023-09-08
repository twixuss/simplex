#include <type_traits>
#include <xtr1common>

namespace tl {
template <class, class>
struct Span;
}
using String = tl::Span<char8_t, unsigned long long>;

void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function);
template <class ...Args>
void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, char const *format, Args ...args);

void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location);
template <class ...Args>
void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, char const *format, Args ...args);

#define ASSERTION_FAILURE(cause_string, expression, ...) (::assertion_failure(cause_string, expression, __FILE__, __LINE__, __FUNCSIG__, __VA_ARGS__), debug_break())

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
#include <tl/fiber.h>
#include <tl/time.h>

#define ENABLE_TIME_LOG 0
#define ENABLE_STRING_HASH_COUNT 0

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

bool fold_constants = true;
bool constant_name_inlining = true;
bool print_uids = false;

#if ENABLE_TIME_LOG
#define timed_block(name) \
	println("{} ...", name); \
	auto timer = create_precise_timer(); \
	defer { timed_results.add({name, get_time(timer)}); }

#define timed_function() \
	static constexpr auto funcname = __FUNCTION__; \
	timed_block(funcname)

#define timed_expression_named(name, expression) \
	[&] { \
		timed_block(name); \
		return expression; \
	}()

#define timed_expression(expression) timed_expression_named(#expression, expression)
#else
#define timed_block(name)
#define timed_function() 
#define timed_expression_named(name, expression) expression
#define timed_expression(expression) timed_expression_named(#expression, expression)
#endif

inline void escape_character(char ch, auto write) {
	switch (ch) {
		case '\a': write('\\'); write('a'); break;
		case '\b': write('\\'); write('b'); break;
		case '\f': write('\\'); write('f'); break;
		case '\n': write('\\'); write('n'); break;
		case '\r': write('\\'); write('r'); break;
		case '\t': write('\\'); write('t'); break;
		case '\v': write('\\'); write('v'); break;
		case '\\': write('\\'); write('\\'); break;
		default:
			write(ch);
			break;
	}
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
inline void escape_string(Span<utf8> string, auto write) {
	for (auto ch : string) {
		escape_character((char)ch, write);
	}
}

inline void escape_string_buffered(Span<utf8> string, auto write) {
	StaticList<utf8, 4096> buffer;
	for (auto ch : string) {
		escape_character((char)ch, [&](char ch) { buffer.add(ch); });
		if (buffer.count == buffer.capacity) {
			write(buffer.span());
			buffer.clear();
		}
	}
	write(buffer.span());
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
//using GHashMap = tl::HashMap<Key, Value, Traits>;

#include "x.h"

enum TokenKind : u16 {
#define x(name) Token_##name,
#define y(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KIND(x, y)
#undef y
#undef x
};

consteval u16 const_string_to_token_kind(Span<char> token) {
	constexpr u32 counter_start = __COUNTER__;
	constexpr u32 long_token_kind_base = 0x9000;
	switch (token.count) {
		case 1:
		case 2: {
			u16 result = 0;
			for (auto c : token) {
				result <<= 8;
				result |= c;
			}
			return result;
		}
#define x(string) if (token == string##s) return long_token_kind_base + __COUNTER__ - counter_start - 1;
		case 3: {
			ENUMERATE_TRIPLE_CHAR_TOKENS(x)
				break;
		}
		default: {
			if (token == "typeof"s) return Token_typeof;
			break;
		}
#undef x
	}


	*(int *)0 = 0; /*invalid token*/;
}
consteval u32 const_string_to_token_kind(char a, char b) {
	char buffer[] { a, b };
	return const_string_to_token_kind(array_as_span(buffer));
}
consteval u32 const_string_to_token_kind(char a, char b, char c) {
	char buffer[] { a, b, c };
	return const_string_to_token_kind(array_as_span(buffer));
}

inline umm append(StringBuilder &builder, TokenKind kind) {
	switch (kind) {
		case Token_eof:    return append(builder, "end of file");
		case Token_eol:    return append(builder, "end of line");
		case Token_name:   return append(builder, "name");
		case Token_number: return append(builder, "number");
	}

	if ((u32)kind <= 0xff)
		return append(builder, (char)kind);

	switch (kind) {
#define x(name) case Token_##name: return append(builder, #name);
		ENUMERATE_TOKEN_KIND(x, x)
#undef x
	}

	return append(builder, "unknown");
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

	result.file = content_start_to_file_name.find(cursor + 1)->value;

	return result;
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
	ReportKind kind;
	String location;
	String message;

	static Report create(ReportKind kind, String location, auto const &message) {
		return {
			.kind = kind,
			.location = location,
			.message = (String)to_string(message),
		};
	}
	static Report create(ReportKind kind, String location, char const *format, auto const &arg, auto const &...args) {
		return {
			.kind = kind,
			.location = location,
			.message = (String)tl::format(format, arg, args...),
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
		if (location.data) {
			auto source_location = get_source_location(location);

			println("{}:{}:{}: ", source_location.file, source_location.line_number, source_location.column_number);
			print_report_kind(kind);
			println(": {}",  message);

			auto max_line_number = source_location.line_number + source_location.lines.count - 1;
			auto line_number_width = log10(max_line_number);
			auto line_number_alignment = align_right(line_number_width, ' ');

			if (source_location.lines.count == 1) {
				auto line = source_location.lines[0];
				auto line_number = source_location.line_number;
				::print(" {} | ", Format(line_number, line_number_alignment));
				print_replacing_tabs_with_spaces(String(line.begin(), location.begin()));
				with(get_color(kind), print_replacing_tabs_with_spaces(location));
				print_replacing_tabs_with_spaces(String(location.end(), line.end()));
				println();
			} else {
				assert(source_location.lines.count > 1);
				{
					auto line = source_location.lines[0];
					auto line_number = source_location.line_number;
					::print(" {} | ", Format(line_number, line_number_alignment));
					print_replacing_tabs_with_spaces(String(line.begin(), location.begin()));
					with(get_color(kind), print_replacing_tabs_with_spaces(String(location.begin(), line.end())));
					println();
				}
				for (auto &line : source_location.lines.skip(1).skip(-1)) {
					auto line_number = source_location.line_number + index_of(source_location.lines, &line);
					::print(" {} | ", Format(line_number, line_number_alignment));
					with(get_color(kind), print_replacing_tabs_with_spaces(line));
					println();
				}
				{
					auto line = source_location.lines.back();
					auto line_number = source_location.line_number + source_location.lines.count - 1;
					::print(" {} | ", Format(line_number, line_number_alignment));
					with(get_color(kind), print_replacing_tabs_with_spaces(String(line.begin(), location.end())));
					print_replacing_tabs_with_spaces(String(location.end(), line.end()));
					println();
				}
			}
		} else {
			print_report_kind(kind);
			println(": {}", message);
		}
		println();
	}
};

bool break_on_error = false;

struct ReporterBase {
	void info   (this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::info,    location, args...)); }
	void warning(this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::warning, location, args...)); }
	void error  (this auto &&self, String location, auto const &...args) {
		if (break_on_error) {
			debug_break();
		}
		self.on_report(Report::create(ReportKind::error, location, args...));
	}
	void help   (this auto &&self, String location, auto const &...args) { self.on_report(Report::create(ReportKind::help, location, args...)); }
	void info   (this auto &&self, auto const &...args) { return self.info   (String{}, args...); }
	void warning(this auto &&self, auto const &...args) { return self.warning(String{}, args...); }
	void error  (this auto &&self, auto const &...args) { return self.error  (String{}, args...); }
	void help   (this auto &&self, auto const &...args) { return self.help   (String{}, args...); }
};

RecursiveSpinLock stdout_mutex;

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

using Tokens = GList<Token>;

Optional<Tokens> source_to_tokens(String source, String path) {
	timed_function();

	const umm characters_per_token_prediction = 4;

	Tokens tokens;
	tokens.reserve(source.count / characters_per_token_prediction);

	utf8 *cursor = source.data;

	auto print_invalid_character_error = [&] {
		immediate_reporter.error({cursor, 1}, "Invalid uft8 character.");
	};

	for (umm i = 0; i < source.count; ++i) {
		auto c = source.data[i];
		if (c <= 0x08 || (0x0b <= c && c <= 0x0c) || (0x0e <= c && c <= 0x1f)) {
			immediate_reporter.error("Invalid character at byte {}: {} (0x{})", i, escape_character(c).span(), FormatInt{.value=(u32)c,.radix=16});
			return {};
		}
	}

	auto next = [&] {
		assert(cursor <= source.end());
		++cursor;
	};

	while (true) {

		while (true) {
			if (*cursor == ' ' || *cursor == '\t' || *cursor == '\r')
				next();
			else
				break;
		}

		if (cursor == source.end())
			break;

		Token token;
		token.string.data = cursor;

		// a &
		// b =
		// &
		// &=
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
		tokens.add(token);                                            \
		break;                                                        \
	}

		// a &
		// b =
		// &
		// &&
		// &=
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
				break;	                                                  \
			}                                                             \
		}                                                                 \
		tokens.add(token);                                                \
		break;                                                            \
	}
		// a <
		// b =
		// <
		// <=
		// <<
		// <<=
#define CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE(a, b)                               \
	case a: {																	 \
		next(); 																 \
		switch (*cursor) {														 \
			case b:																 \
				token.kind = (TokenKind)const_string_to_token_kind(a, b); 		 \
				next();														     \
				token.string.count = 2; 										 \
				break; 															 \
			case a: {															 \
				next();														     \
				if (*cursor == b) {												 \
					next();													     \
					token.kind = (TokenKind)const_string_to_token_kind(a, a, b); \
					token.string.count = 3;										 \
				} else {														 \
					token.kind = (TokenKind)const_string_to_token_kind(a, a); 	 \
					token.string.count = 2; 									 \
				}																 \
				break;															 \
			}																	 \
			default: {															 \
				token.kind = (TokenKind)a; 										 \
				token.string.count = 1;											 \
				break;															 \
			}																	 \
		}																		 \
		tokens.add(token); 														 \
		break;																	 \
	}
		switch (*cursor) {
			case '(': case ')':
			case '[': case ']':
			case '{': case '}':
			case '`': case '~':
			case '!': case '@':
			case '$': case ';':
			case ':': case ',':
			case '?':
			case '\\': case '\n': {
				token.kind = (TokenKind)*cursor;
				token.string.count = 1;
				tokens.add(token);
				next();
				break;
			}
					 CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE('>', '=')
						 CASE_SINGLE_OR_TWO_DOUBLES_OR_TRIPLE('<', '=')
						 CASE_SINGLE_OR_DOUBLE('.', '.');
					 CASE_SINGLE_OR_DOUBLE('+', '=');
					 CASE_SINGLE_OR_DOUBLE('-', '=');
					 CASE_SINGLE_OR_DOUBLE('*', '=');
					 CASE_SINGLE_OR_DOUBLE('%', '=');
					 CASE_SINGLE_OR_DOUBLE('^', '=');
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
				} else {
					token.kind = (TokenKind)'/';
					token.string.count = 1;
					tokens.add(token);
				}
				break;
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
				tokens.add(token);
				break;
			}

					ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
						token.kind = Token_number;

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
					number_loop_end:;

						token.string.set_end(cursor);
						tokens.add(token);
						break;
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
#define x(name) max = tl::max(max, u8#name##s.count);
					ENUMERATE_KEYWORDS(x)
#undef x
						return max; 
				}();

				// NOTE:
				// This relies on compiler being able to optimize out branches.
				// Based on string size, check only keywords with that size.
				switch (token.string.count) {
#define x(name) \
	else if (u8#name##s.count == COUNT && token.string == #name) { \
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
				}

				tokens.add(token);
				break;
			}
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

enum class BinaryOperation : u16 {
#define x(name, token, precedence) name = const_string_to_token_kind(token##s),
	ENUMERATE_BINARY_OPERATIONS(x)
#undef x
};

enum class UnaryOperation : u16 {
#define x(name, token) name = const_string_to_token_kind(token##s),
#define y(name)
	ENUMERATE_UNARY_OPERATIONS(x, y)
#undef y
#undef x

	_unused = 0xefff,

#define x(name, token)
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

template <class T>
struct NodeBase {
	NodeBase() {
		((T *)this)->kind = NodeTypeToKind<T>::kind;
	}
	static T *create() {
		return DefaultAllocator{}.allocate<T>();
	}
	void free() {
		((T *)this)->free_impl();
	}
};

enum class Mutability : u8 {
	readonly, // can not be modified by anyone.
	constant, // known at compile time. can be casted to readonly
	variable, // can be modified by anyone.
};

inline umm append(StringBuilder &builder, Mutability mutability) {
	switch (mutability) {
		case Mutability::constant: return append(builder, "const");
		case Mutability::readonly: return append(builder, "let");
		case Mutability::variable: return append(builder, "var");
	}
	return append_format(builder, "(unknown Mutability {})", (u32)mutability);
}

struct Meaning {
	Mutability value = {};
};

Meaning meaning(Mutability mutability) {
	return {mutability};
}

inline umm append(StringBuilder &builder, Meaning mutability) {
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
#define x(name, value) case BuiltinType::name: return append(builder, #name);
		ENUMERATE_BUILTIN_TYPES(x)
#undef x
	}
	return append_format(builder, "(unknown BuiltinType {})", (u32)type_kind);
}

inline BuiltinType token_kind_to_builtin_type_kind(TokenKind kind) {
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

struct Value {
	ValueKind kind = {};
	union {
		u64 u64;
		s64 s64;
		bool boolean;
		Lambda *lambda;
		Expression *type;
		Value *pointer;
	};
	Block *break_tag = 0;
};


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
};
DEFINE_EXPRESSION(Definition) {
	String name;
	Expression *parsed_type = 0;
	Expression *initial_value = 0;
	Optional<Value> constant_value = {};
	Mutability mutability = {};
	bool is_parameter : 1 = false;
};
DEFINE_EXPRESSION(IntegerLiteral) {
	u64 value = 0;
};
DEFINE_EXPRESSION(BooleanLiteral) {
	bool value = false;
};
DEFINE_EXPRESSION(LambdaHead) {
	LambdaHead() {
		parameters_block.container = this;
	}
	Block parameters_block;
	Expression *return_type = 0;
};
DEFINE_EXPRESSION(Lambda) {
	Definition *definition = 0;
	Expression *body = 0;
	LambdaHead head;

	GList<Return *> returns;
	
	Inline inline_status = {};
	bool is_intrinsic : 1 = false;
};
DEFINE_EXPRESSION(Name) {
	String name;
	Definition *definition = 0;
};
DEFINE_EXPRESSION(If) {
	Expression *condition = 0;
	Node *true_branch = 0;
	Node *false_branch = 0;
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
};
DEFINE_EXPRESSION(Unary) {
	Expression *expression = 0;
	UnaryOperation operation = {};
	Mutability mutability = {};
};
DEFINE_STATEMENT(Return) {
	Expression *value = 0;
	Lambda *lambda = 0;
};
DEFINE_STATEMENT(While) {
	Expression *condition = 0;
	Node *body = 0;
};
DEFINE_STATEMENT(Continue) {};
DEFINE_STATEMENT(Break) {
	Block *tag_block = 0;
	Expression *value = 0;
};

template <class T>
concept CNode = std::_Is_any_of_v<T, Expression, Statement
#define x(name) , name
ENUMERATE_NODE_KIND(x)
#undef x
>;

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
		case ValueKind::u64: return append(builder, value.u64);
		case ValueKind::s64: return append(builder, value.s64);
		case ValueKind::boolean: return append(builder, value.boolean);
		case ValueKind::type:    return append(builder, value.type);
		case ValueKind::lambda:  return append(builder, value.lambda);
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
ForEachDirective visit_impl(If **node, auto &&visitor) {
	auto If = *node;
	VISIT(&If->condition);
	VISIT(&If->true_branch);
	if (If->false_branch)
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
	VISIT(&head->return_type);
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
ForEachDirective visit_impl(Name **, auto &&) { return ForEach_continue; }
ForEachDirective visit_impl(IntegerLiteral **, auto &&) { return ForEach_continue; }
ForEachDirective visit_impl(BooleanLiteral **, auto &&) { return ForEach_continue; }
ForEachDirective visit_impl(BuiltinTypeName **, auto &&) { return ForEach_continue; }
ForEachDirective visit_impl(Continue **, auto &&) { return ForEach_continue; }
ForEachDirective visit_impl(Break **, auto &&) { return ForEach_continue; }

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

	switch ((*node)->kind) {
#define x(name)                                                      \
	case NodeKind::name:                                             \
		if (visitor_wrapper((name **)node) == ForEach_break) \
			return ForEach_break;                                    \
		return visit_impl((name **)node, visitor);

		ENUMERATE_NODE_KIND(x)
#undef x
		default: {
			invalid_code_path();
			return {};
		}
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
			if (name->definition->mutability == Mutability::constant) {
				node = name->definition->initial_value;
				continue;
			} else {
				return name->definition;
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

// TODO: Use CheckResult here?
bool types_match(Expression *a, Expression *b) {
	a = direct(a);
	b = direct(b);

	if (a->kind != b->kind)
		return false;

	switch (a->kind) {
		case NodeKind::BuiltinTypeName: {
			REDECLARE_VAL(a, (BuiltinTypeName *)a);
			REDECLARE_VAL(b, (BuiltinTypeName *)b);

			return a->type_kind == b->type_kind;
		}
		case NodeKind::LambdaHead: {
			REDECLARE_VAL(a, (LambdaHead *)a);
			REDECLARE_VAL(b, (LambdaHead *)b);

			if (a->parameters_block.definition_list.count != b->parameters_block.definition_list.count)
				return false;

			if (!types_match(a->return_type, b->return_type))
				return false;

			for (umm i = 0; i < a->parameters_block.definition_list.count; ++i) {
				if (!types_match(a->parameters_block.definition_list[i]->type, b->parameters_block.definition_list[i]->type))
					return false;
			}
			return true;
		}
		case NodeKind::Unary: {
			REDECLARE_VAL(a, (Unary *)a);
			REDECLARE_VAL(b, (Unary *)b);

			if (a->operation != b->operation)
				return false;

			assert(a->operation == UnaryOperation::pointer);

			if (!types_match(a->expression, b->expression))
				return false;

			return true;
		}
	}
	invalid_code_path("invalid node kind {} in types_match", a->kind);
}

bool types_match(Expression *a, BuiltinType b) {
	a = direct(a);

	if (auto ab = as<BuiltinTypeName>(a)) {
		return ab->type_kind == b;
	}

	return false;
}
bool types_match(BuiltinType a, Expression *b) {
	return types_match(b, a);
}

bool is_type(Expression *expression) {
	return types_match(expression->type, BuiltinType::Type);
}

bool is_integer(Expression *type) {
	type = direct(type);
	assert(is_type(type));
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
bool is_signed_integer(Expression *type) {
	type = direct(type);
	assert(is_type(type));
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
bool is_unsigned_integer(Expression *type) {
	type = direct(type);
	assert(is_type(type));
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

bool is_concrete(Expression *type) {
	type = direct(type);
	assert(is_type(type));
	if (auto builtin_type = as<BuiltinTypeName>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinType::UnsizedInteger:
				return false;
		}
	}

	return true;
}

void propagate_concrete_type(Expression *expression, Expression *type) {
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
		case NodeKind::If: {
			auto If = (::If *)expression;
			If->type = type;

			if (auto true_expression = as<Expression>(If->true_branch)) {
				if (auto false_expression = as<Expression>(If->false_branch)) {
					propagate_concrete_type(true_expression, type);
					propagate_concrete_type(false_expression, type);
				}
			}

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
		case BuiltinType::U8:  return 1;
		case BuiltinType::U16: return 2;
		case BuiltinType::U32: return 4;
		case BuiltinType::U64: return 8;
		case BuiltinType::S8:  return 1;
		case BuiltinType::S16: return 2;
		case BuiltinType::S32: return 4;
		case BuiltinType::S64: return 8;
		default: invalid_code_path("Invalid BuiltinType {}", type_kind);
	}
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
	print(" = ");
	if (definition->constant_value) {
		print_ast(definition->constant_value.value());
	} else if (definition->initial_value) {
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
void print_ast_impl(LambdaHead *head, bool print_braces = true) {
	if (print_braces) {
		print("{");
		++tabs;
	}
	print("(");
	tabbed {
		List<List<Definition *>> grouped_parameters;

		for (auto parameter : head->parameters_block.definition_list) {
			if (grouped_parameters.count && types_match(grouped_parameters.back().front()->parsed_type, parameter->parsed_type)) {
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
			print_ast(group[0]->parsed_type);
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
		print(name->definition->uid);
	}
}
void print_ast_impl(Return *return_) {
	print("return");
	if (return_->value) {
		print(' ');
		print_ast(return_->value);
	}
}
void print_ast_impl(If *If) {
	print("if ");
	print_ast(If->condition);
	print(" then ");
	print_ast(If->true_branch);
	if (If->false_branch) {
		print(" else ");
		print_ast(If->false_branch);
	}
}
void print_ast_impl(While *While) {
	print("while ");
	print_ast(While->condition);
	print(" then ");
	print_ast(While->body);
}
void print_ast_impl(BuiltinTypeName *type) {
	switch (type->type_kind) {
#define x(name, value) case BuiltinType::name: print(#name); return;
		ENUMERATE_BUILTIN_TYPES(x)
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
			print_ast(Case.from);
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
				return append(builder, '*') + append(builder, unary->expression);
			}
			break;
		}
	}
	return append(builder, "(unknown)");
}

Block global_block;

struct Parser {
	Token *token = 0;
	Block *current_block = &global_block;
	While *current_loop = 0;
	Expression *current_container = 0;
	Reporter reporter;
	Fiber parent_fiber = {};
	List<Node *> result_nodes;
	bool success = false;

	// Parses parse_expression_1 with binary operators and definitions.
	Expression *parse_expression(bool whitespace_is_skippable_before_binary_operator = false, int right_precedence = 0) {
		switch (token->kind) {
			case Token_var:
			case Token_let:
			case Token_const: {
				auto definition = Definition::create();
				definition->mutability = [&] {
					switch (token->kind) {
						case Token_const: return Mutability::constant;
						case Token_let:   return Mutability::readonly;
						case Token_var:   return Mutability::variable;
						default: invalid_code_path();
					}
				}();

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

					definition->parsed_type = parse_expression_1(); // NOTE: don't parse '='
				}

				if (token->kind == '=') {
					next();
					skip_lines();

					definition->initial_value = parse_expression();

					if (definition->mutability == Mutability::constant) {
						if (auto lambda = as<Lambda>(definition->initial_value)) {
							lambda->definition = definition;
						}
					}
				} else {
					if (definition->mutability != Mutability::variable) {
						reporter.error(definition->location, "Definitions can't be marked as {} and have no initial expression.", definition->mutability);
						reporter.help(definition->location, "You can either change {} to {}, or provide an initial expression.", definition->mutability, Mutability::variable);
						yield(false);
					}

					expect({Token_eol, Token_eof});
				}
				return definition;
			}
		}


		//null denotation
		auto left = parse_expression_1();

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
	// Parses parse_expression_0 plus parentheses or brackets after, e.g. calls, subscripts.
	Expression *parse_expression_1() {
		auto node = parse_expression_0();

		while (token->kind == '(') {
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
						continue;
					}
					if (token->kind == ')') {
						break;
					}

					reporter.error(token->string, "Unexpected token {} when parsing call argument list. Expected ',' or ')'.", token->string);
					yield(false);
				}
			}

			call->location = {call->location.begin(), token->string.end()};

			next();
			node = call;
		}
		return node;
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
						expect(Token_name);

						List<Definition *> parameter_group;

						{
							auto parameter = Definition::create();
							parameter->name = token->string;
							parameter_group.add(parameter);
						}

						next();
						skip_lines();

						while (token->kind == ',') {
							next();
							skip_lines();
							expect(Token_name);

							auto parameter = Definition::create();
							parameter->name = token->string;
							parameter_group.add(parameter);
							next();
						}

						expect(':');

						next();
						skip_lines();

						auto parsed_type = parse_expression();

						for (auto parameter : parameter_group) {
							parameter->parsed_type = parsed_type;
							parameter->is_parameter = true;
							parameter->mutability = Mutability::readonly;
							lambda->head.parameters_block.add(parameter);
						}

						skip_lines();
						if (token->kind == ',') {
							next();
							skip_lines();
							continue;
						}
						if (token->kind == ')') {
							break;
						}
					}
				}
				
				next();
				skip_lines();

				if (token->kind == ':') {
					next();
					skip_lines();

					lambda->head.return_type = parse_expression_0();
				}
				
				lambda->head.location = lambda->location = { lambda->location.begin(), token[-1].string.end() };

				constexpr auto arrow = const_string_to_token_kind("=>"s);
				if (token->kind == arrow) {
					next();
					skip_lines();

					while (token->kind == Token_directive) {
						if (token->string == u8"#intrinsic"s) {
							lambda->is_intrinsic = true;
						} else {
							reporter.error(token->string, "Unknown lambda directive '{}'.", token->string);
							yield(false);
						}
						next();
					}

					if (lambda->is_intrinsic) {
						return lambda;
					}

					lambda->body = parse_expression();

					return lambda;
				}

				if (token->kind == '{') {
					reporter.warning(lambda->head.location, "This was parsed as a lambda head type. But there is '{' immediately after it. Did you mean this to be a lambda? If so you missed an '=>'.");
				}

				// LEAK: the rest of the lambda is unused.
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

				if (block->children.count == 1) {
					if (auto expression = as<Expression>(block->children[0])) {
						block->free();
						return expression;
					}
				}

				return block;
			}
			case Token_name: {
				auto name = Name::create();
				name->location = token->string;
				name->name = token->string;
				next();
				return name;
			}
			case Token_number: {
				auto literal = IntegerLiteral::create();
				literal->location = token->string;

				auto parsed = parse_u64(token->string);
				if (!parsed) {
					reporter.error(token->string, "Could not parse number {}", token->string);
					yield(false);
				}

				literal->value = parsed.value();

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
			case Token_if: {
				auto If = If::create();
				If->location = token->string;
				next();
				skip_lines();

				If->condition = parse_expression();

				skip_lines();
				if (token->kind == Token_then) {
					next();
					skip_lines();
				}

				If->true_branch = parse_statement();

				auto saved = token;
				skip_lines();
				if (token->kind == ';') {
					next();
					skip_lines();
				}
				if (token->kind == Token_else) {
					next();
					skip_lines();

					If->false_branch = parse_statement();
					
					If->location = {If->location.begin(), If->false_branch->location.end()};
				} else {
					token = saved;
					If->location = {If->location.begin(), If->true_branch->location.end()};
				}

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
					auto from = parse_expression();

					skip_lines();
					expect('=>');
					next();
					skip_lines();

					auto to = parse_expression();

					match->cases.add({from, to});

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
				auto expr = parse_expression_1();
				if (auto lambda = as<Lambda>(expr)) {
					lambda->inline_status = status;
					return lambda;
				} else if (auto call = as<Call>(expr)) {
					call->inline_status = status;
					return call;
				}

				reporter.error(inline_token->string, "{} keyword must precede a lambda or a call, not a {}", inline_token->string, expr->kind);
				yield(false);
			}
#define x(name) case Token_##name:
			ENUMERATE_CONCRETE_BUILTIN_TYPES(x)
#undef x
			{
				auto type = BuiltinTypeName::create();
				type->location = token->string;
				type->type_kind = token_kind_to_builtin_type_kind(token->kind);
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
					unop->expression = parse_expression_1();
					unop->location = {unop->location.begin(), unop->expression->location.end()};
					return unop;
				}

				reporter.error(token->string, "Unexpected token {} when parsing expression.", *token);
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
		}

		auto expression = parse_expression();

		return expression;
	}

	void yield(bool result) {
		if (result)
			success = true;
		fiber_yield(parent_fiber);
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
			case NodeKind::Definition:
			case NodeKind::Block:
			case NodeKind::Return:
			case NodeKind::Call:
			case NodeKind::If:
			case NodeKind::While:
			case NodeKind::Continue:
			case NodeKind::Break:
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

				reporter.error(node->location, "{} {} are not allowed in statement context.", node->kind, binary->operation);
				yield(false);
			}
		}

		reporter.error(node->location, "{}s are not allowed in statement context.", node->kind);
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
	void expect(std::initializer_list<std::underlying_type_t<TokenKind>> expected_kinds) {
		for (auto expected_kind : expected_kinds) {
			if (token->kind == expected_kind) {
				return;
			}
		}

		StringBuilder builder;
		append(builder, "Expected one of ");
		for (auto expected_kind : expected_kinds) {
			append_format(builder, "'{}', ", (TokenKind)expected_kind);
		}
		append_format(builder, "but got '{}'.\0"s, *token);
		reporter.error(token->string, (char *)to_string(builder).data);
		yield(false);
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

	auto fiber = fiber_create(Parser::fiber_main, &parser);
	fiber_yield(fiber);

	if (parser.success)
		return parser.result_nodes;

	return {};
}

struct ExecutionContext {
	struct Scope {
		// This needs to be pointer-stable
		BucketHashMap<Definition *, Value> variables;
	};

	static ExecutionContext *create(Fiber parent_fiber) {
		auto result = DefaultAllocator{}.allocate<ExecutionContext>();
		auto &scope = result->scope_stack.add(); // global scope
		for (auto definition : global_block.definition_list) {
			auto value = result->execute(definition->initial_value);
			scope.variables.get_or_insert(definition) = value;
		}
		result->parent_fiber = parent_fiber;

		result->fiber = fiber_create(fiber_main, result);

		return result;
	}

	Optional<Value> run(Node *node) {
		node_to_execute = node;
		success = false;
		fiber_yield(fiber);
		if (success) {
			return result_value;
		}
		return {};
	}

private:

	static constexpr umm recursion_limit = 256;
	umm recursion_level = 0;

	Node *current_node = 0;
	Block *current_block = 0;
	bool breaking_from_block = false;

	List<Scope> scope_stack;
	Value return_value;

	Fiber fiber;
	Fiber parent_fiber;

	Node *node_to_execute;
	Value result_value;
	bool success;

	static void fiber_main(void *param) {
		((ExecutionContext *)param)->main();
	}
	void main() {
		result_value = execute(node_to_execute);
		yield(true);
	}

	void yield(bool success) {
		this->success = success;
		fiber_yield(parent_fiber);
	}

#define PERFORM_WITH_BREAKS(name, execute, node) \
	name = execute(node); \
	switch (name.kind) { \
		case ValueKind::return_: \
		case ValueKind::break_: \
		case ValueKind::continue_: return name; \
		default: \
			if (breaking_from_block) { \
				if (name.break_tag == current_node) { \
					breaking_from_block = false; \
				} \
				return name; \
			} \
			break; \
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
	Value load_address_impl(Definition *definition) {
		for (auto &scope : reverse_iterate(scope_stack)) {
			if (auto found = scope.variables.find(definition)) {
				return {.kind = ValueKind::pointer, .pointer = &found->value};
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
		assert(name->definition);
		return load_address(name->definition);
	}
	Value load_address_impl(Call *) { invalid_code_path(); }
	Value load_address_impl(If *) { invalid_code_path(); }
	Value load_address_impl(BuiltinTypeName *) { invalid_code_path(); }
	Value load_address_impl(Binary *) { invalid_code_path(); }
	Value load_address_impl(Match *) { invalid_code_path(); }
	Value load_address_impl(Unary *unary) {
		if (unary->operation == UnaryOperation::dereference) {
			auto pointer = execute(unary->expression);
			assert(pointer.kind == ValueKind::pointer);
			return pointer;
		}
		invalid_code_path();
	}

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
		if (::is_unsigned_integer(literal->type))
			return { .kind = ValueKind::u64, .u64 = literal->value };
		if (::is_signed_integer(literal->type))
			return { .kind = ValueKind::s64, .s64 = (s64)literal->value };

		invalid_code_path();
		return {};
	}
	Value execute_impl(BooleanLiteral *literal) {
		return { .kind = ValueKind::boolean, .boolean = literal->value };
	}
	Value execute_impl(Definition *definition) {
		if (definition->mutability == Mutability::constant) {
			assert(definition->constant_value);
			return definition->constant_value.value();
		}

		assert(definition->initial_value, "Default-initialized variables are not implemented in ExecutionContext");
		EXECUTE_DEFN(value, definition->initial_value);
		scope_stack.back().variables.get_or_insert(definition) = value;
		return value;
	}
	Value execute_impl(Return *return_) {
		if (return_->value) {
			EXECUTE_INTO(return_value, return_->value);
		}
		return { .kind = ValueKind::return_ };
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
		return {.kind = ValueKind::lambda, .lambda = lambda}; 
	}
	Value execute_impl(LambdaHead *head) {
		return {.kind = ValueKind::type, .type = head};
	}
	Value execute_impl(Name *name) {
		assert(name->definition);
		for (auto &scope : reverse_iterate(scope_stack)) {
			if (auto found = scope.variables.find(name->definition)) {
				return found->value;
			}
		}
		invalid_code_path();
	}
	Value execute_impl(Call *call) {
		auto &arguments = call->arguments;

		auto callable = execute(call->callable);

		if (callable.kind != ValueKind::lambda || !callable.lambda) {
			immediate_reporter.error(call->location, "You can only call constant lambdas right now.");
			yield(false);
		}

		auto lambda = callable.lambda;
		auto &parameters = lambda->head.parameters_block.definition_list;

		if (lambda->is_intrinsic) {
			assert(lambda->definition);

			List<Value> argument_values;
			argument_values.allocator = temporary_allocator;
			argument_values.reserve(arguments.count);

			for (auto argument : arguments) {
				EXECUTE_DEFN(argument_value, argument);
				argument_values.add(argument_value);
			}

			auto name = lambda->definition->name;

			if (name == u8"println"s) {
				assert(arguments.count == 1);

				scoped(temporary_allocator_and_checkpoint);

				List<utf8> result;

				switch (argument_values[0].kind) {
					case ValueKind::s64: result = to_string(argument_values[0].s64); break;
					case ValueKind::u64: result = to_string(argument_values[0].u64); break;
					case ValueKind::type: result = to_string(argument_values[0].type); break;
					case ValueKind::boolean: result = to_string(argument_values[0].boolean); break;
					case ValueKind::lambda: result = format(u8"(lambda {})", argument_values[0].lambda->uid); break;
					case ValueKind::pointer: result = format(u8"0x{}", FormatInt{.value = (umm)argument_values[0].pointer, .radix = 16}); break;
					case ValueKind::none: result = to_list(u8"none"s); break;
					default: {
						immediate_reporter.error(arguments[0]->location, "Unknown type {} in println", argument_values[0].kind);
						yield(false);
					}
				}

				println(result);

				return {.kind = ValueKind::s64, .s64 = (s64)result.count };
			} else if (name == u8"exit"s) {
				yield(true);
			}
			
			immediate_reporter.error(lambda->location, "Attempt to execute invalid intrinsic '{}'.", name);
			yield(false);
		}

		List<Value> executed_arguments;
		executed_arguments.allocator = temporary_allocator;
		executed_arguments.reserve(arguments.count);
		for (auto argument : arguments) {
			EXECUTE_DEFN(argument_value, argument);
			executed_arguments.add(argument_value);
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
			param_scope.variables.get_or_insert(parameters[i]) = executed_arguments[i];
		}

		auto result = execute(lambda->body);
		if (result.kind == ValueKind::return_) {
			return return_value;
		} else {
			return result;
		}
	}
	Value execute_impl(If *If) {
		EXECUTE_DEFN(condition, If->condition);
		if (condition.boolean) {
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
	Value execute_impl(While *While) {
		while (true) {
			EXECUTE_DEFN(condition, While->condition);
			if (!condition.boolean) {
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
	Value execute_impl(Continue *Continue) { return { .kind = ValueKind::continue_ }; }
	Value execute_impl(Break *Break) {
		if (!Break->value) {
			return { .kind = ValueKind::break_ };
		}

		assert(Break->tag_block);

		EXECUTE_DEFN(value, Break->value);
		value.break_tag = Break->tag_block;
		breaking_from_block = true;
		return value;
	}
	Value execute_impl(BuiltinTypeName *type) { return {.kind = ValueKind::type, .type = type}; }
	Value execute_impl(Binary *binary) {
		if (binary->operation == BinaryOperation::ass) {
			LOAD_ADDRESS_DEFN(address, binary->left);
			EXECUTE_DEFN(value, binary->right);
			*address.pointer = value;
			return {};
		}

		EXECUTE_DEFN(left, binary->left);
		EXECUTE_DEFN(right, binary->right);

#define OPS(t) 																									\
		if (left.kind == ValueKind::t && right.kind == ValueKind::t) {											\
			switch (binary->operation) {																		\
				case BinaryOperation::add: return { .kind = ValueKind::t, .t = left.t + right.t };			    \
				case BinaryOperation::sub: return { .kind = ValueKind::t, .t = left.t - right.t };			    \
				case BinaryOperation::mul: return { .kind = ValueKind::t, .t = left.t * right.t };			    \
				case BinaryOperation::div: return { .kind = ValueKind::t, .t = left.t / right.t };			    \
				case BinaryOperation::mod: return { .kind = ValueKind::t, .t = left.t % right.t }; /* TODO: ensure this is right operation */ \
				case BinaryOperation::bor: return { .kind = ValueKind::t, .t = left.t | right.t };			    \
				case BinaryOperation::ban: return { .kind = ValueKind::t, .t = left.t & right.t };			    \
				case BinaryOperation::bxo: return { .kind = ValueKind::t, .t = left.t ^ right.t };			    \
				case BinaryOperation::bsl: return { .kind = ValueKind::t, .t = left.t << right.t };			    \
				case BinaryOperation::bsr: return { .kind = ValueKind::t, .t = left.t >> right.t };			    \
				case BinaryOperation::equ: return { .kind = ValueKind::boolean, .boolean = left.t == right.t };	\
				case BinaryOperation::neq: return { .kind = ValueKind::boolean, .boolean = left.t != right.t };	\
				case BinaryOperation::les: return { .kind = ValueKind::boolean, .boolean = left.t <  right.t };	\
				case BinaryOperation::leq: return { .kind = ValueKind::boolean, .boolean = left.t <= right.t };	\
				case BinaryOperation::grt: return { .kind = ValueKind::boolean, .boolean = left.t >  right.t };	\
				case BinaryOperation::grq: return { .kind = ValueKind::boolean, .boolean = left.t >= right.t };	\
				default: invalid_code_path();																	\
			}																									\
		}

		OPS(u64);
		OPS(s64);

#undef OPS

		immediate_reporter.error(binary->location, "Invalid binary operation");
		invalid_code_path();
	}
	Value execute_impl(Match *match) {
		EXECUTE_DEFN(value, match->expression);
		assert(value.kind == ValueKind::s64, "Only this is implemented");

		for (auto Case : match->cases) {
			EXECUTE_DEFN(from, Case.from);
			assert(from.kind == ValueKind::s64, "Only this is implemented");
			if (value.s64 == from.s64) {
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
					case ValueKind::s64: value.s64 = -value.s64; break;
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
				return {.kind = ValueKind::type, .type = unary->expression->type};
			}
			default:
				invalid_code_path();
		}
	}
};

#undef PERFORM_WITH_BREAKS
#undef EXECUTE_INTO
#undef EXECUTE_DEFN
#undef LOAD_ADDRESS_INTO
#undef LOAD_ADDRESS_DEFN

Expression *make_pointer(Expression *type, Mutability mutability) {
	auto pointer = Unary::create();
	pointer->expression = type;
	pointer->operation = UnaryOperation::pointer;
	pointer->mutability = mutability;
	pointer->type = get_builtin_type(BuiltinType::Type);
	return pointer;
}
Unary *as_pointer(Expression *type) {
	if (auto unary = as<Unary>(type); unary && unary->operation == UnaryOperation::pointer) {
		return unary;
	}
	return 0;
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
	assert(block->children.count == 1, "not implemented");

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
CheckResult is_constant_impl(Lambda *lambda) { return true; }
CheckResult is_constant_impl(LambdaHead *head) { return true; }
CheckResult is_constant_impl(Name *name) { return is_constant_impl(name->definition); }
CheckResult is_constant_impl(Call *call) { 
	MUST_BE_CONSTANT(call->callable);

	for (auto argument : call->arguments) {
		MUST_BE_CONSTANT(argument);
	}

	return true;
}
CheckResult is_constant_impl(If *If) { 
	MUST_BE_CONSTANT(If->condition);
	if (auto true_expression = as<Expression>(If->true_branch)) {
		MUST_BE_CONSTANT(true_expression);
		if (If->false_branch) {
			if (auto false_expression = as<Expression>(If->false_branch)) {
				MUST_BE_CONSTANT(false_expression);
				return true;
			}
		}
	}
	return {false, If};
}
CheckResult is_constant_impl(BuiltinTypeName *type) { return true; }
CheckResult is_constant_impl(Binary *binary) { return is_constant(binary->left) && is_constant(binary->right); }
CheckResult is_constant_impl(Match *match) {
	MUST_BE_CONSTANT(match->expression);
	for (auto &Case : match->cases) {
		MUST_BE_CONSTANT(Case.to);
	}

	return true;
}
CheckResult is_constant_impl(Unary *unary) { 
	MUST_BE_CONSTANT(unary->expression);
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
CheckResult is_mutable_impl(Lambda *lambda) { return {false, lambda}; }
CheckResult is_mutable_impl(LambdaHead *head) { return {false, head}; }
CheckResult is_mutable_impl(Name *name) { return is_mutable_impl(name->definition); }
CheckResult is_mutable_impl(Call *call) { return {false, call}; }
CheckResult is_mutable_impl(If *If) { return {false, If}; }
CheckResult is_mutable_impl(BuiltinTypeName *type) { return {false, type}; }
CheckResult is_mutable_impl(Binary *binary) { return {false, binary}; }
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

SpinLock retired_typecheckers_lock;
List<struct Typechecker *> retired_typecheckers;

enum class YieldResult : u8 {
	fail,
	success,
	wait,
};

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

	SpinLock dependants_lock;
	List<TypecheckEntry *> dependants;
};

List<TypecheckEntry> typecheck_entries;

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

bool no_more_progress = false;

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

	template <class T>
	[[nodiscard]] T *copy_base(T *from) {
		auto to = T::create();
		copied_nodes.get_or_insert(from) = to;
		to->location = from->location;
		return to;
	}

	template <class T>
	[[nodiscard]] T *deep_copy_new(T *from) {
		auto to = copy_base(from);
		deep_copy_impl(from, to);
		if (auto expression = as<Expression>(to))
			assert(expression->type);
		return to;
	}

	[[nodiscard]] Node *deep_copy(Node *from) {
		switch (from->kind) {
#define x(name) case NodeKind::name: return deep_copy_new((name *)from);
			ENUMERATE_NODE_KIND(x)
#undef x
		}
		invalid_code_path();
	}
	[[nodiscard]] Expression *deep_copy(Expression *from) {
		switch (from->kind) {
#define x(name) case NodeKind::name: return deep_copy_new((name *)from);
			ENUMERATE_EXPRESSION_KIND(x)
#undef x
		}
		invalid_code_path();
	}
	[[nodiscard]] Statement *deep_copy(Statement *from) {
		switch (from->kind) {
#define x(name) case NodeKind::name: return deep_copy_new((name *)from);
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
		to->arguments.resize(from->arguments.count);
		for (umm i = 0; i < from->arguments.count; ++i) {
			DEEP_COPY(arguments[i]);
		}
		COPY(inline_status);

		auto [lambda, head] = get_lambda_and_head(to->callable);

		assert(head);
		to->type = head->return_type;
	} 
	[[nodiscard]] void deep_copy_impl(Definition *from, Definition *to) {
		COPY(name);
		if (from->parsed_type)
			DEEP_COPY(parsed_type);
		if (from->initial_value)
			DEEP_COPY(initial_value);

		COPY(is_parameter);
		COPY(mutability);

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
		DEEP_COPY(return_type);
		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(Name *from, Name *to) {
		COPY(name);
		LOOKUP_COPY(definition);
		to->type = to->definition->type;
	} 
	[[nodiscard]] void deep_copy_impl(If *from, If *to) {
		DEEP_COPY(condition);
		DEEP_COPY(true_branch);
		if (from->false_branch)
			DEEP_COPY(false_branch);

		COPY(type);
	} 
	[[nodiscard]] void deep_copy_impl(BuiltinTypeName *from, BuiltinTypeName *to) {
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
		LOOKUP_COPY(lambda, copied_lambdas);
		to->lambda->returns.add(to);
	}
	[[nodiscard]] void deep_copy_impl(While *from, While *to) {
		DEEP_COPY(condition);
		DEEP_COPY(body);
	} 
	[[nodiscard]] void deep_copy_impl(Continue *from, Continue *to) {
	} 
	[[nodiscard]] void deep_copy_impl(Break *from, Break *to) {
		LOOKUP_COPY(tag_block);
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
		case NodeKind::If: {
			auto if_ = (If *)node;
			if (do_all_paths_return(if_->condition)) {
				return true;
			}

			if (!if_->false_branch) {
				return false;
			}

			if (do_all_paths_return(if_->true_branch) && do_all_paths_return(if_->false_branch)) {
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
		case NodeKind::BuiltinTypeName:
		case NodeKind::Break:
		case NodeKind::Continue:
		case NodeKind::Name:
		case NodeKind::IntegerLiteral:
		case NodeKind::BooleanLiteral:
		case NodeKind::Lambda:
		case NodeKind::LambdaHead:
			return false;
	}
	not_implemented("do_all_paths_return not implemented for {}", node->kind);
	return false;
}

struct Typechecker {
	const u32 uid = atomic_add(&typechecker_uid_counter, 1);
	u32 progress = 0;

	static Typechecker *create(Node **node) {
		assert(node);

		auto typechecker = [&] {
			if (auto typechecker_ = with(retired_typecheckers_lock, retired_typecheckers.pop())) {
				auto typechecker = typechecker_.value();
				// immediate_reporter.info("created cached typechecker {} for node {}", typechecker->uid, node->location);

				assert(typechecker->debug_stopped);
				assert(typechecker->yield_result == YieldResult{});
				assert(typechecker->reporter.reports.count == 0);
				assert(typechecker->initial_node == 0);
				assert(typechecker->current_block == 0);


				return typechecker;
			}

			auto typechecker = new Typechecker();
			typechecker->fiber = fiber_create([](void *param) {
				((Typechecker *)param)->fiber_main(); 
			}, typechecker);

			// immediate_reporter.info("created new typechecker {} for node {}", typechecker->uid, node->location);

			return typechecker;
		}();

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

			fiber_yield(fiber);
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

		with(retired_typecheckers_lock, retired_typecheckers.add(this));
	}

private:
	Fiber parent_fiber = {};
	Fiber fiber = {};
	YieldResult yield_result = {};
	Reporter reporter;
	Node **initial_node = 0;
	Block *current_block = 0;
	List<Node *> node_stack;
	TypecheckEntry *entry = 0;

	[[nodiscard]]
	bool yield_while(String location, auto predicate) {
		while (true) {
			if (predicate()) {
				// immediate_reporter.info(location, "Yield");

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
		fiber_yield(parent_fiber);
		if (result == YieldResult::fail) {
			throw Unwind{};
		}
	}

	void fiber_main() {
		while (true) {
			try {
				assert(initial_node);
				*initial_node = typecheck(*initial_node, true);
				yield(YieldResult::success);
			} catch (Unwind) {

			}
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

		if (types_match(direct_source_type, direct_target_type)) {
			return true;
		}

		if (types_match(direct_source_type, BuiltinType::UnsizedInteger)) {
			if (::is_integer(direct_target_type)) {
				if (apply) {
					propagate_concrete_type(expression, target_type);
				}
				return true;
			}
		}

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

		if (reporter)
			reporter->error(expression->location, "Expression of type `{}` is not implicitly convertible to `{}`.", source_type, target_type);
		return false;
	}
	bool implicitly_cast(Expression **expression, Expression *target_type, bool apply) {
		return implicitly_cast(expression, target_type, &reporter, apply);
	}

	void why_is_this_immutable(Expression *expr) {
		if (auto unary = as<Unary>(expr)) {
			if (unary->operation == UnaryOperation::dereference) {
				if (auto name = as<Name>(unary->expression)) {
					reporter.info(name->definition->location, "Because {} is a pointer to {}.", name->name, meaning(name->definition->mutability));
					if (name->definition->initial_value) {
						why_is_this_immutable(name->definition->initial_value);
					}
				}
			} else if (unary->operation == UnaryOperation::addr) {
				if (auto name = as<Name>(unary->expression)) {
					reporter.info(name->definition->location, "Because {} is marked as {}. Mark it with `var` instead to make it mutable.", name->name, name->definition->mutability);
				}
			}
		} else if (auto name = as<Name>(expr)) {
			reporter.info(name->definition->location, "Because {} is {}.", name->name, meaning(name->definition->mutability));
			why_is_this_immutable(name->definition->initial_value);
		}
	}

	Expression *inline_body(Call *call, Lambda *lambda) {
		if (!yield_while_null(call->location, &lambda->body->type)) {
			reporter.error(lambda->location, "Could not wait for lambda's body to typecheck for inlining");
			yield(YieldResult::fail);
		}
		
		// LEAK: copied_lambda
		auto copied_lambda = as<Lambda>(Copier{}.deep_copy(lambda));
		assert(copied_lambda);

		auto result_block = Block::create();
		result_block->location = call->location;

		assert(lambda->head.parameters_block.definition_list.count == call->arguments.count);
		for (umm i = 0; i < call->arguments.count; ++i) {
			auto argument = call->arguments[i];
			auto parameter = copied_lambda->head.parameters_block.definition_list[i];
			parameter->initial_value = argument;
			result_block->add(parameter);
		}

		if (auto body_block = as<Block>(copied_lambda->body)) {
			body_block->tag = format(u8"_{}", body_block->uid);
			visit(body_block, Combine{
				[&](Node *) {},
				[&](Return *ret) -> Statement * {
					if (ret->lambda == lambda) {
						auto Break = Break::create();
						Break->value = ret->value;
						assert(body_block);
						Break->tag_block = body_block;
						body_block->breaks.add(Break);
						// LEAK: ret
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
			if (auto name = as<Name>(node)) {
				return name;
			}

			if (auto block = as<Block>(node)) {
				if (block->children.count == 0) {
					return 0;
				}

				node = block->children.back();
				continue;
			} else {
				return 0;
			}
		}
	}

	//
	// These `typecheck` overloads automatically substitute old node with new one.
	//
	void typecheck(Node **node) {
		*node = typecheck(*node, true);
	}
	template <CNode T>
	void typecheck(T **node) { 
		auto new_node = typecheck(*node, true);
		*node = as<T>(new_node);
		assert(*node);
	}

	//
	// This `typecheck` overload doesn't substitute the node
	//
	template <CNode T>
	void typecheck(T &node) {
		auto new_node = typecheck(&node, false);
		assert(new_node == &node, "Attempt to substitute a node which can't be substituted.");
	}

	[[nodiscard]] Node *typecheck(Node *node, bool can_substitute) {
		++progress;
		defer { ++progress; };

		scoped_replace(debug_current_location, node->location);

		node_stack.add(node);
		defer { node_stack.pop(); };

		switch (node->kind) {
#define x(name) case NodeKind::name: node = typecheck_impl((##name *)node, can_substitute); break;
			ENUMERATE_NODE_KIND(x)
#undef x
			default:
				invalid_code_path();
		}

		assert(node);

		if (auto expression = as<Expression>(node)) {
			if (!expression->type) {
				reporter.error(expression->location, "Could not compute the type of this expression.");
				yield(YieldResult::fail);
			}
		}
		return node;
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
				if (block->children.count == 1) {
					if (can_substitute) {
						// LEAK: block
						return last_expression;
					}
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
						reporter.error((*other)->location, "Return value of type {} can't be converted to {}", (*other)->type, picked_value->type);
						reporter.info(get_non_block_location(picked_value), "Return type {} was deduced from this expression:", picked_value->type);
						reporter.info("Here's the conversion attempt report:");
						reporter.reports.add(cast_reporter.reports);
						yield(YieldResult::fail);
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

			if (definition->parsed_type) {
				if (!implicitly_cast(&definition->initial_value, definition->parsed_type, true)) {
					yield(YieldResult::fail);
				}
			} else {
				make_concrete(definition->initial_value);
			}

			if (definition->mutability == Mutability::constant) {
				auto constant_check = is_constant(definition->initial_value);
				if (!constant_check) {
					reporter.error(definition->location, "Initial value is not constant.");
					assert(constant_check.failed_node);
					reporter.info(constant_check.failed_node->location, "Because this is not constant.");
					yield(YieldResult::fail);
				}

				// NOTE:
				// Maybe this should not be an error, I just don't wanna deal with it rigt now.
				if (types_match(definition->initial_value->type, BuiltinType::None)) {
					reporter.error(definition->location, "Definitions with type None can't exist.");
					yield(YieldResult::fail);
				}

				auto context = ExecutionContext::create(fiber);
				definition->constant_value = context->run(definition->initial_value).value_or([&] {
					reporter.error(definition->initial_value->location, "Failed to evaluate value.");
					yield(YieldResult::fail);
					return Value{};
				});
			}
		} else {
			if (!definition->is_parameter) {
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
	[[nodiscard]] LambdaHead *typecheck_impl(LambdaHead *head, bool can_substitute) {
		typecheck(head->parameters_block);

		if (head->return_type) {
			typecheck(&head->return_type);
		}

		head->type = get_builtin_type(BuiltinType::Type);
		return head;
	}
	[[nodiscard]] Lambda *typecheck_impl(Lambda *lambda, bool can_substitute) {
		if (lambda->is_intrinsic) {
			if (lambda->inline_status == Inline::always) {
				reporter.error(lambda->location, "Lambda can't be intrinsic and inline at the same time.");
				yield(YieldResult::fail);
			}
			if (lambda->inline_status == Inline::never) {
				reporter.warning(lambda->location, "intrinsic lambda was marked as noinline, which is redundant");
			}
		}
		typecheck(lambda->head);

		lambda->type = &lambda->head;

		if (lambda->head.return_type) {
			if (lambda->definition) {
				lambda->definition->type = lambda->type;
			}
		}

		bool all_paths_return = false;

		if (lambda->body) {
			scoped_replace(current_block, &lambda->head.parameters_block);

			typecheck(&lambda->body);

			all_paths_return = do_all_paths_return(lambda->body);
		}

		if (lambda->head.return_type) {
			for (auto ret : lambda->returns) {
				if (!implicitly_cast(&ret->value, lambda->head.return_type, true)) {
					reporter.info(lambda->head.return_type->location, "Return type specified here:");
					yield(YieldResult::fail);
				}
			}
		} else {
			if (lambda->body) {
				List<Expression **> return_values;
				List<Return *> empty_returns;

				auto mixed_return_value_presence_error = [&] (String location, String location2) {
					reporter.error(location, "Right now you are not allowed to mix return statements with values and without.");
					reporter.info(location2, "Here's the other return statement:");
					yield(YieldResult::fail);
				};

				for (auto &ret : lambda->returns) {
					if (ret->value) {
						return_values.add(&ret->value);
						if (empty_returns.count) {
							mixed_return_value_presence_error(ret->location, empty_returns.back()->location);
						}
					} else {
						empty_returns.add(ret);
						if (return_values.count) {
							mixed_return_value_presence_error(ret->location, (*return_values.back())->location);
						}
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
							yield(YieldResult::fail);
						}
					}

					if (lambda->returns.count && lambda->returns.count == empty_returns.count) {
						if (!types_match(lambda->body->type, BuiltinType::None)) {
							reporter.error(lambda->location, "All return statements in this lambda do not provide a value, but lambda's body has a type {}", lambda->body->type);
							reporter.info(get_non_block_location(lambda->body), "Here's the expression that is implicitly returned");
							yield(YieldResult::fail);
						}
					}

					if (!all_paths_return) {
						if (!types_match(lambda->body->type, picked_value->type)) {
							reporter.error(lambda->location, "Not all paths return a value.");
							yield(YieldResult::fail);
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
						yield(YieldResult::fail);
					}

					if (concrete_return_types.count) {
						lambda->head.return_type = concrete_return_types[0];
					} else if (empty_returns.count) {
						lambda->head.return_type = get_builtin_type(BuiltinType::None);
					} else {
						make_concrete(lambda->returns[0]->value);
						for (auto ret : lambda->returns.skip(1)) {
							if (!implicitly_cast(&ret->value, lambda->returns[0]->value->type, true)) {
								yield(YieldResult::fail);
							}
						}
						lambda->head.return_type = lambda->returns[0]->value->type;
					}

					for (auto ret : lambda->returns) {
						if (ret->value) {
							if (!types_match(ret->value->type, lambda->head.return_type)) {
								reporter.error(ret->location, "Type {} does not match previously deduced return type {}.", ret->value->type, lambda->head.return_type);
								reporter.info(lambda->returns[0]->location, "First deduced here:");
								yield(YieldResult::fail);
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

				if (definitions.count > 1) {
					reporter.error(name->location, "`{}` is ambiguous.", name->name);
					for (auto definition : definitions) {
						reporter.info(definition->location, "Declared here:");
					}
					yield(YieldResult::fail);
				}

				auto definition = definitions[0];

				name->definition = definition;

				auto definition_index = find_index_of(block->children, definition);
				assert(definition_index < block->children.count);

				if (block->container && as<Lambda>(block->container)) {
					// Find our parent node in found definition's block
					for (auto node : reverse_iterate(node_stack)) {
						auto parent_index = find_index_of(block->children, node);
						if (parent_index < block->children.count) {
							if (parent_index < definition_index) {
								reporter.error(name->location, "Can't access definition because it is declared after.");
								reporter.info(definition->location, "Here is the definition.");
								yield(YieldResult::fail);
							}
							break;
						}
					}
				}

				TypecheckEntry *dependency_entry = 0;

				if (block == &global_block) {
					for (auto &typecheck_entry : typecheck_entries) {
						if (*typecheck_entry.node == definition) {
							dependency_entry = &typecheck_entry;
							break;
						}
					}
				}

				if (dependency_entry) {
					scoped(dependency_entry->dependants_lock);
					dependency_entry->dependants.add(entry);
				}

				if (!yield_while_null(name->location, &definition->type)) {
					reporter.error(name->location, "Couldn't wait for definition type.");
					yield(YieldResult::fail);
				}

				if (dependency_entry) {
					scoped(dependency_entry->dependants_lock);
					find_and_erase_unordered(dependency_entry->dependants,entry);
				}

				name->type = definition->type;

				if (constant_name_inlining) {
					if (definition->mutability == Mutability::constant) {
						if (definition->initial_value->kind != NodeKind::Lambda) {
							auto literal = Copier{}.deep_copy(definition->initial_value);
							assert(literal->kind == NodeKind::IntegerLiteral || literal->kind == NodeKind::BooleanLiteral);
							return literal;
						}
					}
				}

				return name;
			}
		}
		reporter.error(name->location, "`{}` was not declared.", name->name);
		yield(YieldResult::fail);
		return 0;
	}
	[[nodiscard]] Expression *typecheck_impl(Call *call, bool can_substitute) {
		typecheck(&call->callable);

		auto &arguments = call->arguments;
		for (auto &argument : arguments) {
			typecheck(&argument);
		}

		auto [lambda, head] = get_lambda_and_head(call->callable);

		if (!head) {
			reporter.error(call->callable->location, "Only lambdas can be called for now");
			yield(YieldResult::fail);
		}

		if (!yield_while_null(call->location, &head->return_type)) {
			reporter.error(head->location, "Could not wait for lambda's return type");
			yield(YieldResult::fail);
		}

		auto &parameters = head->parameters_block.definition_list;

		if (arguments.count != parameters.count) {
			reporter.error(call->location, "Too {} arguments. Expected {}, but got {}.", arguments.count > parameters.count ? "much"s : "few"s, parameters.count, arguments.count);
			reporter.info(head->location, "Lambda is here:");
			yield(YieldResult::fail);
		}

		for (umm i = 0; i < arguments.count; ++i) {
			auto &argument = arguments[i];
			auto &parameter = head->parameters_block.definition_list[i];
			if (!implicitly_cast(&argument, parameter->type, true)) {
				yield(YieldResult::fail);
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
				return inline_body(call, lambda);
			}
		}

		call->type = head->return_type;
		return call;
	}
	[[nodiscard]] If *typecheck_impl(If *If, bool can_substitute) {
		typecheck(&If->condition);

		typecheck(&If->true_branch);

		if (If->false_branch) {
			typecheck(&If->false_branch);

			if (auto true_branch_expression = as<Expression>(If->true_branch)) {
				if (auto false_branch_expression = as<Expression>(If->false_branch)) {
					if (types_match(true_branch_expression->type, false_branch_expression->type)) {
						If->type = true_branch_expression->type;
					} else {
						defer {
							If->true_branch = true_branch_expression;
						If->false_branch = false_branch_expression;
						};

						Reporter cast_reporter;
						cast_reporter.reports.allocator = temporary_allocator;
						auto t2f = implicitly_cast(&true_branch_expression, false_branch_expression->type, &cast_reporter, false);
						auto f2t = implicitly_cast(&false_branch_expression, true_branch_expression->type, &cast_reporter, false);

						if (!t2f && !f2t) {
							reporter.error(If->location, "Branch types {} and {} don't match in any way.", true_branch_expression->type, false_branch_expression->type);
							reporter.reports.add(cast_reporter.reports);
							cast_reporter.reports.clear();
							yield(YieldResult::fail);
						} else if (t2f && f2t) {
							reporter.error(If->location, "Branch types {} and {} are both implicitly convertible to each other.", true_branch_expression->type, false_branch_expression->type);
							reporter.reports.add(cast_reporter.reports);
							cast_reporter.reports.clear();
							yield(YieldResult::fail);
						} else if (t2f) {
							assert_always(implicitly_cast(&true_branch_expression, false_branch_expression->type, &cast_reporter, true));
							If->type = true_branch_expression->type;
						} else {
							assert_always(implicitly_cast(&false_branch_expression, true_branch_expression->type, &cast_reporter, true));
							If->type = false_branch_expression->type;
						}
					}
				}
			}
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
		typecheck(&binary->left);
		typecheck(&binary->right);

		if (binary->operation == BinaryOperation::ass) {
			auto result = is_mutable(binary->left);
			if (!result) {
				reporter.error(binary->left->location, "This expression can not be modified.");
				assert(result.failed_node);
				//reporter.info(result.failed_node->location, "Because this is not mutable.");
				why_is_this_immutable(binary->left);

				yield(YieldResult::fail);
			}
			if (!implicitly_cast(&binary->right, binary->left->type, true)) {
				yield(YieldResult::fail);
			}
			binary->type = get_builtin_type(BuiltinType::None);
			return binary;
		}

		auto dleft  = direct(binary->left->type);
		auto dright = direct(binary->right->type);

		if (fold_constants) {
			if (auto li = as<IntegerLiteral>(binary->left)) {
				if (auto ri = as<IntegerLiteral>(binary->right)) {
				
					if (dleft != dright) {
						reporter.error(binary->location, "No binary operation {} defined for types {} and {}.", binary->operation, binary->left->type, binary->right->type);
						yield(YieldResult::fail);
						return 0;
					}

					auto builtin_type = as<BuiltinTypeName>(dleft);
					assert(builtin_type);

					auto make_integer = [&] (u64 value) {
						auto result = IntegerLiteral::create();
						result->value = value;
						result->type = builtin_type;
						return result;
					};
#define OPS(T) \
					switch (binary->operation) { \
						case BinaryOperation::add: return make_integer((T)li->value + (T)ri->value); \
						case BinaryOperation::sub: return make_integer((T)li->value - (T)ri->value); \
						case BinaryOperation::mul: return make_integer((T)li->value * (T)ri->value); \
						case BinaryOperation::div: return make_integer((T)li->value / (T)ri->value); \
						case BinaryOperation::mod: return make_integer((T)li->value % (T)ri->value); \
						case BinaryOperation::bxo: return make_integer((T)li->value ^ (T)ri->value); \
						case BinaryOperation::ban: return make_integer((T)li->value & (T)ri->value); \
						case BinaryOperation::bor: return make_integer((T)li->value | (T)ri->value); \
						case BinaryOperation::bsl: return make_integer((T)li->value << (T)ri->value); \
						case BinaryOperation::bsr: return make_integer((T)li->value >> (T)ri->value); \
					}

					switch (builtin_type->type_kind) {
						case BuiltinType::U8:  OPS(u8); 
						case BuiltinType::U16: OPS(u16); 
						case BuiltinType::U32: OPS(u32); 
						case BuiltinType::U64: OPS(u64); 
						case BuiltinType::S8:  OPS(s8); 
						case BuiltinType::S16: OPS(s16); 
						case BuiltinType::S32: OPS(s32); 
						case BuiltinType::S64: OPS(s64); 
					}

					invalid_code_path();
#undef OPS
				}
			}
		}

		if (auto found = binary_typecheckers.find({ dleft, dright, binary->operation })) {
			return (this->*(found->value))(binary);
		}

		// if (types_match(binary->left->type, binary->right->type)) {
		// 	switch (binary->operation) {
		// 		case BinaryOperation::add:
		// 		case BinaryOperation::sub:
		// 		case BinaryOperation::mul:
		// 		case BinaryOperation::div:
		// 			binary->type = binary->left->type;
		// 			break;
		// 		case BinaryOperation::equ:
		// 		case BinaryOperation::neq:
		// 		case BinaryOperation::les:
		// 		case BinaryOperation::leq:
		// 		case BinaryOperation::grt:
		// 		case BinaryOperation::grq:
		// 			binary->type = get_builtin_type(BuiltinType::Bool);
		// 			break;
		// 
		// 		default:
		// 			invalid_code_path("Invalid binary operation {}.", binary->operation);
		// 	}
		// }

		reporter.error(binary->location, "No binary operation {} defined for types {} and {}.", binary->operation, binary->left->type, binary->right->type);
		yield(YieldResult::fail);
		return 0;
	}
	[[nodiscard]] Match *typecheck_impl(Match *match, bool can_substitute) {
		typecheck(&match->expression);

		make_concrete(match->expression);

		for (auto &Case : match->cases) {
			typecheck(&Case.from);

			if (!is_constant(Case.from)) {
				reporter.error(Case.from->location, "Match case expression must be constant.");
				yield(YieldResult::fail);
			}

			if (!implicitly_cast(&Case.from, match->expression->type, true))
				yield(YieldResult::fail);

			typecheck(&Case.to);
		}

		for (auto &Case : match->cases) {
			if (is_concrete(Case.to->type)) {
				match->type = Case.to->type;
			}
		}

		if (!match->type) {
			make_concrete(match->cases[0].to);
			match->type = match->cases[0].to->type;
		}

		for (auto &Case : match->cases) {
			if (!implicitly_cast(&Case.to, match->type, true)) {
				yield(YieldResult::fail);
			}
		}

		return match;
	}
	[[nodiscard]] Expression *typecheck_impl(Unary *unary, bool can_substitute) {
		typecheck(&unary->expression);

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
					yield(YieldResult::fail);
				}
				break;
			}
			case UnaryOperation::addr: {
				if (auto name = get_bottom_name(unary->expression)) {
					unary->type = make_pointer(unary->expression->type, name->definition->mutability);
				} else {
					reporter.error(unary->location, "You can only take address of names or blocks that end with a name.");
					yield(YieldResult::fail);
				}
				break;
			}
			case UnaryOperation::plus: {
				if (auto literal = as<IntegerLiteral>(unary->expression)) {
					return literal;
				}
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
					yield(YieldResult::fail);
				}
				break;
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
					yield(YieldResult::fail);
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
			typecheck(&return_->value);

		return return_;
	}
	[[nodiscard]] While *typecheck_impl(While *While, bool can_substitute) {
		typecheck(&While->condition);

		if (auto builtin_type = direct_as<BuiltinTypeName>(While->condition->type); !builtin_type || builtin_type->type_kind != BuiltinType::Bool) {
			reporter.error(While->condition->location, "Condition type must be Bool.");
			yield(YieldResult::fail);
		}

		typecheck(&While->body);

		return While;
	}
	[[nodiscard]] Continue *typecheck_impl(Continue *Continue, bool can_substitute) { return Continue; }
	[[nodiscard]] Break *typecheck_impl(Break *Break, bool can_substitute) {
		if (Break->value) {
			typecheck(&Break->value);
		}
		return Break;
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
	Expression *bt_both_unsized_ints(Binary *binary) {
		binary->left->type =
			binary->right->type = get_builtin_type(BuiltinType::S64);

		auto e = ExecutionContext::create(fiber);
		auto l = e->run(binary->left).value_or([&] {
			reporter.error(binary->left->location, "Failed to evaluate value.");
			yield(YieldResult::fail);
			return Value{};
		});
		auto r = e->run(binary->right).value_or([&] {
			reporter.error(binary->right->location, "Failed to evaluate value.");
			yield(YieldResult::fail);
			return Value{};
		});

		auto result = IntegerLiteral::create();
		switch (binary->operation) {
			case BinaryOperation::add: result->value = l.s64 + r.s64; break;
			case BinaryOperation::sub: result->value = l.s64 - r.s64; break;
			case BinaryOperation::mul: result->value = l.s64 * r.s64; break;
			case BinaryOperation::div: result->value = l.s64 / r.s64; break;
			case BinaryOperation::mod: result->value = l.s64 % r.s64; break;
			case BinaryOperation::bxo: result->value = l.s64 ^ r.s64; break;
			case BinaryOperation::ban: result->value = l.s64 & r.s64; break;
			case BinaryOperation::bor: result->value = l.s64 | r.s64; break;
			case BinaryOperation::bsl: result->value = l.s64 << r.s64; break;
			case BinaryOperation::bsr: result->value = l.s64 >> r.s64; break;
			default: invalid_code_path("Attempt to evaluate binary {} on unsized integers. This is not supported/implemented", binary->operation);
		}
		result->type = get_builtin_type(BuiltinType::UnsizedInteger);
		result->location = binary->location;
		return result;
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
		}

#define ORDERABLE(type) \
	x(type, type, les) = &bt_set_bool; \
	x(type, type, leq) = &bt_set_bool; \
	x(type, type, grt) = &bt_set_bool; \
	x(type, type, grq) = &bt_set_bool

#define MATHABLE(type) \
	x(type, type, add) = &bt_take_left; \
	x(type, type, sub) = &bt_take_left; \
	x(type, type, mul) = &bt_take_left; \
	x(type, type, div) = &bt_take_left; \
	x(type, type, mod) = &bt_take_left;

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

		MATHABLE(Bool);
		MATHABLE(U8);
		MATHABLE(U16);
		MATHABLE(U32);
		MATHABLE(U64);
		MATHABLE(S8);
		MATHABLE(S16);
		MATHABLE(S32);
		MATHABLE(S64);

		UNSIZED_INT_AND_SIZED_INT(U8);
		UNSIZED_INT_AND_SIZED_INT(U16);
		UNSIZED_INT_AND_SIZED_INT(U32);
		UNSIZED_INT_AND_SIZED_INT(U64);
		UNSIZED_INT_AND_SIZED_INT(S8);
		UNSIZED_INT_AND_SIZED_INT(S16);
		UNSIZED_INT_AND_SIZED_INT(S32);
		UNSIZED_INT_AND_SIZED_INT(S64);

		x(UnsizedInteger, UnsizedInteger, add) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, sub) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, mul) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, div) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, mod) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, bxo) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, ban) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, bor) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, bsl) = &bt_both_unsized_ints;
		x(UnsizedInteger, UnsizedInteger, bsr) = &bt_both_unsized_ints;

#undef UNSIZED_INT_AND_SIZED_INT
#undef SYMMETRIC
#undef MATHABLE
#undef ORDERABLE
#undef x
#undef y
	}

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

	//GlobalAllocator::init();
}

struct ParsedArguments {
	String source_name;
	u32 thread_count = 0;
	bool print_ast = false;
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
		} else if (args[i] == "-print-ast") {
			result.print_ast = true;
		} else if (args[i] == "-print-uids") {
			print_uids = true;
		} else if (args[i] == "-no-fold-constants") {
			fold_constants = false;
		} else if (args[i] == "-no-constant-name-inlining") {
			constant_name_inlining = false;
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
#define x(name, value) \
	{ \
		auto type = get_builtin_type(BuiltinType::name); \
		type->type_kind = BuiltinType::name; \
		type->type = get_builtin_type(BuiltinType::Type); \
	}
	ENUMERATE_BUILTIN_TYPES(x)
#undef x
}

s32 tl_main(Span<Span<utf8>> args) {
	auto main_fiber = fiber_init(0);

	set_console_encoding(Encoding::utf8);

	defer {
		for (auto time : timed_results) {
			println("{} took {} ms", time.name, time.seconds * 1000);
		}

#if ENABLE_STRING_HASH_COUNT
		println("Total string hashes: {}", string_hash_count);
#endif
	};

	init_globals();
	init_builtin_types();
	Typechecker::init_binary_typecheckers();
	
	timed_function();

	auto maybe_arguments = parse_arguments(args);
	if (!maybe_arguments) {
		immediate_reporter.error("Failed to parse arguments.");
		return 1;
	}
	auto arguments = maybe_arguments.value();

	auto source_contents_buffer = read_entire_file(arguments.source_name, {.extra_space_before = 1, .extra_space_after = 1});
	if (!source_contents_buffer.data) {
		immediate_reporter.error("Could not read input file '{}'", arguments.source_name);
		return 1;
	}
	defer { free(source_contents_buffer); };

	auto source_contents = (String)source_contents_buffer.subspan(1, source_contents_buffer.count - 2);

	content_start_to_file_name.get_or_insert(source_contents.data) = arguments.source_name;

	auto tokens = source_to_tokens(source_contents, arguments.source_name);
	if (!tokens) {
		immediate_reporter.error("Failed to tokenize source code.");
		return 1;
	}
	
	auto global_nodes = tokens_to_nodes(main_fiber, tokens.value());
	if (!global_nodes) {
		immediate_reporter.error("Failed to build an ast.");
		return 1;
	}
	
	{
		timed_block("typecheck");
		for (auto node : global_nodes.value())
			global_block.add(node);

		auto cpu_info = get_cpu_info();

		u32 thread_count;
		if (arguments.thread_count == 0) {
			thread_count = cpu_info.logical_processor_count;
		} else {
			thread_count = min(arguments.thread_count, cpu_info.logical_processor_count);
		}

		static SpinLock thread_id_to_fiber_lock; // needed only for safe insertion
		static HashMap<u32, Fiber> thread_id_to_fiber;
	
		thread_id_to_fiber.insert(get_current_thread_id(), main_fiber);

		static auto init_worker_fiber = [] {
			auto worker_fiber = fiber_init(0);
			auto thread_id = get_current_thread_id();

			scoped(thread_id_to_fiber_lock);
			thread_id_to_fiber.insert(thread_id, worker_fiber);
		};

		ThreadPool thread_pool;
		init_thread_pool(&thread_pool, thread_count - 1, { .worker_initter = init_worker_fiber });
		defer { deinit_thread_pool(&thread_pool); };

		static bool failed = false;

		typecheck_entries = map(global_nodes.value(), [&](auto &node) { return TypecheckEntry{.node = &node}; });


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

			thread_pool.wait_for_completion();

			auto current_progress = get_typechecking_progress();

			if (current_progress <= initial_progress) {

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

				thread_pool.wait_for_completion();

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
				u32 parent = -1;
				List<u32> pointees;
			};

			List<Vertex> vertices;
			vertices.resize(typecheck_entries.count);

			auto add_edge = [&] (u32 from, u32 to) {
				vertices[from].pointees.add(to);
			};

			for (auto &dependency : typecheck_entries) {
				auto dependency_index = index_of(typecheck_entries, &dependency);
				assert(dependency_index < typecheck_entries.count);

				for (auto &dependant : dependency.dependants) {
					auto dependant_index = index_of(typecheck_entries, dependant);
					assert(dependant_index < typecheck_entries.count);

					add_edge(dependant_index, dependency_index);
				}
			}

			List<u32> cycle;
			List<List<u32>> cycles;

			auto dfs = [&](this auto &&self, u32 u) -> void {
				vertices[u].state = VertexState::visited;
				for (auto v : vertices[u].pointees) {
					switch (vertices[v].state) {
						case VertexState::none: {
							vertices[v].parent = u;
							self(v);
							break;
						}
						case VertexState::visited: {
							// cycle found, backtrack to find vertices in cycle
							auto p = u;
							cycle.add(v);
							while (p != v) {
								cycle.add(p);
								p = vertices[p].parent;
							}
							reverse(cycle); // reverse to get correct order
							cycles.add(cycle);
							cycle.clear();
							break;
						}
					}
				}
				vertices[u].state = VertexState::finished;
			};

			for (umm i = 0; i < typecheck_entries.count; i++) {
				if (vertices[i].state == VertexState::none) {
					dfs(i);
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
					auto &entry = typecheck_entries[cycle[j]];

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
					auto &entry = typecheck_entries[cycle[j]];
					auto &next_entry = typecheck_entries[cycle[(j + 1) % cycle.count]];

					immediate_reporter.info((*entry.node)->location, "{} depends on {}.", (*entry.node)->location, (*next_entry.node)->location);
				}
			}
		}

		for (auto &report : deferred_reports.use_unprotected()) {
			report.print();
		}

		if (failed) {
			immediate_reporter.error("Typechecking failed.");
			return 1;
		}
	}

	if (arguments.print_ast) {
		print_ast(&global_block);
	}

	for (auto node : global_nodes.value()) {
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
					!::is_integer(lambda->head.return_type)) 
				{
					immediate_reporter.error(definition->location, "main must return integer or None, not {}.", lambda->head.return_type);
					return 1;
				}

				timed_block("executing main");

				auto call = Call::create();
				call->callable = lambda;

				auto context = ExecutionContext::create(main_fiber);
				if (auto value = context->run(call)) {
					println("main returned {}", value.value());
				} else {
					println("main failed to execute");
				}
			}
		}
	}

	with(ConsoleColor::green, println("Build success"));

	return 0;
}