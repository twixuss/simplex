// TODO: track current node for assertion messages.

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

#if BUILD_DEBUG
u32 string_hash_count;
#endif

template <>
constexpr u64 get_hash(String const &string) {
#if BUILD_DEBUG
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

struct GlobalAllocator : AllocatorBase<GlobalAllocator> {

	static void init() {
		base = (u8 *)VirtualAlloc(0, buffer_size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
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

	inline static constexpr umm buffer_size = 2 * GiB;
	inline static u8 *base = 0;
	inline static u8 *cursor = 0;
	inline static SpinLock lock;
};

template <class T>
using GList = tl::List<T, GlobalAllocator>;

template <class Key, class Value, class Traits = DefaultHashTraits<Key>>
using GHashMap = tl::ContiguousHashMap<Key, Value, Traits, GlobalAllocator>;

#define ENUMERATE_CHARS_ALPHA(x) \
	x('a') x('A') x('n') x('N') \
	x('b') x('B') x('o') x('O') \
	x('c') x('C') x('p') x('P') \
	x('d') x('D') x('q') x('Q') \
	x('e') x('E') x('r') x('R') \
	x('f') x('F') x('s') x('S') \
	x('g') x('G') x('t') x('T') \
	x('h') x('H') x('u') x('U') \
	x('i') x('I') x('v') x('V') \
	x('j') x('J') x('w') x('W') \
	x('k') x('K') x('x') x('X') \
	x('l') x('L') x('y') x('Y') \
	x('m') x('M') x('z') x('Z') \

#define ENUMERATE_CHARS_DIGIT(x) \
	x('0') x('1') x('2') x('3') x('4') \
	x('5') x('6') x('7') x('8') x('9') \

#define ENUMERATE_DOUBLE_CHAR_TOKENS(x) \
	x("==") \
	x("!=") \
	x(">=") \
	x("<=") \
	x("+=") \
	x("-=") \
	x("*=") \
	x("/=") \
	x("%=") \
	x("&=") \
	x("|=") \
	x("^=") \
	x("&&") \
	x("||") \
	x("<<") \
	x(">>") \
	x("..") \
	x("=>") \

#define ENUMERATE_TRIPLE_CHAR_TOKENS(x) \
	x("<<=") \
	x(">>=") \


#define ENUMERATE_CONCRETE_BUILTIN_TYPES(x) \
	x(Type) \
	x(U8) \
	x(U16) \
	x(U32) \
	x(U64) \
	x(S8) \
	x(S16) \
	x(S32) \
	x(S64) \
	x(Bool) \
	x(None) \

#define ENUMERATE_ABSTRACT_BUILTIN_TYPES(x) \
	x(UnsizedInteger) \

#define ENUMERATE_BUILTIN_TYPES(x) \
	ENUMERATE_CONCRETE_BUILTIN_TYPES(x) \
	ENUMERATE_ABSTRACT_BUILTIN_TYPES(x) \

#define ENUMERATE_KEYWORDS(x) \
	ENUMERATE_CONCRETE_BUILTIN_TYPES(x) \
	x(const) \
	x(let) \
	x(var) \
	x(return) \
	x(if) \
	x(then) \
	x(else) \
	x(false) \
	x(true) \
	x(while) \
	x(break) \
	x(continue) \
	x(match) \

// #define x(name, token, precedence)
#define ENUMERATE_BINARY_OPERATIONS(x) \
	x(mul, "*" , 7) \
	x(div, "/" , 7) \
	x(mod, "%" , 7) \
	\
	x(add, "+" , 6) \
	x(sub, "-" , 6) \
	\
	x(bor, "|" , 5) \
	x(ban, "&" , 5) \
	x(bxo, "^" , 5) \
	x(bsl, "<<", 5) \
	x(bsr, ">>", 5) \
	\
	x(equ, "==", 4) \
	x(neq, "!=", 4) \
	x(les, "<" , 4) \
	x(leq, "<=", 4) \
	x(grt, ">" , 4) \
	x(grq, ">=", 4) \
	\
	x(lan, "||", 3) \
	x(lor, "&&", 3) \
	\
	x(ran, "..", 2) \
	\
	x(ass, "=" , 1) \
	\
	x(addass, "+=" , 1) \
	x(subass, "-=" , 1) \
	x(mulass, "*=" , 1) \
	x(divass, "/=" , 1) \
	x(modass, "%=" , 1) \
	x(borass, "|=" , 1) \
	x(banass, "&=" , 1) \
	x(bxoass, "^=" , 1) \
	x(bslass, "<<=", 1) \
	x(bsrass, ">>=", 1) \

#define ENUMERATE_TOKEN_KIND(x, y) \
	y(eof, '\0') \
	y(eol, '\n') \
	y(name, 'a') \
	y(number, '0') \
	y(directive, '#') \
	y(unused__, 0x7fff) \
	ENUMERATE_KEYWORDS(x) \

#define ENUMERATE_EXPRESSION_KIND(x) \
	x(Block) \
	x(Call) \
	x(Definition) \
	x(IntegerLiteral) \
	x(BooleanLiteral) \
	x(Lambda) \
	x(LambdaHead) \
	x(Name) \
	x(If) \
	x(BuiltinType) \
	x(Binary) \
	x(Match) \

#define ENUMERATE_STATEMENT_KIND(x) \
	x(Return) \
	x(While) \
	x(Continue) \
	x(Break) \

#define ENUMERATE_NODE_KIND(x) \
	ENUMERATE_EXPRESSION_KIND(x) \
	ENUMERATE_STATEMENT_KIND(x) \

consteval u16 const_string_to_token_kind(Span<char> token) {
	if (token.count <= 2) {
		u16 result = 0;
		for (auto c : token) {
			result <<= 8;
			result |= c;
		}
		return result;
	}

	u16 iota = 0x9000;
#define x(string) if (token == string##s) return iota; ++iota;
	ENUMERATE_TRIPLE_CHAR_TOKENS(x)
#undef x

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

enum TokenKind : u16 {
#define x(name) Token_##name,
#define y(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KIND(x, y)
#undef y
#undef x
};

inline umm append(StringBuilder &builder, TokenKind kind) {
	switch (kind) {
		case Token_eof: return append(builder, "end of file");
		case Token_eol: return append(builder, "end of line");
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

// This hash map panics on hash collision. Here it is used for
// mapping token strings to token kinds. Because all of them
// are inserted at compile time, panic is not a problem.
template <umm capacity_, class Key_, class Value_, class Traits = DefaultHashTraits<Key_, true>>
struct FixedHashMap {
	using Key = Key_;
	using Value = Value_;
	using KeyValue = KeyValue<Key, Value>;
	inline static constexpr umm capacity = capacity_;
	
	KeyValue kv[capacity] {};
	bool init[capacity] {};
	
	[[no_unique_address]] Traits traits = {};

	constexpr bool insert(Key const &key, Value value) {
		auto index = traits.get_index(key, capacity);
		assert(!init[index]); // NOTE: if theres a compilation error, that means this assertion failed.
		init[index] = true;
		kv[index].key = key;
		kv[index].value = value;
		return true;
	}
	constexpr const KeyValue *find(Key const &key) const {
		auto index = traits.get_index(key, capacity);
		if (init[index] && kv[index].key == key) {
			return &kv[index];
		}
		return 0;
	}
};

// NOTE: Currently these maps are way bigger than needed. They could use a
//       custom hash function to reduce their size. Need to figure that out.

constexpr auto keywords = []() consteval {
	FixedHashMap<128, String, TokenKind> keywords = {};
#define x(name, value) keywords.insert(u8#name##s, Token_##name);
	ENUMERATE_KEYWORDS(x)
#undef x
	return keywords;
}();

struct SourceLocation {
	String file;
	u32 line_number = 0;
	u32 column_number = 0;
	List<String> lines;
};

HashMap<utf8 *, String> content_start_to_file_name;

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

	static Report create(ReportKind kind, String location, char const *format, auto const &...args) {
		return {
			.kind = kind,
			.location = location,
			.message = (String)tl::format(format, args...),
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
	void info   (this auto &&self, String location, char const *format, auto const &...args) { self.on_report(Report::create(ReportKind::info,    location, format, args...)); }
	void warning(this auto &&self, String location, char const *format, auto const &...args) { self.on_report(Report::create(ReportKind::warning, location, format, args...)); }
	void error  (this auto &&self, String location, char const *format, auto const &...args) {
		if (break_on_error) {
			debug_break();
		}
		self.on_report(Report::create(ReportKind::error,   location, format, args...));
	}
	void help   (this auto &&self, String location, char const *format, auto const &...args) { self.on_report(Report::create(ReportKind::help,    location, format, args...)); }
	void info   (this auto &&self, char const *format, auto const &...args) { return self.info   (String{}, format, args...); }
	void warning(this auto &&self, char const *format, auto const &...args) { return self.warning(String{}, format, args...); }
	void error  (this auto &&self, char const *format, auto const &...args) { return self.error  (String{}, format, args...); }
	void help   (this auto &&self, char const *format, auto const &...args) { return self.help   (String{}, format, args...); }
};

SpinLock stdout_mutex;

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

	Tokens tokens;
	tokens.reserve(source.count / 4); // NOTE: predict 4 characters per token on average.

	utf8 *cursor = source.data;

	auto print_invalid_character_error = [&] {
		immediate_reporter.error({cursor, 1}, "Invalid uft8 character.");
	};

	while (true) {

		while (true) {
			if (*cursor == ' ' || *cursor == '\t' || *cursor == '\r')
				++cursor;
			else
				break;
		}

		if (*cursor == '\0')
			break;

		Token token;
		token.string.data = cursor;

		// a &
		// b =
		// &
		// &=
#define CASE_SINGLE_OR_DOUBLE(a, b)                                   \
	case a: {                                                         \
		++cursor;                                                     \
		if (*cursor == b) {                                           \
			++cursor;                                                 \
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
		++cursor;                                                         \
		switch (*cursor) {                                                \
			case a:                                                       \
				++cursor;                                                 \
				token.kind = (TokenKind)const_string_to_token_kind(a, a); \
				token.string.count = 2;                                   \
				break;                                                    \
			case b:                                                       \
				++cursor;                                                 \
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
		++cursor; 																 \
		switch (*cursor) {														 \
			case b:																 \
				token.kind = (TokenKind)const_string_to_token_kind(a, b); 		 \
				++cursor;														 \
				token.string.count = 2; 										 \
				break; 															 \
			case a: {															 \
				++cursor;														 \
				if (*cursor == b) {												 \
					++cursor;													 \
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
				++cursor;
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
				++cursor;
				if (*cursor == '/') {
					while (*cursor != '\n') {
						++cursor;
					}
				} else if (*cursor == '*') {
					while (true) {
						if (String(cursor, 2) == "*/") {
							cursor += 2;
							break;
						}
						++cursor;
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

				++cursor;
				while (true) {
					switch (*cursor) {
						ENUMERATE_CHARS_ALPHA(PASTE_CASE)
						ENUMERATE_CHARS_DIGIT(PASTE_CASE)
						case '_': {
							++cursor;
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
							++cursor;
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
					
					++cursor;
				}
			name_loop_end:;

				token.string.set_end(cursor);
				if (token.string.count == 0) {
					immediate_reporter.error({token.string.data, 1}, "Invalid character ({}).", (u32)*cursor);
				}

				if (auto keyword = keywords.find(token.string))
					token.kind = keyword->value;

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

umm append(StringBuilder &builder, BinaryOperation operation) {
	switch (operation) {
#define x(name, token, precedence) case BinaryOperation::name: return append(builder, token);
		ENUMERATE_BINARY_OPERATIONS(x)
#undef x
	}
	return append(builder, "(unknown binary)");
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

inline bool is_right_associative(BinaryOperation operation) {
	return false;
}

enum class NodeKind : u8 {
	Unknown,
#define x(name) name,
	ENUMERATE_NODE_KIND(x)
#undef x
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
		return GlobalAllocator{}.allocate<T>();
	}
	void free() {
		((T *)this)->free_impl();
	}
};

enum class Mutability : u8 {
	constant, // known at compile time. can be casted to readonly
	readonly, // can not be modified by anyone.
	variable, // can     be modified by anyone.
};

inline umm append(StringBuilder &builder, Mutability mutability) {
	switch (mutability) {
		case Mutability::constant: return append(builder, "const");
		case Mutability::readonly: return append(builder, "let");
		case Mutability::variable: return append(builder, "var");
	}
	return append_format(builder, "(unknown Mutability {})", (u32)mutability);
}
 

#define DEFINE_EXPRESSION(name) struct name : Expression, NodeBase<name>
#define DEFINE_STATEMENT(name) struct name : Statement, NodeBase<name>

enum class BuiltinTypeKind : u8 {
#define x(name) name,
	ENUMERATE_BUILTIN_TYPES(x)
#undef x
	count,
};

umm append(StringBuilder &builder, BuiltinTypeKind type_kind) {
	switch (type_kind) {
#define x(name, value) case BuiltinTypeKind::name: return append(builder, #name);
		ENUMERATE_BUILTIN_TYPES(x)
#undef x
	}
	return append_format(builder, "(unknown BuiltinTypeKind {})", (u32)type_kind);
}

inline BuiltinTypeKind token_kind_to_builtin_type_kind(TokenKind kind) {
	switch (kind) {
#define x(name) case Token_##name: return BuiltinTypeKind::name;
		ENUMERATE_CONCRETE_BUILTIN_TYPES(x);
#undef x
	}

	invalid_code_path();
	return {};
}

DEFINE_EXPRESSION(Block) {
	Block *parent = 0;
	Expression *container = 0;

	GList<Node *> children;
	GList<Definition *> definition_list;
	GHashMap<String, GList<Definition *>> definition_map;

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
};
DEFINE_EXPRESSION(Definition) {
	String name;
	Expression *parsed_type = 0;
	Expression *initial_value = 0;
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
DEFINE_EXPRESSION(BuiltinType) {
	BuiltinTypeKind type_kind = {};
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
DEFINE_STATEMENT(Return) {
	Expression *value = 0;
};
DEFINE_STATEMENT(While) {
	Expression *condition = 0;
	Node *body = 0;
};
DEFINE_STATEMENT(Continue) {};
DEFINE_STATEMENT(Break) {};

void Block::add(Node *child) {
	children.add(child);
	if (auto definition = as<Definition>(child)) {
		definition_list.add(definition);
		definition_map.get_or_insert(definition->name).add(definition);
	}
}

BuiltinType *builtin_types[(u32)BuiltinTypeKind::count];

// NOTE: Do not use this for types in the source code. These do not have a location.
BuiltinType *&get_builtin_type(BuiltinTypeKind kind) {
	return builtin_types[(u32)kind];
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

Expression *direct(Expression *node) {
	while (true) {
		if (auto name = as<Name>(node)) {
			if (name->definition->mutability == Mutability::constant) {
				node = name->definition->initial_value;
				continue;
			}
		}
		break;
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

bool types_match(Expression *a, Expression *b) {
	a = direct(a);
	b = direct(b);

	if (a->kind != b->kind)
		return false;

	switch (a->kind) {
		case NodeKind::BuiltinType: {
			REDECLARE_VAL(a, (BuiltinType *)a);
			REDECLARE_VAL(b, (BuiltinType *)b);

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
	}
	invalid_code_path("invalid node kind {} in types_match", a->kind);
}

bool types_match(Expression *a, BuiltinTypeKind b) {
	a = direct(a);

	if (auto ab = as<BuiltinType>(a)) {
		return ab->type_kind == b;
	}

	return false;
}
bool types_match(BuiltinTypeKind a, Expression *b) {
	return types_match(b, a);
}

bool is_integer(Expression *type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinType>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinTypeKind::U8:
			case BuiltinTypeKind::U16:
			case BuiltinTypeKind::U32:
			case BuiltinTypeKind::U64:
			case BuiltinTypeKind::S8:
			case BuiltinTypeKind::S16:
			case BuiltinTypeKind::S32:
			case BuiltinTypeKind::S64:
				return true;
		}
	}

	return false;
}

bool is_concrete(Expression *type) {
	type = direct(type);
	if (auto builtin_type = as<BuiltinType>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinTypeKind::UnsizedInteger:
				return false;
		}
	}

	return true;
}

void make_concrete(Expression *expression) {
	auto type = direct(expression->type);

	if (auto builtin_type = as<BuiltinType>(type)) {
		switch (builtin_type->type_kind) {
			case BuiltinTypeKind::UnsizedInteger: {
				expression->type = get_builtin_type(BuiltinTypeKind::S64);
				return;
			}
		}
	}
}

u64 get_size(BuiltinTypeKind type_kind) {
	switch (type_kind) {
		case BuiltinTypeKind::U8:  return 1;
		case BuiltinTypeKind::U16: return 2;
		case BuiltinTypeKind::U32: return 4;
		case BuiltinTypeKind::U64: return 8;
		case BuiltinTypeKind::S8:  return 1;
		case BuiltinTypeKind::S16: return 2;
		case BuiltinTypeKind::S32: return 4;
		case BuiltinTypeKind::S64: return 8;
		default: invalid_code_path("Invalid BuiltinTypeKind {}", type_kind);
	}
}

enum class Sign : u8 {
	Unsigned,
	Signed
};
 
Sign get_sign(BuiltinTypeKind type_kind) {
	switch (type_kind) {
		case BuiltinTypeKind::U8: 
		case BuiltinTypeKind::U16:
		case BuiltinTypeKind::U32:
		case BuiltinTypeKind::U64: 
			return Sign::Unsigned;
		case BuiltinTypeKind::S8:  
		case BuiltinTypeKind::S16: 
		case BuiltinTypeKind::S32: 
		case BuiltinTypeKind::S64: 
			return Sign::Signed;
		default: invalid_code_path("Invalid BuiltinTypeKind {}", type_kind);
	}
}

void print_ast(Node *node);
void print_ast_impl(Block *block) {
	println("{");
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

	for (auto &argument : call->arguments) {
		if (&argument != call->arguments.data) {
			print(", ");
		}
		print_ast(argument);
	}

	print(')');
}
void print_ast_impl(Definition *definition) {
	switch (definition->mutability) {
		case Mutability::constant: print("const"); break;
		case Mutability::readonly: print("let"); break;
		case Mutability::variable: print("var"); break;
		default: invalid_code_path();
	}
	print(" {}: ", definition->name);
	print_ast(definition->type);
	print(" = ");
	print_ast(definition->initial_value);
}
void print_ast_impl(IntegerLiteral *literal) {
	print('{');
	print(literal->value);
	print(" as ");
	print_ast(literal->type);
	print('}');
}
void print_ast_impl(BooleanLiteral *literal) {
	print(literal->value);
}
void print_ast_impl(LambdaHead *head) {
	print("(");

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

	print("): ");
	print_ast(head->return_type);
}
void print_ast_impl(Lambda *lambda) {
	print_ast(&lambda->head);
	print(" => ");
	if (lambda->is_intrinsic) 
		print("#intrinsic");
	if (lambda->body) {
		print_ast(lambda->body);
	}
}
void print_ast_impl(Name *name) { print(name->name); }
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
void print_ast_impl(BuiltinType *type) {
	switch (type->type_kind) {
#define x(name, value) case BuiltinTypeKind::name: print(#name); return;
		ENUMERATE_BUILTIN_TYPES(x)
#undef x
	}
	invalid_code_path();
}
void print_ast_impl(Continue *) { print("continue"); }
void print_ast_impl(Break *) { print("break"); }
void print_ast_impl(Binary *binary) {
	print('{');
	print_ast(binary->left);
	print(' ');
	print(binary->operation);
	print(' ');
	print_ast(binary->right);
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
void print_ast(Node *node) {
	switch (node->kind) {
#define x(name) case NodeKind::name: return print_ast_impl((##name *)node);
		ENUMERATE_NODE_KIND(x)
#undef x
	}
}

umm append(StringBuilder &builder, Node *node) {
	switch (node->kind) {
		case NodeKind::Name: {
			return append(builder, ((Name *)node)->name);
		}
		case NodeKind::BuiltinType: {
			switch (((BuiltinType *)node)->type_kind) {
#define x(name) case BuiltinTypeKind::name: return append(builder, #name);
				ENUMERATE_BUILTIN_TYPES(x)
#undef x
			}
			return append(builder, "(unknown BuiltinType)");
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
		default: {
			return append(builder, "(unknown)");
		}
	}
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
	// Parses single-part experssions
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

#define x(name) case Token_##name:
			ENUMERATE_CONCRETE_BUILTIN_TYPES(x)
#undef x
			{
				auto type = BuiltinType::create();
				type->location = token->string;
				type->type_kind = token_kind_to_builtin_type_kind(token->kind);
				next();
				return type;
			}

			default: 
				reporter.error(token->string, "Unexpected token '{}' when parsing expression.", *token);
				yield(false);
				return 0;
		}
	}
	Node *parse_statement() {
		switch (token->kind) {
			case Token_return: {
				auto return_ = Return::create();
				return_->location = token->string;

				if (auto lambda = as<Lambda>(current_container)) {
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
				if (!current_loop) {
					reporter.error(token->string, "`break` must be inside a loop.");
					yield(false);
				}

				auto Break = Break::create();
				Break->location = token->string;
				next();
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
		defer { reporter.print_all(); };

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
			reporter.error(token->string, "Expected '{}', but got '{}'", (TokenKind)expected_kind, *token);
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

	auto fiber = fiber_create(Parser::fiber_main, &parser);
	fiber_yield(fiber);

	if (parser.success)
		return parser.result_nodes;

	return {};
}

//#define x(name)
#define ENUMERATE_EXECUTION_VALUE_KIND \
	x(none) \
	x(integer) \
	x(boolean) \
	x(lambda) \
	x(type) \
	x(break_) \
	x(continue_) \
	x(return_) \

struct ExecutionContext {
	enum class ValueKind : u8 {
#define x(name) name,
		ENUMERATE_EXECUTION_VALUE_KIND
#undef x
	};

	friend umm append(StringBuilder &builder, ValueKind kind) {
		switch (kind) {
#define x(name) case ValueKind::name: return append(builder, #name);
			ENUMERATE_EXECUTION_VALUE_KIND
#undef x
		}
		return append_format(builder, "(unknown ExecutionContext::ValueKind {})", (u32)kind);
	}

	struct Value {
		ValueKind kind = {};
		u64 integer = 0;
		bool boolean = false;
		Lambda *lambda = 0;
		Expression *type = 0;
	};

	struct Scope {
		// Make this pointer-stable just in case.
		BucketHashMap<Definition *, Value> variables;
	};

	static constexpr umm recursion_limit = 256;
	umm recursion_level = 0;

	List<Scope> scope_stack;
	Value return_value;

	static ExecutionContext create() {
		ExecutionContext result;
		result.scope_stack.add(); // global scope
		return result;
	}

	Value execute(Node *node) {
		scoped_replace(debug_current_location, node->location);

		switch (node->kind) {
#define x(name) case NodeKind::name: return execute_impl((##name *)node);
			ENUMERATE_NODE_KIND(x)
#undef x
		}
		invalid_code_path();
	}
	Value execute_impl(IntegerLiteral *literal) {
		return { .kind = ValueKind::integer, .integer = literal->value };
	}
	Value execute_impl(BooleanLiteral *literal) {
		return { .kind = ValueKind::boolean, .boolean = literal->value };
	}
	Value execute_impl(Definition *definition) {
		assert(definition->initial_value, "Default-initialized variables are not implemented in ExecutionContext");
		auto value = execute(definition->initial_value);
		scope_stack.back().variables.get_or_insert(definition) = value;
		return value;
	}
	Value execute_impl(Return *return_) {
		if (return_->value)
			return_value = execute(return_->value);
		return { .kind = ValueKind::return_ };
	}
	Value execute_impl(Block *block) {
		Value result = {};
		for (auto child : block->children) {
			result = execute(child);
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

		auto lambda = direct_as<Lambda>(call->callable);
		if (!lambda) {
			immediate_reporter.error(call->location, "You can only call lambdas right now.");
			return {};
		}
		auto &parameters = lambda->head.parameters_block.definition_list;

		if (lambda->is_intrinsic) {
			assert(lambda->definition);

			List<Value> argument_values;
			argument_values.allocator = temporary_allocator;
			argument_values.reserve(arguments.count);

			for (auto argument : arguments)
				argument_values.add(execute(argument));

			auto name = lambda->definition->name;

			if (name == u8"println"s) {
				println(argument_values[0].integer);
				return Value{};
			}
			
			immediate_reporter.error(lambda->location, "Attempt to execute invalid intrinsic.");
			invalid_code_path();
		}

		List<Value> executed_arguments;
		executed_arguments.allocator = temporary_allocator;
		executed_arguments.reserve(arguments.count);
		for (auto argument : arguments) {
			executed_arguments.add(execute(argument));
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
		if (execute(If->condition).boolean) {
			return execute(If->true_branch);
		} else if (If->false_branch) {
			return execute(If->false_branch);
		}
		return {};
	}
	Value execute_impl(While *While) {
		while (execute(While->condition).boolean) {
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
	Value execute_impl(Break *Break) { return { .kind = ValueKind::break_ }; }
	Value execute_impl(BuiltinType *type) {
		return {.kind = ValueKind::type, .type = type};
	}
	Value execute_impl(Binary *binary) {
		if (binary->operation == BinaryOperation::ass) {
			if (auto name = as<Name>(binary->left)) {
				auto stored_value = get_stored_value(name);
				if (!stored_value) {
					immediate_reporter.error(name->location, "Could not find stored value for this name.");
					return {};
				}
				*stored_value = execute(binary->right);
			} else {
				immediate_reporter.error(binary->location, "TODO: Left expression must be a name for now.");
			}
			return {};
		}

		auto left = execute(binary->left);
		auto right = execute(binary->right);

		assert(left.kind == ValueKind::integer);
		assert(right.kind == ValueKind::integer);
		switch (binary->operation) {
			case BinaryOperation::add: return { .kind = ValueKind::integer, .integer = left.integer +  right.integer };
			case BinaryOperation::sub: return { .kind = ValueKind::integer, .integer = left.integer -  right.integer };
			case BinaryOperation::mul: return { .kind = ValueKind::integer, .integer = left.integer +  right.integer };
			case BinaryOperation::div: return { .kind = ValueKind::integer, .integer = left.integer /  right.integer };
			case BinaryOperation::equ: return { .kind = ValueKind::boolean, .boolean = left.integer == right.integer };
			case BinaryOperation::neq: return { .kind = ValueKind::boolean, .boolean = left.integer != right.integer };
			case BinaryOperation::les: return { .kind = ValueKind::boolean, .boolean = left.integer <  right.integer };
			case BinaryOperation::leq: return { .kind = ValueKind::boolean, .boolean = left.integer <= right.integer };
			case BinaryOperation::grt: return { .kind = ValueKind::boolean, .boolean = left.integer >  right.integer };
			case BinaryOperation::grq: return { .kind = ValueKind::boolean, .boolean = left.integer >= right.integer };
		}

		immediate_reporter.error(binary->location, "Invalid binary operation");
		invalid_code_path();
	}
	Value execute_impl(Match *match) {
		auto value = execute(match->expression);
		assert(value.kind == ValueKind::integer, "Only this is implemented");

		for (auto Case : match->cases) {
			auto from = execute(Case.from);
			assert(from.kind == ValueKind::integer, "Only this is implemented");
			if (value.integer == from.integer) {
				return execute(Case.to);
			}
		}

		invalid_code_path(match->location, "match did not match value {}", value);
	}

	Value *get_stored_value(Name *name) {
		assert(name->definition);
		for (auto &scope : reverse_iterate(scope_stack)) {
			if (auto found = scope.variables.find(name->definition)) {
				return &found->value;
			}
		}
		invalid_code_path();
	}
};

umm append(StringBuilder &builder, ExecutionContext::Value value) {
	switch (value.kind) {
		case ExecutionContext::ValueKind::none: return 0;
		case ExecutionContext::ValueKind::integer: return append(builder, value.integer);
		case ExecutionContext::ValueKind::boolean: return append(builder, value.boolean);
		case ExecutionContext::ValueKind::type:    return append(builder, value.type);
	}
	return append_format(builder, "(unknown ExecutionContext::Value {})", value.kind);
}

bool is_constant(Expression *expression);
bool is_constant_impl(Block *block) {
	assert(block->children.count == 1, "not implemented");

	auto last_expression = as<Expression>(block->children.back());
	assert(last_expression, "not implemented");

	return is_constant(last_expression);
}
bool is_constant_impl(Definition *definition) { return definition->mutability == Mutability::constant; }
bool is_constant_impl(IntegerLiteral *literal) { return true; }
bool is_constant_impl(BooleanLiteral *literal) { return true; }
bool is_constant_impl(Lambda *lambda) { return true; }
bool is_constant_impl(LambdaHead *head) { return true; }
bool is_constant_impl(Name *name) { return is_constant_impl(name->definition); }
bool is_constant_impl(Call *call) { not_implemented(); }
bool is_constant_impl(If *If) { not_implemented(); }
bool is_constant_impl(BuiltinType *type) { return true; }
bool is_constant_impl(Binary *binary) { return is_constant(binary->left) && is_constant(binary->right); }
bool is_constant_impl(Match *) { not_implemented(); }
bool is_constant(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return is_constant_impl((##name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path();
}

bool is_mutable(Expression *expression);
bool is_mutable_impl(Block *block) { not_implemented(); }
bool is_mutable_impl(Definition *definition) { return definition->mutability == Mutability::variable; }
bool is_mutable_impl(IntegerLiteral *literal) { return false; }
bool is_mutable_impl(BooleanLiteral *literal) { return false; }
bool is_mutable_impl(Lambda *lambda) { return false; }
bool is_mutable_impl(LambdaHead *head) { return false; }
bool is_mutable_impl(Name *name) { return is_mutable_impl(name->definition); }
bool is_mutable_impl(Call *call) { not_implemented(); }
bool is_mutable_impl(If *If) { not_implemented(); }
bool is_mutable_impl(BuiltinType *type) { return false; }
bool is_mutable_impl(Binary *binary) { return false; }
bool is_mutable_impl(Match *) { not_implemented(); }
bool is_mutable(Expression *expression) {
	scoped_replace(debug_current_location, expression->location);
	switch (expression->kind) {
#define x(name) case NodeKind::name: return is_mutable_impl((##name *)expression);
		ENUMERATE_EXPRESSION_KIND(x)
#undef x
	}
	invalid_code_path();
}

SpinLock retired_typecheckers_lock;
List<struct Typechecker *> retired_typecheckers;

enum class YieldResult : u8 {
	fail,
	success,
	wait,
};

volatile u32 typechecker_uid_counter;

enum class TypecheckEntryStatus : u8 {
	suspended,
	in_progress,
	succeeded,
	failed,
};

struct TypecheckEntry {
	Node* node = 0;
	Typechecker* typechecker = 0;
	TypecheckEntryStatus status = {};
	SpinLock lock;
	u32 last_thread_index = 0;
	SpinLock dependants_lock;
	List<TypecheckEntry *> dependants;
	bool dependency_failed = false;
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

struct Typechecker {
	const u32 uid = atomic_add(&typechecker_uid_counter, 1);

	static Typechecker *create(Node *node) {
		auto typechecker = [&] {
			if (auto typechecker_ = with(retired_typecheckers_lock, retired_typecheckers.pop())) {
				auto typechecker = typechecker_.value();
				// println("created cached typechecker {}", typechecker->uid);

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

			// println("created new typechecker {}", typechecker->uid);

			return typechecker;
		}();

		typechecker->reporter.reports.clear();
		typechecker->current_block = &global_block;
		typechecker->initial_node = node;
		typechecker->debug_stopped = true;
		return typechecker;
	}

	YieldResult continue_typechecking(Fiber parent_fiber, u32 thread_index, TypecheckEntry *entry) {
		assert(debug_thread_id == 0);
		debug_thread_id = get_current_thread_id();
		defer { debug_thread_id = 0; };

		debug_start();

		{
			scoped_replace(this->parent_fiber, parent_fiber);
			scoped_replace(this->thread_index, thread_index);
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
		status.status = Status::finished;
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
	struct Status {
		u32 progress = 0;

		enum {
			working,
			waiting,
			finished,
		} status = {};
	};

	using TypecheckProgress = List<Status>;

	Fiber parent_fiber = {};
	Fiber fiber = {};
	YieldResult yield_result = {};
	Reporter reporter;
	Node *initial_node = 0;
	Block *current_block = 0;
	Status status;
	u32 thread_index = -1;
	TypecheckEntry *entry = 0;
	List<Node *> node_stack;

	void get_total_progress(TypecheckProgress& progress) {
		progress.clear();

		for (auto &entry : typecheck_entries) {
			auto typechecker = entry.typechecker;
			if (typechecker) {
				progress.add(typechecker->status);
			} else {
				// NOTE: Typechecker was not created yet. This means that progress can always be made.
				progress.add({ .status = Status::waiting});
			}
		}
	}

	bool has_progress(TypecheckProgress old, TypecheckProgress now) {
		// NOTE: yield_while does not query the progress before the waiting loop, because if 
		//       predicate fails the first time, another query will be made almost immediately.
		if (!old.count)
			return true;

		auto is_finished = [&](Status s) { return s.status == Status::finished; }; 

		if (all(old, is_finished) && all(now, is_finished))
			return false;

		for (auto status : now) {
			if (status.status == Status::working) {
				return true;
			}
		}

		assert(old.count == now.count);

		for (umm i = 0; i < old.count; ++i) {
			if (now[i].status == Status::finished && old[i].status != Status::finished) {
				return true;
			}

			if (now[i].status != Status::waiting || old[i].status == Status::waiting)
				return true;

			if (now[i].progress > old[i].progress)
				return true;
		}

		return false;
	}

	[[nodiscard]]
	bool yield_while(String location, auto predicate) {
		// TODO: Make this more robust. I'm not sure this will never fail to typecheck a valid program.

		TypecheckProgress old_progress;
		TypecheckProgress current_progress;

		scoped_replace(status.status, Status::waiting);

		// TODO: FIXME: Get rid of this threshold.
		const int wait_sleep_threshold = 16;

		int iteration = 0;
		while (true) {
			if (predicate()) {
				if (entry->dependency_failed) {
					return false;
				}

				if (iteration >= wait_sleep_threshold) {
					// HACK: This feels bad, but it doesn't fail as much.
					sleep_nanoseconds(1000);
				}
				++iteration;

				get_total_progress(current_progress);
				if (!has_progress(old_progress, current_progress)) {
					return false;
				}
				Swap(old_progress, current_progress);

				// NOTE: This breaks typechecking...
				// immediate_reporter.info(location, "Yield");

				yield_smt();
				switch_thread();

				yield(YieldResult::wait);
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
				typecheck(initial_node);
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

		switch (expression->kind) {
			case NodeKind::IntegerLiteral: {
				if (::is_integer(direct_target_type)) {
					if (apply) {
						expression->type = target_type;
					}
					return true;
				}
				break;
			}
		}

		if (auto src_builtin_type = as<BuiltinType>(direct_source_type)) {
			switch (src_builtin_type->type_kind) {
				case BuiltinTypeKind::U8:
				case BuiltinTypeKind::U16:
				case BuiltinTypeKind::U32:
				case BuiltinTypeKind::U64:
				case BuiltinTypeKind::S8:
				case BuiltinTypeKind::S16:
				case BuiltinTypeKind::S32:
				case BuiltinTypeKind::S64: {
					if (auto dst_builtin_type = as<BuiltinType>(direct_target_type)) {
						switch (dst_builtin_type->type_kind) {
							case BuiltinTypeKind::U8:
							case BuiltinTypeKind::U16:
							case BuiltinTypeKind::U32:
							case BuiltinTypeKind::U64:
							case BuiltinTypeKind::S8:
							case BuiltinTypeKind::S16:
							case BuiltinTypeKind::S32:
							case BuiltinTypeKind::S64: {
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

	void typecheck(Node *node) {
		++status.progress;
		defer{ ++status.progress; };

		scoped_replace(debug_current_location, node->location);

		node_stack.add(node);
		defer { node_stack.pop(); };

		switch (node->kind) {
#define x(name) case NodeKind::name: typecheck_impl((##name *)node); break;
			ENUMERATE_NODE_KIND(x)
#undef x
			default:
				invalid_code_path();
		}

		if (auto expression = as<Expression>(node)) {
			if (!expression->type) {
				reporter.error(expression->location, "Could not compute the type of this expression.");
				yield(YieldResult::fail);
			}
		}
	}
	void typecheck_impl(Block *block) {
		scoped_replace(current_block, block);
		for (auto child : block->children) {
			typecheck(child);
		}

		if (block->children.count) {
			if (auto last_expression = as<Expression>(block->children.back()))
				block->type = last_expression->type;
		}

		if (!block->type)
			block->type = get_builtin_type(BuiltinTypeKind::None);
	}
	void typecheck_impl(Definition *definition) {
		if (definition->parsed_type) {
			typecheck(definition->parsed_type);
		} else {
			assert(definition->initial_value);
		}

		if (definition->initial_value) {
			typecheck(definition->initial_value);

			if (definition->parsed_type) {
				if (!implicitly_cast(&definition->initial_value, definition->parsed_type, true)) {
					yield(YieldResult::fail);
				}
			}

			switch (definition->mutability) {
				case Mutability::constant: {
					if (!is_constant(definition->initial_value)) {
						reporter.error(definition->location, "Definition marked constant, but its initial value is not.");
						yield(YieldResult::fail);
					}
					break;
				}
				case Mutability::readonly: {
					break;
				}
				case Mutability::variable: {
					break;
				}
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
	}
	void typecheck_impl(IntegerLiteral *literal) {
		literal->type = get_builtin_type(BuiltinTypeKind::UnsizedInteger);
	}
	void typecheck_impl(BooleanLiteral *literal) {
		literal->type = get_builtin_type(BuiltinTypeKind::Bool);
	}
	void typecheck_impl(LambdaHead *head) {
		typecheck(&head->parameters_block);

		if (head->return_type) {
			typecheck(head->return_type);
		}

		head->type = get_builtin_type(BuiltinTypeKind::Type);
	}
	void typecheck_impl(Lambda *lambda) {
		typecheck(&lambda->head);

		lambda->type = &lambda->head;

		if (lambda->head.return_type) {
			if (lambda->definition) {
				lambda->definition->type = lambda->type;
			}
		}

		if (lambda->body) {
			scoped_replace(current_block, &lambda->head.parameters_block);

			typecheck(lambda->body);
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
				auto body_type = lambda->body->type;
				if (lambda->returns.count) {
					List<Expression *> concrete_return_types;
					List<Return *> empty_returns;

					for (auto ret : lambda->returns) {
						if (!ret->value) {
							empty_returns.add(ret);
						} else if (is_concrete(ret->value->type)) {
							concrete_return_types.add(ret->value->type);
						}
					}

					if (empty_returns.count > lambda->returns.count) {
						reporter.error(empty_returns[0]->location, "TODO: Using both valued and empty return statement in a single function is not yet implemented.");
						yield(YieldResult::fail);
					}

					if (concrete_return_types.count) {
						lambda->head.return_type = concrete_return_types[0];
					} else if (empty_returns.count) {
						lambda->head.return_type = get_builtin_type(BuiltinTypeKind::None);
					} else {
						make_concrete(lambda->returns[0]->value);
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
			} else {
				lambda->head.return_type = get_builtin_type(BuiltinTypeKind::None);
			}
		}

		assert(lambda->head.return_type);
	}
	void typecheck_impl(Name *name) {
		for (auto block = current_block; block; block = block->parent) {
			if (auto found_definitions = block->definition_map.find(name->name)) {
				auto definitions = found_definitions->value;
				if (definitions.count == 1) {
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
							if (typecheck_entry.node == definition) {
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
					return;
				}

				if (definitions.count == 0) {
					continue;
				}

				reporter.error(name->location, "`{}` is ambiguous.", name->name);
				for (auto definition : definitions) {
					reporter.info(definition->location, "Declared here:");
				}
				yield(YieldResult::fail);
			}
		}
		reporter.error(name->location, "`{}` was not declared.", name->name);
		yield(YieldResult::fail);
	}
	void typecheck_impl(Return *return_) {
		if (return_->value)
			typecheck(return_->value);
	}
	void typecheck_impl(Call *call) {
		typecheck(call->callable);
		
		auto &arguments = call->arguments;
		for (auto argument : arguments) {
			typecheck(argument);
		}

		auto lambda = direct_as<Lambda>(call->callable);
		assert(lambda, "Other stuff not implemented");
		assert(lambda->head.return_type, "TODO: wait here");

		auto &parameters = lambda->head.parameters_block.definition_list;

		if (arguments.count != parameters.count) {
			reporter.error(call->location, "Too {} arguments. Expected {}, but got {}.", arguments.count > parameters.count ? "much"s : "few"s, parameters.count, arguments.count);
			reporter.info(lambda->location, "Lamda is here:");
			yield(YieldResult::fail);
		}

		call->type = lambda->head.return_type;
	}
	void typecheck_impl(If *If) {
		typecheck(If->condition);

		typecheck(If->true_branch);

		if (If->false_branch)
			typecheck(If->false_branch);

		if (If->false_branch) {
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
			If->type = get_builtin_type(BuiltinTypeKind::None);
	}
	void typecheck_impl(While *While) {
		typecheck(While->condition);

		if (auto builtin_type = direct_as<BuiltinType>(While->condition->type); !builtin_type || builtin_type->type_kind != BuiltinTypeKind::Bool) {
			reporter.error(While->condition->location, "Condition type must be Bool.");
			yield(YieldResult::fail);
		}

		typecheck(While->body);
	}
	void typecheck_impl(Continue *Continue) {}
	void typecheck_impl(Break *Break) {}
	void typecheck_impl(BuiltinType *type) { 
		type->type = get_builtin_type(BuiltinTypeKind::Type);
	}
	void typecheck_impl(Binary *binary) {
		typecheck(binary->left);

		typecheck(binary->right);

		auto dleft  = direct(binary->left->type);
		auto dright = direct(binary->right->type);

		if (binary->operation == BinaryOperation::ass) {
			if (!is_mutable(binary->left)) {
				reporter.error(binary->left->location, "This expression is read-only.");
				yield(YieldResult::fail);
			}
			if (!implicitly_cast(&binary->right, binary->left->type, true)) {
				yield(YieldResult::fail);
			}
			binary->type = get_builtin_type(BuiltinTypeKind::None);
			return;
		}

		if (auto found = binary_typecheckers.find({ dleft, dright, binary->operation })) {
			(this->*found->value)(binary);
			return;
		}

		if (!types_match(binary->left->type, binary->right->type)) {
			reporter.error(binary->location, "No binary operation {} defined for types {} and {}.", binary->operation, binary->left->type, binary->right->type);
			yield(YieldResult::fail);
		}

		switch (binary->operation) {
			case BinaryOperation::add:
			case BinaryOperation::sub:
			case BinaryOperation::mul:
			case BinaryOperation::div:
				binary->type = binary->left->type;
				break;
			case BinaryOperation::equ:
			case BinaryOperation::neq:
			case BinaryOperation::les:
			case BinaryOperation::leq:
			case BinaryOperation::grt:
			case BinaryOperation::grq:
				binary->type = get_builtin_type(BuiltinTypeKind::Bool);
				break;

			default:
				invalid_code_path("Invalid binary operation {}.", binary->operation);
		}
	}
	void typecheck_impl(Match *match) {
		typecheck(match->expression);

		make_concrete(match->expression);

		for (auto &Case : match->cases) {
			typecheck(Case.from);
			
			if (!is_constant(Case.from)) {
				reporter.error(Case.from->location, "Match case expression must be constant.");
				yield(YieldResult::fail);
			}

			if (!implicitly_cast(&Case.from, match->expression->type, true))
				yield(YieldResult::fail);

			typecheck(Case.to);
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
	
	inline static HashMap<BinaryTypecheckerKey, void (Typechecker::*)(Binary *)> binary_typecheckers;

	void bt_take_left(Binary *binary) {
		binary->type = binary->left->type;
	}
	void bt_set_bool(Binary *binary) {
		binary->type = get_builtin_type(BuiltinTypeKind::Bool);
	}

	static void init_binary_typecheckers() {
		construct(binary_typecheckers);

#define y(left, right, operation) binary_typecheckers.get_or_insert({ get_builtin_type(left), get_builtin_type(right), operation })
#define x(left, right, operation) y(BuiltinTypeKind::left, BuiltinTypeKind::right, BinaryOperation::operation)

		//
		// Every type is equatable
		// 
		for (u32 i = 0; i < (u32)BuiltinTypeKind::count; ++i) {
			y((BuiltinTypeKind)i, (BuiltinTypeKind)i, BinaryOperation::equ) = &bt_set_bool;
		}

#define ORDERABLE(type) \
		x(type, type, les) = &bt_set_bool; \
		x(type, type, leq) = &bt_set_bool; \
		x(type, type, grt) = &bt_set_bool; \
		x(type, type, grq) = &bt_set_bool

		ORDERABLE(Bool);
		ORDERABLE(U8);
		ORDERABLE(U16);
		ORDERABLE(U32);
		ORDERABLE(U64);
		ORDERABLE(S8);
		ORDERABLE(S16);
		ORDERABLE(S32);
		ORDERABLE(S64);

#define MATHABLE(type) \
		x(type, type, add) = &bt_take_left; \
		x(type, type, sub) = &bt_take_left; \
		x(type, type, mul) = &bt_take_left; \
		x(type, type, div) = &bt_take_left; \
		x(type, type, mod) = &bt_take_left;

		MATHABLE(Bool);
		MATHABLE(U8);
		MATHABLE(U16);
		MATHABLE(U32);
		MATHABLE(U64);
		MATHABLE(S8);
		MATHABLE(S16);
		MATHABLE(S32);
		MATHABLE(S64);

#undef MATHABLE
#undef ORDERABLE
#undef x
#undef y
	}

};

void init_globals() {
	construct(timed_results);
	timed_results.reserve(16);
	construct(content_start_to_file_name);
	construct(retired_typecheckers);
	construct(global_block);
	construct(builtin_types);
	construct(typecheck_entries);
	construct(deferred_reports);

	GlobalAllocator::init();
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
	static BuiltinType _builtin_type_##name; \
	{ \
		auto &type = get_builtin_type(BuiltinTypeKind::name); \
		type = &_builtin_type_##name; \
		type->type_kind = BuiltinTypeKind::name; \
		type->type = &_builtin_type_Type; \
	}
	ENUMERATE_BUILTIN_TYPES(x)
#undef x
}

s32 tl_main(Span<Span<utf8>> args) {
	auto main_fiber = fiber_init(0);

	set_console_encoding(Encoding_utf8);

	defer {
		for (auto time : timed_results) {
			println("{} took {} ms", time.name, time.seconds * 1000);
		}

#if BUILD_DEBUG
		println("Total string hashes: {}", string_hash_count);
#endif
	};

	init_globals();
	init_builtin_types();
	
	timed_function();

	auto maybe_arguments = parse_arguments(args);
	if (!maybe_arguments)
		return 1;
	auto arguments = maybe_arguments.value();

	auto source_contents_buffer = read_entire_file(arguments.source_name, {.extra_space_before = 1, .extra_space_after = 1});
	if (!source_contents_buffer.data) {
		with(ConsoleColor::red, println("Could not read input file '{}'", arguments.source_name));
		return 1;
	}
	defer { free(source_contents_buffer); };

	auto source_contents = (String)source_contents_buffer.subspan(1, source_contents_buffer.count - 2);

	content_start_to_file_name.get_or_insert(source_contents.data) = arguments.source_name;

	auto tokens = source_to_tokens(source_contents, arguments.source_name);
	if (!tokens)
		return 1;

	auto global_nodes = tokens_to_nodes(main_fiber, tokens.value());
	if (!global_nodes)
		return 1;

	for (auto node : global_nodes.value())
		global_block.add(node);

	auto cpu_info = get_cpu_info();

	u32 thread_count;
	if (arguments.thread_count == 0) {
		thread_count = cpu_info.logical_processor_count;
	} else {
		thread_count = min(arguments.thread_count, cpu_info.logical_processor_count);
	}

	bool failed = false;

	typecheck_entries = map(global_nodes.value(), [&](auto node) { return TypecheckEntry{.node = node}; });

	SpinLock next_entry_lock;
	auto next_entry = typecheck_entries.data;

	auto get_next_entry = [&] {
		scoped(next_entry_lock);

		auto result = next_entry;
		++next_entry;
		if (next_entry == typecheck_entries.end())
			next_entry = typecheck_entries.data;
		return result;
	};

	List<Thread *> threads;
	threads.resize(thread_count);

	{
		timed_block("typecheck");
		for (umm thread_index = 0; thread_index < thread_count; ++thread_index) {
			threads[thread_index] = create_thread([thread_index, &failed, get_next_entry] { 
#if 0
				auto log = open_file(format("tmp/logs/log{}.txt\0"s, thread_index).data, {.read = true, .write = true});
				current_printer = {
					.func = [] (Span<utf8> string, void *state) {
						auto log = File{state};
						write(log, as_bytes(string));
					},
					.state = log.handle
				};
#endif

				auto my_fiber = fiber_init(0);

				while (true) {
					auto entry = [&]() -> TypecheckEntry * {
						u32 done_entries_count = 0;
						while (true) {
							auto entry = get_next_entry();

							scoped(entry->lock);

							switch (entry->status) {
								case TypecheckEntryStatus::suspended: {
									// NOTE: This prevents a single thread running single typechecker all the time and gives opportunity to do this to others.
									//       I added this because one thread was waiting on an identifier all the time and i guess it was the only one who could
									//       hold the entry lock.
									//       I'm not sure if this really solves the 
									// if (entry->last_thread_index == thread_index) {
									// 	entry->last_thread_index = -1;
									// 	done_entries_count = 0;
									// 	break;
									// }
									// entry->last_thread_index = thread_index;

									entry->status = TypecheckEntryStatus::in_progress;
									if (!entry->typechecker) {
										entry->typechecker = Typechecker::create(entry->node);
									}

									// println("{} took {} {}", thread_index, entry->typechecker->uid, entry->node->location);
									return entry;
								}
								case TypecheckEntryStatus::in_progress: {
									done_entries_count = 0;
									break;
								}
								case TypecheckEntryStatus::succeeded:
								case TypecheckEntryStatus::failed: {
									++done_entries_count;
									if (done_entries_count == typecheck_entries.count)
										return 0;
									break;
								}
							}
						}
					}();

					if (!entry) {
						// println("{} exit", thread_index);
						return;
					}

					auto result = entry->typechecker->continue_typechecking(my_fiber, thread_index, entry);

					if (result == YieldResult::wait) {
						// println("{} wait {} {}", thread_index, entry->typechecker->uid, entry->node->location);
					} else {
						// println("{} done {} {} {}", thread_index, entry->typechecker->uid, entry->node->location, result == YieldResult::fail ? "fail" : "success");
					}

					{
						scoped(entry->lock);
						entry->typechecker->stop();

						if (result == YieldResult::wait) {
							entry->status = TypecheckEntryStatus::suspended;

							// NOTE: put waiting entry to the back of the list to give priority to others.
							//auto found = find(typecheck_entries, entry);
							//assert(found);
							//while (found < typecheck_entries.end() && found[1]->status != TypecheckEntryStatus::suspended) {
							//	Swap(found[0], found[1]);
							//}
						} else {
							entry->typechecker->retire();

							if (result == YieldResult::fail) {
								entry->status = TypecheckEntryStatus::failed;
								failed = true;

								scoped(entry->dependants_lock);
								for (auto dependant : entry->dependants) {
									dependant->dependency_failed = true;
								}
							} else {
								entry->status = TypecheckEntryStatus::succeeded;
							}
						}
					}
				}
			});
		}
	
		for (umm thread_index = 0; thread_index < thread_count; ++thread_index) {
			join(threads[thread_index]);
		}
	}

	for (auto &entry : typecheck_entries) {
		assert(entry.status == TypecheckEntryStatus::succeeded || entry.status == TypecheckEntryStatus::failed);
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
				auto &entry = typecheck_entries[cycle[j]];
				auto &next_entry = typecheck_entries[cycle[(j + 1) % cycle.count]];

				immediate_reporter.info(entry.node->location, "{} depends on {}.", entry.node->location, next_entry.node->location);
			}
		}
	}

	for (auto &report : deferred_reports.use_unprotected()) {
		report.print();
	}

	if (failed) {
		return 1;
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

				auto lambda = as<Lambda>(definition->initial_value);
				if (!lambda) {
					immediate_reporter.error(definition->location, "main must be a lambda");
					return 1;
				}

				if (!types_match(lambda->head.return_type, get_builtin_type(BuiltinTypeKind::None)) &&
					!::is_integer(lambda->head.return_type)) 
				{
					immediate_reporter.error(definition->location, "main must return integer or None, not {}.", lambda->head.return_type);
					return 1;
				}

				println(" ==== EXECUTING main ==== ");
				defer { println(" ==== EXECUTION ENDED ==== "); };

				auto call = Call::create();
				call->callable = lambda;

				auto context = ExecutionContext::create();
				println(context.execute(call));
			}
		}
	}

	with(ConsoleColor::green, println("Build success"));

	return 0;
}