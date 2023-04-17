// TODO: track current node for assertion messages.

void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line);
template <class ...Args>
void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line, char const *format, Args ...args);

#define ASSERTION_FAILURE(cause_string, expression, ...) (::assertion_failure_impl(cause_string, expression, __FILE__, __LINE__, __VA_ARGS__), debug_break())

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

using String = Span<utf8>;

template <>
constexpr u64 get_hash(String const &string) {
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

void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line) {
	println("COMPILER ERROR: {} {} at {}:{}", cause_string, expression, file, line);
}
void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line, Span<char> message) {
	assertion_failure_impl(cause_string, expression, file, line);
	println("Message: {}", message);
}
template <class ...Args>
void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line, char const *format, Args ...args) {
	assertion_failure_impl(cause_string, expression, file, line, tformat(format, args...));
}

struct TimedResult {
	char const *name = 0;
	f64 seconds = 0;
};

List<TimedResult> timed_results;

#define timed_block(name) \
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

#define ENUMERATE_CHARS_SYMBOL(x) \
	x("(") x(")") x("[") x("]") x("{") x("}") x("<") x(">") \
	x("`") x("~") x("!") x("@") x("$") x("%") x("^") x("&") \
	x("*") x("-") x("=") x("+") x("\\") x("|") x(";") x(":") \
	x(",") x(".") x("?") x("\n") \

#define ENUMERATE_SINGLE_CHAR_TOKENS(x) \
	x("(") x(")") x("[") x("]") x("{") x("}") x("<") x(">") \
	x("`") x("~") x("!") x("@") x("#") x("$") x("%") x("^") \
	x("&") x("*") x("-") x("=") x("+") x(";") x(":") x(",") \
	x(".") x("/") x("?") \
	x("\n") x("\\") \

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

consteval u8 const_string_to_token_kind(Span<char> token) {
	if (token.count == 1)
		return token[0];

	u8 iota = 0xa0;
#define x(string) if (iota > 0xaf) *(int *)0 = 0; /*overflow*/ if (token == string##s) return iota; ++iota;
	ENUMERATE_DOUBLE_CHAR_TOKENS(x)
#undef x

	*(int *)0 = 0; /*invalid token*/;
}

#define ENUMERATE_BUILTIN_TYPES(x) \
	x(Type, 0x80) \
	x(U8,   0x81) \
	x(U16,  0x82) \
	x(U32,  0x83) \
	x(U64,  0x84) \
	x(S8,   0x85) \
	x(S16,  0x86) \
	x(S32,  0x87) \
	x(S64,  0x88) \
	x(Bool, 0x89) \
	x(None, 0x8a) /* NOTE: might also be used for empty optionals? */ \

#define ENUMERATE_KEYWORDS(x) \
	ENUMERATE_BUILTIN_TYPES(x) \
	x(const,    0x90) \
	x(let,      0x91) \
	x(var,      0x92) \
	x(return,   0x93) \
	x(if,       0x94) \
	x(then,     0x95) \
	x(else,     0x96) \
	x(false,    0x97) \
	x(true,     0x98) \
	x(while,    0x99) \
	x(break,    0x9a) \
	x(continue, 0x9b) \

// #define x(name, token, precedence)
#define ENUMERATE_BINARY_OPERATIONS(x) \
	x(multiply,       "*" , 4) \
	x(divide,         "/" , 4) \
	x(add,            "+" , 3) \
	x(subtract,       "-" , 3) \
	x(equals,         "==", 2) \
	x(not_equals,     "!=", 2) \
	x(less,           "<" , 2) \
	x(less_equals,    "<=", 2) \
	x(greater,        ">" , 2) \
	x(greater_equals, ">=", 2) \
	x(assign,         "=" , 1) \

#define ENUMERATE_TOKEN_KIND(x) \
	ENUMERATE_KEYWORDS(x) \
	x(end_of_file, 0) \
	x(end_of_line, '\n') \
	x(name,        'a') \
	x(number,      '0') \
	x(directive,   '#') \

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

#define ENUMERATE_STATEMENT_KIND(x) \
	x(Return) \
	x(While) \
	x(Continue) \
	x(Break) \

#define ENUMERATE_NODE_KIND(x) \
	ENUMERATE_EXPRESSION_KIND(x) \
	ENUMERATE_STATEMENT_KIND(x) \

enum TokenKind : u8 {
#define x(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KIND(x)
#undef x
};


umm append_known_token_kind(StringBuilder &builder, TokenKind kind) {
	switch (kind) {
		case Token_end_of_file: return append(builder, "end of file");
		case Token_end_of_line: return append(builder, "end of line");
	}

	if (0x21 <= kind && kind <= 0x7e)
		return append(builder, (char)kind);

	switch (kind) {
#define x(name, value) case Token_##name: return append(builder, #name);
		ENUMERATE_TOKEN_KIND(x)
#undef x
	}

	return 0;
}

inline umm append(StringBuilder &builder, TokenKind kind) {
	if (auto appended = append_known_token_kind(builder, kind))
		return appended;

	return append(builder, "unknown");
}

struct Token {
	TokenKind kind = {};
	String string;
};

inline umm append(StringBuilder &builder, Token token) {
	switch (token.kind) {
		case Token_end_of_file: return append(builder, "end of file");
		case Token_end_of_line: return append(builder, "end of line");
	}

	return append(builder, token.string);
}

#define PASTE_CASE(x) case x:
#define PASTE_CASE_0(x) case x[0]:

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
		assert(!init[index]); // NOTE: if theres a compilation error, that means this failed.
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

constexpr auto keywords = []() consteval {
	FixedHashMap<128, String, TokenKind> keywords = {};
#define x(name, value) keywords.insert(u8#name##s, Token_##name);
	ENUMERATE_KEYWORDS(x)
#undef x
	return keywords;
}();

struct MulticharHashTraits : DefaultHashTraits<String> {
	inline static constexpr u64 get_hash(String const &key) {
		if (key.count == 1)
			return key[0];
		return key[0] * 128 + key[1];
	}
};

using MulticharTokens = FixedHashMap<128, String, TokenKind/*, MulticharHashTraits*/>;

// 0th map contains tokens with length 1, 1st map - length 2, etc...
constexpr MulticharTokens multichar_tokens[2] = {
	[]() consteval {
		MulticharTokens multichar_tokens_0;
#define x(token) multichar_tokens_0.insert(u8##token##s, (TokenKind)const_string_to_token_kind(token##s));
		ENUMERATE_SINGLE_CHAR_TOKENS(x)
#undef x
		return multichar_tokens_0;
	}(),
	[]() consteval {
		MulticharTokens multichar_tokens_1;
#define x(token) multichar_tokens_1.insert(u8##token##s, (TokenKind)const_string_to_token_kind(token##s));
		ENUMERATE_DOUBLE_CHAR_TOKENS(x)
#undef x
		return multichar_tokens_1;
	}(),
};

struct SourceLocation {
	String file;
	u32 line = 0;
	u32 column = 0;
	String line_string;
};

HashMap<utf8 *, String> content_start_to_file_name;

SourceLocation get_source_location(String location) {
	SourceLocation result;

	auto cursor = location.data;
	result.column = 0;
	while (*cursor != '\n' && *cursor != '\0') {
		++result.column;
		--cursor;
	}

	if (location == u8"\n"s) {
		result.line_string = location;
	} else {
		result.line_string.data = cursor + 1;
		while (*result.line_string.end() != '\n' && *result.line_string.end() != '\0')
			++result.line_string.count;
	}

	assert(result.line_string.begin() <= location.begin());
	assert(result.line_string.end() >= location.end());

	result.line = 1;
	while (*cursor != '\0') {
		if (*cursor == '\n') 
			++result.line;
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

	template <class ...Args>
	static Report create(ReportKind kind, String location, char const *format, Args ...args) {
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
			println("{}:{}:{}: ", source_location.file, source_location.line, source_location.column);
			print_report_kind(kind);
			println(": {}",  message);
			::print("{}| ", Format(source_location.line, align_right(4, ' ')));
			print_replacing_tabs_with_spaces(String(source_location.line_string.begin(), location.begin()));
			with(get_color(kind), print_replacing_tabs_with_spaces(location));
			print_replacing_tabs_with_spaces(String(location.end(), source_location.line_string.end()));
			println();
		} else {
			print_report_kind(kind);
			println(": {}", message);
		}
		println();
	}
};

bool break_on_error = false;

struct ReporterBase {
	template <class ...Args> void info   (this auto &&self, String location, char const *format, Args const &...args) { self.on_report(Report::create(ReportKind::info,    location, format, args...)); }
	template <class ...Args> void warning(this auto &&self, String location, char const *format, Args const &...args) { self.on_report(Report::create(ReportKind::warning, location, format, args...)); }
	template <class ...Args> void error  (this auto &&self, String location, char const *format, Args const &...args) {
		if (break_on_error) {
			debug_break();
		}
		self.on_report(Report::create(ReportKind::error,   location, format, args...));
	}
	template <class ...Args> void help   (this auto &&self, String location, char const *format, Args const &...args) { self.on_report(Report::create(ReportKind::help,    location, format, args...)); }
	template <class ...Args> void info   (this auto &&self, char const *format, Args const &...args) { return self.info   (String{}, format, args...); }
	template <class ...Args> void warning(this auto &&self, char const *format, Args const &...args) { return self.warning(String{}, format, args...); }
	template <class ...Args> void error  (this auto &&self, char const *format, Args const &...args) { return self.error  (String{}, format, args...); }
	template <class ...Args> void help   (this auto &&self, char const *format, Args const &...args) { return self.help   (String{}, format, args...); }
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

Optional<List<Token>> source_to_tokens(String source, String path) {
	timed_function();

	List<Token> tokens;

	utf8 *cursor = 0;
	utf8 *next_cursor = source.begin();
	utf32 code_point = 0;

	auto print_invalid_character_error = [&] {
		immediate_reporter.error({cursor, 1}, "Invalid uft8 character.");
	};

#define next_char()                                                                 \
	do {                                                                            \
		cursor = next_cursor;                                                       \
		Optional<utf32> maybe_code_point = get_char_and_advance_utf8(&next_cursor); \
		if (!maybe_code_point) {                                                    \
			print_invalid_character_error();                                        \
			return {};                                                              \
		}                                                                           \
		code_point = maybe_code_point.value();                                      \
	} while (0)

	auto go_back = [&](u32 bytes) {
		cursor -= bytes;
		next_cursor -= bytes;
	};

	next_char();

	while (true) {

		while (true) {
			if (code_point == ' ' || code_point == '\t' || code_point == '\r')
				next_char();
			else
				break;
		}

		if (code_point == '\0')
			break;

		Token token;
		token.string.data = cursor;

		switch (code_point) {
			case '/': {
				token.kind = (TokenKind)code_point;
				next_char();
				token.string.set_end(cursor);

				if (code_point == '/') {
					while (code_point != '\n') {
						next_char();
					}
					next_char();
				} else {
					go_back(1);
					goto default_case;
				}
				break;
			}
			case '#': {
				token.kind = Token_directive;

				next_char();
				while (true) {
					switch (code_point) {
						ENUMERATE_CHARS_ALPHA(PASTE_CASE)
						ENUMERATE_CHARS_DIGIT(PASTE_CASE)
						case '_': {
							next_char();
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

			ENUMERATE_CHARS_ALPHA(PASTE_CASE)
			case '_': {
				token.kind = Token_name;

				next_char();
				while (true) {
					switch (code_point) {
						ENUMERATE_CHARS_ALPHA(PASTE_CASE)
						ENUMERATE_CHARS_DIGIT(PASTE_CASE)
						case '_': {
							next_char();
							break;
						}
						default:
							goto name_loop_end;
					}
				}
			name_loop_end:;
				
				token.string.set_end(cursor);

				if (auto keyword = keywords.find(token.string))
					token.kind = keyword->value;
				
				tokens.add(token);
				break;
			}

			ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
				token.kind = Token_number;

				while (true) {
					switch (code_point) {
						ENUMERATE_CHARS_DIGIT(PASTE_CASE) {
							next_char();
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
			default_case:;

				next_char();
				while (true) {
					switch (code_point) {
						ENUMERATE_CHARS_SYMBOL(PASTE_CASE_0) {
							next_char();
							break;
						}
						default:
							goto symbol_loop_end;
					}
				}
			symbol_loop_end:;

				auto remaining_symbols = token.string;
				remaining_symbols.set_end(cursor);

				while (remaining_symbols.count) {
					for (umm i = count_of(multichar_tokens) - 1; i != -1; --i) {
						auto possible_tokens = multichar_tokens[i];
						auto token_char_count = i + 1;

						auto x = String { remaining_symbols.data, token_char_count };
						if (auto found = possible_tokens.find(x)) {
							token.kind = found->value;
							token.string = { remaining_symbols.data, token_char_count };
							tokens.add(token);

							remaining_symbols.set_begin(remaining_symbols.begin() + token_char_count);
						
							goto continue_remaining_symbols;
						}
					}

					immediate_reporter.error({remaining_symbols.data, 1}, "Could not parse token.");
					return {};

				continue_remaining_symbols:;
				}

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

enum class BinaryOperation : u8 {
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
#define x(name, token, precedence) case const_string_to_token_kind(token##s): return BinaryOperation::name;
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
		return new T();
	}
};

// NOTE: pointer to variable can't be converted to pointer to readonly.
enum class Mutability : u8 {
	constant,
	readonly,
	variable,
};

#define DEFINE_EXPRESSION(name) struct name : Expression, NodeBase<name>
#define DEFINE_STATEMENT(name) struct name : Statement, NodeBase<name>

enum class BuiltinTypeKind : u8 {
#define x(name, value) name = value,
	ENUMERATE_BUILTIN_TYPES(x)
#undef x
};

DEFINE_EXPRESSION(Block) {
	Block *parent = 0;
	List<Node *> children;
	List<Definition *> definition_list;
	HashMap<String, List<Definition *>> definition_map;

	void add(Node *child);
};
DEFINE_EXPRESSION(Call) {
	Expression *callable = 0;
	List<Expression *> arguments;
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
	Block parameters_block;
	Expression *return_type = 0;
	bool is_intrinsic : 1 = false;
};
DEFINE_EXPRESSION(Lambda) {
	Definition *definition = 0;
	Expression *body = 0;
	LambdaHead head;
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

HashMap<BuiltinTypeKind, BuiltinType *> builtin_types;
BuiltinType *get_builtin_type(BuiltinTypeKind kind) {
	return builtin_types.find(kind)->value;
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

	if (auto ab = as<BuiltinType>(a)) {
		if (auto bb = as<BuiltinType>(b)) {
			return ab->kind == bb->kind;
		}
	}

	return false;
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
	print(" {} = ", definition->name);
	print_ast(definition->initial_value);
}
void print_ast_impl(IntegerLiteral *literal) { print(literal->value); }
void print_ast_impl(BooleanLiteral *literal) { print(literal->value); }
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

	print(")");

	if (head->is_intrinsic) print(" #intrinsic");
}
void print_ast_impl(Lambda *lambda) {
	print_ast(&lambda->head);
	if (lambda->body) {
		print(' ');
		print_ast(lambda->body);
	} else {
		print(';');
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
	print('(');
	print_ast(binary->left);
	print(binary->operation);
	print_ast(binary->right);
	print(')');
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
	Reporter reporter;

	Expression *parse_expression(bool whitespace_is_skippable_before_binary_operator = false, int right_precedence = 0) {
		//null denotation
		auto left = parse_expression_1();
		if (!left)
			return 0;

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
					if (!right)
						return 0;

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

					if (!binop->right)
						return 0;

					left = binop;
					continue;
				}
			}
			break;
		}
		return left;
	}
	Expression *parse_expression_1() {
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
				if (!expect(Token_name))
					return 0;

				definition->name = token->string;
				definition->location = token->string;

				next();
				if (!expect('='))
					return 0;

				next();

				definition->initial_value = parse_expression();
				if (!definition->initial_value)
					return 0;

				if (definition->mutability == Mutability::constant) {
					if (auto lambda = as<Lambda>(definition->initial_value)) {
						lambda->definition = definition;
					}
				}

				return definition;
			}
		}

		auto node = parse_expression_0();
		if (!node)
			return 0;

		while (token->kind == '(') {
			auto call = Call::create();
			call->callable = node;
			call->location = node->location;

			next();
			skip_lines();
			if (token->kind != ')') {
				while (true) {
					auto argument = parse_expression();
					if (!argument)
						return 0;

					call->arguments.add(argument);

					skip_lines();
					if (token->kind == ',') {
						next();
						continue;
					}
					if (token->kind == ')') {
						break;
					}
				}
			}

			call->location = {call->location.begin(), token->string.end()};

			next();
			node = call;
		}
		return node;
	}
	Expression *parse_expression_0() {
		switch (token->kind) {
			case '(': {
				auto lambda = Lambda::create();
				lambda->location = token->string;
				lambda->head.parameters_block.parent = current_block;
				scoped_replace(current_block, &lambda->head.parameters_block);
				scoped_replace(current_loop, 0);

				next();
				skip_lines();
				if (token->kind != ')') {
					while (true) {
						if (!expect(Token_name))
							return 0;

						List<Definition *> parameter_group;

						{
							auto parameter = Definition::create();
							parameter->name = token->string;
							parameter_group.add(parameter);
						}

						next();

						while (token->kind == ',') {
							next();
							if (!expect(Token_name))
								return 0;

							auto parameter = Definition::create();
							parameter->name = token->string;
							parameter_group.add(parameter);
							next();
						}

						if (!expect(':'))
							return 0;

						next();

						auto parsed_type = parse_expression();

						for (auto parameter : parameter_group) {
							parameter->parsed_type = parsed_type;
							parameter->is_parameter = true;
							lambda->head.parameters_block.add(parameter);
						}

						skip_lines();
						if (token->kind == ',') {
							next();
							continue;
						}
						if (token->kind == ')') {
							break;
						}
					}
				}
				next();

				skip_lines();

				if (token->kind != ':') {
					lambda->head.return_type = parse_expression();
					if (!lambda->head.return_type)
						 return 0;

					if (!expect(':'))
						return 0;
				}
				next();


				if (token->kind == Token_directive && token->string == u8"#intrinsic"s) {
					next();

					lambda->head.is_intrinsic = true;
				} else {
					lambda->body = parse_expression();
					if (!lambda->body)
						return 0;
				}


				return lambda;
			}
			case '{': {
				auto block = Block::create();
				block->location = token->string;

				block->parent = current_block;
				scoped_replace(current_block, block);

				next();

				while (true) {
					skip_lines();
					if (token->kind == '}') {
						break;
					}

					if (!parse_statement_into_block(block))
						return 0;
				}
				next();

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
					return 0;
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

				If->condition = parse_expression();
				if (!If->condition)
					return 0;

				skip_lines();
				if (token->kind == Token_then) {
					next();
					skip_lines();
				}

				If->true_branch = parse_statement();
				if (!If->true_branch)
					return 0;

				skip_lines();
				if (token->kind == Token_else) {
					next();
					skip_lines();

					If->false_branch = parse_statement();
					if (!If->false_branch)
						return 0;
				}

				return If;
			}

#define x(name, value) case value:
			ENUMERATE_BUILTIN_TYPES(x)
#undef x
			{

				auto type = (BuiltinTypeKind)token->kind;
				next();
				return get_builtin_type(type);
			}

			default: 
				reporter.error(token->string, "Unexpected token '{}' when parsing expression.", *token);
				return 0;
		}
	}
	Node *parse_statement() {
		switch (token->kind) {
			case Token_return: {
				auto return_ = Return::create();
				return_->location = token->string;

				next();

				if (token->kind != '\n') {
					return_->value = parse_expression();
					if (!return_->value)
						return 0;
				}

				return return_;
			}
			case Token_while: {
				auto While = While::create();
				While->location = token->string;
				next();

				scoped_replace(current_loop, While);

				While->condition = parse_expression();
				if (!While->condition)
					return 0;

				skip_lines();
				if (token->kind == Token_then) {
					next();
					skip_lines();
				}

				While->body = parse_statement();
				if (!While->body)
					return 0;

				return While;
			}
			case Token_continue: {
				if (!current_loop) {
					reporter.error(token->string, "`continue` must be inside a loop.");
					return 0;
				}

				auto Continue = Continue::create();
				Continue->location = token->string;
				next();
				return Continue;
			}
			case Token_break: {
				if (!current_loop) {
					reporter.error(token->string, "`break` must be inside a loop.");
					return 0;
				}

				auto Break = Break::create();
				Break->location = token->string;
				next();
				return Break;
			}
		}

		auto expression = parse_expression();
		if (!expression)
			return 0;

		return expression;
	}
	bool allowed_in_statement_context(Node *node) {
		switch (node->kind) {
			case NodeKind::Definition:
			case NodeKind::Block:
			case NodeKind::Return:
			case NodeKind::Call:
			case NodeKind::If:
			case NodeKind::While:
			case NodeKind::Continue:
			case NodeKind::Break:
				return true;
			case NodeKind::Binary:
				switch (((Binary *)node)->operation) {
					case BinaryOperation::assign:
						return true;
				}
				break;
		}
		return false;
	}
	Node *parse_statement_into_block(Block *block) {
		auto child = parse_statement();
		if (!child)
			return 0;

		if (!allowed_in_statement_context(child)) {
			reporter.error(child->location, "{}s are not allowed in statement context.", child->kind);
			return 0;
		}

		block->add(child);
		return child;
	}
	bool next() {
		++token;
		return token->kind != Token_end_of_file;
	}
	bool expect(std::underlying_type_t<TokenKind> expected_kind) {
		if (token->kind != expected_kind) {
			reporter.error(token->string, "Expected '{}', but got '{}'", (TokenKind)expected_kind, *token);
			return false;
		}
		return true;
	}
	bool expect(std::initializer_list<std::underlying_type_t<TokenKind>> expected_kinds) {
		for (auto expected_kind : expected_kinds) {
			if (token->kind == expected_kind) {
				return true;
			}
		}

		StringBuilder builder;
		append(builder, "Expected one of ");
		for (auto expected_kind : expected_kinds) {
			append_format(builder, "'{}', ", (TokenKind)expected_kind);
		}
		append_format(builder, "but got '{}'.\0"s, *token);
		reporter.error(token->string, (char *)to_string(builder).data);
		return false;
	}
	void skip_lines() {
		while (token->kind == '\n')
			++token;
	}
};

Optional<List<Node *>> tokens_to_nodes(Span<Token> tokens) {
	timed_function();

	Parser parser;
	parser.token = tokens.data;
	List<Node *> nodes;
	
	defer { parser.reporter.print_all(); };

	while (true) {
		auto saved = parser.token;
		parser.skip_lines();
		if (parser.token->kind == Token_end_of_file) {
			break;
		}

		auto node = parser.parse_statement_into_block(&global_block);
		if (!node)
			return {};

		nodes.add(node);
	}

	return nodes;
}

struct ExecutionContext {
	enum class ValueKind : u8 {
		nothing,
		integer,
		boolean,
		break_,
		continue_,
		return_,
	};

	struct Value {
		ValueKind kind = {};
		u64 integer = 0;
		bool boolean = false;
	};

	struct Scope {
		// Make this pointer-stable just in case.
		BucketHashMap<Definition *, Value> variables;
	};

	List<Scope> scope_stack;
	Value return_value;

	static ExecutionContext create() {
		ExecutionContext result;
		result.scope_stack.add(); // global scope
		return result;
	}

	Value execute(Node *node) {
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
	Value execute_impl(Lambda *lambda) { not_implemented(); }
	Value execute_impl(LambdaHead *head) { not_implemented(); }
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

		if (lambda->head.is_intrinsic) {
			assert(lambda->definition);

			List<Value> argument_values;
			argument_values.allocator = temporary_allocator;
			argument_values.reserve(arguments.count);

			for (auto argument : arguments)
				argument_values.add(execute(argument));

			auto name = lambda->definition->name;

			if (name == u8"print"s) {
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
	Value execute_impl(BuiltinType *type) { not_implemented(); }
	Value execute_impl(Binary *binary) {
		if (binary->operation == BinaryOperation::assign) {
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
			case BinaryOperation::add:            return { .kind = ValueKind::integer, .integer = left.integer +  right.integer };
			case BinaryOperation::subtract:       return { .kind = ValueKind::integer, .integer = left.integer -  right.integer };
			case BinaryOperation::multiply:       return { .kind = ValueKind::integer, .integer = left.integer +  right.integer };
			case BinaryOperation::divide:         return { .kind = ValueKind::integer, .integer = left.integer /  right.integer };
			case BinaryOperation::equals:         return { .kind = ValueKind::boolean, .boolean = left.integer == right.integer };
			case BinaryOperation::not_equals:     return { .kind = ValueKind::boolean, .boolean = left.integer != right.integer };
			case BinaryOperation::less:           return { .kind = ValueKind::boolean, .boolean = left.integer <  right.integer };
			case BinaryOperation::less_equals:    return { .kind = ValueKind::boolean, .boolean = left.integer <= right.integer };
			case BinaryOperation::greater:        return { .kind = ValueKind::boolean, .boolean = left.integer >  right.integer };
			case BinaryOperation::greater_equals: return { .kind = ValueKind::boolean, .boolean = left.integer >= right.integer };
		}

		immediate_reporter.error(binary->location, "Invalid binary operation");
		invalid_code_path();
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
		case ExecutionContext::ValueKind::integer: return append(builder, value.integer);
		case ExecutionContext::ValueKind::boolean: return append(builder, value.boolean);
	}
	return 0;
}

bool is_constant(Expression *expression);
bool is_constant_impl(Block *block) { not_implemented(); }
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
bool is_constant(Expression *expression) {
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
bool is_mutable(Expression *expression) {
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

auto println_(auto ...params) {
	scoped(stdout_mutex);
	return tl::println(params...);
}
#define println println_

volatile u32 typechecker_uid_counter;

enum class TypecheckStatus : u8 {
	suspended,
	in_progress,
	done,
};

struct TypecheckEntry {
	Node* node = 0;
	Typechecker* typechecker = 0;
	TypecheckStatus status = {};
	SpinLock lock;
	u32 last_thread_index = 0;
	SpinLock dependants_lock;
	List<TypecheckEntry *> dependants;
	bool dependency_failed = false;
};

SpinLock global_typecheck_lock;
List<TypecheckEntry> typecheck_entries;

struct Typechecker {
	const u32 uid = atomic_add(&typechecker_uid_counter, 1);

	static Typechecker *create(Node *node) {
		auto typechecker = [&] {
			if (auto typechecker_ = with(retired_typecheckers_lock, retired_typecheckers.pop())) {
				auto typechecker = typechecker_.value();
				// println("created cached typechecker {}", typechecker->uid);

				assert(typechecker->debug_stopped);
				assert(typechecker->parent_fiber == 0);
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
			reporter.print_all();
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
	struct Status {
		u32 progress = 0;
		bool waiting = false;
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

	void get_total_progress(TypecheckProgress& progress) {
		progress.clear();

		for (auto &entry : typecheck_entries) {
			auto typechecker = entry.typechecker;
			if (typechecker)
				progress.add(typechecker->status);
			else
				progress.add({ .waiting = false });
		}
	}

	bool has_progress(TypecheckProgress old, TypecheckProgress now) {
		// NOTE: yield_while does not query the progress before the waiting loop, because if 
		//       predicate fails the first time, another query will be made almost immediately.
		if (!old.count)
			return true;

		for (auto status : now) {
			if (!status.waiting) {
				return true;
			}
		}

		assert(old.count == now.count);

		for (umm i = 0; i < old.count; ++i) {
			if (!now[i].waiting || old[i].waiting) {
				return true;
			}
			if (now[i].progress > old[i].progress) {
				return true;
			}
		}

		return false;
	}

	[[nodiscard]]
	bool yield_while(String location, auto predicate) {
		// TODO: Make this more robust. I'm not sure this will never fail to typecheck a valid program.

		TypecheckProgress old_progress;
		TypecheckProgress current_progress;

		scoped_replace(status.waiting, true);

		// TODO: FIXME: Get rid of the limits.
		const int wait_sleep_limit = 16;
		const int wait_fatal_limit = INT_MAX;

		int iteration = 0;
		while (true) {
			if (predicate()) {
				if (entry->dependency_failed) {
					return false;
				}

				if (iteration >= wait_sleep_limit) {
					// HACK: This feels bad, but it doesn't fail as much.
					sleep_nanoseconds(1000);
				}
				if (iteration >= wait_fatal_limit) {
					debug_break();
					return false;
				}
				++iteration;

				get_total_progress(current_progress);
				if (!has_progress(old_progress, current_progress)) {
					debug_break();
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

	void yield(YieldResult result) {
		yield_result = result;
		fiber_yield(parent_fiber);
	}

	void fiber_main() {
		while (true) {
			auto success = typecheck(initial_node);
			yield(success ? YieldResult::success : YieldResult::fail);
		}
	}

	bool typecheck(Node *node) {
		++status.progress;
		defer{ ++status.progress; };

		switch (node->kind) {
#define x(name) case NodeKind::name: if (!typecheck_impl((##name *)node)) return false; break;
			ENUMERATE_NODE_KIND(x)
#undef x
			default:
				invalid_code_path();
		}

		if (auto expression = as<Expression>(node)) {
			if (!expression->type) {
				reporter.error(expression->location, "Could not compute the type of this expression.");
				return false;
			}
		}
		return true;
	}
	bool typecheck_impl(Block *block) {
		scoped_replace(current_block, block);
		for (auto child : block->children) {
			if (!typecheck(child))
				return false;
		}

		if (block->children.count) {
			if (auto last_expression = as<Expression>(block->children.back()))
				block->type = last_expression->type;
		}

		if (!block->type)
			block->type = get_builtin_type(BuiltinTypeKind::None);

		return true;
	}
	bool typecheck_impl(Definition *definition) {
		if (definition->parsed_type) {
			if (!typecheck(definition->parsed_type))
				return false;
		} else {
			assert(definition->initial_value);
		}

		if (definition->initial_value) {
			if (!typecheck(definition->initial_value))
				return false;

			if (definition->parsed_type) {
				reporter.warning(definition->location, "TODO: Make sure parsed_type and initial_value->type match");
			}

			switch (definition->mutability) {
				case Mutability::constant: {
					if (!is_constant(definition->initial_value)) {
						reporter.error(definition->location, "Definition marked constant, but its initial value is not.");
						return false;
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

		if (definition->parsed_type) {
			definition->type = definition->parsed_type;
		} else {
			definition->type = definition->initial_value->type;
		}

		return true;
	}
	bool typecheck_impl(IntegerLiteral *literal) {
		literal->type = get_builtin_type(BuiltinTypeKind::S64);
		return true; 
	}
	bool typecheck_impl(BooleanLiteral *literal) {
		literal->type = get_builtin_type(BuiltinTypeKind::Bool);
		return true;
	}
	bool typecheck_impl(LambdaHead *head) {
		if (!typecheck(&head->parameters_block))
			return false;

		if (head->return_type) {
			if (!typecheck(head->return_type))
				return false;
		}

		head->type = get_builtin_type(BuiltinTypeKind::Type);

		return true;
	}
	bool typecheck_impl(Lambda *lambda) {
		if (!typecheck_impl(&lambda->head))
			return false;

		lambda->type = &lambda->head;

		if (lambda->body) {
			scoped_replace(current_block, &lambda->head.parameters_block);

			if (!typecheck(lambda->body))
				return false;

			if (!lambda->head.return_type) {
				reporter.warning(lambda->location, "TODO: proper return type deduction. For now using body type {}.", lambda->body->type);
				lambda->head.return_type = lambda->body->type;
			}
		} else {
			if (!lambda->head.return_type) {
				lambda->head.return_type = get_builtin_type(BuiltinTypeKind::None);
			}
		}

		assert(lambda->head.return_type);
		return true;
	}
	bool typecheck_impl(Name *name) {
		for (auto block = current_block; block; block = block->parent) {
			auto definitions = block->definition_map.get_or_insert(name->name);
			if (definitions.count == 1) {
				name->definition = definitions[0];

				if (block == &global_block) {
					for (auto &typecheck_entry : typecheck_entries) {
						if (typecheck_entry.node == name->definition) {
							scoped(typecheck_entry.dependants_lock);
							typecheck_entry.dependants.add(entry);
							break;
						}
					}

				}
				
				if (!yield_while_null(name->location, &name->definition->type)) {
					reporter.error(name->location, "Couldn't wait for definition type.");
					return false;
				}

				name->type = name->definition->type;
				return true;
			}

			if (definitions.count == 0) {
				continue;
			}

			reporter.error(name->location, "`{}` is ambiguous.", name->name);
			for (auto definition : definitions) {
				reporter.info(definition->location, "Declared here:");
			}
			return false;
		}
		reporter.error(name->location, "`{}` was not declared.", name->name);
		return false;
	}
	bool typecheck_impl(Return *return_) {
		if (return_->value)
			if (!typecheck(return_->value))
				return false;

		return true;
	}
	bool typecheck_impl(Call *call) {
		if (!typecheck(call->callable))
			return false;
		
		auto &arguments = call->arguments;
		for (auto argument : arguments) {
			if (!typecheck(argument))
				return false;
		}

		auto lambda = direct_as<Lambda>(call->callable);
		assert(lambda, "Other stuff not implemented");
		assert(lambda->head.return_type, "TODO: wait here");

		auto &parameters = lambda->head.parameters_block.definition_list;

		if (arguments.count != parameters.count) {
			reporter.error(call->location, "Too {} arguments. Expected {}, but got {}.", arguments.count > parameters.count ? "much"s : "few"s, parameters.count, arguments.count);
			reporter.info(lambda->location, "Lamda is here:");
			return false;
		}

		call->type = lambda->head.return_type;

		return true;
	}
	bool typecheck_impl(If *If) {
		if (!typecheck(If->condition))
			return false;

		if (!typecheck(If->true_branch))
			return false;

		if (If->false_branch)
			if (!typecheck(If->false_branch))
				return false;

		reporter.warning(If->location, "TODO: proper type deduction");
		if (If->false_branch) {
			if (auto true_branch_expression = as<Expression>(If->true_branch)) {
				if (auto false_branch_expression = as<Expression>(If->false_branch)) {
					If->type = true_branch_expression->type;
				}
			}
		}
		if (!If->type)
			If->type = get_builtin_type(BuiltinTypeKind::None);

		return true;
	}
	bool typecheck_impl(While *While) {
		if (!typecheck(While->condition))
			return false;

		if (auto builtin_type = direct_as<BuiltinType>(While->condition->type); !builtin_type || builtin_type->type_kind != BuiltinTypeKind::Bool) {
			reporter.error(While->condition->location, "Condition type must be Bool.");
			return false;
		}

		if (!typecheck(While->body))
			return false;

		return true;
	}
	bool typecheck_impl(Continue *Continue) { return true; }
	bool typecheck_impl(Break *Break) { return true; }
	bool typecheck_impl(BuiltinType *type) { return true; }
	bool typecheck_impl(Binary *binary) {
		if (!typecheck(binary->left))
			return false;

		if (!typecheck(binary->right))
			return false;

		if (!types_match(binary->left->type, binary->right->type)) {
			reporter.error(binary->location, "No binary operation defined for types {} and {}.", binary->left->type, binary->right->type);
			return false;
		}

		switch (binary->operation) {
			case BinaryOperation::add:
			case BinaryOperation::subtract:
			case BinaryOperation::multiply:
			case BinaryOperation::divide:
				binary->type = binary->left->type;
				break;
			case BinaryOperation::equals:
			case BinaryOperation::not_equals:
			case BinaryOperation::less:
			case BinaryOperation::less_equals:
			case BinaryOperation::greater:
			case BinaryOperation::greater_equals:
				binary->type = get_builtin_type(BuiltinTypeKind::Bool);
				break;

			case BinaryOperation::assign:
				if (!is_mutable(binary->left)) {
					reporter.error(binary->left->location, "This expression is read-only.");
					return false;
				}
				binary->type = get_builtin_type(BuiltinTypeKind::None);
				break;

			default:
				invalid_code_path("Invalid binary operation {}.", binary->operation);
		}

		return true;
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
};

void init_globals() {
	construct(timed_results);
	timed_results.reserve(16);

	timed_function();

	construct(content_start_to_file_name);
	construct(retired_typecheckers);
	construct(global_block);
	construct(builtin_types);
	construct(typecheck_entries);
}

struct ParsedArguments {
	String source_name;
	u32 thread_count = 0;
};

Optional<ParsedArguments> parse_arguments(Span<Span<utf8>> args) {
	timed_function();

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
	timed_function();

#define x(name, value) \
	static BuiltinType _builtin_type_##name; \
	{ \
		auto &type = builtin_types.get_or_insert(BuiltinTypeKind::name); \
		type = &_builtin_type_##name; \
		type->type_kind = BuiltinTypeKind::name; \
		type->type = &_builtin_type_Type; \
	}
	ENUMERATE_BUILTIN_TYPES(x)
#undef x
}

s32 tl_main(Span<Span<utf8>> args) {
	defer {
		for (auto time : timed_results) {
			println("{} took {} ms", time.name, time.seconds * 1000);
		}
	};

	init_globals();
	init_builtin_types();

	auto maybe_arguments = parse_arguments(args);
	if (!maybe_arguments)
		return {};
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

	auto global_nodes = tokens_to_nodes(tokens.value());
	if (!global_nodes)
		return 0;

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
							case TypecheckStatus::suspended: {
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

								entry->status = TypecheckStatus::in_progress;
								if (!entry->typechecker) {
									entry->typechecker = Typechecker::create(entry->node);
								}

								// println("{} took {} {}", thread_index, entry->typechecker->uid, entry->node->location);
								return entry;
							}
							case TypecheckStatus::in_progress: {
								done_entries_count = 0;
								break;
							}
							case TypecheckStatus::done: {
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
						entry->status = TypecheckStatus::suspended;

						// NOTE: put waiting entry to the back of the list to give priority to others.
						//auto found = find(typecheck_entries, entry);
						//assert(found);
						//while (found < typecheck_entries.end() && found[1]->status != TypecheckStatus::suspended) {
						//	Swap(found[0], found[1]);
						//}
					} else {
						entry->typechecker->retire();
						entry->status = TypecheckStatus::done;

						if (result == YieldResult::fail) {
							failed = true;

							scoped(entry->dependants_lock);
							for (auto dependant : entry->dependants) {
								dependant->dependency_failed = true;
							}
						}
					}
				}
			}
		});
	}
	
	for (umm thread_index = 0; thread_index < thread_count; ++thread_index) {
		join(threads[thread_index]);
	}

	for (auto &entry : typecheck_entries)
		assert(entry.status == TypecheckStatus::done);

	if (failed) {
		return 1;
	}

	print_ast(&global_block);

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