#pragma once

#include <type_traits>
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

#if OS_WINDOWS
#define CURRENT_FUNCTION __FUNCSIG__
#else
#define CURRENT_FUNCTION __FUNCTION__
#endif

#define ASSERTION_FAILURE(cause_string, expression, ...) (\
	::assertion_failure(cause_string, expression, __FILE__, __LINE__, CURRENT_FUNCTION __VA_OPT__(,) __VA_ARGS__), \
	(BUILD_DEBUG || debugger_attached()) ? (debug_break(), 0) : (exit(-1), 0) \
)

#define ENABLE_ASSERTIONS BUILD_DEBUG

#if !ENABLE_ASSERTIONS
#define assert(...)
#endif
#define TL_DEBUG BUILD_DEBUG
#include <tl/file.h>
#include <tl/thread.h>
#include <tl/string.h>
#include <tl/cpu.h>
#include <tl/contiguous_hash_map.h>
#include <tl/hash_set.h>
#include <tl/reusable_fiber.h>
#include <tl/time.h>
#include <tl/debug.h>
#include <tl/macros.h>
#include <tl/bits.h>
#include <tl/block_list.h>
#include <tl/dynamic_lib.h>

#if OS_LINUX
#include <sys/mman.h>
#include <errno.h>
#endif

#pragma warning(error: 4996)

using namespace tl;

void assertion_failure_impl(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, Span<char> message);

inline void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function) {
	assertion_failure_impl(cause_string, expression, file, line, function, {}, {});
}

inline void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location) {
	assertion_failure_impl(cause_string, expression, file, line, function, location, {});
}

template <class ...Args>
inline void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, String location, char const *format, Args ...args) {
	assertion_failure_impl(cause_string, expression, file, line, function, location, tformat(format, args...));
}

template <class ...Args>
inline void assertion_failure(char const *cause_string, char const *expression, char const *file, int line, char const *function, char const *format, Args ...args) {
	assertion_failure_impl(cause_string, expression, file, line, function, {}, tformat(format, args...));
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

inline void append(StringBuilder &builder, Comparison c) {
	switch (c) {
		#define x(name) case Comparison::name: return append(builder, #name);
		#define y(name, value) x(name)
		ENUMERATE_COMPARISONS
		#undef y
		#undef x
	}
	append_format(builder, "((Comparison){})", (u64)c);
}

template <class Key, class Value, class Traits = DefaultHashTraits<Key>, class Allocator = Allocator>
using HashMap = tl::ContiguousHashMap<Key, Value, Traits, Allocator>;

template <class Value, class Traits = DefaultHashTraits<Value>>
using HashSet = tl::ContiguousHashMap<Value, Empty, Traits, Allocator>;

template <class T>
using GList = tl::List<T, DefaultAllocator>;

template <class Key, class Value, class Traits = DefaultHashTraits<Key>>
using GHashMap = HashMap<Key, Value, Traits, DefaultAllocator>;

template <class Value, class Traits = DefaultHashTraits<Value>>
using GHashSet = HashMap<Value, Empty, Traits, DefaultAllocator>;

inline bool operator==(String a, char const *b) {
	return a == as_utf8(as_span(b));
}

#define PASTE_CASE(x) case x:
#define PASTE_CASE_0(x) case x[0]:

#define __FILE_NAME__ ([]{auto e = __FILE__;while (*e) ++e;while (*e != '\\') --e;return e + 1;}())


inline void log_error_path(char const *file, int line, auto &&...args) {
	with(ConsoleColor::dark_yellow, print("{}:{}: ", file, line));
	println(args...);
}

#define LOG_ERROR_PATH(...) \
	if (context->enable_log_error_path) { \
		log_error_path(__FILE__, __LINE__ __VA_OPT__(,) __VA_ARGS__); \
	}

#define dbgln(...) (context->is_debugging ? println(__VA_ARGS__) : 0)

struct TimedResult {
	char const *name = 0;
	f64 seconds = 0;
};

#define timed_block(name) \
	if (context->enable_time_log) println("{} ...", name); \
	auto timer = create_precise_timer(); \
	defer { if (context->enable_time_log) context->timed_results.add({name, elapsed_time(timer)}); }

#define timed_function() \
	static constexpr auto funcname = __FUNCTION__; \
	timed_block(funcname)

#define timed_expression_named(name, expression) \
	[&] { \
		timed_block(name); \
		return expression; \
	}()

#define timed_expression(expression) timed_expression_named(#expression, expression)

#define locked_use_it(protected, expr) protected.use([&](auto &it) { return expr; })


struct CompilerContext {
	String compiler_path;
	String compiler_bin_directory;
	String compiler_root_directory;
	String generated_source_directory;
	String input_source_path;

	GList<TimedResult> timed_results;
	
	OsLock stdout_mutex;

	bool constant_name_inlining            : 1 = true;
	bool print_uids                        : 1 = false;
	bool report_yields                     : 1 = false;
	bool enable_time_log                   : 1 = false;
	bool is_debugging                      : 1 = false;
	bool print_tokens                      : 1 = false;
	bool print_wait_failures               : 1 = false;
	bool enable_log_error_path             : 1 = false;
	bool break_on_error                    : 1 = false;
	bool run_compiled_code                 : 1 = false;
	bool print_stats                       : 1 = false;
	bool should_print_ast                  : 1 = false;
	bool run_interactive                   : 1 = false;
	bool should_inline_unspecified_lambdas : 1 = false;
	
	u32 requested_thread_count = 0;
	u32 nested_reports_verbosity = 1;

};

extern CompilerContext *context;