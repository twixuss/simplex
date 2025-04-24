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

extern OsLock stdout_mutex;
