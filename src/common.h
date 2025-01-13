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

void print_crash_info();

#if OS_WINDOWS
#define CURRENT_FUNCTION __FUNCSIG__
#else
#define CURRENT_FUNCTION __FUNCTION__
#endif

#define ASSERTION_FAILURE(cause_string, expression, ...) (\
	::assertion_failure(cause_string, expression, __FILE__, __LINE__, CURRENT_FUNCTION __VA_OPT__(,) __VA_ARGS__), \
	print_crash_info(), \
	(BUILD_DEBUG || debugger_attached()) ? (debug_break(), 0) : (exit(-1), 0) \
)

#define ENABLE_ASSERTIONS BUILD_DEBUG

#if !ENABLE_ASSERTIONS
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
