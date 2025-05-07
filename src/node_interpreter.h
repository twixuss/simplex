#pragma once
#include "common.h"
#include "fiber.h"
#include "nodes_fwd.h"
#include "value.h"
#include "reporter.h"

#include <tl/bucket_hash_map.h>

extern bool no_more_progress;

struct NodeInterpreter {
	enum class YieldResult : u8 {
		fail,
		success,
		wait,
	};

	struct Scope {
		// This needs to be pointer-stable
		BucketHashMap<Definition *, Value> variables;
	};

	static NodeInterpreter *create(Node *node);

	Result<Value, YieldResult> run();

private:

	[[nodiscard]]
	bool yield_while(String location, auto predicate) {
		while (true) {
			if (predicate()) {
				if (context_base->report_yields)
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

	[[nodiscard]] 
	bool yield_while_null(String location, auto *pointer) {
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

	HashMap<String, Dll> loaded_extern_libraries;

	void fiber_main();

	void yield(YieldResult result);

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

	Value load_address(Expression *expression);
	
	#define x(name) Value load_address_impl(name *); 
	ENUMERATE_EXPRESSION_KIND(x)
	#undef x
		
	Value execute(Node * node); 

	#define x(name) Value execute_impl(name *); 
	ENUMERATE_NODE_KIND(x)
	#undef x
};

#undef PERFORM_WITH_BREAKS
#undef EXECUTE_INTO
#undef EXECUTE_DEFN
#undef LOAD_ADDRESS_INTO
#undef LOAD_ADDRESS_DEFN
