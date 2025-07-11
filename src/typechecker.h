#pragma once
#include "common.h"
#include "nodes_fwd.h"
#include "reporter.h"
#include "binary_operation.h"
#include "type.h"
#include "debug.h"

#ifndef TYPECHECKER_GLOBAL
#define TYPECHECKER_GLOBAL extern
#endif

struct Value;
struct Typechecker;

enum class TypecheckEntryStatus : u8 {
	unstarted,
	unfinished,
	succeeded,
	failed,
};

struct TypecheckEntry {
	Node* node = 0;
	Typechecker* typechecker = 0;
	TypecheckEntryStatus status = TypecheckEntryStatus::unstarted;

	TypecheckEntry *dependency = 0;
};

TYPECHECKER_GLOBAL StaticBlockList<TypecheckEntry, 256, DefaultAllocator> typecheck_entries;
TYPECHECKER_GLOBAL GHashMap<Node *, TypecheckEntry *> typecheck_entries_by_node;


struct BinaryTypecheckerKey {
	Expression *left_type = 0;
	Expression *right_type = 0;
	BinaryOperation operation = {};

	constexpr auto operator<=>(BinaryTypecheckerKey const &) const = default;
};

template <>
inline u64 get_hash(BinaryTypecheckerKey const &key) {
	return (u64)key.left_type ^ rotate_left((u64)key.left_type, 21) ^ rotate_left((u64)key.operation, 42);
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

TYPECHECKER_GLOBAL LockProtected<GHashMap<VectorizedLambdaKey, VectorizedLambda, VectorizedLambdaKeyHashTraits>, SpinLock> vectorized_lambdas;


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

TYPECHECKER_GLOBAL LockProtected<GHashMap<VectorizedBinaryKey, VectorizedBinaryValue>, SpinLock> vectorized_binarys;


// NOTE: jmp_buf is an array alias, which forces to use memcpy. Put it in a struct to avoid that.
struct CopyableJmpBuf {
	jmp_buf buf;
};


#define ENABLE_TYPECHECKER_REUSE 0

TYPECHECKER_GLOBAL volatile u32 typechecker_uid_counter;
TYPECHECKER_GLOBAL LockProtected<GList<struct Typechecker *>, SpinLock> retired_typecheckers;
TYPECHECKER_GLOBAL LockProtected<GList<Report>, SpinLock> deferred_reports;
TYPECHECKER_GLOBAL bool no_more_progress;

void call_each_reverse(auto call) {

}
void call_each_reverse(auto call, auto ...args, auto last) {
	call(last);
	call_each_reverse(call, args...);
}

struct Typechecker {
	enum class YieldResult : u8 {
		fail,
		success,
		wait,
	};

	enum class FailStrategy {
		yield,
		unwind,
	};


	const u32 uid = atomic_add(&typechecker_uid_counter, 1);
	u32 progress = 0;

	static Typechecker *create(Node *node);
	YieldResult continue_typechecking(TypecheckEntry *entry);

	void stop();
	void retire();

private:
	struct TemplateInstantiationForReport {
		Lambda *original_lambda;
		Lambda *instantiated_lambda;
		List<Definition *> template_parameters;
	};
	
	static constexpr int fail_unwind_tag = 42;

	Fiber parent_fiber = {};
	ReusableFiber fiber = {};
	YieldResult yield_result = {};
	Reporter reporter;
	Node *initial_node = 0;
	Block *current_block = 0;
	Expression *current_container = 0;
	While *current_loop = 0;
	List<Node *> node_stack;
	TypecheckEntry *entry = 0;
	CopyableJmpBuf main_loop_unwind_point = {};
	CopyableJmpBuf current_unwind_point = {};
	bool can_generate_vectorized_lambdas = true;
	List<TemplateInstantiationForReport> template_instantiation_stack_for_reports;
	FailStrategy fail_strategy = FailStrategy::yield;

	GList<Definition *> currently_used_definitions;

	// Yield may occur while holding a global block lock.
	// Before yield fully release the lock, aquire it back after.
	u32 global_block_locks_count;

	u32 debug_thread_id = 0;
	bool debug_stopped = false;

	void debug_start();
	void debug_stop();

	[[noreturn]]
	void fail();

	//
	// Expression with_unwind_strategy
	//
	/* Example:
	
	auto thing = with_unwind_strategy {
		return typecheck(expression); // if `typecheck` fails, rest of the lambda is skipped
		                              // result of `typecheck` is assigned to `thing`, or null on failure.
	};
	
	NOTE: I tried to implement this without a callback lambda using a for loop so you can just return values, e.g.

			WITH_UNWIND_STRATEGY {
				typecheck(expression);
				return expression;
			}
			// <- go here on failure
		
	      but the problem is that for loop's "incremeter" or whatever its called is not executed after return,
		  so the previous state needs to be restored by requiring defer in the body, which in turn requires opening
		  for's body in the macro.
		  So you can't really make this a more concise and neat statement.
	*/

	struct UnwindStrategyEnabler {
		Typechecker *t = 0;
		auto operator->*(auto &&fn) -> decltype(fn()) {
			scoped_replace(t->fail_strategy, FailStrategy::unwind);
			auto saved_unwind_point = t->current_unwind_point;
			defer { 
				t->current_unwind_point = saved_unwind_point;
			};
			if (setjmp(t->current_unwind_point.buf) == fail_unwind_tag) {
				return decltype(fn()){};
			}
			return fn();
		}
	};

	#define with_unwind_strategy UnwindStrategyEnabler{this}->*[&]()

	[[nodiscard]]
	bool yield_while(String location, auto predicate) {
		while (true) {
			if (predicate()) {
				if (no_more_progress)
					return false;

				if (context_base->report_yields)
					immediate_reporter.info(location, "Yield");

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

	struct Unwind {};

	void yield(YieldResult result);

	void fiber_main();

	struct ImplicitCastResult {
		bool did_cast = true; // Most of the paths do a cast, so make it default
		bool success = false;
		Expression **expression = 0;
		Expression *target_type = 0;

		InlineFunction<void(), 32> apply;

		explicit operator bool() { return success; }
	};

	ImplicitCastResult implicitly_cast(Expression **expression, Expression *target_type);
	
	inline ImplicitCastResult implicitly_cast(Expression **expression, Expression *target_type, Reporter *reporter) {
		assert(reporter != &this->reporter, "Use this overload for different reporters, not the current one.");

		auto old_reporter = this->reporter;
		this->reporter = {};
		auto result = implicitly_cast(expression, target_type);
		reporter->reports.add(this->reporter.reports);
		this->reporter = old_reporter;
		return result;
	}
	
	inline ImplicitCastResult implicitly_cast_apply(Expression **expression, Expression *target_type) {
		auto result = implicitly_cast(expression, target_type);
		if (result) {
			result.apply();
		}
		return result;
	}

	inline ImplicitCastResult implicitly_cast_apply(Expression **expression, Expression *target_type, Reporter *reporter) {
		auto result = implicitly_cast(expression, target_type, reporter);
		if (result) {
			result.apply();
		}
		return result;
	}

	void why_is_this_immutable(Expression *expr);

	Expression *inline_body(Call *call, Lambda *lambda);
	
	Node *get_last_child_recursive(Node *node);

	Value execute(Node *node);

	Expression *make_broadcast(Expression *scalar, u64 count);
	
	struct VectorizeContext {
		Lambda *original_lambda;
		Lambda *instantiated_lambda;
		Definition *instantiated_definition;
		u64 vector_size;
		String instantiation_location;
	};

	VectorizeContext vc;

	[[nodiscard]] Block                *vectorize_node_impl(Block *node);
	[[nodiscard]] Call                 *vectorize_node_impl(Call *node);
	[[nodiscard]] Definition           *vectorize_node_impl(Definition *node);
	[[nodiscard]] Expression           *vectorize_node_impl(IntegerLiteral *node);
	[[nodiscard]] FloatLiteral         *vectorize_node_impl(FloatLiteral *node);
	[[nodiscard]] BooleanLiteral       *vectorize_node_impl(BooleanLiteral *node);
	[[nodiscard]] NoneLiteral          *vectorize_node_impl(NoneLiteral *node);
	[[nodiscard]] StringLiteral        *vectorize_node_impl(StringLiteral *node);
	[[nodiscard]] Lambda               *vectorize_node_impl(Lambda *node);
	[[nodiscard]] LambdaHead           *vectorize_node_impl(LambdaHead *node);
	[[nodiscard]] Name                 *vectorize_node_impl(Name *node);
	[[nodiscard]] Expression           *vectorize_node_impl(IfExpression *node);
	[[nodiscard]] BuiltinTypeName      *vectorize_node_impl(BuiltinTypeName *node);
	[[nodiscard]] Expression           *vectorize_node_impl(Binary *node);
	[[nodiscard]] Match                *vectorize_node_impl(Match *node);
	[[nodiscard]] Unary                *vectorize_node_impl(Unary *node);
	[[nodiscard]] Struct               *vectorize_node_impl(Struct *node);
	[[nodiscard]] ArrayType            *vectorize_node_impl(ArrayType *node);
	[[nodiscard]] Enum                 *vectorize_node_impl(Enum *node);
	[[nodiscard]] Subscript            *vectorize_node_impl(Subscript *node);
	[[nodiscard]] ArrayConstructor     *vectorize_node_impl(ArrayConstructor *node);
	[[nodiscard]] ZeroInitialized      *vectorize_node_impl(ZeroInitialized *node);
	[[nodiscard]] CallerLocation       *vectorize_node_impl(CallerLocation *node);
	[[nodiscard]] CallerArgumentString *vectorize_node_impl(CallerArgumentString *node);
	[[nodiscard]] IfStatement          *vectorize_node_impl(IfStatement *node);
	[[nodiscard]] Return               *vectorize_node_impl(Return *node);
	[[nodiscard]] While                *vectorize_node_impl(While *node);
	[[nodiscard]] For                  *vectorize_node_impl(For *node);
	[[nodiscard]] Continue             *vectorize_node_impl(Continue *node);
	[[nodiscard]] Break                *vectorize_node_impl(Break *node);
	[[nodiscard]] Import               *vectorize_node_impl(Import *node);
	[[nodiscard]] Defer                *vectorize_node_impl(Defer *node);
	[[nodiscard]] Use                  *vectorize_node_impl(Use *node);
	[[nodiscard]] Node                 *vectorize_node(Node *node);
	[[nodiscard]] Expression           *vectorize_node(Expression *expression);
	[[nodiscard]] Statement            *vectorize_node(Statement *statement);
	[[nodiscard]] VectorizedLambda get_or_instantiate_vectorized_lambda(Lambda *original_lambda, u64 vector_size, String instantiation_location);

	struct SortArgumentOptions {
		bool allow_missing = false;
	};

	void sort_arguments(GList<CallArgument> &arguments, GList<Definition *> &parameters, String call_location, Node *lambda_head_or_struct, Definition *lambda_or_struct_definition, SortArgumentOptions options = {});
	
	bool match_one_template_parameter_type(Type expression_type, Type parameter_type, Block *template_parameters);

	Expression *instantiate_lambda_template(Call *original_call, Lambda *original_lambda);

	bool should_inline(Call *call, Lambda *lambda);
	
	Struct *get_struct_template_instantiation(Struct *template_struct, Expression *argument);

	struct TypecheckLambdaCallResult {
		Expression *expression = 0;
		umm number_of_implicit_casts = 0;
	};

	// `lambda` can be null if it's a function pointer call
	TypecheckLambdaCallResult typecheck_lambda_call(Call *call, Lambda *lambda, LambdaHead *head, bool apply = true);
	Expression *typecheck_constructor(Call *call, Struct *Struct);;

	Expression *typecheck_binary_dot(Binary *binary);

	bool ensure_not_overloaded(Name *name);
	bool ensure_not_overloaded(Expression *expression);
	
	void add_defers(GList<Defer *> &defers);
	
	void ensure_mutable(Expression *expression);

	bool can_reference(Block *block, Definition *definition);

	void resolve_name_in_block(GList<Definition *> &possible_definitions, Block *block, String location, String name);
	void resolve_name(GList<Definition *> &possible_definitions, String location, String name);
	
	Definition *try_find_enum_value(Enum *Enum, Name *name);
	Definition *find_enum_value(Enum *Enum, Name *name);

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
			auto new_node_as_T = as<T>(new_node);
			assert(new_node_as_T);
			*node = new_node_as_T;
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

	[[nodiscard]] Node             *typecheck(Node *node, bool can_substitute);
	[[nodiscard]] Expression       *typecheck_impl(Block *block, bool can_substitute);
	[[nodiscard]] Definition       *typecheck_impl(Definition *definition, bool can_substitute);
	[[nodiscard]] IntegerLiteral   *typecheck_impl(IntegerLiteral *literal, bool can_substitute);
	[[nodiscard]] FloatLiteral     *typecheck_impl(FloatLiteral *literal, bool can_substitute);
	[[nodiscard]] BooleanLiteral   *typecheck_impl(BooleanLiteral *literal, bool can_substitute);
	[[nodiscard]] NoneLiteral      *typecheck_impl(NoneLiteral *literal, bool can_substitute);
	[[nodiscard]] StringLiteral    *typecheck_impl(StringLiteral *literal, bool can_substitute);
	[[nodiscard]] LambdaHead       *typecheck_impl(LambdaHead *head, bool can_substitute);
	[[nodiscard]] Lambda           *typecheck_impl(Lambda *lambda, bool can_substitute);
	[[nodiscard]] Expression       *typecheck_impl(Name *name, bool can_substitute);
	[[nodiscard]] Expression       *typecheck_impl(Call *call, bool can_substitute);
	[[nodiscard]] Node             *typecheck_impl(IfStatement *If, bool can_substitute);
	[[nodiscard]] Expression       *typecheck_impl(IfExpression *If, bool can_substitute);
	[[nodiscard]] BuiltinTypeName  *typecheck_impl(BuiltinTypeName *type, bool can_substitute);
	[[nodiscard]] Expression       *typecheck_impl(Binary *binary, bool can_substitute);
	[[nodiscard]] Match            *typecheck_impl(Match *match, bool can_substitute);
	[[nodiscard]] Expression       *typecheck_impl(Unary *unary, bool can_substitute);
	[[nodiscard]] Return           *typecheck_impl(Return *return_, bool can_substitute);
	[[nodiscard]] While            *typecheck_impl(While *While, bool can_substitute);
	[[nodiscard]] Block            *typecheck_impl(For *For, bool can_substitute);
	[[nodiscard]] Continue         *typecheck_impl(Continue *Continue, bool can_substitute);
	[[nodiscard]] Break            *typecheck_impl(Break *Break, bool can_substitute);
	[[nodiscard]] Struct           *typecheck_impl(Struct *Struct, bool can_substitute);
	[[nodiscard]] Enum             *typecheck_impl(Enum *Enum, bool can_substitute);
	[[nodiscard]] ArrayType        *typecheck_impl(ArrayType *arr, bool can_substitute);
	[[nodiscard]] Expression       *typecheck_impl(Subscript *Subscript, bool can_substitute);
	[[nodiscard]] ArrayConstructor *typecheck_impl(ArrayConstructor *arr, bool can_substitute);
	[[nodiscard]] Import           *typecheck_impl(Import *import, bool can_substitute);
	[[nodiscard]] Defer            *typecheck_impl(Defer *defer_, bool can_substitute);
	[[nodiscard]] ZeroInitialized  *typecheck_impl(ZeroInitialized *zi, bool can_substitute);
	[[nodiscard]] CallerLocation   *typecheck_impl(CallerLocation *cl, bool can_substitute);
	[[nodiscard]] CallerArgumentString *typecheck_impl(CallerArgumentString *cl, bool can_substitute);
	[[nodiscard]] Use              *typecheck_impl(Use *Use, bool can_substitute);
public:
	/////////////////////////
	// Binary Typecheckers //
	/////////////////////////
	
	inline static LockProtected<GHashMap<BinaryTypecheckerKey, Expression *(Typechecker::*)(Binary *)>, SpinLock> binary_typecheckers;

	Expression *bt_take_left(Binary *binary);
	Expression *bt_set_bool(Binary *binary);
	Expression *bt_equ(Binary *binary);
	Expression *bt_neq(Binary *binary);
	Expression *bt_unsized_int_and_sized_int_math(Binary *binary);
	Expression *bt_unsized_int_and_sized_int_comp(Binary *binary);
	Expression *bt_unsized_int(Binary *binary);
	Expression *bt_sized_int_modify_ass_unsized_int(Binary *binary);
	Expression *bt_unsized_float_and_sized_float_math(Binary *binary);
	Expression *bt_unsized_float_and_sized_float_comp(Binary *binary);
	Expression *bt_unsized_float(Binary *binary);
	Expression *bt_sized_float_modify_ass_unsized_float(Binary *binary);
	Expression *bt_enum_and_some_enum(Binary *binary);

	template <bool invert>
	Expression *bt_comp_Type(Binary *binary);

	template <auto operation>
	Expression *bt_math_opt(Binary *binary);

	static void init_binary_typecheckers();
};

u64 get_typechecking_progress();
