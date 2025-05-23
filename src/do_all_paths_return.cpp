#include "do_all_paths_return.h"
#include "nodes.h"
#include "x.h"

bool do_all_paths_return(Node *node);
bool do_all_paths_return_impl(Block *block) {
	for (auto child : block->children) {
		if (do_all_paths_return(child)) {
			return true;
		}
	}
	return false;
}
bool do_all_paths_return_impl(Call *call) {
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
bool do_all_paths_return_impl(Definition *definition) {
	if (definition->initial_value) {
		if (do_all_paths_return(definition->initial_value)) {
			return true;
		}
	}
	return false;
}
bool do_all_paths_return_impl(IntegerLiteral *literal) { return false; }
bool do_all_paths_return_impl(BooleanLiteral *literal) { return false; }
bool do_all_paths_return_impl(NoneLiteral *literal) { return false; }
bool do_all_paths_return_impl(StringLiteral *literal) { return false; }
bool do_all_paths_return_impl(Lambda *lambda) { return false; }
bool do_all_paths_return_impl(LambdaHead *head) { return false; }
bool do_all_paths_return_impl(Name *name) { return false; }
bool do_all_paths_return_impl(IfExpression *If) {
	if (do_all_paths_return(If->condition)) {
		return true;
	}
	if (do_all_paths_return(If->true_branch) && do_all_paths_return(If->false_branch)) {
		return true;
	}
	return false;
}
bool do_all_paths_return_impl(BuiltinTypeName *name) { return false; }
bool do_all_paths_return_impl(Binary *bin) {
	if (do_all_paths_return(bin->left)) {
		return true;
	}
	if (do_all_paths_return(bin->right)) {
		return true;
	}
	return false;
}
bool do_all_paths_return_impl(Match *match) {
	if (do_all_paths_return(match->expression)) {
		return true;
	}

	if (match->default_case) {
		int n = 0;

		for (auto Case : match->cases) {
			for (auto from : Case.froms) {
				if (do_all_paths_return(from)) {
					++n;
					goto continue_outer;
				}
			}
			if (do_all_paths_return(Case.to)) {
				++n;
				goto continue_outer;
			}
		continue_outer:;
		}
		return false;
	} else {
		// TODO: figure out what to do in that case.
		//       don't want to be annoying and force a default case.
		return false;
	}
}
bool do_all_paths_return_impl(Unary *un) {
	if (do_all_paths_return(un->expression)) {
		return true;
	}
	return false;
}
bool do_all_paths_return_impl(Struct *Struct) { return false; }
bool do_all_paths_return_impl(Enum *Enum) { return false; }
bool do_all_paths_return_impl(ArrayType *arr) {
	if (do_all_paths_return(arr->count_expression)) {
		return true;
	}
	if (do_all_paths_return(arr->element_type)) {
		return true;
	}
	return false;
}
bool do_all_paths_return_impl(Subscript *sub) {
	if (do_all_paths_return(sub->subscriptable)) {
		return true;
	}
	if (do_all_paths_return(sub->index)) {
		return true;
	}
	return false;
}
bool do_all_paths_return_impl(ArrayConstructor *arr) {
	for (auto element : arr->elements) {
		if (do_all_paths_return(element)) {
			return true;
		}
	}
	return false;
}
bool do_all_paths_return_impl(IfStatement *If) {
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
bool do_all_paths_return_impl(Return *node) {
	return true;
}
bool do_all_paths_return_impl(While *node) {
	// TODO: check constant condition
	return do_all_paths_return(node->condition);
}
bool do_all_paths_return_impl(Continue *node) { return false; }
bool do_all_paths_return_impl(Break *node) { return false; }
bool do_all_paths_return_impl(Import *node) { return false; }
bool do_all_paths_return_impl(Defer *node) { return false; }
bool do_all_paths_return_impl(ZeroInitialized *zi) { return false; }
bool do_all_paths_return_impl(CallerLocation *) { return false; }
bool do_all_paths_return_impl(CallerArgumentString *) { return false; }
bool do_all_paths_return_impl(For *For) {
	return do_all_paths_return(For->range);
}
bool do_all_paths_return(Node *node) {
	switch (node->kind) {
		#define x(name) case NodeKind::name: return do_all_paths_return_impl((name *)node);
		ENUMERATE_NODE_KIND(x)
		#undef x
	}
	invalid_code_path("invalid node kind {}", node->kind);
	return false;
}
