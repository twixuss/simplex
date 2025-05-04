#include "fiber.h"

ReusableFiber get_new_fiber() {
	auto &fibers_to_reuse = context_base->fibers_to_reuse;
	return locked_use_ret(fibers_to_reuse) {
		if (auto popped = fibers_to_reuse.pop()) {
			return popped.value();
		} else {
			atomic_increment(&context_base->allocated_fiber_count);
			return create_reusable_fiber();
		}
	};
}

void add_fiber_to_reuse(ReusableFiber fiber) {
	auto &fibers_to_reuse = context_base->fibers_to_reuse;
	locked_use(fibers_to_reuse) {
		fibers_to_reuse.add(fiber);
	};
}

u32 get_allocated_fiber_count() {
	return context_base->allocated_fiber_count;
}
