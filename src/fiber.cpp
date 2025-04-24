#include "fiber.h"

LockProtected<GList<ReusableFiber>, SpinLock> fibers_to_reuse;
u32 allocated_fiber_count;

ReusableFiber get_new_fiber() {
	return locked_use_ret(fibers_to_reuse) {
		if (auto popped = fibers_to_reuse.pop()) {
			return popped.value();
		} else {
			atomic_increment(&allocated_fiber_count);
			return create_reusable_fiber();
		}
	};
}

void add_fiber_to_reuse(ReusableFiber fiber) {
	locked_use(fibers_to_reuse) {
		fibers_to_reuse.add(fiber);
	};
}

u32 get_allocated_fiber_count() {
	return allocated_fiber_count;
}
