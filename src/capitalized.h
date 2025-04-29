#pragma once
#include "common.h"

template <class T>
struct Capitalized {
	T value;
};

template <class T>
void append(StringBuilder &builder, Capitalized<T> capitalized) {
	scoped(temporary_allocator_and_checkpoint);

	StringBuilder tmp;
	tmp.allocator = current_temporary_allocator;

	append(tmp, capitalized.value);

	if (tmp.first.count) {
		auto c = tmp.first.data();
		*c = to_upper((char )*c);

		append(builder, tmp);
	}
}
