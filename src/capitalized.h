#pragma once
#include "common.h"

template <class T>
struct Capitalized {
	T value;
};

template <class T>
umm append(StringBuilder &builder, Capitalized<T> capitalized) {
	u8 *c = builder.last->end();
	umm result = append(builder, capitalized.value);
	*c = to_upper((char )*c);
	return result;
}
