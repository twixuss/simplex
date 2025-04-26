#pragma once
#include "common.h"

template <class T>
struct Nameable {
	T value = {};
};

umm append(StringBuilder &builder, Nameable<String> str);
