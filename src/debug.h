#pragma once
#include "common.h"

template <class T>
void debug_make_readonly(Span<T> &span) {
	auto rodata = VirtualAlloc(0, span.count * sizeof(T), MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	memcpy(rodata, span.data, span.count * sizeof(T));
	VirtualProtect(rodata, span.count * sizeof(T), PAGE_READONLY, 0);
	span.data = (T *)rodata;
}

template <class T>
void debug_make_readonly(T *&t) {
	auto rodata = VirtualAlloc(0, sizeof(T), MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	memcpy(rodata, t, sizeof(T));
	VirtualProtect(rodata, sizeof(T), PAGE_READONLY, 0);
	t = (T *)rodata;
}
