#include "nameable.h"

umm append(StringBuilder &builder, Nameable<String> str) {
	char c = str.value[0];
	if (c == '_' || is_alpha(c)) {
		append(builder, c);
	} else {
		append(builder, '_');
	}
	for (auto c : str.value.skip(1)) {
		if (c == '_' || is_alpha((ascii)c) || is_digit((ascii)c)) {
			append(builder, c);
		} else {
			append(builder, '_');
		}
	}
	return str.value.count;
}
