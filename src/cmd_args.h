#pragma once
#include "common.h"
#include <tl/variant.h>

struct CmdArg {
	char const *key;
	Variant<
		void (*)(),
		void (*)(u64),
		void (*)(String)
	> run;
};
