#pragma once
#include "common.h"

// 64 - s64
// -1 - BigInt
//  N - SignedIntWithBits<N>
#define UNSIZED_INTEGER_BITS 64


#if UNSIZED_INTEGER_BITS == -1
#include <tl/big_int.h>
#elif UNSIZED_INTEGER_BITS != 64
#include <tl/signed_int.h>
#endif

#if UNSIZED_INTEGER_BITS == 64
using UnsizedInteger = s64;
#elif UNSIZED_INTEGER_BITS == -1
using UnsizedInteger = BigInt<DefaultAllocator>;
#else
using UnsizedInteger = SignedIntWithBits<UNSIZED_INTEGER_BITS>;
#endif

using UnsizedFloat = f64;
