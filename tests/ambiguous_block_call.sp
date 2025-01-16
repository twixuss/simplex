// COMPILER ERROR
// COMPILER OUTPUT `x` was declared multiple times and is ambiguous.

// Call expression is allowed to be overloaded.
// This test makes sure that stuff _inside_ the callable is properly reported as ambiguous
const main = fn () => {
    {fn () => {
        const x = 1;
        const x = 2;
        return x;
    }}()

    42
}