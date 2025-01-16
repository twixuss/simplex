// COMPILER ERROR
// COMPILER OUTPUT `x` was declared multiple times and is ambiguous.
const main = fn () => {
    const x = 1;
    const x = 2;
    return x;
}