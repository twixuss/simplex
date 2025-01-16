// COMPILER ERROR
// COMPILER OUTPUT Cyclic dependencies detected.
// COMPILER OUTPUT All nodes in this cycle are lambdas. Recursive functions must have explicit return types. This lambda does not have one.
// COMPILER OUTPUT main depends on foo
// COMPILER OUTPUT foo depends on bar
// COMPILER OUTPUT bar depends on main

const main = fn () => foo()
const foo = fn () => bar()
const bar = fn () => main()