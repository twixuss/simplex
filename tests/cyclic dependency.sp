// COMPILER ERROR
// COMPILER OUTPUT Cyclic dependencies detected.
// COMPILER OUTPUT All nodes in this cycle are lambdas. Recursive functions must have explicit return types. This lambda does not have one.

const main = () => {
    foo()
}

const foo = () => main()