// COMPILER ERROR
// COMPILER OUTPUT Cyclic dependencies detected.
// COMPILER OUTPUT All of the lambdas in this cycle are marked as `inline`. All-inline recursion leads to infinite body substitution, hence not allowed. At least one lambda in this cycle must be not inline
// COMPILER OUTPUT foo depends on bar
// COMPILER OUTPUT bar depends on foo

inline fn foo(): None => {
    bar()
}
inline fn bar(): None => {
    foo()
}
fn main() {
    bar()
}
