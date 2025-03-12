<style>
    .wip {
        color: #FF0
    }
</style>

# Simplex
## --- This project is unfinished ---
## Stuff in this readme that is not yet implemented is colored with <div class="wip">yellow</div>

# Syntax
## Whitespace, semicolons and separators
Spaces and tabs are not significant, however newlines are. Semicolons are optional. Less-noisy syntax made parsing ambiguous in some cases. For that you can use expression separators like `then` and `do`.

Ambiguity example:
```simplex
if condition  *pointer = 42
    // Here `*` could mean dereference or multiplication.

    // To disambiguate you can do one of two things:
    // Use `then` keyword
if condition then *pointer = 42

    // Or put body on new line
if condition
    *pointer = 42
```
## Comments
They are just like in C, except nestable.
```simplex
// Single line comment
/* Multiline comment */
/* /* Nested multiline comment */ */
```
## Expressions
### Level 0
#### None literal
`none`
A value that represents nothing. 
Has type `None`, which is like `void` in C.
It is implicitly convertible to:
* Pointers
* <div class="wip">Options</div>
<div class="wip">
Explicitly it is convertible to any type, resulting in everything being set to zero, equivalent to <code>memset(&value, 0, sizeof(value))</code>
</div>

---
#### Boolean Literal
`true`
`false`

Type: `Bool`.

---
#### Integer Literal
`69` - decimal
`069` - invalid, use `0o` for octal
`0x45` - hexadecimal
`0o105` - octal
`0b0100_0101` - binary

Use underscore `_` as a separator.

Type: `UnsizedInteger`, which is implicitly convertible to any sized integer type if there is no data loss. There is no need for suffixes. 
<details><summary>Internal representation</summary>
`UnsizedInteger` is a signed 64-bit number, so operations on them will work accordingly.
I don't know if it is worth for the number of bits to be bigger or unlimited.
</details>

----
#### Definition
```simplex
var x = 42
const y: Int = 690
let condition = true
```
`: Type` is optional when initial value is provided.
`var` - runtime variable.
`let` - runtime immutable.
`const` - compile time constant.

---
#### Block
```simplex
{statement1; statement2; result_expression}
```
Block is a list of statements, last of which can be an expression. Statements are executed in order. Value of last expression is the result.

## Types
### `Type`
The type of all types.
<div class="wip">
Values of type <code>Type</code> store information about some type, e.g. name, size, alignment, members, arguments etc.
</div>

---
### `None`
The type of `none` literal. It is like `void` in C.
Lambdas that do not return anything have this return type.

---
### `Bool`
May have one of two values: `true` or `false`

---
### `UnsizedInteger`
Not available to the user. This is the internal type for integer literals.

---
### `U8` / `U6` / `U32` / `U64` / `S8` / `S6` / `S32` / `S64`
Two's complement integer of N bits.
If there is no way data could be lost, one is implicitly convertible to another.
Otherwise implicit attempt will result in an error.

---
### `String`
A structure like this:
```simplex
const String = struct {
    data: *U8
    count: Int
}
```

---
### <code class="wip">Range</code>
```simplex
const Range = struct [T: Type] {
    begin: T
    end: T
}

fn get_count[T: Type](range: Range[T]) {
    range.end - range.begin
}
```
```simplex
const String = Range[*U8]
```


---
### Aliases
`Int` = `S64`