<style>
    .wip {
        color: #F80
    }
</style>

# Simplex
## --- This project is unfinished ---
## Stuff in this readme that is not yet implemented is <span class="wip">colored like this</span>

# Syntax
## Comments
They are just like in C, except nestable.
```simplex
// Single line comment
/* Multiline comment */
/* /* Nested multiline comment */ */
```
## Whitespace, semicolons and separators
Spaces and tabs are not significant, newlines are. Semicolons are optional (required for multiple statements on a single line). Keywords `then` and `do` separate condition and body when both are on single line.

<details><summary><b>Separators example</b></summary>
Here `*` could mean dereference or multiplication:

<!--
"<pre data-role="codeBlock" data-info="simplex" class="language-simplex simplex"><code>if condition  *pointer = 42
</code></pre>"
-->

```simplex
if condition  *pointer = 42
```

To disambiguate you can:
* Use `then` keyword:
  ```simplex
  if condition then *pointer = 42
  ```
* Use braces:
  ```simplex
  if condition { *pointer = 42 }
  ```
* Put body on new line:
  ```simplex
  if condition
      *pointer = 42
  ```
</details>

## Expressions
### Primary
#### Names (Identifiers)
A sequence of characters that starts with `_` or `a-Z`, and continues with `_` or ` ` (space, experimental) or `a-Z` or `0-9`.
I'm experimenting with multi-word names. For example
`hello world` is a single identifier, like `hello_world`. If there are multiple spaces between names, they are not merged and treated as two separate tokens.
...
I think only one space should be allowed. That way there will not be different text that results in same name, which helps with search. If there are multiple spaces, treat as two separate names, this can reduce separator usage (`then`/`do`).
#### None literal
`none`
A value that represents nothing. 
Has type `None`, which is like `void` in C.
It is implicitly convertible to:
* Pointers
* <div class="wip">Options</div>
<div class="wip">
Explicitly it is convertible to any type, resulting in all bytes being set to zero, equivalent to <code>memset(&value, 0, sizeof(value))</code>
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

Type: `UnsizedInteger`, which is implicitly convertible to any sized integer type if there is no data loss. See [Unsized Integer](#unsizedinteger) for more details.

---
#### String literal
```simplex
"Hello, world!\n"
```

---
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

<p class="wip">
Definitions are *expressions*, meaning you can place them in unusual places, for example in `if` conditions:
</p>

```simplex
if let found = find(array, needle) {
    println(*found)
}
```

---
#### Block
Block is a list of statements, last of which can be an expression. Statements are executed in order. Value of last expression is the result.
```simplex
{
    statement1;
    statement2;
    result_expression;
}
```
If last statement of a block is an expression, the block itself becomes an expression:
```simplex
let foo = {
    println("something")
    42
}
```
Here "something" is printed, then `42` is assigned to `foo`.

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
### `U8` / `U16` / `U32` / `U64` / `S8` / `S16` / `S32` / `S64`
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
```

Ranges can represent a consecutive set of integers:
```simplex
var range: Range[Int] = 0..10  // 10 is not included
for it in range do println(it) // prints 0 1 2 ... 9
```
or a memory span:
```simplex
var string = .["hello", "world"]
var span: Range[*String] = strings
```
```simplex
fn get_count[T: Type](range: Range[T]) {
    range.end - range.begin
}
```
```simplex
const String = Range[*U8]
```

---
### Arrays
```simplex
[count]Int
```
`count` must be `const`. Arrays implicitly convert to [ranges](#range)

---
### Pointers
```simplex
*String
```

---
### Function types
```simplex
fn (a: Arg1, b: Arg2): ReturnType
```

---
### Aliases
`Int` = `S64`
`UInt` = `US64`

### Internal
#### `UnsizedInteger`
A signed 64-bit number
